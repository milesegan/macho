;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: HTML-TEMPLATE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/html-template/template.lisp,v 1.19 2006/04/03 08:35:53 edi Exp $

;;; Copyright (c) 2003-2006, Dr. Edmund Weitz. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package #:html-template)

(defmacro with-use-value-restart ((symbol) error-form)
  "Provide a USE-VALUE restart for ERROR-FORM in case the value
associated with SYMBOL isn't to our liking."
  `(restart-case
      ,error-form
    (use-value (other-value)
      :report (lambda (stream)
                (format stream
                        "Use another value for symbol ~S: "
                        ,symbol))
      :interactive (lambda ()
                     (format t
                             "Enter another value for symbol ~S: "
                             ,symbol)
                     (multiple-value-list (eval (read))))
      other-value)))
    

(defun create-simple-printer (string-list &optional symbol/pathname (next-fn #'no-values))
  "Used internally to create template printers for TMPL_VAR,
TMPL_INCLUDE, and for strings which don't include template
tags. SYMBOL/PATHNAME is the symbol or pathname associated with the
tag. NEXT-FN is the next function to be called in the chain of
closures. STRING-LIST is a list of strings in reverse order to be
printed first."
  (let ((string (list-to-string string-list)))
    (etypecase symbol/pathname
      (null
        ;; no tag, just print STRING
        (lambda (values)
          (write-string string *template-output*)
          (funcall next-fn values)))
      (symbol
        ;; TMPL_VAR tag
        (lambda (values)
          (write-string string *template-output*)
          (write-string (let ((value (funcall *value-access-function*
                                              symbol/pathname values)))
                          (typecase value
                            (null
                              (if *convert-nil-to-empty-string*
                                ""
                                (with-use-value-restart (symbol/pathname)
                                  (signal-template-missing-value-error
                                   "Value for symbol ~S is NIL"
                                   symbol/pathname))))
                            (string
                              value)
                            (otherwise
                              (with-use-value-restart (symbol/pathname)
                                (error 'template-not-a-string-error
                                       :value value
                                       :format-control "Value ~S for symbol ~S is not a string"
                                       :format-arguments (list value symbol/pathname))))))
                        *template-output*)
          (funcall next-fn values)))
      (pathname
        ;; TMPL_INCLUDE tag
        (lambda (values)
          (write-string string *template-output*)
          (funcall (car (gethash symbol/pathname *printer-hash*)) values)
          (funcall next-fn values))))))
  
(defun create-if-printer (string-list symbol if-fn else-fn next-fn unlessp)
  "Used internally to create template printers for TMPL_IF and
TMPL_UNLESS tags. SYMBOL is the symbol associated with the tag.  IF-FN
is the printer for the IF branch, ELSE-FN is the printer for the ELSE
branch.  NEXT-FN is the next function to be called in the chain of
closures.  STRING-LIST is a list of strings in reverse order to be
printed first.  If UNLESSP is true, IF-FN and ELSE-FN are switched."
  (let ((string (list-to-string string-list)))
    (when unlessp
      (rotatef if-fn else-fn))
    (lambda (values)
      (write-string string *template-output*)
      (if (funcall *value-access-function* symbol values)
        (funcall if-fn values)
        (funcall else-fn values))
      (funcall next-fn values))))

(defun create-loop-printer (string-list symbol loop-fn next-fn)
  "Used internally to create template printers for TMPL_LOOP
tags. SYMBOL is the symbol associated with the tag. LOOP-FN is the
template printer for the body of the loop. NEXT-FN is the next
function to be called in the chain of closures. STRING-LIST is a list
of strings in reverse order to be printed first."
  (let ((string (list-to-string string-list)))
    (cond (*sequences-are-lists*
            (lambda (values)
              (write-string string *template-output*)
              (dolist (value (funcall *value-access-function*
                                      symbol values))
                (funcall loop-fn value))
              (funcall next-fn values)))
          (t
            (lambda (values)
              (write-string string *template-output*)
              (loop for value across (funcall *value-access-function*
                                              symbol values)
                    do (funcall loop-fn value))
              (funcall next-fn values))))))

(defun create-template-printer-aux (string-stack end-token)
  "Reads from *STANDARD-INPUT* and returns a template printer from
what it reads.  When this function is entered the stream pointer must
not be inside a template tag.  STRING-STACK is a list of strings (in
reverse order) read so far which haven't been used to build a template
printer.  END-TOKEN is either NIL or one of :LOOP, :IF, :IF-ELSE, or
:UNLESS-ELSE denoting that we expect certain tags to close open
TMPL_LOOP, TMPL_IF, or TMPL_UNLESS tags.  This function returns a
second value which is true if, after reading TMPL_IF or TMPL_UNLESS, a
corresponding TMPL_ELSE was seen."
  (let* ((string
           ;; read text up to the next template start marker
           (read-until *template-start-marker*
                       ;; don't skip it, return it
                       :skip nil
                       :eof-action (lambda (collector)
                                     (when end-token
                                       ;; make sure we don't accept
                                       ;; EOF if there are still tags
                                       ;; waiting to be closed
                                       (signal-template-syntax-error
                                        "Unexpected EOF, ~A tag is missing"
                                        (case end-token
                                          ((:loop) "<!-- /TMPL_LOOP -->")
                                          ((:if :if-else) "<!-- /TMPL_IF -->")
                                          ((:unless :unless-else) "<!-- /TMPL_UNLESS -->"))))
                                     ;; otherwise (EOF before another
                                     ;; start marker was seen) just
                                     ;; return a template printer
                                     ;; which unconditionally prints
                                     ;; the rest of the stream
                                     (return-from create-template-printer-aux
                                       (create-simple-printer
                                        (cons collector string-stack))))))
         (whitespace
           ;; skip whitespace but keep it in case this turns out not
           ;; to be a template tag
           (skip-whitespace :skip nil))
         (token
           ;; read what could be a template token's name
           (with-syntax-error-location ()
             (read-while (lambda (c)
                           (or (alpha-char-p c)
                               (char= c #\_)
                               (char= c #\/)))
                         :skip nil
                         :eof-action (lambda (collector)
                                       (declare (ignore collector))
                                       ;; complain about tags which
                                       ;; haven't been closed
                                       (signal-template-syntax-error
                                        "EOF while inside of tag starting with ~S"
                                        *template-start-marker*))))))
    (cond ((string-equal token "TMPL_INCLUDE")
            ;; TMPL_INCLUDE tag - first read the pathname which has to
            ;; follow and merge it with *DEFAULT-TEMPLATE-PATHNAME*
            (let* ((pathname (read-tag-rest :read-attribute t :intern nil))
                   (merged-pathname
                     (merge-pathnames pathname
                                      *default-template-pathname*)))
              (when (member merged-pathname *included-files*
                            :test #'equal)
                ;; raise an error if this file has been included
                ;; before - infinite recursion ahead!
                (with-syntax-error-location ()
                  (signal-template-syntax-error
                   "Infinite recursion - file ~S includes itself"
                   merged-pathname)))
              ;; otherwise create (and cache) a template printer
              (create-template-printer merged-pathname)
              (multiple-value-bind (next-fn else-follows)
                  ;; first we recursively create the template printer
                  ;; for the rest of the stream
                  (create-template-printer-aux (skip-trailing-whitespace)
                                               end-token)
                ;; then we combine it with the strings before the tag
                ;; to create a template printer for TMPL_INCLUDE
                (values
                 (create-simple-printer (cons (skip-leading-whitespace string)
                                              string-stack)
                                        merged-pathname
                                        next-fn)
                 else-follows))))
          ((string-equal token "TMPL_VAR")
            ;; TMPL_VAR tag - first read the symbol which has to
            ;; follow and intern it
            (let ((symbol (read-tag-rest :read-attribute t)))
              (multiple-value-bind (next-fn else-follows)
                  ;; first we recursively create the template printer
                  ;; for the rest of the stream
                  (create-template-printer-aux nil end-token)
                (values
                 ;; then we combine it with the strings before the tag
                 ;; to create a template printer for TMPL_VAR - note
                 ;; that we don't skip leading and trailing whitespace
                 ;; here
                 (create-simple-printer (cons string string-stack)
                                        symbol
                                        next-fn)
                 else-follows))))
          ((string-equal token "TMPL_LOOP")
            ;; TMPL_LOOP tag - first read the symbol which has to
            ;; follow and intern it
            (let* ((symbol (read-tag-rest :read-attribute t))
                   ;; then read the stream up to the corresponding
                   ;; /TMPL_LOOP and create a template printer for the
                   ;; loop body
                   (loop-fn (with-syntax-error-location ()
                              (create-template-printer-aux
                               (skip-trailing-whitespace)
                               ;; this argument denotes that we expect
                               ;; to see /TMPL_LOOP and want to stop
                               ;; there
                               :loop))))
              (multiple-value-bind (next-fn else-follows)
                  ;; now we recursively create the template printer
                  ;; for the rest of the stream
                  (create-template-printer-aux (skip-trailing-whitespace)
                                               end-token)
                (values
                 ;; then we combine it with the strings before the tag
                 ;; and the body printer to create a template printer
                 ;; for TMPL_LOOP
                 (create-loop-printer (cons (skip-leading-whitespace string)
                                            string-stack)
                                      symbol
                                      loop-fn
                                      next-fn)
                 else-follows))))
          ((string-equal token "/TMPL_LOOP")
            (unless (eq end-token :loop)
              ;; check if we expected /TMPL_LOOP here, i.e. if an open
              ;; TMPL_LOOP was pending
              (with-syntax-error-location ()
                (signal-template-syntax-error "Unexpected /TMPL_LOOP")))
            ;; read the rest of the tag but ignore it - no attributes
            ;; expected
            (read-tag-rest)
            ;; just create a simple template printer for strings -
            ;; this is the end of some TMPL_LOOP body
            (create-simple-printer (cons (skip-leading-whitespace string)
                                         string-stack)))
          ((or (string-equal token "TMPL_IF")
               (string-equal token "TMPL_UNLESS"))
            ;; TMPL_IF or TMPL_UNLESS tag - first read the symbol
            ;; which has to follow and intern it
            (let ((symbol (read-tag-rest :read-attribute t))
                  (unlessp (string-equal token "TMPL_UNLESS")))
              (multiple-value-bind (if-fn else-follows)
                  (with-syntax-error-location ()
                    ;; then read the stream up to the corresponding
                    ;; TMPL_ELSE, /TMPL_IF, or /TMPL_UNLESS and create
                    ;; a template printer for the "if" (or "unless") part
                    (create-template-printer-aux
                     (skip-trailing-whitespace)
                     ;; this argument denotes that we expect to see
                     ;; TMPL_ELSE _or_ one of /TMPL_IF, /TMPL_UNLESS and,
                     ;; in the second case, want to stop there
                     (if unlessp :unless-else :if-else)))
                (let ((else-fn (if else-follows
                                 ;; if we encountered TMPL_ELSE read
                                 ;; the stream up to the corresponding
                                 ;; /TMPL_IF or /TMPL_UNLESS and
                                 ;; create a template printer for the "else" part
                                 (with-syntax-error-location ()
                                   (create-template-printer-aux
                                    (skip-trailing-whitespace)
                                    ;; this argument denotes that we
                                    ;; expect to see /TMPL_IF or /TMPL_UNLESS
                                    ;; (but not TMPL_ELSE) and want to stop
                                    ;; there
                                    (if unlessp :unless :if)))
                                 ;; use a dummy printer for the "else"
                                 ;; part if we didn't see TMPL_ELSE
                                 #'no-values)))
                  (multiple-value-bind (next-fn else-follows)
                      ;; now we recursively create the template printer
                      ;; for the rest of the stream
                      (create-template-printer-aux (skip-trailing-whitespace)
                                                   end-token)
                    (values
                     ;; then we combine it with the strings before the
                     ;; tag and the "if" and "else" parts to create a
                     ;; template printer for TMPL_IF or TMPL_UNLESS
                     (create-if-printer (cons (skip-leading-whitespace string)
                                              string-stack)
                                        symbol
                                        if-fn
                                        else-fn
                                        next-fn
                                        unlessp)
                     else-follows))))))
          ((string-equal token "TMPL_ELSE")
            (unless (member end-token '(:if-else :unless-else))
              ;; check if we expected /TMPL_ELSE here, i.e. if an open
              ;; TMPL_IF or TMPL_UNLESS was pending and we haven't
              ;; seen TMPL_ELSE before
              (with-syntax-error-location ()
                (signal-template-syntax-error "Unexpected TMPL_ELSE")))
            ;; read the rest of the tag but ignore it - no attributes
            ;; expected
            (read-tag-rest)
            ;; just create a simple template printer for strings -
            ;; this is the end of some "if" part
            (values
             (create-simple-printer (cons (skip-leading-whitespace string)
                                          string-stack))
             ;; return a true second value to denote that we've seen
             ;; TMPL_ELSE
             t))
          ((string-equal token "/TMPL_IF")
            (unless (or (eq end-token :if) (eq end-token :if-else))
              ;; check if we expected /TMPL_IF here, i.e. if an open
              ;; TMPL_IF was pending
              (with-syntax-error-location ()
                (signal-template-syntax-error "Unexpected /TMPL_IF")))
            ;; read the rest of the tag but ignore it - no attributes
            ;; expected
            (read-tag-rest)
            ;; just create a simple template printer for strings -
            ;; this is the end of some "if" or "else" part
            (create-simple-printer (cons (skip-leading-whitespace string)
                                         string-stack)))
          ((string-equal token "/TMPL_UNLESS")
            (unless (or (eq end-token :unless) (eq end-token :unless-else))
              ;; check if we expected /TMPL_UNLESS here, i.e. if an open
              ;; TMPL_UNLESS was pending
              (with-syntax-error-location ()
                (signal-template-syntax-error "Unexpected /TMPL_UNLESS")))
            ;; read the rest of the tag but ignore it - no attributes
            ;; expected
            (read-tag-rest)
            ;; just create a simple template printer for strings -
            ;; this is the end of some "unless" or "else" part
            (create-simple-printer (cons (skip-leading-whitespace string)
                                         string-stack)))
          (t
            ;; we couldn't identify a valid tag, so we treat
            ;; everything we've read so far as a literal string and
            ;; carry on - if we're lucky our CL implementation will
            ;; optimize this tail call into an iterative loop
            (create-template-printer-aux
             (cons token
                   (cons whitespace
                         (cons *template-start-marker*
                               (cons string string-stack))))
             end-token)))))

(defun %create-template-printer-aux (&rest args)
  "Wrapper for CREATE-TEMPLATE-PRINTER-AUX to initialize
*CURRENT-COLUMN* and *CURRENT-LINE*."
  (let ((*current-column* 0)
        (*current-line* 1))
    (apply #'create-template-printer-aux args)))