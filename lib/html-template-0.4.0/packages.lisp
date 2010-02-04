;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/html-template/packages.lisp,v 1.15 2006/01/03 18:41:44 edi Exp $

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

(in-package #:cl-user)

#-:cormanlisp
(defpackage #:html-template
  (:nicknames #:template)
  (:use #:cl)
  (:export #:*convert-nil-to-empty-string*
           #:*default-template-output*
           #:*default-template-pathname*
           #:*force-default*
           #:*ignore-empty-lines*
           #:*no-cache-check*
           #:*sequences-are-lists*
           #:*template-end-marker*
           #:*template-start-marker*
           #:*template-symbol-package*
           #:*upcase-attribute-strings*
           #:*value-access-function*
           #:*warn-on-creation*
           #:clear-template-cache
           #:create-template-printer
           #:delete-from-template-cache
           #:fill-and-print-template
           #:template-error
           #:template-invocation-error
           #:template-missing-value-error
           #:template-not-a-string-error
           #:template-not-a-string-error-value
           #:template-syntax-error
           #:template-syntax-error-col
           #:template-syntax-error-line
           #:template-syntax-error-stream))

#+:cormanlisp
(defpackage "HTML-TEMPLATE"
  (:nicknames "TEMPLATE")
  (:use "CL")
  (:export "*CONVERT-NIL-TO-EMPTY-STRING*"
           "*DEFAULT-TEMPLATE-OUTPUT*"
           "*DEFAULT-TEMPLATE-PATHNAME*"
           "*FORCE-DEFAULT*"
           "*IGNORE-EMPTY-LINES*"
           "*NO-CACHE-CHECK*"
           "*SEQUENCES-ARE-LISTS*"
           "*TEMPLATE-END-MARKER*"
           "*TEMPLATE-START-MARKER*"
           "*TEMPLATE-SYMBOL-PACKAGE*"
           "*UPCASE-ATTRIBUTE-STRINGS*"
           "*VALUE-ACCESS-FUNCTION*"
           "*WARN-ON-CREATION*"
           "CLEAR-TEMPLATE-CACHE"
           "CREATE-TEMPLATE-PRINTER"
           "DELETE-FROM-TEMPLATE-CACHE"
           "FILL-AND-PRINT-TEMPLATE"
           "TEMPLATE-ERROR"
           "TEMPLATE-INVOCATION-ERROR"
           "TEMPLATE-MISSING-VALUE-ERROR"
           "TEMPLATE-NOT-A-STRING-ERROR"
           "TEMPLATE-NOT-A-STRING-ERROR-VALUE"
           "TEMPLATE-SYNTAX-ERROR"
           "TEMPLATE-SYNTAX-ERROR-COL"
           "TEMPLATE-SYNTAX-ERROR-LINE"
           "TEMPLATE-SYNTAX-ERROR-STREAM"))

(pushnew :html-template *features*)