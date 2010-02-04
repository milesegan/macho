(in-package :macho)

(defvar *message-id-re* "<([^>@]+)@\\[?([^>\\]]+)\\]?>")
(defvar *nl-nl* #.(format nil "~C~C" #\Newline #\Newline))
(defvar *iso-header-re* (create-scanner "=\\?ISO-8859-\\d+\\?.\\?(.+)\\?=" 
                                        :case-insensitive-mode t))

(defun trim-email-addresses (text)
  "Strips all but the user@domain.tld part of an email address."
  (regex-replace-all "\"?([^<\"]+)\"?\\s*<?\\S+@[^ \\t\\r\\n>]+>?" text "\\1"))

(defun obscure-email-addresses (text)
  "Rewrites email addresses into (hopefully) non machine-parsable form."
  (regex-replace-all "(&lt;|<)?(\\S+)@([^ \\t\\r\\n>]+)>?" text "&lt;\\3 at \\2&gt;"))

(defclass message ()
  ((archive :accessor message-archive)
   (author :accessor message-author)
   (children :accessor message-children :initform nil)
   (content-type :accessor message-content-type :initform nil)
   (depth :accessor message-depth :initform 0)
   (descendents :initform nil)
   (dirty :accessor message-dirty :initform nil)
   (end :accessor message-end :initarg :end)
   (id :accessor message-id)
   (in-reply :accessor message-in-reply :initform nil)
   (is-multipart :accessor message-is-multipart :initform nil)
   (parent :accessor message-parent :initform nil)
   (serial :accessor message-serial :initarg :serial)
   (start :accessor message-start :initarg :start)
   (subject :accessor message-subject)
   (text :accessor message-text :initarg :text)
   (time :accessor message-time)
   (thread :accessor message-thread :initform nil)
   (thread-prev :accessor message-thread-prev :initform nil)
   (thread-next :accessor message-thread-next :initform nil)))

(defun message-descendents (msg)
  "Lazily initializes message descendents list. Builds thread-prev/next links
  along the way."
  (with-slots (descendents) msg
    (unless descendents
      (loop with stack = (list msg)
            for m = (pop stack)
            while (or m stack)
            do (progn
                 (aif (first descendents)
                      (setf (message-thread-next it) m
                            (message-thread-prev m) it))
                 (push m descendents)
                 (setf stack (append (message-children m) stack))))
      (setf descendents (nreverse descendents)))
    descendents))

(defun message-short-subject (msg)
  "Returns ... if subject is the same as parent. Also strips [name] prefixes."
  (with-slots (archive parent subject) msg
    (let ((trim 
           (regex-replace-all (archive-subject-trim-re archive) 
                              subject 
                              ""))
          (parent-trim
           (and parent
                (regex-replace-all (archive-subject-trim-re archive) 
                                   (message-subject parent) 
                                   ""))))
    (if (and parent-trim
             (search parent-trim trim))
        "..."
        trim))))

(defun translate-mime-char (target-string start end match-start match-end reg-starts reg-ends)
  "Translates mime-encoded character to its entity-encoded counterpart."
  (let* ((hex-literal (concatenate 'string "#x" (subseq target-string (1+ match-start) match-end)))
         (int-value (read-from-string hex-literal)))
    (if (< int-value (length *html-entities*))
        (aref *html-entities* int-value)
        (concatenate-string "&" hex-literal ";"))))

(defun chop-multipart (stream content-type)
  "Looks for a text section in a mime multipart message."
  (let* (body match matches parts)
    (loop for e in (split ";" content-type)
          while (not match)
          do (setf (values match matches) 
                   (scan-to-strings "boundary=\"?([^\" ]+)\"?" e)))
    (if match
        (let ((boundary (s+ "--" (elt matches 0))))
          (loop with padding = (read-delimited stream boundary)
                while (not body)
                do (if (char= (peek-char nil stream) #\Newline) ;; part with no header
                       (progn 
                         (read-char stream)
                         (setf body (read-delimited stream boundary)))
                       (let* ((header (read-delimited stream *nl-nl*))
                              (headers (parse-mime-header header))
                              (ctype (first (gethash :content-type headers))))
                         (if (scan "text" ctype)
                             (setf body (read-delimited stream boundary))))))))
    body))

(defun line-qlevel (line)
  "Determines the depth of quotation of a single line in a message."
  (loop with level = 0
        with done = nil
        for c across line
        while (not done)
        do (case c
             ((#\Space #\Tab) t)
             (#\> (incf level))
             (t (setf done t)))
        finally (return level)))
  
(defun markup-body (body)
  "Highlights urls, colorizes quotations, etc. in a message."
  (setf body (regex-replace-all "<" body "&lt;"))
  (setf body (regex-replace-all "(http|ftp)://([-0-9A-Za-z#%&+.,/:;?_~\=]*[-0-9A-Za-z#%&+/:;?_~\=])"
                                body 
                                "<a href=\"\\1://\\2\">http://\\2</a>"))
  (setf body (regex-replace-all "=\\n" body #.(format nil "~C" #\Newline)))
  (let ((out (make-string-output-stream))
        (lines (split "\\n" body)))
    (loop with lastlevel = 0
          for line in lines
          for level = (line-qlevel line)
          do (cond
               ((> level lastlevel)
                (loop for i from (1+ lastlevel) to level
                      do (format out "<div class='bquote bquote~A'>" i)))
               ((< level lastlevel)
                (loop for i from lastlevel downto (1+ level)
                      do (write-string "</div>" out))))
          do (format out "~A~%" line)
          do (setf lastlevel level))
    (get-output-stream-string out)))

(defun message-body (message)
  "Returns the body of a message.  Lazily loaded from the archive if necessary."
  (with-slots (archive content-type start end is-multipart) message
    (let* ((mbox (archive-mbox archive))
           (headers (progn
                      (file-position mbox start)
                      (read-delimited mbox *nl-nl*)))
           body)
      (if is-multipart
          (setf body (chop-multipart mbox content-type))
          (let* ((size (- end (file-position mbox))))
            (setf body (make-string size))
            (read-sequence body mbox)))
      body)))
      ;;;(regex-replace-all "=[0-9A-F][0-9A-F]" body #'translate-mime-char))))

(defun decode-mime-header-line (line)
  (let (match matches)
    (setf (values match matches) (scan-to-strings *iso-header-re* line))
    (if match
        (regex-replace-all "=[0-9A-F][0-9A-F]" 
                           (elt matches 0)
                           #'translate-mime-char)
        line)))

(defun parse-mime-header (header)
  "Parses a mime header.  Returns key,value header hashtable."
  (let* ((header-lines (split "\\n" (regex-replace-all "\\n\\s+" header " ")))
         (headers (make-hash-table :test #'equal)))
    (dolist (header-line header-lines)
      (let* ((hdrparts (split ":\\s+" header-line :limit 2))
             (key (intern (string-upcase (car hdrparts)) 'keyword))
             (val (decode-mime-header-line (cadr hdrparts))))
        (push val (gethash key headers))))
    headers))

(defun parse-message (header)
  "Parses raw message text into message instance."
  (let ((msg (make-instance 'message))
        (headers (parse-mime-header header))
        received)
    (with-slots (author body content-type id in-reply is-multipart subject time) msg
      (if (gethash :original-received headers)
          (setf received (first (last (gethash :original-received headers))))
          (setf received (first (last (gethash :received headers)))))
      (setf time (time-struct 
                  (or (net.telent.date:parse-time (cadr (split ";\\s*" received)))
                      (net.telent.date:parse-time (first (last (gethash :date headers))))
                      (get-universal-time)))
            author (trim-email-addresses (first (gethash :from headers)))
            content-type (first (gethash :content-type headers))
            id (regex-replace-all *message-id-re* 
                                  (first (gethash :message-id headers)) "\\1@\\2")
            in-reply (regex-replace-all *message-id-re*
                                        (if (gethash :in-reply-to headers)
                                            (first (split "\\s+" (first (gethash :in-reply-to headers))))
                                            (first (last (split "\\s+" (first (gethash :references headers))))))
                                        "\\1@\\2")
            is-multipart (scan "multipart" (first (gethash :content-type headers)))
            subject (first (gethash :subject headers))))
    msg))

