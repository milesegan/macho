(in-package :macho)

;;;-----------------------------------------------------------------------------
;;; basic utilities
;;;-----------------------------------------------------------------------------
(defun last1 (lst)
  "The last item in a list."
  (declare (list lst))
  (car (last lst)))

(defun lastn (seq n)
  "At most, the last n items of seq."
  (let ((count (min n (length seq))))
    (subseq seq (- (length seq) count))))

(defmacro aif (test-form then-form &optional else-form)
  "Anaphoric if: use `it' in then-form, else-form to
   refer to result of the test-form."
  `(let ((it ,test-form))
    (declare (ignorable it))
    (if it ,then-form ,else-form)))

(defmacro aunless (test-form &body body)
  "Anaphoric unless: use `it' in forms to refer to result of the test-form."
  `(let ((it ,test-form))
    (declare (ignorable it))
    (unless it ,@body)))

;;;-----------------------------------------------------------------------------
;;; debug utilities
;;;-----------------------------------------------------------------------------
(declaim (inline msg))
(defun msg (&rest args)
  "Simple logging function."
  (if *verbose*
      (apply #'format t args)))

;;;-----------------------------------------------------------------------------
;;; string utilities
;;;-----------------------------------------------------------------------------
(defun make-extendable-string ()
  "Creates a resizable string."
  (make-array 0 :fill-pointer t :adjustable t :element-type 'character))
  
(defun s+ (&rest strings)
  "Concatenates passed strings into one string."
  (format nil "~{~A~}" strings))

;;;-----------------------------------------------------------------------------
;;; time utilities
;;;-----------------------------------------------------------------------------
(defstruct time
  universal year month date day hour minute second zone daylight-p)

(defun time-struct (&optional (time (get-universal-time)))
  "Returns a time structure representing the given universal time."
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (decode-universal-time time)
    (make-time
     :universal time
     :second second
     :minute minute
     :hour hour
     :date date
     :month month
     :year year
     :day day
     :daylight-p daylight-p
     :zone zone)))

(defun time< (a b)
  "Compares two time structs for sorting purposes."
  (< (time-universal a) (time-universal b)))

(defun time> (a b)
  "Compares two time structs for sorting purposes."
  (> (time-universal a) (time-universal b)))

(defun this-month ()
  "Returns the current month as an integer."
  (time-month (time-struct)))

(defun this-year ()
  "Returns the current year as an integer."
  (time-year (time-struct)))

;;;-----------------------------------------------------------------------------
;;; directory and pathname utilities
;;;-----------------------------------------------------------------------------
(defun join-path (&rest items)
  "joins arguments by file separator into a new path."
  (labels ((tostr (x)
             (typecase x
               (string x)
               (t (format nil "~D" x)))))
    (reduce (lambda (a b) (concatenate 'string (tostr a) "/" (tostr b)))
            items)))

;;;-----------------------------------------------------------------------------
;;; I/O utilities
;;;-----------------------------------------------------------------------------
(defun read-int32 (stream)
  "Reads a 32 bit integer from a binary stream."
  (let ((num (read-byte stream)))
    (setf num (+ (ash num 8) (read-byte stream)))
    (setf num (+ (ash num 8) (read-byte stream)))
    (setf num (+ (ash num 8) (read-byte stream)))
    num))

(defun write-int32 (int stream)
  "Writes a 32 bit integer to a binary stream."
  (declare (type integer int))
  (write-byte (logand (ash int -24) 255) stream)
  (write-byte (logand (ash int -16) 255) stream)
  (write-byte (logand (ash int -8) 255) stream)
  (write-byte (logand int 255) stream))

(defun read-tagged-string (stream)
  "Reads a size-prefixed string from a binary stream."
  (let* ((size (read-byte stream))
         (string (make-string size)))
    (read-sequence string stream :end size)
    string))

(defun write-tagged-string (string stream)
  "Writes a size-prefixed string to a binary stream."
  (let ((length (min 255 (length string))))
    (write-byte length stream)
    (write-sequence string stream :end length)))

(defun read-file (stream)
  "Reads entire contents of stream into a string."
  (loop with result = (make-extendable-string)
        for c = (read-char stream nil)
        while c
        do (vector-push-extend c result)
        finally (return result)))

(defun read-delimited (stream token)
  "Reads stream up to delimiter."
  (let ((string (make-extendable-string)))
    (handler-case
        (loop with tok-length = (length token)
              with state = 0
              initially (vector-push-extend (read-char stream) string) ;; skip first char
              for c = (read-char stream)
              while (< state tok-length)
              do (let ((match (char= c (aref token state))))
                   (cond
                     ((and (> state 0) (not match))
                      (unread-char c stream)
                      (setf state 0))
                     (t
                      (if match (incf state))
                      (vector-push-extend c string))))
              finally (progn
                        (file-position stream (- (file-position stream) tok-length))
                        (adjust-array string (- (length string) tok-length) :fill-pointer t)
                        (return (values string t))))
      (end-of-file () (values (if (> (length string) 0) string nil)
                              nil)))))

(defun scan-for-token (stream token)
  "Scans stream for first occurence of token.  Returns position of token."
  (handler-case
      (loop with tok-length = (length token)
            with state = 0
            initially (read-char stream) ;; skip first char
            for c = (read-char stream)
            while (< state tok-length)
            do (let ((match (char= c (aref token state))))
                 (cond
                   ((and (> state 0) (not match))
                    (unread-char c stream)
                    (setf state 0))
                   (t
                    (if match (incf state)))))
            finally (progn
                      (file-position stream (- (file-position stream) tok-length))
                      (return (file-position stream))))
    (end-of-file () nil)))
