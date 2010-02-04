(in-package :macho)

(defvar *current-index-version* 4)

(defun index-load (archive)
  "Loads index from filesystem."
  (let ((index-path (archive-index-path archive)))
    (if (probe-file index-path)
        (with-open-file (s index-path :direction :input :element-type :default)
          (if (= *current-index-version* (read-int32 s))
              (loop initially (msg "loading index for ~S~%" (archive-name archive))
                    with last-pos = (read-int32 s)
                    with count = (read-int32 s)
                    for i from 1 to count
                    for msg = (make-instance 'message)
                    do (with-slots (start end time serial content-type id is-multipart author subject in-reply) msg
                         (setf (message-archive msg) archive
                               start (read-int32 s)
                               end (read-int32 s)
                               time (time-struct (read-int32 s))
                               serial i
                               is-multipart (if (= (read-int32 s) 1) t nil)
                               id (read-tagged-string s)
                               author (read-tagged-string s)
                               subject (read-tagged-string s)
                               in-reply (read-tagged-string s)
                               content-type (read-tagged-string s))
                         (setf (gethash id (archive-id-table archive)) msg)
                         (vector-push-extend msg (archive-messages archive)))
                    finally (setf (archive-last-pos archive) last-pos)))))))

(defun index-save (archive)
  "Saves index back to filesystem."
  (msg "saving index for ~S~%" (archive-name archive))
  (with-open-file (s (archive-index-path archive) 
                     :direction :output 
                     :element-type :default 
                     :if-exists :supersede)
    (let ((messages (archive-messages archive)))
      (write-int32 *current-index-version* s) ;; file format version
      (write-int32 (archive-last-pos archive) s)
      (write-int32 (length messages) s)
      (loop for m across messages
            do (with-slots (start end time id author subject in-reply content-type is-multipart) m
                 (write-int32 start s)
                 (write-int32 end s)
                 (write-int32 (time-universal time) s)
                 (write-int32 (if is-multipart 1 0) s)
                 (write-tagged-string id s)
                 (write-tagged-string author s)
                 (write-tagged-string subject s)
                 (write-tagged-string in-reply s)
                 (write-tagged-string content-type s))))))

(defun index-update (archive)
  "Updates index to reflect all messages in archive."
  (msg "updating index for ~S~%" (archive-name archive))
  (with-slots (id-table mbox mbox-path index-path last-pos messages) archive
    (index-load archive)
    (msg "adding to index for ~S from mbox~%" (archive-name archive))
    (file-position mbox last-pos)
    (loop with next = (length messages)
          for start-pos = (file-position mbox)
          for headers = (read-delimited mbox #.(format nil "~C~C" #\Newline #\Newline))
          for body = (read-delimited mbox #.(format nil "~CFrom " #\Newline))
          for end-pos = (file-position mbox)
          for msg = (parse-message headers)
          while headers do (with-slots (start end serial id dirty) msg
                             (if id ;; skip unparsable messages
                                 (progn
                                   (setf dirty t
                                         (message-archive msg) archive
                                         serial (incf next)
                                         (gethash id id-table) msg
                                         start start-pos
                                         end end-pos)
                                   (msg "read message ~A ~A ~A (~A-~A)~%"
                                        next
                                        (message-subject msg)
                                        (message-author msg)
                                        start
                                        end)
                                   (vector-push-extend msg messages))))
          finally (setf last-pos end-pos))
    (index-save archive)))
