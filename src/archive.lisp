(in-package :macho)

(defclass archive ()
  ((path :accessor archive-path :initarg :path)
   (name :accessor archive-name :initarg :name)
   (dirty-table :accessor archive-dirty-table)
   (id-table :accessor archive-id-table)
   (last-pos :accessor archive-last-pos :initform 0)
   (messages :accessor archive-messages)
   (url-root :accessor archive-url-root :initform nil)
   (index-path :accessor archive-index-path)
   (latest-message-date :accessor archive-latest-message-date :initform nil)
   (mbox-path :accessor archive-mbox-path)
   (mbox :accessor archive-mbox)
   (subject-trim-re :accessor archive-subject-trim-re)
   (year-table :accessor archive-year-table)))

(defmethod initialize-instance :after ((archive archive) &rest args)
  (declare (ignore args))
  (with-slots (path name id-table dirty-table mbox mbox-path subject-trim-re index-path) archive
    (setf name (pathname-name (archive-path archive))
          dirty-table (make-hash-table)
          id-table (make-hash-table :test #'equal)
          mbox-path (join-path path "mbox")
          index-path (join-path path "index")
          subject-trim-re (create-scanner (s+ "\\[" name "\\]\\s+")  :case-insensitive-mode t)
          mbox (open (archive-mbox-path archive) :direction :input :external-format :latin1))))

(defun archive-root-path (archive-name)
  (join-path *workdir* "root" "archives" archive-name))

(defun deliver (name)
  "Delivers a message from standard-input to archive and updates web archive."
  (let* ((path (archive-root-path name))
         (mbox-path (join-path path "mbox"))
         (msg (read-file *standard-input*)))
    (with-open-file (mbox-file mbox-path :direction :output :if-exists :append)
      (write-string msg mbox-file))
    (process-archive name)))

(defun rebuild-tables (archive)
  (with-slots (latest-message-date messages id-table dirty-table year-table) archive
    (loop for msg across messages
          do (let* ((year (time-year (message-time msg)))
                    (month (time-month (message-time msg)))
                    (id (message-id msg))
                    (in-reply (message-in-reply msg)))
               ;; put in year hash
               (or (gethash year year-table)
                   (setf (gethash year year-table) (make-array 12 :initial-element nil)))
               (push msg (svref (gethash year year-table) (1- month)))
               ;; mark dirty table if dirty
               (if (message-dirty msg)
                   (progn
                     (or (gethash year dirty-table)
                         (setf (gethash year dirty-table) (make-array 12 :initial-element nil)))
                     (setf (svref (gethash year dirty-table) (1- month)) t))))
          do (if (or (null latest-message-date)
                     (time< latest-message-date (message-time msg)))
                 (setf latest-message-date (message-time msg))))))

(defun rebuild-threads (archive)
  "Rebuilds message thread relationships."
  (with-slots (latest-message-date depth messages id-table) archive
    (loop for msg across messages
          do (loop with m = msg
                   with chain = (list msg)
                   for p = (gethash (message-in-reply m) id-table)
                   while (and p 
                              (not (member p chain)) ;; check for loop
                              (not (message-thread m)))
                   do (push m (message-children p))
                   do (push p chain)
                   do (setf (message-parent m) p
                            m p)
                   finally (progn
                             (let ((thread (or (message-thread m) m)))
                               (loop for link in chain
                                     for depth upfrom 0
                                     do (setf (message-thread link) thread
                                              (message-depth link) (+ depth (message-depth m))))
                               (if (message-dirty msg) 
                                   (setf (message-dirty thread) t))))))))

(defun read-archive-properties (archive)
  "Loads per-archive properties for the archive."
  (msg "reading archive properties file~%")
  (let ((prop-path (join-path (archive-path archive) "properties")))
    (if (probe-file prop-path)
        (handler-case
            (with-open-file (props prop-path :direction :input)
              (loop for line = (read-line props nil nil)
                    while line
                    do (let ((parts (split "\\s*=\\s*" line :limit 2)))
                         (if (eql 2 (length parts))
                             (cond
                               ((string= (first parts) "url-root")
                                (setf (archive-url-root archive) (second parts))))))))
          ((or end-of-file reader-error)) ()))))

(defun archive-load (archive)
  "Loads archive from filesystem."
  (with-slots (messages year-table) archive
    (setf messages (make-array 0 :fill-pointer t))
    (setf year-table (make-hash-table))
    (index-update archive)
    (rebuild-tables archive)
    (rebuild-threads archive)))

(defun process-archive (name)
  "Updates archive webdocs with any new changes to the archive mbox."
  (let ((a (make-instance 'archive :path (archive-root-path name))))
    (read-archive-properties a)
    (with-slots (index-path mbox-path name) a
      (if (or (not (probe-file index-path))
              (< (file-write-date index-path) (file-write-date mbox-path)))
          (progn
            (msg "processing archive ~A~%" name)
            (archive-load a)
            (if (> (hash-table-size (archive-dirty-table a)) 0)
                (write-archive a (join-path *workdir* "root" "archives" name))))
          (msg "archive ~A is already up to date~%" name)))))
