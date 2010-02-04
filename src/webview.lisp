(in-package :macho)

(defun nice-date (time)
  "Formats a date in a satndard format."
  (format nil "~A.~A.~A" 
          (time-year time) 
          (time-month time) 
          (time-date time)))

(defun short-date (time)
  "Formats a date in an abbreviated standard format."
  (format nil "~A/~A" 
          (time-month time) 
          (time-date time)))

(defun nice-time (time)
  "Formats a time in a standard format."
  (format nil "~A/~A ~D:~2,'0D" 
          (time-month time) 
          (time-date time) 
          (time-hour time) 
          (time-minute time)))

(defun write-archive-index (archive path)
  "Writes the toplevel archive index page."
  (msg "writing archive index to ~A~%" path)
  (ensure-directories-exist path)
  (with-slots (name latest-message-date messages year-table) archive
    (let* ((year (this-year))
           (month (this-month))
           (years (sort (loop for k being each hash-key of year-table collect k) #'>))
           (msgs (loop for year in years
                       for msgs = (gethash year year-table)
                       collect `(:year ,(s+ year)
                                 :months ,(loop for i from 0 to 11
                                                for m = (svref msgs i)
                                                for count = (length m)
                                                collect `(:link ,(s+ year "-" (1+ i) "/index.html")
                                                          :link-text ,(s+ count)
                                                          :count ,(if (> count 0) count nil)))))))
      (with-open-file (index path :direction :output :if-exists :supersede)
        (let ((vars `(:name ,name
                      :last-message-date ,(nice-date latest-message-date)
                      :total-messages ,(s+ (length messages))
                      :years ,(loop for year in years collect (s+ year))
                      :msgs ,msgs
                      :rss ,(archive-url-root archive)
                      :months ,(loop for i from 1 to 12 
                                     collect `(:value ,(format nil "~3,3/net.telent.date:monthname/" i))))))
          (html-template:fill-and-print-template 
           (pathname (join-path *workdir* "templates" "archive-index.html"))
           vars
           :stream index))))))

(defun message-path (message)
  "Computes the path to the html file for the message."
  (with-slots (time serial) message
    (s+ "../" (time-year time) "-" (time-month time) "/" serial ".html")))
  
(defun month-index-entry (msg num &key (offset 0) pointer long-subject)
  "Builds the standard month-index representation of a message."
  (with-slots (serial depth author time) msg
    `(:href ,(message-path msg)
      :class ,(if pointer "pointed" "normal")
      :number ,(s+ num)
      :author ,(s+ author)
      :depth ,(s+ (- depth offset))
      :pointer ,pointer
      :subject ,(s+ (if long-subject
                        (message-subject msg)
                        (message-short-subject msg)))
      :date ,(short-date time))))

(defun build-month-index-entries (archive year month)
  "Builds the month index entries for the non-threaded views."
  (with-slots (year-table) archive
    (loop for msg in (svref (gethash year year-table) month)
          for i upfrom 1
          collect (month-index-entry msg 
                                     i 
                                     :offset (message-depth msg)
                                     :long-subject t))))

(defun build-month-thread-index-entries (archive year month)
  "Builds the month index entries for the threaded views."
  (loop with number = 0
        with msgs = nil
        for msg in (svref (gethash year (archive-year-table archive)) month)
        if (not (and (message-parent msg)
                     (= (time-month (message-time (message-parent msg)))
                        (1+ month))))
        do (loop with offset = (message-depth msg)
                 for m in (message-descendents msg)
                 for i upfrom 1
                 do (incf number)
                 do (push (month-index-entry m number :offset offset :long-subject (= i 1))
                          msgs))
        finally (return (nreverse msgs))))

(defun write-month-index (archive year month msgs path)
  "Writes the month index page for the given year and month."
  (msg "writing month index for ~A / ~A to ~A~%" year (1+ month) path)
  (ensure-directories-exist path)
  (with-open-file (index path :direction :output :if-exists :supersede)
    (with-slots (name messages year-table) archive
      (let* ((title (format nil "~A mail archive - ~/net.telent.date:monthname/ ~A" name (1+ month) year))
             (vars `(:archive-name ,name
                     :title ,title
                     :year ,(s+ year)
                     :month ,(s+ (1+ month))
                     :msgs ,msgs)))
        (html-template:fill-and-print-template 
         (pathname (join-path *workdir* "templates" "month-index.html"))
         vars 
         :stream index)))))

(defun build-message (msg prev next)
  (with-slots (archive author parent serial subject time thread-prev thread-next) msg
    `(:date ,(s+ (time-year time) "-" (time-month time) "-" (time-date time))
      :author ,author
      :subject ,subject
      :message-date ,(format nil "~A-~A-~A ~D:~2,'0D" 
                             (time-year time)
                             (time-month time)
                             (time-date time)
                             (time-hour time)
                             (time-minute time))
      :prev ,(if prev (message-path prev))
      :next ,(if next (message-path next))
      :thread-prev ,(if thread-prev (message-path thread-prev))
      :thread-next ,(if thread-next (message-path thread-next))
      :body ,(obscure-email-addresses (markup-body (message-body msg))))))

(defun write-message (path msg &optional prev next)
  "Writes the html page for the given message."
  (ensure-directories-exist path)
  (with-open-file (stream path :direction :output :if-exists :supersede)
    (with-slots (archive author depth parent serial subject id) msg
      (msg "writing msg ~S ~S ~S ~S~%" serial author subject id)
      (let* ((descendents (loop with all = (message-descendents (message-thread msg))
                                with offset = (message-depth (car all))
                                for m in all
                                for num upfrom 1
                                collect (month-index-entry m 
                                                           num 
                                                           :offset offset
                                                           :pointer (eql m msg)
                                                           :long-subject (= num 1))))
             (vars `(:name ,(archive-name archive)
                     :descendents ,descendents
                     ,@(build-message msg prev next))))
        (html-template:fill-and-print-template 
         (pathname (join-path *workdir* "templates" "message.html"))
         vars 
         :stream stream)))))

(defun write-rss (archive &key (limit 15))
  "Writes rss feed containing most recent messages added to the archive."
  (msg "writing rss feed for ~S~%" (archive-name archive))
  (let* ((rss-path (join-path (archive-path archive) "latest.xml"))
         (messages (reverse (lastn (archive-messages archive) limit)))
         (rss `(|rss| (("version" "0.92"))
                (|channel| nil
                 (|language| nil "en")
                 (|link| nil ,(archive-url-root archive))
                 (|title| nil ,(s+ (archive-name archive) " mail archive"))
                 (|description| nil ,(s+ (archive-name archive) " mail archive"))
                 ,@(loop for msg across messages
                         for url = (s+ (archive-url-root archive)
                                       "/"
                                       (time-year (message-time msg)) "-" (time-month (message-time msg))
                                       "/"
                                       (message-serial msg) ".html")
                         for descr = (s+ (message-author msg) " / " (message-subject msg))
                         for body = (s+ "<pre>"
                                        (obscure-email-addresses (message-body msg))
                                        "</pre>")
                         collect `(|item| nil
                                   (|title| nil ,descr)
                                   (|description| nil ,body)
                                   (|link| nil ,url)))))))
    (with-open-file (stream rss-path :direction :output :if-exists :supersede :external-format :latin1)
      (xmls:write-xml rss stream :indent t))))

(defun write-archive (archive path)
  "Writes all unwritten html pages for the archive."
  (msg "building web pages for ~S~%" (archive-name archive))
  (with-slots (latest-message-date name messages year-table dirty-table) archive
    (write-archive-index archive (join-path path "index.html"))
    (let ((latest-path (join-path path "latest")))
      (if (probe-file latest-path)
          (sb-posix:unlink (join-path path "latest")))
      (sb-posix:symlink (s+ (time-year latest-message-date) 
                            "-" 
                            (time-month latest-message-date))
                        latest-path))
    (if (archive-url-root archive)
        (write-rss archive))
    (loop for year being each hash-key of year-table
          using (hash-value msgs)
          do (if (gethash year dirty-table)
                 (progn
                   (dotimes (month 12)
                     (let ((month-dir (join-path path (s+ year "-" (1+ month))))
                           (month-dirty (svref (gethash year dirty-table) month)))
                       (if month-dirty
                           (progn
                             (loop for name in (list "author" "date" "subject")
                                   for pred in (list #'string< #'time< #'string<)
                                   for key in (list #'message-author #'message-time #'message-subject)
                                   do (loop for suffix in (list "ascending" "descending")
                                            for pred2 in (list pred #'(lambda (a b) (not (funcall pred a b))))
                                            do (progn
                                                 (setf (svref msgs month)
                                                       (stable-sort (svref msgs month) pred2 :key key))
                                                 (write-month-index archive
                                                                    year
                                                                    month
                                                                    (build-month-index-entries archive year month)
                                                                    (join-path month-dir (s+ name "-" suffix ".html"))))))
                             (setf (svref msgs month)
                                   (stable-sort (svref msgs month) #'time< :key #'message-time))
                             (write-month-index archive
                                                year
                                                month
                                                (build-month-thread-index-entries archive year month)
                                                (join-path month-dir "thread-ascending.html"))
                             (setf (svref msgs month)
                                   (stable-sort (svref msgs month) #'time> :key #'message-time))
                             (write-month-index archive
                                                year
                                                month
                                                (build-month-thread-index-entries archive year month)
                                                (join-path month-dir "thread-descending.html"))
                             (let ((index-path (join-path month-dir "index.html")))
                               (if (probe-file index-path)
                                   (sb-posix:unlink index-path))
                               (sb-posix:symlink "thread-descending.html" index-path))
                             (loop with msgs = (svref msgs month)
                                   with last = (length messages)
                                   for msg in msgs
                                   for serial = (message-serial msg)
                                   do (if (message-dirty (message-thread msg))
                                          (ignore-errors
                                            (write-message (join-path month-dir (s+ serial ".html"))
                                                           msg
                                                           (if (= serial 1) nil (aref messages (- serial 2)))
                                                           (if (= serial last) nil (aref messages serial)))))))))))))))
