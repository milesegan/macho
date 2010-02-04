#-asdf(require 'asdf)

;;; quiet asdf & sbcl so we can run out of cron
(setf asdf::*verbose-out* nil)
(declaim (optimize (sb-ext:inhibit-warnings 3)))

;;; setup asdf module paths
(push (concatenate 'string (second sb-ext:*posix-argv*) "/src/")
      asdf:*central-registry*)
(dolist (d (directory (concatenate 'string (second sb-ext:*posix-argv*) "/lib/*/")))
  (push d asdf:*central-registry*))

(require 'macho)
(use-package :macho)

;; hacky - if no archives given generate a core
(if (= 3 (length sb-ext:*posix-argv*))
    (save-lisp-and-die "macho.core"))

(setf macho::*workdir* (second sb-ext:*posix-argv*))
(setf macho::*verbose* (string= (third sb-ext:*posix-argv*) "true"))
(setf html-template:*warn-on-creation* nil)

(let ((command (fourth sb-ext:*posix-argv*)))
  (cond
    ((string= command "update")
     (dolist (list (nthcdr 4 sb-ext:*posix-argv*))
       (handler-case (macho:process-archive list)
         (t (e) (format *error-output* "error processing ~A: ~A~%" list e)))))
    ((string= command "deliver")
     (macho::msg "delivering to ~A~%" (fifth sb-ext:*posix-argv*)) 
     (macho:deliver (fifth sb-ext:*posix-argv*)))
    (t (format t "unknown command: ~A~%" command))))

(sb-ext:quit)
