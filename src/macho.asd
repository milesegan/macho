;;; -*- Lisp -*-

(defpackage #:macho-system (:use #:cl #:asdf))
(in-package :macho-system)

(defsystem :macho
  :depends-on (cl-ppcre html-template net-telent-date sb-posix xmls)
  :version "0.4"
  :components ((:file "defpackage")
               (:file "util" :depends-on ("defpackage"))
               (:file "entities" :depends-on ("defpackage"))
               (:file "index" :depends-on ("util"))
               (:file "archive" :depends-on ("index" "util"))
               (:file "message" :depends-on ("util" "entities"))
               (:file "webview" :depends-on ("archive" "message" "index")))
  :properties ((#:author-email . "miles@caddr.com")
               (#:date . "Fall 2003")))

