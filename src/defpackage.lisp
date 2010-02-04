(defpackage macho
  (:use :cl-ppcre :cl-user :cl)
  (:export :deliver :process-archive))

(defvar macho::*workdir* nil)
(defvar macho::*verbose* nil)


