;;; See comment in ../make-executable.lisp for this.
(defpackage #:cl-user (:use #:cl #:hyde))
(in-package :cl-user)

(process-cmdline)
(start-server)
