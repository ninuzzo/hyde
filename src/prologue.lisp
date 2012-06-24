;;; See comment in ../server.sh for this.
(defpackage #:cl-user (:use #:cl #:hyde))
(in-package :cl-user)

(process-cmdline)
(start-server)
