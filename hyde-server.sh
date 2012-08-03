#!/bin/bash
#|
# Trick to pass command line arguments to Lisp and allow further option to the
# ecl binary. E.g. -norc is a good addition so as to avoid unnecessary loading
# of modules. The shebang #!/usr/bin/ecl -shell does not allow further options!
exec ecl -norc -q -shell $0 -- ${1+"$@"}
|#

;;; ECL comes with ASDF bundled in.
(require 'asdf)

;;; For more debugging information, although this only affects errors that
;;; happen during compilation. For some reasons it must be put after requiring
;;; 'asdf or it will be ignored!
(declaim (optimize (speed 0) (space 0) (debug 3)))

(setf ASDF:*ASDF-VERBOSE* nil) ; quiets ASDF output some
(setf *load-verbose* nil) ; quiets the LOAD process some

;;; See: http://common-lisp.net/project/asdf/asdf/Configuring-ASDF.html
(push (make-pathname :directory '(:RELATIVE "src"))
      asdf:*central-registry*)
;;; I want to bundle dependencies in separate directories from Hyde's sources
;;; to not clutter the file list. Using symbolic links is not portable between
;;; Windows and Linux.
(push (make-pathname :directory '(:RELATIVE "src" "cl-ppcre-2.0.3"))
      asdf:*central-registry*)

(asdf:oos 'asdf:load-op :hyde)

#|
We do not do (in-package :hyde), because we do not want user code to be in the
HYDE package. That would be unsafe. So we remain in the standard
COMMON-LISP-USER package and so user code (that is Lisp files generating HTML
ones). That means that user code won't be able to see HYDE package's private
functions, unless it refers to them with the prefix HYDE::. Anyway all
functions exported by the Hyde library will be available to user code without
any need for prefixes.
|#
(use-package :hyde)
(process-cmdline :start-arg-num 6)
(start-server) ; Start web server.

;;; vim: set syntax=lisp:
