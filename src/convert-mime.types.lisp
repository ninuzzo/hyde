#!/bin/bash
#|
exec ecl -norc -q -shell $0 -- ${1+"$@"}
|#

(require 'asdf)
(setf ASDF:*ASDF-VERBOSE* nil)
(setf *load-verbose* nil)

(push (make-pathname :directory '(:RELATIVE "cl-ppcre-2.0.3"))
      asdf:*central-registry*)
(asdf:oos 'asdf:load-op :cl-ppcre)
(use-package :cl-ppcre)

(defun convert-line (line)
  (destructuring-bind (mime-type extensions)
                      (split (concatenate 'string '(#\tab) "+") line)
    (loop
      for extension in (split #\space extensions)
      do (format t "~s ~a~%" extension (split #\/ mime-type :limit 2)))))

(defun parse-mime-type-file (file)
  (with-open-file (f file :direction :input)
    (loop
      for line = (read-line f nil)
      while line
      unless (eql #\# (char line 0))
      do (convert-line line))))

(parse-mime-type-file "mime.types")
