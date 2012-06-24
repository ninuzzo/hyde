#|

File utilities.
Copyright (c) 2012 Antonio Bonifati <antonio.bonifati@gmail.com>

This file is part of Hyde.

Hyde is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Hyde is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Hyde.  If not, see <http://www.gnu.org/licenses/>.

|#

(in-package :hyde)

;;; This is actually used to write HTML files
(defun write-file (file-path data)
  "Write data on path as a text file."
  (with-open-file (output-file file-path
                    :direction :output :if-exists :supersede)
    ;; princ is allowed to insert additional whitespace.
    ;; Better to use write-string!
    (write-string data output-file)))

#|
TODO: this function is quite slow since it reads and prints the file
character by character. It should be optimized, if possible. See:
http://www.ymeme.com/slurping-a-file-common-lisp-83.html
http://stackoverflow.com/questions/8816485/reading-the-binary-output-of-an-external-program-in-common-lisp
|#
(defun read-file (file-path)
  "Read and output a binary file."
  (with-open-file (file file-path :element-type 'unsigned-byte)
    ;; http://stackoverflow.com/questions/1904768/mystified-by-end-of-file-condition-in-common-lisp
    (do ((byte (read-byte file nil) (read-byte file nil)))
        ((null byte))
      (write-byte byte *standard-output*))))

(defun read-lisp (file-path)
  "Read and return Lisp data or code from a file.
Return nil and ignore all conditions but Lisp errors."
  (if file-path
      (with-open-file (file file-path :if-does-not-exist nil)
        (if file (read file nil)))))

;;; Since the way to tell a directory from a file is tricky in Common Lisp,
;;; let's make it a bit clearer.
(defun directory-p (pathname)
  "Tell whether a pathname represents a directory or a file.
Assume directories are in directory form and not in file form,
as returned by probe-file."
  (null (pathname-name pathname)))

(defun file-p (pathname)
  "Tell whether a pathname represents a directory or a file.
Assume directories are in directory form and not in file form,
as returned by probe-file."
  (pathname-name pathname))

;;; TODO: this function should rather try to determine the correct
;;; content-type from the file's suffix.
(defun mime-type (file-path)
  "Use file extension to detect content-type of a file according to a
mime-type-map. Fall back to \"application/octet-stream\" as a last resort."
  (let* ((mime-type (gethash (string-downcase (pathname-type file-path))
                             +mime-type-map+)))
   (if mime-type
       (format nil "~(~a/~a~)" (first mime-type) (second mime-type))
       "application/octet-stream")))

(defun file-size (file-path)
  "Return the number of bytes in a file."
  (with-open-file (file file-path :element-type 'unsigned-byte)
    (file-length file)))

; Actually not used.
(defun file-more-recent-p (source-file dest-file)
  "Tell if a source file needs to be re-build into a dest file. Useful to
conditionally execute a 'make' rule."
  (let ((dest-date (file-write-date dest-file))) ; nil if dest-file is not found.
    (or (not dest-date) (> (file-write-date source-file) dest-date))))

(defun truepath-create (file-path)
  "Create a file as empty if it does not exist. Return its truepathname."
  (with-open-file (file file-path :direction :probe :if-does-not-exist :create)
    (truename file)))
