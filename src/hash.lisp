#|

Hash functions.
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

;;; Unfortunately the standard does not provide a way to initialize a hash.
;;; See:
;;; http://stackoverflow.com/questions/10706024/common-lisp-shorthand-to-initialize-a-hash-table-with-many-entries
;;; TODO: the flexibility and all options of make-hash-table are lost.
(defmacro list-to-hash (plain-list &key (test 'equal))
  "Macro to ease static hash initialization based on a plain list."
  (let ((hash (gensym))) ; This is to avoid potential symbol clashes.
    `(let ((,hash (make-hash-table :test ',test)))
       (loop for (key value) on ,plain-list by #'cddr
         do (setf (gethash key ,hash) value)) ,hash)))

;;; This is useful for serialization. Unfortunately, hashes are not printable
;;; in Common Lisp. Common Lisp is not perfect, but there are some more
;;; mainstream languages that are much worse.
(defun hash-to-list (hash)
  "Convert a hash into a plain list like the one used by ini-hashtable."
  (loop for key being the hash-keys of hash
    for value being the hash-values of hash
    appending (list key value)))

(defun read-hash (file)
  "Read a hash serialized as a plain list on disk."
  (list-to-hash (read-lisp file)))

(defun write-hash (hash file-path)
  "Write a hash on disk, serialized as a plain list."
  (with-open-file (file file-path :direction :output :if-exists :supersede)
    ;; Use the pretty printer, so that if a user wants to inspect this file,
    ;; it is easier to read.
    (pprint (hash-to-list hash) file)))

;;; TODO: is there a better way to load this file? I mean not here.
(load "conf.lisp")
