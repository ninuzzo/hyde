#|

List functions.
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

;;; We need symbol support, that is when flattening a list containing quoted
;;; elements, we don't want quote QUOTE elements to appear.  See:
;;; http://stackoverflow.com/questions/10638379/common-lisp-flatting-a-list-that-may-contain-symbols/
(defun flatten (list)
  "Return the flattened version of a list."
  ;; (null list) or (not list) are the same, it's just a matter of style.
  (cond ((null list) nil)
        ((or (atom list) (and (eq (first list) 'quote) (null (cddr list))))
         (list list))
        ;; The nconc procedure (called append! in Scheme) performs the same
        ;; function as append, but destructively: it alters the cdr of each
        ;; argument (save the last), pointing it to the next list.
        (t (loop for elem in list nconc (flatten elem)))))
#|

TODO: is the one above the fastest implementation?

Alternate implementations (with no symbol support):

(defun flatten (list)
  (loop for i in list if (listp i) append (flatten i) else collect i) 

(defun flatten (list)
  (cond
    ((null list) nil)
    ((atom list) (list list))
    (t (loop for elem in list appending (flatten elem)))))

(defun flatten (list)
  (if list
      (if (atom list) (list list) (mapcan #'flatten list))))

(defun flatten (list)
  (cond
    ((null list) nil)
    ((atom list) (list list))
    (t (append (flatten (car list)) (flatten (cdr list))))))

(defun flatten (orig-list)
  (if (eql orig-list nil)
      nil
      (let ((elem (car orig-list)) (resto-list (cdr orig-list)))
        (if (listp elem)
            (append (flatten elem) (flatten resto-list))
            (append (cons elem nil) (flatten resto-list)))))))
|#

(defun even-elements (items) 
  "Return the even elements of a list as another list."
  (loop for (odd even) on items by #'cddr 
    if even collect even))

(defun odd-elements (items) 
  "Return the odd elements of a list as another list."
  (loop for (odd even) on items by #'cddr 
    if odd collect odd))
#|
TODO: which implementation is faster?
Alternate implementations:

;;; See http://www.gnu.org/software/emacs/manual/html_node/cl/Porting-Common-Lisp.html
(defun odd-elements (list)
  (loop for elem in list
    for flag = t then (not flag)
    if flag collect elem))

(defun even-elements (list)
  (loop for elem in list
    for flag = nil then (not flag)
    if flag collect elem))
|#

(defun pair-elements (items)
  "Pair elements of a list (but not recursively) and return the new list of
pairs."
  (loop for (odd even) on items by #'cddr collect (list odd even)))
#|

Alternate implementation:

(defun pair-elements (items)
  (read-from-string (format nil "(~{(~s ~s)~^ ~})" items)))
|#
