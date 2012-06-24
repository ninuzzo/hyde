#|

A library for inspecting and manipulating HTML source code in Hyde syntax.
This is to simplify the writing of users functions and macros.
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

(defun attr-val (elem attr)
  "Return the value of the attr attribute of element elem, or nil if not found. It assumes elem arguments are flattened."
  (let ((arg (second elem)))
    (if (and arg (symbolp arg)) ; Is it an attribute?
        (if (eq arg attr)
            (third elem) ; Return the attribute value.
            ;; cddr instead of cdddr, so the first element
            ;; will be taken as the element name and discarded.
            (attr-val (cddr elem) attr)))))

(defun cont-list (elem)
  "Return the content of element elem as a list, or nil if void/empty. It assumes elem arguments are flattened."
  (let ((arg (second elem)))
    (if (and arg (symbolp arg))
        ;; cddr instead of cdddr, so the first element
        ;; will be taken as the element name and discarded.
        (cont-list (cddr elem))
        (rest elem))))

;;; TODO: functions like getElementById, getElementsByName, etc should be
;;; implemented here. Or even an XPath-like facility using Lisp syntax, of
;;; course.
