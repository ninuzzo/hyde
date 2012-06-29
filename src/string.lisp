#|

String functions.
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

#|
Not simply a concatenation of strings like:
(apply #'concatenate 'string strings).
It also converts each argument into a string and flattens nested lists.
E.g. this way we do not need a separate string-append or cat-list function
to concatenate a list of strings (e.g. the one naturally returned by a loop
command). One feline does everything LOL
TODO: NIL values are actually printed as nil. If this is a problem when this
function is used for HTML generation, change the behaviour and ignore them,
that is convert to to empty strings.
|#
(defun cat (&rest args)
  "Convert any argument into a string, flatten nested lists and return the
concatenation of all strings."
  #|
  In CLISP you would need to prevent an extra newline to be inserted by format
  for multiline body strings. This is rather annoying if one uses a lot of
  format forms, anyway we do only support ECL.
 
  (let ((*print-pretty*)) (format nil "~{~a~}" (flatten args)))
  See: http://www.clisp.org/impnotes/multi-rep.html#ppr-first-newline
  https://sourceforge.net/tracker/index.php?func=detail&aid=2795290&group_id=1355&atid=101355
  |#
  (format nil "~{~a~}" (flatten args)))

;;; Like mapcar, but instead of a list of strings returns their concatenation.
;;; If your function returns a list, it is flattened. Like cat it is loosely
;;; typed. It will happily convert every kind of element to a string.
(defun mapcat (funct list)
  "Apply funct to every member of list, convert the result to a string
and return the concatenation of results as a string."
  (if list
      (cat (funcall funct (first list)) (mapcat funct (rest list)))
      ""))

;;; Facility to print multiple strings to *standard-output* by concatenating
;;; them. This is like princ or better like writestring, in that it does not
;;; add whitespace, but supports multiple string arguments and no stream
;;; parameter. Named after PHP echo function LOL
(defun echo (&rest strings)
  "Flat lists, convert elements to strings, concatenate strings
and output everything to *standard-output*."
  (write-string (cat strings)))

;;; A comprehensive string concatenation function, that works with lists and
;;; strings is just "cat", this is a more limited and faster version that only
;;; catenates strings.
;;; NIL values are ignored, that is they equate to empty strings.
;;; This function is similar to CLISP's string-concat.
(defmacro strcat (&rest strings)
  "A macro shortcut to concatenate strings."
  `(concatenate 'string ,@strings))
#|
TODO: what about a normal function implementation vs a macro?
Maybe with the inline declaration? Which one is faster?
(defun strcat (&rest args) (apply #'concatenate 'string args))

http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node143.html

(defun strcat (&rest args)
  (reduce #'(lambda (string-1 string-2)
              (concatenate 'string string-1 string-2)) args :initial-value ""))
|#
