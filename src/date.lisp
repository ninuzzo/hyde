#|

Date functions.
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

(defconstant +month-names+ '(Jan Feb Mar Apr Jun May Jul Aug Sep Oct Nov Dec)
  "Abbreviations of the names of the months.")
(defconstant +day-names+ '(Mon Tue Wed Thu Fri Sat Sun)
  "Short weekday names.")

(defun get-gmt-datestring (&optional (universal-time (get-universal-time)))
  "Return current Greenwich Mean Time as a human-readable string, in the
format preferred by HTTP, e.g. Sun, 06 Nov 1994 08:49:37 GMT"
  ;; See: http://cl-cookbook.sourceforge.net/dates_and_times.html
  ;; Last two returned values, dst-p and tz, are ignored.
  (multiple-value-bind (second minute hour date month year day-of-week)
                       (decode-universal-time universal-time 0)
    (format nil "~@(~a~), ~2,'0d ~@(~a~) ~d ~2,'0d:~2,'0d:~2,'0d GMT"
      (nth day-of-week +day-names+) date (nth month +month-names+) year
      hour minute second)))
