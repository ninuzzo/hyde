#|

Hyde global configuration file.
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

(defvar *server-address* "127.0.0.1" "Default web server listening address.")

;; Port numbers less than 1024 usually require superuser privileges.
;; This is why we use 8080 and not 80 by default.
(defvar *server-port* 8080 "Default web server listening port.")

#|
5 by default. We set it high because browsers are likely to make many
connections to fetch page resources (e.g. images) in parallel, due to current
HTTP 1.0 only implementation and to still missing multi-threading
implementation.
|#
(defvar *server-backlog* 100 "Default web server queue length.")

(defvar *document-root* "site"
  "Default site directory and server document root.")

#|
Key searches are case insensitive. There is no :initial-content option for
make-hash-table in the Common Lisp standard this should be fixed with a macro.
TODO: make sure all commonly used file types are covered here.
You should write a program to convert Apache mime.types file into a Lisp hash.
|#
(defconstant +mime-type-map+ #.(list-to-hash '(
  ;; To sort this list in VIM use :'a,.sort i assuming you have marked the
  ;; first line with ma and the current line is the last. You can also select
  ;; the list typing V on the first line, then /^) , return, k and type :sort i
  "css" (text css) 
  "flv" (video x-flv)
  "gif" (image gif) 
  "htm" (text html)
  "html" (text html)
  "ico" (image x-icon)
  "jpg" (image jpeg)
  "js" (application javascript)
  "mp3" (audio mpeg)
  "ogg" (audio ogg)
  "png" (image png)
  "swf" (application x-shockwave-flash)
  "txt" (text plain)
)) "Web server mime-type map.")

(defvar *lisp-dep-file* "hyde-dependencies"
  "Default path and file name for the auto-generated Lisp makefile, relative
to +document-root+. A \".lisp\" extension will be automatically added.")

(defvar *exclude-file-name* "hyde-exclude-list"
  "Name of a file containing a list of files and directory in the current
directory to be excluded from the make process. A \".lisp\" extension will
be automatically added.")
