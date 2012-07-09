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
The mime-type map can not be configured here any more, any change must be made
to src/mime-type.lisp and thus requires a program re-build. But since the
mime-type table is rather complete, there should be very little need for
changes.
|#

(defvar *lisp-dep-file* "hyde-dependencies"
  "Default path and file name for the auto-generated Lisp makefile, relative
to +document-root+. A \".lisp\" extension will be automatically added.")

(defvar *exclude-file-name* "hyde-exclude-list"
  "Name of a file containing a list of files and directory in the current
directory to be excluded from the make process. A \".lisp\" extension will
be automatically added.")
