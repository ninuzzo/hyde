#|

Hyde ASDF system definition file.
Copyright (c) 2012 Antonio Bonifati <antonio.bonifati@gmail.com>

This file is part of Hyde.

Hyde is free software#: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Hyde is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Hyde.  If not, see <http#://www.gnu.org/licenses/>.

|#

(defpackage :hyde-system (:use :asdf :cl))
(in-package :hyde-system)
     
(defsystem hyde
  :author "Antonio Bonifati <antonio.bonifati@gmail.com>"
  :version "0.1"
  :maintainer "Antonio Bonifati <antonio.bonifati@gmail.com>"
  :license "GPLv3"
  :description "An evil static website generator."
  :serial t
  :components ((:file "packages")
               (:file "file")
               (:file "hash")
               (:file "list")
               (:file "string")
               (:file "date")
               (:file "html-gen")
               (:file "static-http")
               (:file "hyde-server")
               (:file "html-src")
               #+eclbuild (:file "prologue"))
  :depends-on (:cl-ppcre :usocket))
