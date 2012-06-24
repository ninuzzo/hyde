#|

Hyde packages definition file. This must be loaded before any other package.
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

(defpackage #:hyde
 (:use #:cl)
 (:import-from #:cl-ppcre #:regex-replace-all)
 (:import-from #:usocket #:socket-accept #:socket-close #:socket-listen
  #:socket-stream)
 (:export
  ;; date
  #:get-gmt-date-string
  ;; file
  #:directory-p #:file-more-recent-p #:file-p #:file-size #:mime-type
  #:read-file #:read-lisp #:truepath-create #:write-file
  ;; hash
  #:hash-to-list #:list-to-hash #:read-hash #:write-hash
  ;; static-http
  #:ok-response #:handle-all #:index-p #:print-condition #:print-response
  #:not-found-response #:serve #:server-error-response
  ;; hyde-server
  #:convert-file #:file-needs-rebuild-p #:process-cmdline
  #:process-directory #:process-file #:start-server
  ;; list
  #:even-elements #:flatten #:odd-elements #:pair-elements
  ;; string functions
  #:cat #:echo #:map-cat #:strcat
  ;; html-gen (HTML utilities)
  #:attr #:cont #:def-elem #:inc #:lvar #:tvar
  ;; html-src (HTML source utilities) 
  #:attr-val #:cont-list
  ;; html-gen (HTML5 elements)
  #:a #:abbr
  #:address ; address conflicts in CLISP, but we support only ECL.
  #:area #:article #:aside #:audio #:b #:base #:bdi #:bdo #:blockquote #:body
  #:br #:button #:canvas #:caption #:cite #:code #:col #:colgroup #:command
  #:datalist #:dd #:del #:details #:dfn #:div #:dl #:dt #:em #:embed #:fieldset
  #:figcaption #:figure #:footer #:form #:h1 #:h2 #:h3 #:h4 #:h5 #:h6 #:head
  #:header #:hgroup #:hr #:html #:i #:iframe #:img #:input #:ins #:kbd #:keygen
  #:label #:legend #:li #:link
  #:imgmap ; map conflicts with a Lisp standard function.
  #:mark #:menu #:meta #:meter #:nav #:noscript #:object #:ol #:optgroup
  #:option #:output #:p #:param #:pre #:progress #:q #:rp #:rt #:ruby #:s
  #:samp #:script #:section #:select #:small #:source #:span #:strong #:style
  #:sub #:summary #:sup #:table #:tbody #:td #:textarea #:tfoot #:th #:thead
  #:datetime ; time conflicts with a Lisp standard function.
  #:title #:tr #:track #:ul
  #:var ; var conflicts in SBCL, not in ECL or CLISP.
  #:video #:wbr))
