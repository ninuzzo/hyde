#|

A simple web server for static files.
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
WARNING: This web server is quite simple and neither optimized nor secure
to be used in production environments. It's a toy server, meant for
development only! Only a small subset of HTTP/1.1 is supported, but that's
enough for testing! E.g. it just ignores the request content.

Anyway, using hunchentoot or toot would have been not only
overkill but also troublesome, because they require a lot of dependencies and
there are issues in making them runder under ECL. Therefore to serve your
statically generated pages use a true HTTP server like the one provided by an
ISP, never use this one, at least until its implementation will be completed
and this warning will disappear.

Reference: http://www.jmarshall.com/easy/http/#toc
http://blog.wachowicz.eu/?p=256
|#

#|
This needs to be defined at read time, because otherwise hyde won't
compile. ASDF is not included in the compiled binary. There are currently troubles including it in ECL, but just for printing the program version, I do not want to make the executable bigger. See:
http://sourceforge.net/mailarchive/forum.php?thread_name=CAE1rE%2BFvmcykNKTtb92-bGDx7o9L6LafaAWhOCuxSyoTWpOKsg%40mail.gmail.com&forum_name=ecls-list
http://sourceforge.net/mailarchive/forum.php?thread_name=CAC2LdgdhmbDg220dvVu5hrvJrw3%3Dq3Vi24HmMZ1cEQ%2BsSOMKjg%40mail.gmail.com&forum_name=ecls-list
http://sourceforge.net/mailarchive/forum.php?thread_name=CANejTzrm6fbnZnB5cYaJh0nUnXqB5Y1YLBjf9VFCLegVPvAOog%40mail.gmail.com&forum_name=ecls-list
http://sourceforge.net/tracker/?func=detail&aid=3534516&group_id=30035&atid=398053
|#
(defconstant +server-version+
             #.(slot-value (asdf:find-system 'hyde) 'asdf:version))

(defun http-char (c1 c2 &optional (default #\Space))
  (let ((code (parse-integer (coerce (list c1 c2) 'string)
                :radix 16 :junk-allowed t)))
    (if code (code-char code) default)))

;;; TODO: slow! It uses non-tail recursion...
(defun decode-url (str)
  (labels ((f (lst)
             (when lst
               (if (eql (car lst) #\%)
                   (cons (http-char (cadr lst) (caddr lst)) (f (cdddr lst)))
                   (cons (car lst) (f (cdr lst)))))))
    (coerce (f (coerce str 'list)) 'string)))

#|
Process the first line of the request:
GET /dir/file.html HTTP/1.1
and return the request method (GET, POST or HEAD) and the decoded URL path. For
simplicity, skip over protocol version and request parameters for both GET and
POST. Query strings can be ignored since this is a simple server for static
pages only.
|#
(defun parse-request (line)
  (let ((first-space (position #\space line))
        (first-mark (position #\? line)))
    (values
      ;; Method name, converted to a symbol.
      (intern (string-upcase (subseq line 0 first-space)))
      ;; Resource path. +2 is to skip both the space and the /
      ;; TODO: better error recovery for illegal request syntax.
      (decode-url (subseq line (+ 2 first-space)
                          (if first-mark first-mark
                              (position #\space line :from-end t)))))))

(defun skip-headers (stream)
  ;; (string= header "") won't work here, since we do not know the kind of
  ;; newline used by the client. We better consider that the header section is
  ;; over when there is no colon.
  (loop for header = (read-line stream nil) while (position #\: header)))
  ;; Longer implementation:
  ;(do ((header (read-line stream nil) (read-line stream nil)))
  ;    ((not (position #\: header))))

(defun print-response (status-code reason-phrase &rest headers)
  "Print the initial HTTP response line (status line)
with possibly following header lines."
  ;; http://stackoverflow.com/questions/5757290/http-header-line-break-style
  (let* ((http-nl (format nil "~C~C" #\return #\linefeed))
         (all-headers ;; These headers are included in all types of responses.
           (nconc headers (list 'Date (get-gmt-datestring)
                            'Server (format nil "hyde/~a" +server-version+)
                   ;; TODO: implement faster HTTP persistent connections!
                   'Connection "close")))
         (fmt-str (strcat "HTTP/1.1 ~a ~a" http-nl
                           "~{~:(~a~): ~a" http-nl "~}" http-nl)))
    (format t fmt-str status-code reason-phrase all-headers)))

(defun print-condition (condition &optional (stream *standard-output*))
  "Print the printed representation of a condition followed by its
human-readable error message."
  (if (boundp '*lisp-file*)
      ;;; We better print the current file name, otherwise when making a
      ;;; directory the user won't know which file is causing trouble.
      (format stream "~%;;; In file ~a" (namestring *lisp-file*))) 
  #|
  In ECL this prints the condition unreadable object as an
  expression that, when evaluated, recreates the
  condition. See:
  http://www.lispworks.com/documentation/lw50/CLHS/Body/09_ac.htm
  http://stackoverflow.com/questions/10917887/embedded-ecl-lisp-error-handling-fetch-default-error-string-and-possibly-line-nu
  TODO: include a stack trace and, if possible in ECL, handy error line
  numbers. Unfortunately the ECL reader will only tell you a character count
  position where an error occurred (blanks are ignored and not counted), so we
  need to implement our own reader, e.g. version of the load form which can
  tell both line and character position of an error. See this post:
  http://sourceforge.net/mailarchive/forum.php?thread_name=CANejTzpTi03ra7aADnUYV0TkADnbEWTgMyWNy3kZnxHYP_GfNA%40mail.gmail.com&forum_name=ecls-list
  and this project:
  http://common-lisp.net/project/trivial-backtrace/
  although it does not seem that trivial-backtrace can help in ECL.
  |#
  (print condition stream)
  #|
  This prints the error message for debugging purposes. For some reasons:
  (let ((*print-escape* nil)) (print condition stream))
  prints only a specific information (e.g. for UNBOUND-VARIABLE the variable
  name). Anyway, princ or write with the :escape nil option or ~a instead of
  ~a in format prints the error message that we want.

  For a simple-condition generated by signal, the error message can also be
  printed this way, but it is not worth.
  (apply #'format ,stream
         (simple-condition-format-control condition)
         (simple-condition-format-arguments condition))
  See:
  http://www.lispworks.com/documentation/lw50/CLHS/Body/f_smp_cn.htm#simple-condition-format-control
  http://tech.jonathangardner.net/wiki/Lisp/Conditions
  |#
  (format stream "~%;;; ~a" condition))

(defmacro handle-all (expression &body forms)
  "Macro to handle all kinds of error that can be handled within Lisp."
  `(handler-case ,expression
     ;; serious-condition(s) are left unhandled, there is little we can do to
     ;; recover so we let the program crash, while warning(s) are ignored.
     (simple-condition (condition) ,@forms)
     (error (condition) ,@forms)))
#|
Generic static server function. See:
http://lisp.livejournal.com/38356.html
TODO: add multi-threading using mp (multi-processing). Make sure to make
the hyde-request-handler is thread-safe, especially access to
+lisp-dependencies+.
See http://ecls.sourceforge.net/new-manual/ch19.html
Usocket supports multi-threading through socket-server.
Threads are enabled by default in Debian, Ubuntu and Arch Linux ECL packages.
|#
(defun serve (request-handler)
  "Start the server, run accept loop."
  #|
  Port 8080 is usually used for sites under development,
  while 80 for production web servers.
  For simplicity we use usocket instead of BSD sockets provided by ECL.
  See: http://paste.lisp.org/display/40916
  http://www.sbcl.org/manual/Networking.html
  TODO: what about using ECL threads directly, without usocket?
  Is that any faster or better?
  |#
  (let ((sock (socket-listen *server-address* *server-port*
                             :backlog *server-backlog* :reuseaddress t)))
    ;; Care should be taken to trap all possible exceptions here, and to avoid
    ;; dropping into the Lisp debugger, which may puzzle users.
    (unwind-protect
     (loop (let* ((connected-sock (socket-accept sock))
                  (stream (socket-stream connected-sock)))
       (unwind-protect
         (handle-all
           (multiple-value-bind (method path)
             #|
             Error conditions may happen here. E.g. a client may close the
             socket without sending anything o sending a partial request
             (without an ending newline). In this case, when we try to read
             the request line, an END-OF-FILE condition will be signalled. If
             we do not handle it, the server will crash.
             BTW if I use (read-line stream nil) I get:

             Condition of type: UNIX-SIGNAL-RECEIVED
             Serious signal 13 caught.
             Signal 13 is SIGPIPE - see signal (7)

             It seems to me better to handle a standard Lisp END-OF-FILE
             condition, so I won't use nil with this read-line.
             |#
             (parse-request (read-line stream)) 

             #|
             Redefine the stdout dynamic variable so that the request-handler
             can just write to stdout, making its debugging easier and generally
             minimizing string concatenation needs. If you add other
             initializations that may have the side effect of printing to
             stdout, remember that this redirection should come last!
             |#
             (let* ((*standard-output* stream))
               ;; We just ignore the request headers and the request body
               ;; for now.
               (skip-headers stream)
               ;; TODO: if the client has closed the connection, do not call
               ;; the request-handler, it is useless.

               ;; Prevent the server from crashing if an unhandled exception is
               ;; triggered in the request-handler.
               (handle-all
                 ;; TODO: any possible output on stdout would mess up with the
                 ;; HTML headers, since stdout has been redirected to the
                 ;; socket stream.

                 ;; Call the specific Hyde request-handler (e.g. Hyde).
                 (funcall request-handler method path) 

                 ;; DEBUG:
                 ;(inspect condition)
                 ;(invoke-debugger condition)

                 ;; We dump unhandled conditions happening in a
                 ;; request-handler on stderr, since stdout has been
                 ;; redirected to the stream!
                 (format *error-output*
                         "~&;;; While handling request for /~a" path)
                 (print-condition condition *error-output*)
                 #|
                 TODO: is there a way to check if the socket is still open
                 before trying to use it? We need either that or ignore-errors
                 or it could generate another uncaught error and stop the
                 server.
                 |#
                 (ignore-errors (server-error-response method condition)))))
           (format *error-output* "~&;;; While handling request first line")
           (print-condition condition *error-output*)) 
         ;; Make sure the connection socket and associated stream is always
         ;; closed. See TODO above.
         (ignore-errors (close stream) (socket-close connected-sock)))))
     ;; This ensures the port is always freed if an unexpected exception
     ;; happens. See TODO above.
     (ignore-errors (socket-close sock)))))

(defun not-found-response (method path)
  "Simple response for a non-existent resource."
  (print-response 404 "Not Found" 'Content-Type "text/plain")
  ;; Print a simple 404-error page.
  (unless (eq 'HEAD method)
    (format t "Hyde cannot find resource `~a'." path)))

(defun server-error-response (method condition)
  (print-response 500 "Lisp to HTML compile error" 'Content-Type "text/plain")
  ;; Print Lisp error condition to ease debugging.
  (unless (eq 'HEAD method)) (print-condition condition))

(defun ok-response (method &key file content mime-type)
  "Implement both GET, PUT and HEAD methods for a simplified static server.
For serving dynamic content pass only the content and no file path. If you
pass both, the message body will be the content and the file content won't be
read. This saves some time if you already have generated the content of the
file in memory. Either one of file or content must be passed. If no
Content-Type is known for a file, the Content-Type will be
application/octet-stream. For dynamic content, default is text/html."
  (print-response 200 "OK"
    #|
    TODO: to be implemented, but only if a file is passed
    The Last-Modified: header gives the modification date of the resource
    that's being returned. It's used in caching and other bandwidth-saving
    activities. See: http://ftp.ics.uci.edu/pub/ietf/http/rfc1945.html#Last-Modified
    http://ftp.ics.uci.edu/pub/ietf/http/rfc1945.html#Code304
    http://stackoverflow.com/questions/5851727/leverage-browser-caching-expires-or-max-age-last-modified-or-etag
    'Last-Modified (get-gmt-datestring (file-write-date file)) 
    |#
    'Content-Type (if mime-type mime-type (if file (mime-type file) "text/html"))
    'Content-Length (if content (length content) (file-size file)))
   ;; Print message body.
  (unless (eq 'HEAD method)
   (if content (write-string content) (read-file file))))

(defun index-p (dir-path)
  "Return the truename (physical pathname) of and index.html page if it exists
in the specified directory, nil otherwise."
  (probe-file (make-pathname :defaults dir-path :name "index" :type "html")))
