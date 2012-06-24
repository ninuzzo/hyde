#|

Caching web server that compiles Lisp to HTML code automatically
and web-based make utility.
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

(defvar *document-root-truepath*)

(defun convert-and-serve-file (method source-pathname dest-pathname)
  "Generates or regenerates HTML code from a Lisp file and serves it."
  (handle-all
    (let ((html (inc-first source-pathname)))
      #|
      Write html code to a corresponding HTML file. This will speed up the
      whole site generation process later and is also a caching mechanims,
      which provides better performance of the HTTP server. Note this must be
      done before serving the file because ok-response expects a file to be
      present.
      |#
      (write-file dest-pathname html)

      ;; Update the dynamic makefile. We better do it after generating each
      ;; single file. If we did it only at server shut down, in case of
      ;; black-out all updates would be lost!
      (write-hash *lisp-dependencies* *lisp-dep-file*)

      ;; Serve re-generated or first generated html code.
      (ok-response method :file dest-pathname :content html))
    (server-error-response method condition)))

(defun convert-file (source-pathname dest-pathname)
 "Generate or regenerate HTML code from a Lisp file and return dest-pathname."
  (let ((html (inc-first source-pathname)))
    (write-file dest-pathname html) dest-pathname))

;;; TODO: during one run cache file-write-date values for efficiency. It is
;;; common for many a script to share the same dependencies (e.g. common
;;; include or layout files). It is assumed that during a directory make, no
;;; files will change :) See: http://www.tfeb.org/lisp/hax.html#MEMOIZE
(defun convert-directory-and-report (method dir-pathname)
  "Make sure a whole directory is up-to-date. Generate an HTML report with
links to changed files which still need to be tested manually by the user."
  (handle-all
    (let ((pathname-list (process-directory dir-pathname))
          (dir-name (namestring dir-pathname)))
      (ok-response method :content
        (html (head (meta :charset "utf8") (title dir-name))
          (body (header (h1 dir-name))
            (if pathname-list
                (cat (p "List of updated files. Click on each to test it.")
                  (ul (cat
                        (loop for abs-file in pathname-list for rel-file
                                ;; Not (si:getcwd), because that is always the
                                ;; document root.
                                = (enough-namestring abs-file dir-pathname)
                          collect
                          (li (a :target "_blank" :href rel-file abs-file))))))
                (p "All HTML files are up to date in this directory."))
            (let ((index-page (index-p dir-pathname)))
              (if index-page
                  (footer (a :href "index.html" "Homepage"))))))))
    (server-error-response method condition)))

(defun file-needs-rebuild-p (source-file dest-file)
  "Tell if a source LISP files needs to be re-compiled into HTML."
  (let ((source-truepath (truename source-file))
        (dest-date (file-write-date dest-file)))
    (or (not dest-date) ; Destination file just does not exist.
        (some #'(lambda (dep-truepath)
                  (let ((dep-date (file-write-date dep-truepath))) ; nil if the file is not found.
                    (and dep-date (> dep-date dest-date))))
              (cons source-truepath
                    (gethash source-truepath *lisp-dependencies*))))))

(defun process-file (source-pathname)
  "Compile a Lisp file into HTML if something has changed in either the Lisp
file or one of its dependencies. Return the (re)generated HTML target
pathname, or nil if nothing is changed."
  (let* ((dest-pathname (make-pathname :defaults source-pathname
                                       :type "html")))
    (if (file-needs-rebuild-p source-pathname dest-pathname)
      ;; CASE 1: Existent HTML is stale. Update it.
      ;; CASE 2: HTML file does not exist. First generation.
      (convert-file source-pathname dest-pathname)))) ;; CASE 3: No update is necessary. Do nothing.

(defun process-directory (topdir-pathname)
  "Compile all Lisp files that have changed or with changing dependencies for
this directory and recursively for all subdirectories. Return a list of
changed target HTML files, if no condition happens."
  (let ((processed-list))
    (labels ((process-level (dir-pathname) ; Side effect: adds to processed-list.
               (let* ((exclude-file
                        (probe-file (make-pathname :defaults dir-pathname
                                      :name *exclude-file-name* :type "lisp")))
                      (exclude-strings (read-lisp exclude-file))
                      #|
                      Transform the list of files to exclude from HTML
                      generation (a list of strings) into a list of pathnames
                      and check their existence.
                      Add to this list:

                      * the dependencies file (actually we need to do that
                        only if this is the root directory, but we do not
                        bother to test for that)
                      * the exclude file itself
                      
                      Note every element of the exclude-list has to be a
                      truename for comparisons with the directory listing to
                      work correctly!
                      |#
                      (exclude-list
                        (nconc (list *lisp-dep-file*)
                          (if exclude-file (list exclude-file))
                          (mapcar #'(lambda (entry)
                            (truename (merge-pathnames entry dir-pathname)))
                                  exclude-strings))))
                 #|
                 Process all files files in current directory, get the list of
                 changed files in this directory and add it to the overall
                 list of processed files.
                 http://stackoverflow.com/questions/1403717/how-do-i-iterate-through-a-directory-in-common-lisp
                 setq is needed because nconc cannot change NIL to a list. In
                 other words, if the value of processed-list is NIL (as
                 initially is), then the value of (nconc processed-list (...))
                 is (...), but processed-list will not have been changed. The
                 "problem" is that nconc simply has a collection of pointers
                 to work with, and does not know where they originally came
                 from, i.e., does not know that this NIL is the value of
                 processed-list. A more efficient alternative would be to
                 implement tconc as described here:
                 http://www.cs.northwestern.edu/academics/courses/325/exercises/lisp-exs.html#tconc
                 Another (but clumsier) alternative would be to ouput
                 everything to stdout, then capture and read the output as a
                 list using the with-output-to-string macro.
                 |#
                 (setq processed-list
                       (nconc processed-list
                         (loop for source-file in
                           (nset-difference
                             (directory (make-pathname :name :wild
                                          :type "lisp" :defaults dir-pathname))
                             exclude-list :test 'equal)
                           when (process-file source-file) collect it)))
                 ;; Process sub-directories recursively, if any.
                 (loop for sub-dir in
                   (nset-difference
                     ;; http://stackoverflow.com/questions/5282089/listing-directories-in-clisp
                     (directory
                       (make-pathname :directory
                         (nconc (pathname-directory dir-pathname) '(:wild))))
                     exclude-list :test 'equal)
                   do (process-level sub-dir)))))
      ;; Labels body (back to process-directory code).
      ;; Start the recursive directory traversal.
      (unwind-protect (progn (process-level topdir-pathname) processed-list)
        #|
        Make sure the dynamic makefile is updated. We do it just once at the
        end of a make process to optimize. In case a condition is signalled,
        it should also be done for any dependencies update discovered in files
        successfully compiled before the error happened, in order for them to
        not be lost.
        |#
        (write-hash *lisp-dependencies* *lisp-dep-file*)))))

;;; This implements the main process logic of the Hyde server.
(defun hyde-request-handler (method path)
  #|
  TODO: isn't it better to convert a web path name into a logical path name?
  We do not want to accept a syntax like:
  http://localhost:8080/C:\path\file.ext
  http://ecls.sourceforge.net/new-manual/ch11.html
  http://cybertiggyr.com/pathnames-0/index.html
  |#
 
  #|
  Get the true path name, nil if the file or directory does not exist.  Case
  of returned pathname will be the same as in the URL.
  http://clhs.lisp.se/Body/19_bbab.htm
  http://cl-cookbook.sourceforge.net/files.html#exists
  Fortunately, probe-file in ECL also supports directories. But the standard
  does not mandate this. In other LISPs you would need to use a wrapper, see:
  http://www.gigamonkeys.com/book/practical-a-portable-pathname-library.html

  TODO: one can use .. in path and get stuff outside the document root.
  Very insecure, but this is just a development server...
  |#
  (let ((truepath (probe-file path))) ; nil if file or dir does not exist
    (if truepath ; directory-p and pathname-type croak on nil
        ;; Existent file or directory. "or" in the previous sentence is an
        ;; "xor". It is not possible to have a file and directory entries with
        ;; the same name in a single directory in any filesystem type!
        (if (directory-p truepath)
            ;; Existent directory.
            (convert-directory-and-report method truepath)
    
            ;; Existent file.
            (if (string-equal (pathname-type truepath) "html") ; get extension and compare
                ;; Existent HTML file.
                #|
                pathname-type is not an Accessor, so it's not setf-able. The
                only way to change a file type is to create a new pathname
                with the same properties but the new extension. See:
                http://objectmix.com/lisp/636192-changing-file-type.html
                
                Also note some nested ifs could be simplified by using boolean
                conditions and relying on short evaluation, but if this forces
                one to introduce side effects, I prefer not to do it.
                |#
                (let ((source-truepath
                        (probe-file (make-pathname :defaults truepath
                                                   :type "lisp"))))
                  ;; Does a LISP generator script for this HTML file exists?
                  ;; And has the LISP generator (or at least one of its
                  ;; dependencies) been modified after last HTML generation
                  ;; date?
                  (if (and source-truepath
                           (file-needs-rebuild-p source-truepath truepath))
                      ;; CASE 1: Existent HTML is stale. Update and
                      ;; serve it.
                      (convert-and-serve-file method source-truepath truepath)

                      ;; CASE 2: Existent HTML is up to date (serve
                      ;; cached HTML code)
                      ;; CASE 3: a corresponding LISP file does not exist
                      ;; (serve the "static" HTML file).
                      (ok-response method :file truepath)))
         
                ;; CASE 4: Existent non-HTML file. Serve it.
                (ok-response method :file truepath)))
         
        ;; Unexistent file or directory.
        (let ((dest-pathname (parse-namestring path)))
          (if (and (file-p dest-pathname)
                  (string= (pathname-type dest-pathname) "html"))
              ;; Unexistent .html file.
              (let ((source-truepath
                      (probe-file (make-pathname :defaults dest-pathname
                                                 :type "lisp"))))
                (if source-truepath
                    ;; CASE 5: .lisp source file for requested .html file
                    ;; exists. First generation of the .html file.
                    (convert-and-serve-file method source-truepath
                                            dest-pathname)
        
                    ;; CASE 6: No .html file found and no source file .lisp
                    ;; exists.
                    (not-found-response method path)))

              ;; CASE 7: Unexistent directory.
              ;; CASE 8: Unexistent non .html file.
              (not-found-response method path))))))

;; TODO: more complex command line parameter processing to allow the user to
;; override server-address, server-port and other config parameters at each
;; server execution. For now, the user can only edit default values in
;; conf.lisp.
(defun process-cmdline (&key (start-arg-num 1))
  "Override the Hyde server default configuration from command line."
  ;; Override the document root from command line.
  (if (> (si:argc) start-arg-num)
      (setq *document-root* (si:argv start-arg-num))))

(defun start-server ()
  #|
  Change to the document root directory. This simplifies matters.
  http://stackoverflow.com/questions/10049338/common-lisp-launch-subprocess-with-different-working-directory-than-lisp-proces
  si:chdir is ECL-specific and sets the current directory as seen by the
  operating system. When a file is included in a sub-dir of this dir, we will
  change the path as used and seen by Lisp to the former dir, but the server
  process will remain here, in the document root.
  |#
  (si:chdir (setq *document-root* (truename *document-root*)))
  ;; Dependencies between Lisp files are computed dynamically at runtime and
  ;; stored on disk whenever the server is stopped, read from disk whenever
  ;; the server is started.
  (setq *lisp-dep-file*
    (truepath-create (make-pathname :type "lisp" :defaults
                       (merge-pathnames *lisp-dep-file* *document-root*))))
  (setq *lisp-dependencies* (read-hash *lisp-dep-file*))

  (format t
          "~&;;; Welcome to Hyde ~a~%;;; Document root is ~a~%;;; Minimize this window and point your browser to http://~a:~a/~%"
          +server-version+ (namestring *document-root*)
          *server-address* *server-port*)
  (serve #'hyde-request-handler))
