#|

A fast library for generating HTML5 files with lispy syntax.
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

;; The dependency table is initially empty.
(defvar *lisp-dependencies* (make-hash-table :test 'equal))

#|
The Lisp main file being currently processed, the one that generates the
overall target HTML file. This variable is (re)bound by the inc-first
function, every time this function is called, but it needs to be declared
unbound outside for rebounding to work. See:
http://en.wikipedia.org/wiki/Common_Lisp#Kinds_of_environment
|#
(defvar *lisp-mainfile*)

;;; Last Lisp file being currently processed, useful for debugging.
;;; This variable is (re)bound but the inc function.
(defvar *lisp-file*)

(defun attribs-content (arg-list)
  "Convert a tag's argument list into a proper HTML attribute string and
content. Return this string. For empty attribute lists, a > sign is returned."
  ;; (not (null arg-list)) is pleonastic.
  (if (and arg-list (symbolp (first arg-list)))
      #|
      Support for attributes with no value is not provided. Anyway their use
      is discouraged, use an empty value "" instead. Moreover, for
      compatibility with XHTML we always use quotation marks to define
      attribute values. I prefer not to break XHTML compatibility just to save
      a few bytes. See:
      http://webdesign.about.com/od/html5tags/f/quotes-required-html-5-attributes.htm
      Lowercase is preferred for attribute names. HTML5 is case-insensitive,
      but lowercase letters are easier to read than uppercase letters.
      Conversion to XHTML is easier too.
      |#
      (format nil " ~(~a~)=\"~a\"~a"
              ;; Note that, although ECL automatically applies tail-call
              ;; optimization when compiling, despite this being not mandated
              ;; by the CL standard, this kind of call cannot be
              ;; tail-optimized.
              (car arg-list) (cadr arg-list) (attribs-content (cddr
                                                                arg-list)))
      ;; No more attributes, close open tag and print element content.
      (format nil ">~{~a~}" arg-list)))

(defun attribs (arg-list)
  "Convert an attribute list into a proper HTML attribute string.
Return this string, except that empty attribute lists are returned as nil."
  (if arg-list
      ;; If arg-list length is not even (cadr is nil) the last attribute will
      ;; be empty.
      (format nil " ~(~a~)=\"~@[~a~]\"~@[~a~]"
              ;; Another recursive call which is not a tail-call and won't be
              ;; optimized. Anyway, arg-lists are usually quite short and
              ;; there shouldn't be a significant performance penalty or risk
              ;; of stack overflow.
              (car arg-list) (cadr arg-list) (attribs (cddr arg-list)))))

(defun cont (&rest args)
  "Escape and catenate one or more content strings. It should always be used
unless you are sure the strings do not contain < or > signs."
  #|
  Note that ampersands must be replaced first, otherwise a < would become
  &amp;lt; See:
  http://cl-cookbook.sourceforge.net/strings.html
  http://stackoverflow.com/questions/4366668/str-replace-in-lisp
  TODO: having to depend on cl-ppcre just for this task is probably overkill.
  Try to develop a fast algorithm (ideally faster) to make such replacements
  in standard Common Lisp. Do the same for the attr function below and you can
  then remove cl-ppcre from the source tarball.
  |#
  (mapcat #'(lambda (str)
              (regex-replace-all ">" (regex-replace-all "<"
                                       (regex-replace-all "&" str "&amp;")
                                       "&lt;")
                "&gt;"))
          args))

(defun attr (str)
  "Escape an attribute string. It should always be used unless you are sure
the string does not contain any double quote character or the attribute is a
URI or URL."
  ;; an & must be HTML-escaped first, otherwise a " would become &amp;quot;
  (regex-replace-all "\"" (regex-replace-all "&" str "&amp;") "&quot;"))

#|
Note that the definition of a "closed element" includes empty tags that happen
to not contain any content (e.g. <script src="..."></script>). The latter
should not be self-closing in HTML5 (e.g. <script src="..."/> is wrong).
|#
(defun closed-elem (tag arg-list)
  ;; Lowercase is preferred - see attribs-content.
  (format nil "<~(~a~)~a</~(~a~)>"
          tag (attribs-content (flatten arg-list)) tag))

;;; Although not mandatory in HTML5, we use self-closing tags for void
;;; elements for XHTML compatibility.
(defun void-elem (tag attr-list)
 #|
 Lowercase is preferred - see attribs-content.
 Whitespace before the slash is not necessary. Using whitespace in that
 fashion is a convention inherited from the compatibility guidelines in XHTML
 1.0, Appendix C: http://www.w3.org/TR/xhtml1/#C_2
 |#
 (format nil "<~(~a~)~@[~a~]/>" tag (attribs (flatten attr-list))))

#|
I prefer to use variadic function calls for passing attribute values,
rather than plain lists or worse lists of pairs.
It makes the syntax less cluttered and the user code simpler.
Unlike LAML, I do now think is useful to allow users to put tag content
before attribute names and values. This is the standard order in HTML
everyone is accustomed to, why change it? Does it make sense to put an
attribute after a long content, where is it unlikely to be seen?
See also here:
http://stackoverflow.com/questions/1263775/how-might-i-format-an-alist-in-common-lisp
http://people.cs.aau.dk/~normark/laml-distributions/laml/info/laml-motivation.html
|#
(defmacro def-elem (name &key fun desc (void nil) newline)
  "Macro to generate a function that outputs an HTML element."
  ;; This FORMAT is for debugging.
  #|(format t
          "~&DEBUG: (def-elem ~a~@[ :fun ~a~]~@[ :desc \"~a\"~]~@[ :void ~a~]~@[ :newline ~a~])"
          name fun desc void newline)|#
  `(defun ,(if fun fun name) (&rest args)
     ,desc
     #|
     Concatenate the arguments, then wrap them in a tag and return the new
     string. So function tags return strings. We do not build a tree data
     structure in Lisp and then traverse it to generate the HTML. That would
     be needed to provide full HTML validation, but that is not the aim of
     this library, because is hard to do better than the W3C validator in this
     area. Indeed, one can always use the official validator on the compressed
     generated HTML code, and then fix possible problems detected by the
     validator in the Lisp generator code. In practice this is not difficult
     to do.
     |#
     ;; Do not use cat, but rather strcat (see string.lisp), for performance
     ;; reasons. Better to generate very tight code here, since these
     ;; functions are used a lot! Use macro-expand to check whether the
     ;; generated code can be further simplified whenever you edit this macro.
     ,(let ((pre-str (strcat
                       (if (eq name 'html)
                           ;; We better insert a newline after the DOCTYPE as
                           ;; suggested here:
                           ;; http://www.whatwg.org/specs/web-apps/current-work/multipage/syntax.html#writing
                           (format nil "~&<!DOCTYPE html>~C" #\linefeed))
                       (if (or (eq newline 'before) (eq newline 'both))
                           ;; We use a linefeed rather than a newline,
                           ;; just to save 1 byte.
                           (string #\linefeed))))
            (post-str (if (or (eq newline 'after) (eq newline 'both))
                          (string #\linefeed))))
        (if (or (string/= pre-str "") post-str)
            `(strcat ,@(if (string= pre-str "") nil (list pre-str))
               ,(if void
                    `(void-elem ',name args)
                    `(closed-elem ',name args))
               ,@(if post-str (list post-str) nil))
            (if void 
                `(void-elem ',name args)
                `(closed-elem ',name args))))))

#|
This function adds modularity to HTML by evaluating and returning the output
of an external file. It indeed implements the 'include' tag that is missing in
HTML, although for convenience it is a special tag with a syntax different
from, e.g. the img tag, in that there is no src attribute for brevity:
(inc "path/file")
This call can also be used to implement layouts. They are nothing else than
a kind of include where some HTML code is passed in one or more 'content'
variables (you choose how to call them).
There is a discussion about the missing include tag in HTML here:
http://programmers.stackexchange.com/questions/7245/why-no-client-side-html-include-tag
|#
(defun inc (file)
  "Include an external Lisp file and add it to the dynamic, auto-generated
dependency list for current *lisp-mainfile*. Return whatever the included file
prints as a string."
  (let ((caller *lisp-file*))
    ;; Save the name of the file in a global-scoped variable for debugging
    ;; purposes, to be printed in case a condition happens.
    ;; Note how we must save the caller name before changing the *lisp-file*
    ;; and that value must be restored before returning from this function.
    (setq *lisp-file* (truename file))
    #|
    TODO: is it worth to provide support for compiled included files? Usually
    users will use includes for layouts and headers/footers, which is not too
    much stuff to be interpreted every time an HTML is updated. If we want to
    compile include files, we would need to keep track of their dependencies
    dynamically. Now we are simply using a hash that stores a flat list of
    dependencies (value) for each target (key). To support compilation of
    include files we would need to complicate the data structure and represent
    a dependency tree for each target. This makes the implementation more
    complex, but it is the price to pay for make the system scalable. But,
    wait, usually include files are not big... so, I don't see the need for
    this feature at the moment and I am sticking to this one-line
    implementation, which is the easiest, but this could change in the future.
    Another implementation option would be to read the include file source
    code and discover its dependencies statically. See file-forms here:
    http://www.cl-user.net/asp/RNFR/sdataQI9SRRA$OwAoDQ3YNypX8yBX8yBXnMq=/sdataQu3F$sSHnB==
    But that way a conditional include would always be considered a
    dependency, possibly leading to more re-makes than necessary. Aside from
    being disadvantageous from a performance point of view, this solution is
    also more complex to implement.
    |#

    ;; Register this dependency. We keep a separate dependency list for each
    ;; target (main Lisp file that generates an HTML file).
    (unless (equal *lisp-file* *lisp-mainfile*)
      (pushnew *lisp-file* (gethash *lisp-mainfile* *lisp-dependencies*)))

    #|
    Change to the current directory, so that further inc found inside the
    loaded file will use a path relative to the file itself rather than the
    document root. This is handier for the user. See:
    http://cybertiggyr.com/gene/pathnames-0/node11.html
    ECL has also si:chdir, but overriding *default-pathname-defaults* locally
    seems to be simpler.
    |#
    (let* ((*default-pathname-defaults*
            (make-pathname :directory (pathname-directory *lisp-file*)))
           (out-string
            (with-output-to-string (*standard-output*)
              ;; In ECL a comment is printed on stdout without :verbose nil.
              (load *lisp-file* :verbose nil))))

      ;; Restore previous variable value.
      (setq *lisp-file* caller)

      ;; Return whatever the included file printed as a string.
      out-string)))

;;; Include the first Lisp source file, the one that directly generated the
;;; overall HTML code. This is not exported, user code should never use this,
;;; but rather use inc (see above).
(defun inc-first (file)
  (let ((*lisp-mainfile* (truename file))) 
    ;; Reset dependencies for the file that is being loaded,
    ;; because they may have changed and we must recompute them.
    (setf (gethash *lisp-mainfile* *lisp-dependencies*) '()) 

    ;; Initialize this variable or inc won't work.
    (setq *lisp-file* *lisp-mainfile*)

    (inc *lisp-mainfile*)))

#|
Do not use (defparameter ...) to declare template variables, that is variables
to be shared among inc-luded scripts, since that defines globally scoped
dynamic variables. Remember that Hyde loads each script in sequence when
making a whole site directory, so one script can pollute the global
environment of the next one that gets executed! We better use locally scoped
dynamic variables.
|#
(defmacro tvar (binding-list &body forms)
  "Macro shortcut to define safe local dynamically scoped template variables,
using a plain list syntax, avoiding the further nest level of a let*."
  ;; This construct already ensuresthe binding-list is evaluated only once,
  ;; no additional let is required in this case.
  `(let* ,(pair-elements binding-list) 
     #|
     Lisp local variables are lexical (static) by default.
     We need to change that since template variables must be still bound if a
     script includes another. This macro just makes the syntax to define local
     dynamical vars easier in Lisp. See:
     http://www.tfeb.org/lisp/hax.html#DYNAMIC-STATE
     http://psg.com/~dlamkins/sl/chapter08.html
     http://www.n-a-n-o.com/lisp/cmucl-tutorials/LISP-tutorial-12.html
     http://www.gigamonkeys.com/book/variables.html
     http://en.wikipedia.org/wiki/Scope_(computer_science)#Lexical_scoping_and_dynamic_scoping
     |#
     (declare (special ,@(odd-elements binding-list))) ,@forms))

(defmacro lvar (binding-list &body forms)
  "Macro equivalent to let* for defining lexical (static) variables,
but using a plain list syntax, avoiding the further nest level of a let*."
  `(let* ,(pair-elements binding-list) ,@forms))

#|
There is no way to define these HTML macros in a loop, since macros arguments
are not evaluated! Macros cannot be used with apply too because they are not
functions. As a consequence, if you add a new tag here, remember to export it
in packages.lisp too. By default HTML output is minified. If you want more
readability, consider addingi :newline before to some tags (e.g. address,
body, h1..h6, head, li, link, meta, ol, p, table, title, tr) and :newline
after to others (e.g. br, hr, html). By default an element is defined as a
closed element. If it is a void element that you want to define, please
remember to set :void to true!
TODO: better descriptions and check that all HTML5 tags are defined.
http://developers.whatwg.org/section-index.html
|#
(def-elem a :desc "Define a hyperlink.")
(def-elem abbr :desc "Define an abbreviation.")
(def-elem address :desc "Define contact information for the author/owner of a document/article. Actually named address in HTML")
(def-elem area :desc "Define an area inside an image-map." :void t)
(def-elem article :desc "Define an article.")
(def-elem aside :desc "Define content aside from the page content.")
(def-elem audio :desc "Define sound content.")
(def-elem b :desc "Define bold text.")
(def-elem base :desc "Specify the base URL/target for all relative URLs in a document." :void t)
(def-elem bdi :desc "Isolate a part of text that might be formatted in a different direction from other text outside it.")
(def-elem bdo :desc "Override the current text direction.")
(def-elem blockquote  :desc "Define a section that is quoted from another source.")
(def-elem body :desc "Define the document's body.")
(def-elem br :desc "Define a single line break." :void t)
(def-elem button :desc "Define a clickable button.")
(def-elem canvas :desc "Use to draw graphics, on the fly, via scripting (usually JavaScript).")
(def-elem caption :desc "Define a table caption.")
(def-elem cite :desc "Define the title of a work.")
(def-elem code :desc "Define a piece of computer code.")
(def-elem col :desc "Specify column properties for each column within a <colgroup> element." :void t)
(def-elem colgroup :desc "Specify a group of one or more columns in a table for formatting.")
(def-elem command :desc "Define a command button that a user can invoke." :void t)
(def-elem datalist :desc "Specify a list of pre-defined options for input controls.")
(def-elem dd :desc "Define a description of an item in a definition list.")
(def-elem del :desc "Define a text that has been deleted from a document.")
(def-elem details :desc "Define additional details that the user can view or hide.")
(def-elem dfn :desc "Define a definition term.")
(def-elem div :desc "Define a section in a document.")
(def-elem dl :desc "Define a definition list.")
(def-elem dt :desc "Define a term (an item) in a definition list.")
(def-elem em :desc "Define emphasized text.")
(def-elem embed :desc "Define a container for an external application or interactive content (a plug-in)." :void t)
(def-elem fieldset :desc "Group related elements in a form.")
(def-elem figcaption  :desc "Define a caption for a <figure> element.")
(def-elem figure :desc "Specify self-contained content.")
(def-elem footer :desc "Define a footer for a document or section.")
(def-elem form :desc "Define an HTML form for user input.")
(def-elem h1 :desc "Define HTML level 1 headings.")
(def-elem h2 :desc "Define HTML level 2 headings.")
(def-elem h3 :desc "Define HTML level 3 headings.")
(def-elem h4 :desc "Define HTML level 4 headings.")
(def-elem h5 :desc "Define HTML level 5 headings.")
(def-elem h6 :desc "Define HTML level 6 headings.")
(def-elem head :desc "Define information about the document.")
(def-elem header :desc "Define a header for a document or section.")
(def-elem hgroup :desc "Group heading (<h1> to <h6>) elements.")
(def-elem hr :desc " Define a thematic change in the content." :void t)
(def-elem html :desc "Define the root of an HTML document.")
(def-elem i :desc "Define a part of text in an alternate voice or mood.")
(def-elem iframe :desc "Define an inline frame.")
(def-elem img :desc "Define an image." :void t)
(def-elem input :desc "Define an input control." :void t)
(def-elem ins :desc "Define a text that has been inserted into a document.")
(def-elem kbd :desc "Define keyboard input.")
(def-elem keygen :desc "Define a key-pair generator field (for forms)." :void t)
(def-elem label :desc "Define a label for an input element.")
(def-elem legend :desc "Define a caption for a <fieldset>, <figure>, or <details> element.")
(def-elem li :desc "Define a list item.")
(def-elem link :desc "Define the relationship between a document and an external resource (most used to link to style sheets)." :void t)
(def-elem map :fun imgmap :desc "Define a client-side image-map. Actually named map in HTML.")
(def-elem mark :desc "Define marked/highlighted text.")
(def-elem menu :desc "Define a list/menu of commands.")
(def-elem meta :desc "Define metadata about an HTML document." :void t)
(def-elem meter :desc "Define a scalar measurement within a known range (a gauge).")
(def-elem nav :desc "Define navigation links.")
(def-elem noscript :desc "Define an alternate content for users that do not support client-side scripts.")
(def-elem object :desc "Define an embedded object.")
(def-elem ol :desc "Define an ordered list.")
(def-elem optgroup :desc "Define a group of related options in a drop-down list.")
(def-elem option :desc "Define an option in a drop-down list.")
(def-elem output :desc "Define the result of a calculation.")
(def-elem p :desc "Define a paragraph.")
(def-elem param :desc "Define a parameter for an object." :void t)
(def-elem pre :desc "Define preformatted text.")
(def-elem progress :desc "Represent the progress of a task.")
(def-elem q :desc "Define a short quotation.")
(def-elem rp :desc "Define what to show in browsers that do not support ruby annotations.")
(def-elem rt :desc "Define an explanation/pronunciation of characters (for East Asian typography).")
(def-elem ruby :desc "Define a ruby annotation (for East Asian typography).")
(def-elem s :desc "Define text that is no longer correct.")
(def-elem samp :desc "Define sample output from a computer program.")
(def-elem script :desc "Define a client-side script.")
(def-elem section :desc "Define a section in a document.")
(def-elem select :desc "Define a drop-down list.")
(def-elem small :desc "Define smaller text.")
(def-elem source :desc "Define multiple media resources for media elements (<video> and <audio>)." :void t)
(def-elem span :desc "Define a section in a document.")
(def-elem strong :desc "Define important text.")
(def-elem style :desc "Define style information for a document.")
(def-elem sub :desc "Define subscripted text.")
(def-elem summary :desc "Define a visible heading for a <details> element.")
(def-elem sup :desc "Define superscripted text.")
(def-elem table :desc "Define a table.")
(def-elem tbody :desc "Group the body content in a table.")
(def-elem td :desc "Define a cell in a table.")
(def-elem textarea :desc "Define a multiline input control (text area).")
(def-elem tfoot :desc "Group the footer content in a table.")
(def-elem th :desc "Define a header cell in a table.")
(def-elem thead :desc "Group the header content in a table.")
(def-elem time :fun datetime :desc "Define a date/time. Actually named time in HTML.")
(def-elem title :desc "Define a title for the document.")
(def-elem tr :desc "Define a row in a table.")
(def-elem track :desc "Define text tracks for media elements (<video> and <audio>)." :void t)
(def-elem ul :desc "Define an unordered list.")
(def-elem var :desc "Define a variable. Actually named var in HTML.")
(def-elem video :desc "Define a video or movie.")
(def-elem wbr :desc "Define a possible line-break." :void t)
