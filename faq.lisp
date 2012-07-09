;;; TODO: Make this page compatible with this microformat:
;;; http://microformats.org/wiki/question-answer

(inc "util.lisp")

(tvar
 (%title% "Hyde FAQ"
  %selected% "faq"
  %tagline% "Things that scare webmasters"
  %content%
   (article
    (header
     (h1 "Hyde FAQ"))
    (footer
     (p "Cannot find the answer here? "
      (a :href "https://github.com/ninuzzo/hyde/issues/new"
       :target "_blank" "Ask a new question...")))
    (with-toc
     (section
      (h1 :class "h2" :id "validate"
       "Why doesn't Hyde fully validate my HTML code?")
      (p "For both performance reasons and ease of implementation I have chosen to not provide this feature. I could provide it in the future, when the HTML5 standard will stabilize, letting you choose whether you want to enable it or not.")
      (p "That is a user could choose to use validate during development as a safety net and then disable it for faster generation when code has become stable and less subject to modifications that can break its correctness.")
      (p "But none of this is implemented yet and won't probably be for a long while. Meanwhile, you can use the "
       (a :href "http://validator.w3.org/" :target "_blank"
        "official W3C HTML validator")
       " on the HTML code generated by Hyde.")
      (p "Also, have a look at this W3C page about the "
       (a :href "http://dev.w3.org/html5/spec/introduction.html#syntax-errors"
        :target "_blank" "kind of mistakes you can make in HTML")
       ". That are syntax errors. The "
       (a :href "http://dev.w3.org/html5/spec/introduction.html#restrictions-on-content-models-and-on-attribute-values"
         :target "_blank" "next section")
       " talks about semantic errors.")
       (p "BTW, a reason why Hyde will never enforce validation is that through CSS one can "
       (a :href "http://stackoverflow.com/questions/6061869/are-block-level-elements-allowed-inside-inline-level-elements-in-html5" :target "_blank" "change a block element to behave like an inline one")
       ", although this is not recommended for all combinations."))
     (section
      (h1 :class "h2" :id "why-lisp" "Why Lisp and not another language?")
      (p "Lisp has a simple syntax just like HTML, plus functional programming support. If Hyde's syntax were more complex than HTML, one would rather use HTML.")
      (p "Lisp symbolic expression (s-exprs) can be nested arbitrarily and substitute HTML tags. Moreover scripts can be embedded using the same simple syntax, because nested lists are used in Lisp both as data and code. This means you still use s-exprs, the equivalent of tags, for conditionals, flow control, expressions, function calls or definitions, even data structures, no matter how simple or complex they are.")
      (p "E.g. by defining a new function using some default HTML5 function-tags provided by Hyde, you actually create a new custom tag that will be expanded into more complex HTML code:")
      (pre (code "(defun control (&key id (name id) (label id) (type \"text\"))
 (cat (label :for id label)
  (input :type type :name name :id id) (br)))

(echo
 (form (control :id \"name\" :label \"first name\")
  (control :id \"surname\") (control :id \"convicted\" :type \"checkbox\")
  (input :type \"submit\")))"))
      (p "This emits the following HTML code:")
      (pre (code (cont "<form>
<label for=\"name\">first name</label><input type=\"text\" name=\"name\" id=\"name\"/><br/>
<label for=\"surname\">surname</label><input type=\"text\" name=\"surname\" id=\"surname\"/><br/>
<label for=\"convicted\">convicted</label><input type=\"checkbox\" name=\"convicted\" id=\"convicted\"/><br/>
<input type=\"submit\"/></form>")))
      (p "You also have the full Common Lisp standard at your disposal. That gives you access to a great deal of library functions, so by generating static HTML code with Lisp you will also learn a lot about programming in a very respectable language.")
      (p "You may ask, why so few people use Lisp if it's so good? I don't know... or better" (em "they")
      " don't know. They don't know how easy and powerful Lisp is. Please, "
      (a :href "contribute.html#ad" "spread the voice") "!"))
     (section
      (h1 :class "h2" :id "menu"
       "How to generate a menu and select the current item?")
      (p "Here is a simple example miming the one found on the "
       (a :href "http://www.w3.org/wiki/Creating_multiple_pages_with_navigation_menus#Site_navigation"
        :target "_blank" "W3C Wiki.") ":")
      (pre (code "(lvar
 (menu '(|index| \"Home\" |about| \"About us\"
         |clients| \"Our Clients\" |products| \"Our Products\"
         |services| \"Our Services\" |contact| \"Contact Us\"))

 (echo
  (nav
   (ul
    (loop for (page desc) in (pair-elements menu) collect
     (li
      (if (eq %page% page)
          (strong desc)
          (a :href (cat page \".html\") desc))))))))"))
      (p "If you put this code in an shared file, e.g. "
       (code "menu.lisp") " then you can use it in any page like this:")
      (pre (code "(tvar
 (%page% '|clients|)

 (echo
  (html :lang \"en\"
   (head
    (title \"Menu demo\")
    (meta :charset \"utf-8\"))
   (body
    (inc \"menu.lisp\")))))"))
      (p "Also remember to add " (code "menu.lisp") " to a list inside "
       (code "hyde-exclude-list.lisp")
       ". If you create this file from scratch, it should simply be this:")
      (code "(\"menu.lisp\")"))
     (section
      (h1 :class "h2" :id "comment" "How to comment out a piece of code?")
      (p "By default Hyde generates minified HTML (all code on one line!). It usually does not make sense to add comments to that mess. There is currently no provision for generating an HTML comment tag, the one delimited by "
       (code (cont "<!-- ... -->"))
       ". But you can insert comments using the Lisp comment syntax: a semicolon ("
       (code ";")
       "), but that has to be repeated at the beginning of every line. To comment out several lines of code in one swoop, you can either repeat the semicolon at the beginning of every line or use the little-known Lisp multiline comment syntax: "
       (code "#| ... |#"))
      (p "I believe Lisp comments are better than HTML comments, since they are stripped from HTML files and thus keep their size low. HTML code generated by Hyde is not meant to be readable, source Lisp code is."))
     (section
      (h1 :class "h2" :id "insert-attrib"
       "How to add attributes conditionally?")
      (p "For example, here is how to omit the link for the current selected option in a menu, but leave the anchor tag as recommended by the HTML5 specification:")
      (pre
       (code "(a :id \"home\" (unless (eq *selected* 'index) '(:href \"index.html\")) \"Home page\")"))
      (p "You can read the previous code this way: unless (if not) the current selected option (stored in the template variable "
       (var "*selected*") ") is equal to " (var "'index")
       " (a symbol or constant, marked by the single quote which means “do not evaluate it”), print the "
       (var "href") " attribute and its value.")
      (p (code "unless") " and " (code "eq")
       " are not HTML element names but Lisp commands, although there is no real difference in Hyde, because both are implemented as functions. But they do different things: an HTML command returns HTML code as a string, while Lisp commands usually implement some generation logic and may not always return a string.")
      (p "In this case, the attribute name+value pair is returned by "
       (code "unless") " as a nested list. This is rather convenient in Lisp, since lists are built-in in the language. But before arguments to a tag are processed, any list is flattened by Hyde, along with all other static arguments given to the "
       (code "a") " tag, so the final result in this case will be the same as if you wrote:")
      (code "(a :id \"home\" :href \"index.html\" \"Home page\")")
      (p " if the condition is true or:")
      (code "(a :id \"home\" \"Home page\")")
      (p " if the condition is false.")
      (p "PS: The single quote before the list returned by "
       (code "unless") " is needed to tell Lisp not to interpret the whole list as a command to execute, but rather as data to return."))
     (section
      (h1 :class "h2" :id "newline" "How to insert a newline?")
      (p "In HTML you practically only need that inside " (code "pre")
       " tags. Simply embed a newline into a double-quoted delimited string, e.g let's print a magic square:")
      (pre (code "(pre (code \"4 9 2
3 5 7
8 1 6\"))"))
      (p "output:")
      (pre (code "4 9 2
3 5 7
8 1 6"))
      (p "Slower and clumsier way in Lisp are: "
       (code "(format nil \"~%\")") " and " (code "(string #\newline)")
       ". You can also insert specific types of newlines with "
       (code "#\\linefeed") ", " (code "#\\return") ", "
       (code "#\\newline")
       ". In Lisp these are technically characters, not strings, but nevertheless they can be passed as arguments to elements. They will be automatically converted into strings and concatenated by Hyde.")
      (p "E.g. in this example:")
      (pre (code "(pre (samp (cont \"> \") (code (kbd \"(+ 1 2)\")) #\newline (code (samp \"3\"))))"))
      (p "I have preferred " (code "#\\newline")
       " to a string containing only a newline lile this:"
       (pre (code " \"
\""))
       " for readability reasons. The output is:")
      (pre (samp (cont "> ") (code (kbd "(+ 1 2)")) #\newline
       (code (samp "3"))))))))

 (echo 
  (inc "layout.lisp")))
