(inc "util.lisp")

(tvar
 (%title% "Hyde documentation"
  %selected% "documentation"
  %tagline% "Dr. Jekyll had something to Hyde"
  %content%
   (article
    (h1 :class "h1" "Documentation/tutorial")
    (p "This is the official documentation/tutorial on Hyde, a static Lisp-based website generator, which is much more than the usual HTML templating engine. Hyde is a whole programming language at your disposal for the most advanced HTML generation task, but still easy and straightforward to use if you are not a programmer and just know a bit of HTML.")
    (p "Hyde's learning curve is very gentle and the Lisp syntax easy and uniform: in a few minutes you will be able to translate HTML into Lisp code and finally modularize your HTML code, without complicating your testing and development process. You can view Lisp as a sort of tag-based language, indeed it is an s-expression based language and, as you will see, you use s-exprs instead of tags.")
    (p "By the way, Lisp stands for " (b "L") "ots of " (b "I") "nsignificant "
     (b "S") "illy " (b "P") "arentheses and HTML for " (b "H") "orrible "
     (b "T") "ag-" (b "M") "ess " (b "L") "anguage.")
    (with-toc
     (section
      (h1 :id "editor" :class "h2" "Editor choice: any but not too spartan")
      (p "The good news is you can probably continue to use your favourite text editor.")
      (p "The bad news is that if you are using Notepad, I advice you to switch to a bit better editor. You really deserve it and anyway, for how spartan you may be, you will at least need a feature to flash matching round brackets when writing and reading Hyde HTML code.")
      (p "This is because in Hyde HTML syntax closed brackets replace closing tags (see " (a :href "#syntax" "next section")
      ").")
      (p "Another piece of advice: always use proper indentation like in the examples below, at least one character width if you do not want your code to become too large.")
      (p "If you always indent your Lisp code properly, you do still need to match parentheses while writing code but you won't have to count or match them when you read your code later on, because indentation will make its structure already clear.")
      (p "In practice, this implies that when you write properly indented Lisp code, you do not need to align each open bracket with its corresponding close bracket. By not doing so and rather stacking parentheses, you can save a lot of vertical space. And stacked parens are much less disturbing than stacked tags, aren't they?")
      (p "So, if you are still using Notepad, please switch to "
       (a :href "http://notepad-plus-plus.org/" :target "_blank" "Notepad++")
       " without a second thought. It is as fast and easy as Notepad.")
      (p "Linux users have a lot of good editor choices too. But again, keep using the editor you know well provided it has syntax highlighting and either flashes or balances parens or both. If you need to change, one editor similar to Notepad++ that runs natively in Linux is "
       (a :href "http://www.geany.org/" :target "_blank" "Geany")
      ". This is very lightweight and has even an embedded terminal to play with."))
     (section
      (h1 :id "devel-cycle" :class "h2" "The Hyde development cycle")
      (p "It is almost the same testing and development cycle without Hyde, with some simple improvements.")
      (section
       (h1 :id "server" :class "h3" "Starting the Hyde server")
       (p "Whenever you want to work on a site, first launch the Hyde web server. The Hyde executable is named "
        (code "hyde-server.exe") " in Windows and just " (code "hyde-server")
        " in Linux.")
       (p "An appalling terminal window will appear. It reminds you the full local path of the document root, that is where you have to put your Lisp files that will be converted into HTML and how to access the same directory via HTTP, with a browser. After reading that, you can just minimize and forget about this text-mode window, but do not close it or the server will stop functioning.")
       (pre (samp ";;; Document root is /home/ant/sites/hyde/site/
   ;;; Minimize this window and point your browser to http://127.0.0.1:8080/"))
       (p "Unlike Jekyll and similar, the HTML generation process in Hyde is test-driven and totally controlled through a web browser. There is no need to fiddle with the command line, apart from running the server initially.")
       (p " Currently, the only option the executable accepts is the path to your document root. By default that is the "
        (code "site")
        " subdirectory, at the same level of the server program. So you need to invoke Hyde from the Windows/Linux command line only if you want to override this path on an execution basis, e.g. if you want to use the same executable to develop more than one site, which is not recommended though. Hyde is small, so you can just have more than one copy of its directory and develop each site inside the standard "
        (code "site")
        " subdir. This way you actually bundle a particular Hyde version with every site of yours, and you can start Hyde by just clicking on the "
        (code "hyde-server") " executable in a file manager.")
       (p "The file " (code "conf.lisp")
        " contains some Lisp code to configure Hyde. For instance here you can change the 8080 default port number. You only need to do that if you see an "
        (samp "ADDRESS-IN-USE-ERROR") " message when starting Hyde. If this happens, just close the Hyde terminal window, change the 8080 port number inside "
        (code "conf.lisp")
        " to another number above 1023, save this file and run the Hyde server again. Repeat until you have found a port that is free on your machine, so that the server works. You have to close and restart Hyde because any changes to "
        (code "conf.lisp") " become effective only the next time you run the server, since this file is only read once when the Hyde server starts."))
      (section
       (h1 :id "make" :class "h3" "Making single pages or whole directories")
       (p "Once the Hyde server is up and running, create and save a simple "
        (code "hello.lisp") " file inside the " (code "site")
        " directory, e.g. containing this text:")
       (code "(echo \"Hyde has killed...\")")
       (p "Leave your editor open and point your browser to "
        (a :href "http://localhost:8080/hello.html" :target "_blank" "http://localhost:8080/hello.html"))
       (p "You will see that sentence printed. The HTML file containing it has been generated by Hyde from the Lisp file with the same name, upon the first request for the HTML file. You usually never request Lisp files via web. If you do that, Hyde will serve you the source code as a download, and no HTML generation will take place. Now, get back to your editor and change the Lisp file to read:")
       (code "(echo \"Hyde has killed... Jekyll!\")")
       (p "Save this file and hit the reload button of your browser, you will see that both the output and the HTML file on disk are updated.")
       (p "As a consequence, if you want to delete a certain file from your site, you should delete both the " (code ".lisp") " and the "
        (code ".html") " version. That is not difficult though, because they are found in the same directory and you do not usually delete files very often anyway.")
       (p "So, this is how you develop your sites using Hyde. Instead of HTML you write Lisp code and you test each modification just as you did before, when using plain HTML.")
       (p "If you prefer you can also add new Lisp files or edit existent ones without testing anything or better putting off testing till later. Whenever you request a directory instead of a file, Hyde will walk through all Lisp files contained inside this directory and its descendants, making each Lisp file that has been modified into the corresponding HTML file and finally generating a report with a link to test each newly made HTML file. You may want to check out all or some of these links to make sure your HTML code does indeed what you wanted it to do.")
       (p "All webmasters know very well how tricky HTML is, especially in combination with CSS and JavaScript. This is why manual testing is so important. Even a simple change may not do what you expect.")
       (p "You can try all this now. Edit the "
        (code "hello.lisp")
        " file again, e.g. use the undo feature of your editor to revert it to the initial version, save it and then create another Lisp file, e.g. one that prints a different string. Save the latter too and finally point your browser to the document root "
        (a :href "http://localhost:8080/" :target "_blank"
         "http://localhost:8080/") ".")
       (p "Requesting the root directory in Hyde is a simple way to make sure that all your HTML files are up-to-date before you upload, synchronize or commit your statically generated web site to the real server.")
       (p "Requesting an inner directory makes sense if your site is big and you know that your latest changes are limited to that directory, so that Hyde will not have to walk recursively the whole big directory tree starting from the root to find them.")
       (p "Unlike Jekyll and other HTML generation systems, Hyde does not watch for file changes. This means that you can save a certain file you are working on very frequently in your text editor without trigging the HTML generation process. So, it does not matter if you are saving an incomplete version that does not run yet, your previously generated HTML won't be overwritten. "
        (em "You") " decide when to save a file, regenerate it and whether to test it right now or later. Hyde does not interfere with your workflow.")))
     (section  :id "syntax"
      (h1 :id "syntax" :class "h2" "Syntax of elements: tags, attributes and content")
      (p "An HTML element has generally the syntax: ") 
      (code
       (cont "<tag attribute1=\"value1\" attribute2=\"value2\">content</tag>"))
      (p "In Hyde HTML, the recommended syntax is this: ")
      (code "(tag :attribute1 \"value1\" :attribute2 \"value2\" \"content\")")
      (p "Attribute names can also be Lisp symbols like "
       (code "'attribute")
       " but you should prefer keywords, which are spelt like "
       (code ":attribute") ". Both are like enums in other languages, except you can generate them at runtime, you do not have to bother with low-level details like assigning integer values to those identifiers, just as you do not need to assign numbers to represent English words and they are printable as string. Lisp smarts...")
      (p "For empty or void elements, just leave out any content:")
      (pre (code "(br)
  (img :src \"me.jpg\")
  (a :name \"empty-anchor\")"))
      (p "If you have any empty attributes (that is an attribute of a tag with no value, aka a boolean attribute), always provide an empty string for it, otherwise there could be ambiguity in Hyde HTML. It may not be clear when an attribute ends and the next begins:")
      (pre (code "(video :controls :src \"video.ogv\") ; WRONG!
 (video :controls \"\" :src \"video.ogv\") ; RIGHT! "))
      (p "or when an attribute list ends and the actual content of the tag begins:")
      (pre (code "(button :disabled \"Try to click me now!\") ; WRONG!
 (button :disabled \"\" \"Try to click me now!\") ; RIGHT!"))
      (p "Content of an element can also be multiple strings to concatenate. No intervening spaces are added, you must explicitely add any blanks needed:")
      (pre
       (code "(p \"In the impenetrable mask of another identity, Hyde set forth\"
   \" upon a sea of licence — to do what he, as Jekyll, could not do.\")"))
      (p "As per Lisp syntax, double quoted strings can span multiple lines and any double quote characters must be escaped with a backslash:")
      (pre
       (code "(blockquote \"\\\"Wouldn't it be marvelous if the two natures in man
  could be separated — housed in different bodies!\\\"\")"))
      (p (cont "If your strings contain any < or > signs, you have to replace them by their respective HTML entities. Alternatively, you can use the handy ")
       (code "cont") " function, which stands for " (em "content")
       " and is especially useful when there are a lot of angle bracket to escape:")
      (pre
       (code (cont "(pre (cont \"<!DOCTYPE html>
<html>
<body>

<h1>My First Heading</h1>

<p>My first paragraph.</p>

</body>
</html>\"))")))
      (p "There is also an " (code "attr") " function to help to escape an attribute string. It replaces " (code "&amp;") " with "
       (code "&amp;amp;") " and " (code "\"") " with " (code "&amp;quot;")
       ". It should always be used unless you are sure the string does not contain any double quote character or the attribute is a
URI or URL. E.g.")
      (code "(img :src \"poster.jpg\" :alt (attr \"\\\"Dr. Jekyll & Mr. Hyde\\\"\"))")
      (p "is the same as:")
      (code "(img :src \"poster.jpg\" :alt \"&amp;quot;Dr. Jekyll &amp;amp; Mr. Hyde&amp;quot;\")")
      (p "In both cases, the HTML code returned is:")
      (code (cont "<img src=\"poster.jpg\" alt=\"&quot;Dr. Jekyll &amp; Mr. Hyde&quot;\"/>"))
      (p "Usually, the former is probably more useful when a variable defines the attribute value and its content is a string that needs to be HTML-escaped:")
      (pre (code "(lvar (title \"\\\"Dr. Jekyll & Mr. Hyde\\\"\") (img :src \"poster.jpg\" :alt (attr title)))"))
      (p "When attribute names and values need to be generated programmatically, e.g. as the result of a "
       (code "loop") " collecting elements or an "
       (code "if")
       " form, you can just return a list of strings, there is no need to concatenate it to a single string:")
      (code "(a (if (eq %selected% 'home) '(:class \"selected\")) :href \"home.html\")")
      (p "The quote character signifies that " (code "(:class \"selected\")")
       " is just data and not code to execute. In other words it prevents Lisp from trying to execute a function named "
       (code ":class")
       ", which would probably result into an error. A quoted list is also named a " (em "literal list") ".")
      (p "If " (code "(eq %selected% 'home)") " is true, this form becomes: ")
      (code "(a '(:class \"selected\") :href \"home.html\")")
      (p "But since all lists in any tag argument are flattened, this is utterly equivalent to:")
      (code "(a :class \"selected\" :href \"home.html\")")
      (p "While if (eq %selected% 'home) is false, the form simply equates to:")
      (code "(a :href \"home.html\")")
      (p "No matter how literal lists are deeply nested, they will be flattened and joined into a single linear list before they are passed to any HTML element function.")
      (p "You can also wrap all the attributes to a tag in a list when writing plain HTML code, with no programming constructs embedded, but that is pleonastic. There are no advantages in this case, it only leads to a needless proliferation of parens. E.g. do not write:")
      (pre (code "(a '(:class \"selected\" :href \"home.html\"))
  (a '(:class \"selected\") '(:href \"home.html\"))
  (a '((:class \"selected\") (:href \"home.html\")))"))
      (p "but, in this case, always prefer the simpler form:")
      (code "(a :class \"selected\" :href \"home.html\")"))
      (section
       (h1 :id "minimal-html" :class "h3" "Minimal HTML document in practice")
       (p "Of course, content can also be other elements nested inside an element. E.g. here is the minimal, recommended HTML5 document to start any site project with, expressed using Hyde HTML syntax:")
       (pre
        (code "(echo
    (html :lang \"en\"
     (head
      (title \"title of the document\")
      (meta :charset \"utf-8\"))
     (body)))"
        )
       )
       (p "The HTML5 doctype is added automatically by the " (code "html")
        " tag. You need the " (code "echo")
        " function because each tag in Hyde is just a function call that returns an HTML string, at any level of nesting. "
        (em "You")
        " decide whether to store the result in a variable for further computations and if to display it later or right away.")
       (p "I could have made the " (code "html")
        " tag only to automatically output the whole document, but that would be a small saving because this tag is usually found in a few layout pages only. Moreover, doing that would also defeat flexibility. E.g. if you want to apply a regular expression to the whole generated HTML string before outputting it, you would need to capture the output first, which by the way is easy to do in Lisp, but I prefer uniformity and simplicity.")
       (p "Hyde gives you this flexibility: you decide what to output and when to output it. Source Lisp files can contain any kind of code. You can even output HTML as it is, without Lisp calls. The (small) price to pay for this is to remember to wrap an "
        (code "echo") " call around the outmost tag of each file, whatever this outmost tag is, if you want to output the whole tag-tree immediately. If you forget it, the whole return value is discarded silently and your HTML file will be mysteriously empty!"))
     (section
      (h1 :id "includes" :class "h2" "Includes")
      (p "If you have a piece of HTML code that you want to use in multiple places inside one or more pages, you can make a single file to generate it, exclude this file from the generation of an HTML file with the same name that would otherwise happen when making the whole directory that contains it and include it wherever you need the code in other files. Alternatively, you can reuse HTML code by means of functions or macros shared by many Lisp generators, but we will see that in the "
       (a :href "#new" "next section") "."
      )
      (p "For example, you may want to centralize the HTML code used to insert an ad banner, so that if you need to change something in the future (e.g. banner provider or banner size), there is only one file to change. Typically, you include the banner code from inside an " (code "aside") " element:")
      (pre
       (code "(echo
   (body
    (header \"...\")
    (aside \"...\" (inc \"banner.lisp\") \"...\")
    (article \"...\")
   )
  )"
       )
      )
      (p "And this is file " (code "banner.lisp") ":")
      (pre
       (code "(echo
   (script \"google_ad_client = \\\"pub-0123456789012345\\\";
  /* my home page ads */
  google_ad_slot = \\\"0123456789\\\";
  google_ad_width = 160;
  google_ad_height = 600;\")
   (script :src \"http://pagead2.googlesyndication.com/pagead/show_ads.js\")
  )")
      )
      (p "Note you cannot have a start tag in a file and the corresponding end tag in another file as in Jekyll or other template systems using plain HTML. This is only bad style and not a limitation anyway. You decide whether to put this outer tag in the file that includes or the file included. For instance if you always have a same "
      (code "div") " to include, then the " (code "div")
      " tag belongs to the included file. But if another time you need to put the same content inside the "
      (code "div") " in another tag, say a " (code "span")
      " then neither " (code "div") " nor " (code "span")
      " should be the top-level tag in the included file, you just need to put the content there."))
     (section
      (h1 :id "layouts" :class "h2" "Layouts")
      (p "Typically, different pages of the same site share the same overall structure. As a consequence, you do not want to type the same boilerplate code over and over, also because making modifications to multiple places is boring and error-prone.")
      (p "For most sites there is only a single layout and a single include file suffices to share this layout amoung many HTML files. E.g. put this code into a file called "
       (code "layout.lisp")
       " (or whatever other name that makes sense to you): ")
      (pre
        (code "(echo
   (html :lang \"en\"
    (head
     (title %title%)
     (meta :charset \"utf-8\"))
    (body
     %content%)))"
       )
      )
      (p "This percent convention for layout/template variables is not mandatory at all. Anyway stick to it. It is a simple way to visually distinguish between local lexical variables (e.g. function arguments) and local dynamic variables (e.g. template variables). Lisp has also global dynamic variables, they are a bit safer than in other languages, but rarely needed in Hyde.")
      (p "Then, remember to create a " (code "hyde-exclude-list.lisp")
        " file in the same directory as " (code "layout.lisp") " containing: ")
      (code "(\"layout.lisp\")")
      (p "This is a list of strings that will be taken literally. It contains one string to start with, but you can add more ones. The purpose is to tell Hyde that "
      (code "layout.lisp")
      " is not a file to be processed in order to generate a corresponding HTML file. If you do not do that you will get an error message during the building of this directory, since the "
       (var "%title%") " and " (var "%content%")
       " variables happen to be undefined (" (em "unbound")
       " in Lisp jargon) when processing this page. For simplicity Hyde stops at the first error and you would get an error message like that instead of the expected build report:"
       (pre
        (cont ";;; In file ...local path.../layout.lisp
  #<a UNBOUND-VARIABLE> 
  ;;; The variable %TITLE% is unbound.")))
      (p "After you have defined a layout like that, for each page you just have to write its body. E.g. call this " (code "index.lisp") ":")
      (pre
       (code "(tvar (
   %title% \"Dr. Jekyll and Mr. Hyde (1920 film)\"
   %content%
    (cat
     (h1 \"Dr. Jekyll and Mr. Hyde\")
     (p \"As Hyde plunged deeper into vice, his trail was soon strewn with victims of his depravity.\")))

   (echo
    (inc \"layout.lisp\")))"))
      (p (code "tvar") " stands for " (em "template variable(s)")
      ". This is special macro provided by Hyde that is used to define an arbitrary number of local template variables, followed by some code that uses them.")
      (p "Local means that outside the " (code "tvar")
       " form the variables are gone, so you do not have to worry about conflicts. Dynamic means that they will be seen from any code executed inside the "
       (code "tvar") " command, e.g. if " (code "layout.lisp") " in turn used "
       (code "inc") " to include other files, those files would still be able to access the template variables defined by "
       (code "index.lisp") ". Whatever the level of inclusion is, they would still be visible.")
      (p "This is in contrast with default Lisp variables that are static, meaning that they are only visible in the block of code that defines them. E.g. the arguments of a Lisp function can only be used inside the function that declares them. If you want to use them in other functions called from this same function, they have to be passed explicitely (just as in JavaScript).")
      (p "When you define template variables with " (code "tvar")
       "do not forget to initialize each variable with a useful value or the empty value "
       (code "nil")
       " if at the moment there is no value and to bracket the whole list of definitions, because only those parens tell "
       (code "tvar") " where definitions end and code begins.")
      (p "So " (code "inc")
       " is a sort of a special include tag you can use to implement both includes, includes with variables (aka layouts) and, we will see shortly, library files. "
       (code "inc") " reads the named Lisp file and, provided it does not contain any errors, returns whatever the file itself prints or the empty string, if it does not print anything. You can pass this string, no matter how big it is as an argument to another tag, store it into a variable, just ignore it, etc. In our case we are just printing the "
       (code "inc") "'s return value using "
       (code "echo") ". If we had forgotten " (code "echo")
       ", the return value from " (code "inc")
       ", containing the whole generated page, would just have been ignored.")
      (p "You can of course have as many layouts and include files as you wish. Usually, if your site has multiple layouts, to not clutter the document root, you may want to put them all inside a "
       (code "layout")
       " directory. You choose directory name and path, " (code "layout")
       " is just an example, but remember to exclude this whole directory from HTML generation. You do that just by adding its name to the Lisp list of strings in the file named "
       (code "hyde-exclude-list.lisp")
       ", which should be a sibling of the directory itself. " (em "Sibling")
       " means that it has to be found in the same directory that contains the directory to exclude. In other words, each directory has its own exclude list, there is no global exclude file that can cover every subdirectory. An "
       (code "hyde-exclude-list.lisp") " placed in the document root can only exclude direct children of the document root itself.")
      (p "Once you have different layout files in a directory like "
       (code "layout")
       " you can, for example, use the default layout in a Lisp file found in the parent directory by specifying a relative path to it: "
       (code "(inc \"layout/default.lisp\")"))
      (p "You can also have various pieces of HTML code (includes with or without template variables) shared between different layouts. Only two primitives, "
       (code "tvar") " and " (code "inc") " are enough for all your code reuse needs.")
      (p "To summarize, it is an inclusion game you have to play, where the aim is to modularize your HTML code as much as possible, in order to improve readability and ease maintenance of your code. In the (rawhide) face of HTML!"))
     (section :id "new"
      (h1 :id "advanced" :class "h2" "Advanced stuff")
      (p "You can probably survive without this. If you know a bit about programming, e.g. in JavaScript or any other language, you should not have any trouble with this. But even if you are not a programmer, I will try to do my best so that you can understand this section and easily become a Hyde power user, even though you can only use magic recipes made by others. So, faints of heart, do not be afraid to read on!")
      (section
       (h1 :id "new-elem" :class "h3" "Defining new elements")
       (p "The " (code "def-elem")
        " macro allows you to define your own HTML element functions which behave in exactly the same manner of all the other predefined standard HTML5 tags:")
       (code "(def-elem blink :desc \"Blink text on and off.\")")
       (p "A description is optional. If the name of the element clashes with a standard Common Lisp function, you can add a "
        (code ":fun") " argument, which is nothing fun, it just renames the generated function to avoid the clash. By the way, all standard HTML5 tags have their standard name in Hyde, except for "
        (code "time") ", which you have to call " (code "datetime") " and "
        (code "map") ", which is " (code "imgmap") ". Unfortunately the "
        (code "map") " and " (code "time")
        " function names were already taken and they do other interesting things in Lisp. Anyway these two elements are not used very often, so typing a longer name for them should not be a big problem.")
       (p "To define new void elements, the " (code ":void")
        " option must be set to true:")
       (code "(def-elem image :desc \"Synonym of img\" :void t)")
       (p "You should define new HTML elements only if you don't care about 100% standard-compliance and want to use some proprietary HTML extensions, to generate some non standard tags to be processed by other software of yours but ignored by browsers, or if you provide a JavaScript implementation for the non-standard or outright invented tag, e.g. "
        (a :href "http://en.wikipedia.org/wiki/Blink_element#Implementation"
         :target "_blank" "this one") " for " (code "blink")
        ". If you instead want to generate a bunch of existent HTML tags programmatically in order to modularize your HTML code, use functions or macros, which are easier to implement in Hyde rather than in JavaScript."))
      (section
       (h1 :id "user-fun" :class "h3" "Custom functions")
       (p "For example, let's say that you are writing a Linux tutorial and you have to typeset a lot of short shell sessions interspersed with explanation text. They all have the same structure: a prompt, followed by a command the user types and a system's response, which is text printed by the system in response to the command. A typical sequence of commands has to be marked up this proper but troublesome way:")
       (pre (code (cont "<pre>
  <samp>$ </samp><code><kbd>scores-gen &gt;scores</kbd></code>
  <samp>$ </samp><code><kbd>sort -nur &lt;scores</kbd></code>
  <samp>10
  7
  6</samp>
  </pre>")))
       (p "Not that it is easier in Hyde HTML as well:")
       (pre (code (cont "(pre (samp \"$ \") (code (kbd (cont \"scores-gen >scores\"))) #\\linefeed
   (samp \"$ \") (code (kbd (cont \"sort -nur <scores\"))) #\\linefeed
   (samp \"10
  7
  6\"))")))
       (p "Hyde gives you the handy " (code "cont")
        " function to automatically escape any " (code "&lt;") " or "
        (code "&gt;") " character from a content string, but having to type this excess baggage of tags a lot of times in the tutorial is a real annoyance in both plain and Hyde HTML.")
       (p "This is a “strange case” where markup can get more complex than content. Considering that the visual output is simply this:")
       (pre (samp "$ ") (code (kbd (cont "scores-gen >scores"))) #\linefeed
        (samp "$ ") (code (kbd (cont "sort -nur <scores"))) #\linefeed
        (samp "10
   7
   6"))
       (p "you are tempted to put this text directly into a nested " (code "pre") " and " (code "code")
        " tag only and to give up on marking up prompts, text entered by the user and computer responses.")
       (p "Indeed, IMHO, this is the reason why semantic markup is used so little. Not only some HTML5 semantic tags are not particularly easy to use, but if you use them a lot, they can make your pages more verbose than necessary.")
       (p "Life is short and people do not want to spend it typing the same things over and over at a computer keyboard. The problem is HTML has no macro feature embedded, apart from using JavaScript. But try to use JavaScript to ease a task like this. It would be both slow and uneasy to generate and print HTML code from JavaScript. You have to work with strings or, what's worse from the simplicity point of view, DOM objects and then inject them into HTML in a clumsly way. This is a situation that tends to discourage people from reusing and centralize their code, with the result that you loose the fundamental benefits associated with that.")
       (p "Aren't you pissed off? Simple properties of good software, like code reuse and modularity are made so difficult to achieve. Software engineering is completely neglected here. HTML sucks and we cannot get rid of it.")
       (p "Let's take revenge. Everything's much easier in Hyde. If you can type Hyde HTML code, you can easily create a function to generate it, using the same syntax. Let's do it step by step. First, take the Hyde HTML code above:")
       (pre (code (cont "(pre (samp \"$ \") (code (kbd (cont \"scores-gen >scores\"))) #\\linefeed
   (samp \"$ \") (code (kbd (cont \"sort -nur <scores\"))) #\\linefeed
   (samp \"10
  7
  6\"))")))
       (p "Now let's simplify it to print only one command and its response and substitute both the command and response string for a variable:")
       (pre (code "(pre (samp \"$ \") (code (kbd (cont command))) #\\linefeed
   (samp response)))"))
       (p "As in mathematics, you choose variable names, in this case I have chosen the symbols "
        (code "command") " and " (code "response")
        ". Those are good names, for us to remember what these variables are for. But the computer just does not care, since it cannot understand the meaning of a word in English or any other natural language they way we do.")
       (p "To turn this into a new function, a sort of custom tag that expands into a more complex piece of HTML code, you just have to wrap the code into another function, a sort of special function to define new functions, which is called "
         (code "defun")
         ". As its first argument, you choose a name for the function (just like "
         (code "cos")
         " is the name for the cosine function in mathematics). I have chosen "
         (code "shell") ". Then you indicate what the parameters to this new function are, in this case two parameters that we must call just like the variables we already have in our code. The have to be inside a literal list, but no quote is needed with a special function like "
         (code "defun")
         ", for it is a macro that does not evaluate its arguments upon the call:")
       (pre (code (b "(defun shell (command response)") #\linefeed
  " (pre (samp \"$ \") (code (kbd (cont command))) #\\linefeed
    (samp response))" (b ")")))
       (p "Then you can now call this function like this:")
       (pre (code "(shell \"pwd\" \"/home/ant\")"))
       (p " and it will return the following HTML string:")
       (pre (code (cont "<pre><samp>$ </samp><code><kbd>pwd</kbd></code>
  <samp>/home/ant</samp></pre>")))
       (p "You can get a confirmation by " (code "echo") "ing the result:")
       (pre (code "(echo (shell \"pwd\" \"/home/ant\"))"))
       (p "Last step is to extend the " (code "shell")
        " function to accept a list of command-response pairs, list that I chose to call "
        (code "command-list")
        " and then we need to loop over all the elements of this list after printing the initial "
        (code "pre")
        " open tag. We destructure each element in the variables "
        (code "command") " and " (code "response")
        " (but any other name will do) and we collect some HTML code created for each pair, which is the same as in the previous simpler version of the function returned. Actually, the HTML code generated inside the loop, is made up of more than one HTML string, while "
        (code "collect")
        " expects a single value. A simple and fast solution to this problem is to wrap everything in a string-"
        (code "cat") "enate call that returns a single string as required:")
       (pre (code "(defun shell (" (b "command-list") ")
   (pre
    " (b "(loop for (command response) in command-list collect
     (cat") " (samp \"$ \") (code (kbd (cont command))) #\\linefeed
      (samp response)" (b "))") "))"))
       (p "Last, we replace " (code "(samp response)") " with "
        (code (code "(if response (samp response))"))
        ". This is a refinement to avoid to print a harmless but empty "
        (code "samp")
        " tag for those commands that do not have a reply. Here is the final version of our function:")
       (pre (code ";;; Make it easy to typeset a shell session
  (defun shell (command-list)
   (pre
    (loop for (command response) in command-list collect
     (cat (samp \"$ \") (code (kbd (cont command))) #\\linefeed
      " (b "(if response") " (samp response)" (b ")") "))))"))
       (p "You can now typeset the previous shell session using this simple function call:")
       (pre (code (cont "(shell '((\"scores-gen >scores\") (\"sort -nur <scores\" \"10
  7
  6\")))")))
       (p "This is much easier to do over and over! You just copy and paste commands and their output from the terminal, make them into a literal list of pairs and this function does the rest of the work for you.")
       (p "With a bit more code, a Lisp programmer could even write a function that accepts directly a transcript of your terminal, recognizes changing prompts and multi-lines commands and typesets everything properly, from one long string only. But this handier implementation needs some more advanced Lisp programming abilities and is beyond the purposes of this tutorial.")
       (p "Here, I just wanted to show you that with Hyde you do not have to be a full-time programmer to be able to create simple functions to automate the most boring and repetitive HTML tasks. You can easily script HTML code with Lisp commands that look like the HTML data themselves."))
      (section
       (h1 :id "macros" :class "h3" "Functions that generate code and macros")
       (p "Another common maintenance headache for all HTML designers is generating a table of contents (TOC) for the heading tags. This commonly needed feature is, increbibly, "
        (a :href "http://rebuildingtheweb.com/en/html5-shortcomings/" "not provided by HTML, not even HTML5!")
        " You need an external generator? Hyde is one. You need a better language than HTML? Lisp is one.")
       (p "So let's do it. Let's add this feature to Hyde HTML by ourselves. In practice, it turns out to be necessary to attach a class id in order for all browsers to style HTML5's "
        (code "h1") " to " (code "h6")
        " elements correctly, because in HTML5 you have nested " (code "section")
        "s and the first section header is (usually) always an " (code "h1")
        " but as yet some browsers will still render all these " (code "h1")
        " headers with a big font.")
       (pre (code "(echo
   (html :lang \"en\"
    (head
     (title \"A book\")
     (meta :charset \"utf-8\")
     (style \"
   .h1 {
    font-size: 2em;
   }
   .h2 {
    font-size: 1.5em;
   }
   .h3 {
    font-size: 1.17em;
   }
   .tag {
    font-size: 1em;
    font-weight: normal;
   }\"))
    (body
     (h1 :class \"h1\" \"Book title\")
     (nav
     (ul
      (li (a :href \"#s1\" \"Section 1\")
       (ul
        (li (a :href \"#s1.1\" \"Section 1.1\"))
        (li (a :href \"#s1.2\" \"Section 1.2\"))))
      (li (a :href \"#s2\" \"Section 2\")
       (ul
        (li (a :href \"#s2.1\" \"Section 2.1\"))))))
     (section
      (h1 :class \"h2\" \"Section 1\")
      \"...\"
      (section
       (hgroup
        (h1 :class \"h3\" \"Section 1.1\")
        (h2 :class \"tag\" \"Subtitle or tagline, not to be part of the TOC\"))
       \"...\")
      (section
       (h1 :class \"h3\" \"Section 1.2\")
       \"...\"))
     (section
      (h1 :class \"h2\" \"Section 2\")
      \"...\"
      (section
       (h1 :class \"h3\" \"Section 2.1\")
       \"...\")))))"))
       (p "Our aim is to automatically generate the " (code "nav")
        " tag and it's content. Note our TOC has to be put before the sections.")
       (p "We can maliciously take advantage of this, which is a browser HTML5 support problem, in order to select implicitely which headings to include in an automatically generated TOC: we will simply include only the "
        (code "h1") " headings.")
       (p "At first blush, you may think it is an HTML transformation problem. Indeed, if you turn each " (code "section")
        " into an " (code "ul")
        " and the first heading of that section into an " (code "li")
        ", discarding all the rest of the section outright (that includes all text content), well, you have the TOC. A complication is to locate the first heading (they invented yet another language to \"ease\" that, XPath).")
       (p "But the transformation should generally not be done on the static source code. E.g. you may have an "
        (code "if")
        " form that evaluates to false and excludes a certain section from the output, so that you do not want to include its header in the index. Therefore, the transformation should be applied to the generated HTML code, but that is in HTML format, a string.")
       (p "Hyde is a fast HTML generator, so you do not get a tree structure from function calls before it gets converted into HTML. Functional calls are evaluated in left to right order (the standard order defined by Lisp) and each tag returns directly a string. Strings are concatenated and you get the HTML code quickly.")
       (p "But I admit, the drawback of this implementation choice is that transformations cannot be done on Lisp lists (which would be easier), if they have to be performed on the generated HTML code, that is your HTML code is not static (you generate tags in a loop, use selection to include/exclude some document portions, etc.). This may change in the future, I may provide the option to process some Hyde HTML code with embedded scripts and return the HTML as a tree structure rather than a string, since the former is easier to transform using Lisp directly.")
       (p "One may suggest to use XSLT on the HTML string. Well, you have to learn another language, while Lisp is simpler and more powerful. XSLT is not even a full programming language, that will come to haunt you when you need to do something that is not directly supported. Again, having to learn another language just to be able to perform simple tree-structure transformations like this is just a way to confuse the issue and to do things the hard way.")
       (p "On the contrary, a very simple way to work with tree-like data like HTML is to use a mathematical process called recursion. Recursion in Lisp could do what XSLT does and more. Once you have expressed HTML in terms of nested lists, you could easily formalize your transformation algorithms in a recursive way, you don't need another language to make the Babel tower higher.")
       (p "Unlike XSLT, you do that in a real programming language and this means that you have full power to do what you want, you do not have to struggle with the limitation of a declarative language. Lisp has all the programming paradigms and I can implement an XSLT-like language in Lisp, but is that really needed? I don't need shackles, do you?")
       (p "Anyway, I do not see a great need for HTML transformations in the context of HTML generation. Most of the time you can generate the code as you need in the first place, so you do not need to apply transformations, but this of the TOC is a problem that really seems to need it. Does it?")
       (p "Well, not really. A SAX-like approach worked well for me. Here's the full code you can also "
        (a :href "book.html" :target "_blank" "try out here") ":")
       (pre (code "
  (defmacro with-toc (&rest args)
   ;; This is optional, but prevents name clashes with user code.
   (lvar (toc (gensym) html (gensym) hyde-h1 (gensym))
   
    `(lvar (,toc '(ul) ,hyde-h1 #'h1)
      (labels ((add (tree level id title)
                (if (= level 2)
                    (setq tree (nconc tree `((li (a :href ,(cat \"#\" id) ,title)))))
                    ;;; level > 2
                    (lvar (last-li (first (last tree)) ul (third last-li))
                      (if ul
                          (add ul (1- level) id title)
                          #|
                          Cannot use '((ul)) here because it would be shared!
                          Quoted lists should be treated carefully
                          (generally as read-only). (list ...) will \"cons\" up
                          a fresh list, independent of all others.
                          |#
                          (progn (nconc last-li (list (list 'ul)))
                           (add (third last-li) (1- level) id title))))))
               #|
               Function bindings cannot be dynamically scoped using FLET (which
               only provides lexically scoped function bindings), but function
               objects (a first-level object in Common Lisp) can be assigned to
               dynamically scoped variables, bound using LET in dynamic scope,
               then called using FUNCALL or APPLY. See also other hacks ")
        (a :target "_blank" :href "http://stackoverflow.com/questions/3074812/common-lisp-redefine-an-existing-function-within-a-scope"
                 "here")
        (code ".
               |#
               (h1 (&rest args)
                (lvar (elem (append '(h1) (flatten args))
                       level (parse-integer (attr-val elem :class) :start 1))
                 (add ,toc level (attr-val elem :id) (cat (cont-list elem)))
                (apply ,hyde-h1 args))))
       (lvar (,html (cat ,@args))
        (cat (nav (eval ,toc)) ,html))))))

  (echo
   (html :lang \"en\"
    (head
     (title \"A book\")
     (meta :charset \"utf-8\")
     (style \"
   .h1 {
    font-size: 2em;
   }
   .h2 {
    font-size: 1.5em;
   }
   .h3 {
    font-size: 1.17em;
   }
   .tag {
    font-size: 1em;
    font-weight: normal;
   }\"))
    (body
     (h1 :class \"h1\" \"Book title\")
     (with-toc
      (section
       (h1 :class \"h2\" :id \"s1\" \"Section 1\")
       \"...\"
       (section
        (hgroup
         (h1 :class \"h3\" :id \"s1.1\" \"Section 1.1\")
         (h2 :class \"tag\" \"Subtitle or tagline, not to be part of the TOC\"))
        \"...\")
       (section
        (h1 :class \"h3\" :id \"s1.2\" \"Section 1.2\")
        \"...\"))
      (section
       (h1 :class \"h2\" :id \"s2\" \"Section 2\")
       \"...\"
       (section
        (h1 :class \"h3\" :id \"s2.1\" \"Section 2.1\")
        \"...\"))))))
  "))
       (p "This script is used in this same page to generate the TOC at the beginning and also in the "
        (a :href "faq.html" "FAQ page")
        ". So I can just add and remove sections from this manual or change their titles and I do not have to update the TOC manually. It is a macro that generates a TOC from some HTML code, by intercepting "
        (code "h1") " calls, rather than by applying HTML transformations! It builds the TOC as data by adding one element at a time to the HTML tree structure and then " (code "eval") "s the code.")
       (p "I have applied this algorithm to the HTML sectioned document above to test it. If you are not a Lisp programmer and you cannot understand it, you can still use it as a black box. The only thing you have to do is to wrap your code with a new special tag "
        (code "with-toc")
        ", you do not have to change anything else and the TOC will be generated for you and printed ahead your code. This kind of magic is provided by Lisp macros. You couldn't use an equivalent XSLT solution so easily, directly from HTML.")
       (p "Some facilities used in the code are directly provided by Hyde, like, for example, getting the content or the attribute value of an element. These are simple recurring sub-algorithms working on lists which have been solved once for all in the Hyde library source code and that you can reuse to implement your SAX-like algorithms. BTW, SAX is not a sort of trombone, it is a kind of HTML parser based on callbacks, that works in a conceptually analogous way to my implementation of our TOC-generator function.")))
     (section
      (h1 :id "the-end" :class "h2" "End of the story")
      (p "That's enough irons in the fire, for now. And this is only Hyde version 0.1! Expect more misdeeds in the near future. Meanwhile, you may also want to read the other "
       (a :href "advantages.html" " page comparing Jekyll to Hyde") " and "
        (a :href "movies.html" "watch the screencast") ", if you have not done it already. Have fun and be a naughty webmaster. Disrespect standards when they are not good and make you do things the hard way. Always Hyde your HTML!")))))

 (echo
  (inc "layout.lisp")))
