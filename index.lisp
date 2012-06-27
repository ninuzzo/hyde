(tvar
 (%title% "Home of Hyde, a monstrous static website generator!" ; Page title.
  %selected% "index" ; Selected menu option (file name)
  ;; A catchy caption for the Hyde logo
  %tagline% "Hyde your HTML and turn it into a monster!"
  %content%
   (cat
    (article
     (h1 :class "h1" :title "What is it?" "What kind of monster is it?")
     (p "Hyde is a battery-included static website generator. It takes "
     (a :href "movies.html"
      "five minutes to learn to use all its basic features")
     ". Hyde makes your HTML5 more readable, checks for basic syntax errors, adds modularity, promotes code reuse and makes it easy and convenient to generate minified HTML, JavaScript and CSS code.")
     (p "Hyde makes your site in situ and gives die-hard Lisp programmers full control and flexibility over the whole HTML generation process by providing tools rather than by enforcing policies. However, it is still easy to use for any casual webmaster, blogger or amateur with no real programming experience."))
    (section :class "news"
     (h1 :class "h1" :title "Latest news" "Latest crime news")
     (article
      (header
       (h1 :class "h2" "First murder: Hyde 0.1 is out!")
       (p "Posted by "
        (a :href "http://ninuzzo.github.com/about.html" :target "_blank" "AB")
        " on " (datetime :datetime "2012-06-23" "Jun 23rd, 2012")))
      (p "Tremble! Hyde's at large! "
       (a :href "download.html" "Download it") ", read the "
       (a :href "documentation.html" "docs")
       " and try it out! "
       (a :href "https://github.com/ninuzzo/hyde/issues" :target "_blank"
        "Feature requests and bug reports") " are highly appreciated.")
      (p "To all naughty "
       (a :href "http://common-lisp.net/" :target "_blank" "(Un)common Lisp")
       " hackers and "
       (a :href "http://www.lisperati.com/" :target "_blank" "lisperati")
       ": come help me to make Hyde more and more powerful! There is still a long to-do list in "
       (a :href "download.html" "the sources") ".")))))

 (echo
  (inc "layout.lisp")))
