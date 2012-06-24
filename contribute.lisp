(tvar
 (%title% "Contribute to the Hyde project"
  %selected% "contribute"
  %tagline% "Money's like manure"
  %content%
   (article
    (h1 :class "h1" "Donate or contribute")
    (p "You can either donate money to this project, directly sponsor the implementation of new features you need or, if you are a good Common Lisp programmer, contribute to Hyde's development.")
    (p "Hyde is and will always remain "
     (a :href "http://www.gnu.org/copyleft/gpl.html" :target "_blank" "GPL")
     "'ed free software. I started this project because I needed a better tool to maintain my personal home page as an "
     (a :href "http://ninuzzo.github.com/" :target "blank"
      "Italian language teacher."))
    (p "I am a humble Lisp beginner and this is just my first useful Lisp project. Feel free to criticize my code and show me a better way to do things. I will only thank you.")))

 (echo 
  (inc "layout.lisp")))
