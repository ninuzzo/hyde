(tvar
 (%title% "Contribute to the Hyde project"
  %selected% "contribute"
  %tagline% "Money's like manure"
  %content%
   (article
    (h1 :class "h1" "Donate or contribute")
    (img :class "align-right" :src "img/lisplogo_warning2_256.png"
     :alt "WARNING: BUILT USING LISP")
    (p "You can either "
     (a :href "https://github.com/ninuzzo/hyde/blob/master/README.textile#donate"
      :target "_blank" "donate money to this project")
     ", directly sponsor the implementation of new features you need or, if you are a good Common Lisp programmer, contribute to Hyde's development.")
    (p "Hyde is and will always remain "
     (a :href "http://www.gnu.org/copyleft/gpl.html" :target "_blank" "GPL")
     "'ed free software. I started this project because I needed a better tool to maintain my personal home page as an "
     (a :href "http://ninuzzo.github.com/" :target "blank"
      "Italian language teacher."))
    (p "I am a humble Lisp beginner and this is just my first useful Lisp project. Feel free to criticize my code and show me a better way to do things. I will only thank you.")
    (section
     (h1 :class "h2" :title "How to become a Hyde's collaborator"
      "How to become a Hyde's accomplice")
     (p "If you wanna be a casual Hyde's developer or for your first contribution, fork the Hyde source code, implement something new or fix a bug and when you are done make a "
      (a :href "https://help.github.com/articles/using-pull-requests"
       :target "_blank" "pull request")
      " for me to check and merge your changes. More stable and trusted collaborators will get full write privileges to the main repo, so they can commit their contributions directly there."))
    (section :id "ad"
     (h1 :class "h2" "Advertise the monster")
     (script :src
      "http://www.ohloh.net/p/603060/widgets/project_users_logo.js"))))

 (echo 
  (inc "layout.lisp")))
