(tvar
 (%title% "Hyde movies"
  %selected% "movies"
  %tagline% "A horror story to prepare children for the newspapers"
  %content%
   (article
    (h1 :class "h1" "Screencasts &amp; movies")
    (figure
     (video :preload "none" :width 400 :height 304 :controls ""
      :poster "img/shot/poster.png"
      (source :src "http://archive.org/download/DrJekyllandMrHyde/DrJekyllandMrHyde.ogv"
       :type "video/ogg")
      (source :src "http://archive.org/download/DrJekyllandMrHyde/DrJekyllandMrHyde_512kb.mp4" :type "video/mp4"))
     (figcaption
      (a :href "http://archive.org/details/DrJekyllandMrHyde" :target "_blank"
       "Dr. Jekyll and Mr. Hyde (1920)")))
    (figure
     (video :controls "" :src "example/homepage.ogv")
     (figcaption "Making a simple personal homepage (Windoze). " (br)
      (a :href "example/homepage.zip" "Download the source code") "."))))
    
 (echo
  (inc "layout.lisp")))
