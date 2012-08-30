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
      (p
       (a :href "http://archive.org/details/DrJekyllandMrHyde" :target "_blank"
        "Dr. Jekyll and Mr. Hyde (1920)") ". "
       (a :href "download/DrJekyllandMrHyde.srt" :download ""
        "Sottotitoli in italiano")
       ". " (br)
       (a :href "download/DrJekyllandMrHyde.srt" :download ""
        "Italian subtitles")
       ", useful to Italian language learners too.")))
    (figure
     (object :classid "clsid:D27CDB6E-AE6D-11cf-96B8-444553540000"
      :width 640 :height 502
      :codebase "http://active.macromedia.com/flash5/cabs/swflash.cab#version=7,0,0,0"
      (param :name "movie" :value "homepage-1.swf")
      ;; See: https://groups.google.com/forum/?fromgroups#!topic/wink-discuss/RhSnxTQvzyk
      (param :name "play" :value "true") ; false has no effect here :-(
      (param :name "loop" :value "false")
      (param :name "wmode" :value "transparent")
      (param :name "quality" :value "low")
      (embed :src "example/homepage-1.swf" :width 640 :height 502
       :quality "low" :loop "false" :wmode "transparent"
       :type "application/x-shockwave-flash"
       :pluginspage "http://www.macromedia.com/shockwave/download/index.cgi?P1_Prod_Version=ShockwaveFlash"))
     (script "obj=document.getElementsByTagName('object');
      for (var i=0; i<obj.length; ++i)
       obj[i].outerHTML=obj[i].outerHTML;")
     (figcaption "Making a simple personal homepage (Windoze) - Part 1. " (br)
      (a :href "example/homepage.zip" "Download full source code") "."))))
    
 (echo
  (inc "layout.lisp")))
