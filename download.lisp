(tvar
 (%title% "Hyde download"
  %selected% "download"
  %tagline% "A deal with the devil"
  %content%
   (article
    (header
     (h1 :class "h1" :title "Download latest version"
       "Damn Yeah I wanna get hurt")
     (h2 :class "h2" "0.1"))
    (section
     (h1 :class "h2" :title "For all users" "For mortal humans")
     (ul
      (li
       (a :title "32-bit executable for Windows"
        :href "download/hyde-0.1-win.zip"
        "Click &amp; pray version for Wingdoze Virus")
       ". All inclusive.")
      (li
       (a :title "32-bit executable for Linux"
        :href "download/hyde-0.1-linux.tgz"
        "Type &amp; swear version for Leenux Pingu")
       ". You also need to have "
       (a :href "http://ecls.sourceforge.net/"
        (abbr :title "Embeddable Common-Lisp" "ECL")) 
       " installed, preferably from a package of your Linux distro.")))
    (section
     (h1 :class "h2" :title "For developers" "For brainsick overhumans")
     (ul
      (li
       (a :title "Portable source code" :href "https://github.com/ninuzzo/hyde/tarball/v0.1"
        "The full spaghetti-code mess, version 0.1")
       ". Compiles and runs under ECL Common-Lisp only. You can also "
       (a :href "https://github.com/ninuzzo/hyde" :target "_blank"
        "browse the source-code repository online") ".")))))

 (echo 
  (inc "layout.lisp")))
