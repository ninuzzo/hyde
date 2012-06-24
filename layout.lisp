(echo
 (html :lang "en-GB"
  (head
   (meta :charset "utf-8")
   (link :rel "stylesheet" :href "site.css")
   (link :rel "shortcut icon" :href "favicon.ico")
   (title %title%)
  )
  (body
   (header
    (hgroup
     (h1
      (a :href "index.html"
       (img :src "img/hyde_logo.png" :alt "Hyde logo"
        :title "Return to Hyde homepage"
       )
      )
     )
     (h2 %tagline%)
    )
    (a :href "movies.html" :title "Hyde movies"
     (img :src (format nil "img/shot/~a.png" %selected%)
      :alt "A snapshot from the 1920 movie &ldquo;Dr. Jekyll and Mr. Hyde&rdquo;"
     )
    )
    (nav
     (ul
      (loop for (file-name link-text snarky-link-text) in
       '(("index" "Home" "Home of horrors") ("download" "Download" "Get it")
        ("advantages" "Advantages over Ruby Jekyll" "Why yield to temptation")
        ("documentation" "Documentation" "Crimes and misdeeds")
        ("movies" "Screencasts &amp; movies" "Film") ("faq" "FAQ" "Fuq")
        ("contribute" "Donate or contribute to this project" "Feed the monster"))
       collect
        (li
         (a :title link-text
          (unless (equal %selected% file-name)
           `(:href ,(cat file-name ".html"))
          )
          snarky-link-text
         )
        )
      )
     )
    )
   )
   (div :id "cont" %content%
    (footer
     (p
      (small "&copy; 2012 by insane programmer "
       (a :href "http://ninuzzo.github.com/about.html" :target "_blank"
        "Antonio Bonifati") " and 1886 by striking writer "
        (a :href "http://en.wikipedia.org/wiki/Robert_Louis_Stevenson"
         :target "_blank" "Robert Louis Stevenson") "."
       )
      ) 
    )
   )
  )
 )
)
