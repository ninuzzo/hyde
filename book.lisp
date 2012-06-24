(inc "util.lisp")

(echo
 (html :lang "en"
  (head
   (title "A book")
   (meta :charset "utf-8")
   (style "
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
 }"))
  (body
   (h1 :class "h1" "Book title")
   (with-toc
    (section
     (h1 :class "h2" :id "s1" "Section 1")
     "..."
     (section
      (hgroup
       (h1 :class "h3" :id "s1.1" "Section 1.1")
       (h2 :class "tag" "Subtitle or tagline, not to be part of the TOC"))
      "...")
     (section
      (h1 :class "h3" :id "s1.2" "Section 1.2")
      "..."))
    (section
     (h1 :class "h2" :id "s2" "Section 2")
     "..."
     (section
      (h1 :class "h3" :id "s2.1" "Section 2.1")
      "..."))))))
