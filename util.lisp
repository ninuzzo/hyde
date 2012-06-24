(defmacro with-toc (&rest args)
 ;; This is optional, but prevents name clashes with user code.
 (lvar (toc (gensym) html (gensym) hyde-h1 (gensym))
 
  `(lvar (,toc '(ul) ,hyde-h1 #'h1)
    (labels ((add (tree level id title)
              (if (= level 2)
                  (setq tree (nconc tree `((li (a :href ,(cat "#" id) ,title)))))
                  ;;; level > 2
                  (lvar (last-li (first (last tree)) ul (third last-li))
                    (if ul
                        (add ul (1- level) id title)
                        #|
                        Cannot use '((ul)) here because it would be shared!
                        Quoted lists should be treated carefully
                        (generally as read-only). (list ...) will "cons" up
                        a fresh list, independent of all others.
                        |#
                        (progn (nconc last-li (list (list 'ul)))
                         (add (third last-li) (1- level) id title))))))
             #|
             Function bindings cannot be dynamically scoped using FLET (which only provides lexically scoped function bindings), but function objects (a first-level object in Common Lisp) can be assigned to dynamically scoped variables, bound using LET in dynamic scope, then called using FUNCALL or APPLY. See also other hacks here:
             http://stackoverflow.com/questions/3074812/common-lisp-redefine-an-existing-function-within-a-scope

             |#
             (h1 (&rest args)
              (lvar (elem (append '(h1) (flatten args))
                     level (parse-integer (attr-val elem :class) :start 1))
               (add ,toc level (attr-val elem :id) (cat (cont-list elem)))
              (apply ,hyde-h1 args))))
     (lvar (,html (cat ,@args))
      (cat (nav (eval ,toc)) ,html))))))
