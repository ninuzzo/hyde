;;; See: http://ecls.sourceforge.net/new-manual/re03.html
;;; http://ecls.sourceforge.net/new-manual/ch16s03.html
;;; https://github.com/ayrnieu/ecl-examples

(require :asdf)
(push (make-pathname :directory '(:RELATIVE "src"))
      asdf:*central-registry*)
(push (make-pathname :directory '(:RELATIVE "src" "cl-ppcre-2.0.3"))
      asdf:*central-registry*)
;;; See hyde-sources/hyde.asd, this flag is used to bootstrap the server.
(pushnew :eclbuild *features*)
(format t "~%~a~%"
  (namestring
    #|
    Since the server does not return we do not have an
    :epilogue-code '(ext:quit 0)
    Instead of using :eclbuild and prologue.lisp to bootstrap we could
    use a :epilogue-code '(funcall (intern ("START-SERVER" 'hyde))
    but unfortunately that does not play nice with dynamically scoped
    template variables, I ignore why. Therefore I am sticking to this
    clumsier solution for now. Maybe if I write some C code for
    initialization and pass it to make-build it works, but that may not be
    easy too.
    |#
    (car (asdf:make-build :hyde :type :program))))
