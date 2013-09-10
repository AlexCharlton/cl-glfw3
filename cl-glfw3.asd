;;;; cl-glfw3.asd

(asdf:defsystem #:cl-glfw3
  :serial t
  :description "Bindings for GLFW 3.x"
  :author "Alex Charlton <alex.n.charlton@gmail.com>"
  :license "BSD-2"
  :depends-on (#:cffi #:cffi-libffi #:alexandria)
  :components ((:file "package")
               (:file "glfw-bindings")
               (:file "cl-glfw3")))

(asdf:defsystem #:cl-glfw3-examples
  :serial t
  :description "Examlples for cl-glfw3"
  :author "Alex Charlton <alex.n.charlton@gmail.com>"
  :license "BSD-2"
  :depends-on (#:cl-glfw3 #:cl-opengl)
  :pathname "examples/"
  :components ((:file "package")
               (:file "basic-window")))
