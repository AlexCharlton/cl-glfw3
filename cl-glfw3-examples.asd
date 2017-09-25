;;;; cl-glfw3-examples.asd

(asdf:defsystem :cl-glfw3-examples
  :serial t
  :description "Examples for cl-glfw3"
  :author "Alex Charlton <alex.n.charlton@gmail.com>"
  :license "BSD-2"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (#:cl-glfw3
                #:cl-opengl
                #:trivial-main-thread

                :cl-glfw3-examples/examples/package))
