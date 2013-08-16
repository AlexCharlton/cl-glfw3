;;;; package.lisp

(defpackage #:cl-glfw3
  (:nicknames :glfw)
  (:use #:alexandria #:cl))

(defpackage #:%cl-glfw3
  (:nicknames :%glfw)
  (:use #:cl #:cffi))
