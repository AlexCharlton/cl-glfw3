;;;; basic-window.lisp
;;;; OpenGL example code borrowed from cl-opengl
(in-package #:cl-glfw3-examples)

(export '(basic-window-example))

(def-key-callback key-callback (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (when (and (eq key :escape) (eq action :press))
    (set-window-should-close)))

(defmethod render ()
  (gl:clear :color-buffer)
  (gl:with-pushed-matrix
    (gl:color 1 1 1)
    (gl:rect -25 -25 25 25)))

(defmethod set-viewport (width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho -50 50 -50 50 -1 1)
  (gl:matrix-mode :modelview)
  (gl:load-identity))

(def-window-size-callback window-size-callback (window w h)
  (declare (ignore window))
  (set-viewport w h))

(defun basic-window-example ()
  (with-init-window (:title "Window test" :width 600 :height 400)
    (setf %gl:*gl-get-proc-address* #'get-proc-address)
    (set-key-callback 'key-callback)
    (set-window-size-callback 'window-size-callback)
    (gl:clear-color 0 0 0 0)
    (set-viewport 600 400)
    (loop until (window-should-close-p)
       do (render)
       do (swap-buffers)
       do (poll-events))))
