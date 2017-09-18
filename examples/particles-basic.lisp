;;;; opengl.lisp
;;;; OpenGL example code borrowed from cl-opengl

(in-package #:cl-glfw3-examples)

(export '(particles-basic-example))

;; Try live-editing these variables in Emacs slime
(defvar color 0)
(defvar rotation 0)

(defparameter *x-min* 1200)
(defparameter *y-min* 800)
(defparameter *z-min* 8000)

(defparameter *x-max* 800)
(defparameter *y-max* 800)
(defparameter *z-max* 800)

(defparameter *n-particles* 1000)

(defun render ()
  (gl:clear :color-buffer)

  (incf color 0.02)
  (incf rotation 0.0001)

  (loop
    for i from 1 to *n-particles*
    do (gl:with-pushed-matrix

         ;; Random color wave
         (gl:color (cos color) (sin color) (sin color))

         ;; Move to the (random) position of this rectangle
         (gl:translate
          (- (random *x-max*) (/ *x-min* 2))
          (- (random *y-max*) (/ *y-min* 2))
          (- (random *z-max*) *z-min* 2))

         ;; Rotate by the global rotation
         (gl:rotate rotation 0.2 0.2 0)

         (gl:rect -25 -25 25 25))))

(def-key-callback quit-on-escape (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (when (and (eq key :escape) (eq action :press))
    (set-window-should-close)))

(defun set-viewport (width height)
  ;; Black background
  (gl:clear-color 0.2 0.2 0.2 0.2)

  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)

  ;; We'll be using a perspective projection
  (let ((h (/ height width)))

    ;; z-near: 9
    ;; z-far: 50000
    (gl:frustum -1 1 (- h) h 9 50000))

  (gl:matrix-mode :modelview)
  (gl:load-identity))

(def-window-size-callback update-viewport (window w h)
  (declare (ignore window))
  (set-viewport w h))

(defun particles-basic-example ()
  ;; Graphics calls on OS X must occur in the main thread
  (with-body-in-main-thread ()
    (with-init-window (:title "OpenGL particle test" :width 600 :height 400)
      (set-key-callback 'quit-on-escape)

      ;; Callback for window resize events
      (set-window-size-callback 'update-viewport)
      (set-viewport 800 400)

      ;; Our render-loop
      (loop until (window-should-close-p)
            do (render)
            do (swap-buffers)
            do (poll-events)))))
