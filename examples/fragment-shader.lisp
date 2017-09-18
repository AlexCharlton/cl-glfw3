(in-package #:cl-glfw3-examples)

(export '(fragment-shader-example))

;; Error handling for shader compilation events
(define-condition compile-error (error)
  ((message
    :initform nil
    :initarg :message
    :reader compile-error-message
    :documentation "The reason given for the error")))

(defparameter +vertex-shader-source+ "

// Input our our time variable
uniform float time;

// Pass to frag shader
varying vec2 vUv;

attribute vec4 vert;

void main()
{
    vUv = vert.xy;
    vec4 offsets = vec4(cos(time), sin(time), 1., 1.);
    gl_Position = gl_ModelViewProjectionMatrix * vert * offsets;
}
")

(defparameter +fragment-shader-source+ "

varying vec2 vUv;

void main()
{
    gl_FragColor = vec4(vUv.x, vUv.y, 1., 1.);
}
")

(defvar *shader-prog* -1)
(defvar *frag-shader* nil)
(defvar *vert-shader* nil)

(defvar *shader-time* 0)

(defun render ()
  (gl:clear :color-buffer)

  ;; Update our time variable in the shader
  (gl:uniformf
   (gl:get-uniform-location *shader-prog* "time")
   (incf *shader-time* 0.01))

  (gl:with-pushed-matrix

    ;; Draw a box far back from the camera
    (gl:translate 0 0 -800)
    (gl:rect -25 -25 25 25)))

(defun check-shader-error (shader)
  "Get the current error status of a shader, throw error if status"
  (let ((error-string (gl:get-shader-info-log shader)))
    (unless (equalp error-string "")
      (progn

        ;; Print to console & then throw error
        (format t "~A~%" error-string)
        (error
         'compile-error
         :message error-string)))))

(defun is-invalid-shader (shader)
  (= shader -1))

(defun setup-shader ()
  ;; Keep trying to load our shader (Allow user to fix compile errors)
  (loop
    while (is-invalid-shader *shader-prog*) do

      (with-simple-restart
          (retry "Retry compiling shaders.")

        ;; Create the OpenGL shader in memory
        (setf *vert-shader* (gl:create-shader :vertex-shader))
        (setf *frag-shader* (gl:create-shader :fragment-shader))

        ;; Copy our shader source to the OpenGL shader
        (gl:shader-source *vert-shader* +vertex-shader-source+)
        (gl:shader-source *frag-shader* +fragment-shader-source+)

        ;; Compile our shader sources into GPU bytecode
        (gl:compile-shader *vert-shader*)
        (gl:compile-shader *frag-shader*)

        (check-shader-error *vert-shader*)
        (check-shader-error *frag-shader*)

        ;; Create the program which controls the shader activation
        (setf *shader-prog* (gl:create-program))

        ;; Then add our shaders to that program
        ;; The same shader can be attached to different programs
        (gl:attach-shader *shader-prog* *vert-shader*)
        (gl:attach-shader *shader-prog* *frag-shader*)

        ;; Linking our shader puts everything together
        (gl:link-program *shader-prog*)

        ;; We'll now draw with this shader in the future
        (gl:use-program *shader-prog*))))


;; Standard window setup below this line
;; -------------------------------------

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

(defun fragment-shader-example ()
  ;; Graphics calls on OS X must occur in the main thread
  (with-body-in-main-thread ()
    (with-init-window (:title "OpenGL test" :width 600 :height 400)
      (set-key-callback 'quit-on-escape)

      ;; Callback for window resize events
      (set-window-size-callback 'update-viewport)
      (set-viewport 800 400)

      ;; Compile our shaders and use the program
      (setup-shader)

      ;; Our render-loop
      (loop until (window-should-close-p)
            do (render)
            do (swap-buffers)
            do (poll-events)))))
