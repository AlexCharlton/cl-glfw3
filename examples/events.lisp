;;;; events.lisp
;;;; This example shows events in the title bar.

(defpackage :cl-glfw3-examples/examples/events
  (:use #:cl)
  (:export #:events-example))

(in-package :cl-glfw3-examples/examples/events)

(defparameter *window-size* nil)
(defparameter *keys-pressed* nil)
(defparameter *buttons-pressed* nil)

(defmacro deletef (key arr)
  `(setf ,arr (delete ,key ,arr)))

(defun update-window-title (window)
  (glfw:set-window-title (format nil "size ~A | keys ~A | buttons ~A"
                            *window-size*
                            *keys-pressed*
                            *buttons-pressed*)
                    window))

(glfw:def-key-callback key-callback (window key scancode action mod-keys)
  (declare (ignore scancode mod-keys))

  ;; Escape key is pressed
  (when (and (eq key :escape) (eq action :press))
    (glfw:set-window-should-close))

  ;; Key is pressed?
  (if (eq action :press)

    ;; Add it to the list
    (pushnew key *keys-pressed*)

    ;; Remove the key
    (deletef key *keys-pressed*))

  (format t "Pressing keys:~A~%" *keys-pressed*)
  (force-output)

  ;; Update the window title
  (update-window-title window))

(glfw:def-mouse-button-callback mouse-callback (window button action mod-keys)
  (declare (ignore mod-keys))

  ;; Mouse button pressed?
  (if (eq action :press)

    ;; Add it to the list
    (pushnew button *buttons-pressed*)

    ;; Remove it
    (deletef button *buttons-pressed*))

  (format t "Pressing mouse buttons:~A~%" *buttons-pressed*)
  (force-output)

  ;; Update the window title
  (update-window-title window))

(glfw:def-window-size-callback window-size-callback (window w h)
  (setf *window-size* (list w h))
  (update-window-title window))

(defun events-example ()
  ;; Graphics calls on OS X must occur in the main thread
  (tmt:with-body-in-main-thread ()
    (glfw:with-init-window (:title "No name" :width 400 :height 400)

      ;; Add our event listeners
      (glfw:set-key-callback 'key-callback)
      (glfw:set-mouse-button-callback 'mouse-callback)
      (glfw:set-window-size-callback 'window-size-callback)

      ;; Setup the initial window
      (setf *window-size* (glfw:get-window-size))

      (update-window-title glfw:*window*)
      (loop until (glfw:window-should-close-p) do (glfw:wait-events)))))
