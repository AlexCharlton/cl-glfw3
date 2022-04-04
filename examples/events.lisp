;;;; events.lisp
;;;; This example shows events in the title bar.
(in-package #:cl-glfw3-examples)

(export '(events-example))

(defparameter *keys-pressed* nil)
(defparameter *buttons-pressed* nil)
(defparameter *window-size* nil)
(defparameter *dropped-files* nil)

(defun update-window-title (window)
  (set-window-title (format nil "size ~A | keys ~A | buttons ~A"
                            *window-size*
                            *keys-pressed*
                            *buttons-pressed*)
                    window))

(def-key-callback key-callback (window key scancode action mod-keys)
  (declare (ignore scancode mod-keys))
  (when (and (eq key :escape) (eq action :press))
    (set-window-should-close))
  (if (eq action :press)
    (pushnew key *keys-pressed*)
    (deletef *keys-pressed* key))
  (update-window-title window))

(def-mouse-button-callback mouse-callback (window button action mod-keys)
  (declare (ignore mod-keys))
  (if (eq action :press)
    (pushnew button *buttons-pressed*)
    (deletef *buttons-pressed* button))
  (update-window-title window))

(def-window-size-callback window-size-callback (window w h)
  (setf *window-size* (list w h))
  (update-window-title window))

(def-window-iconify-callback iconify-callback (window minp)
  (declare (ignore window))
  (format t "~a~%" (if minp
                       'min
                       'not-min)))

(def-window-maximize-callback maximize-callback (window maxp)
  (declare (ignore window))
  (format t "~a~%" (if maxp
                       'max
                       'not-max)))

(def-drop-callback drop-print-callback (window num pathes)
  (declare (ignore window pathes))
  (pushnew num *dropped-files*)
  (deletef *dropped-files* num))

(defun events-example ()
  ;; Graphics calls on OS X must occur in the main thread
  (with-body-in-main-thread ()
    (with-init-window (:title "" :width 400 :height 400)
      (set-key-callback 'key-callback)
      (set-mouse-button-callback 'mouse-callback)
      (set-window-size-callback 'window-size-callback)
      (set-window-iconify-callback 'iconify-callback)
      (set-window-maximize-callback 'maximize-callback)
      (set-drop-callback 'drop-print-callback)
      (setf *window-size* (get-window-size))
      (update-window-title *window*)
      (loop until (window-should-close-p) do (wait-events)))))
