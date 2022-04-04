;;;; icons.lisp
;;;; This example shows changed icon.
(in-package #:cl-glfw3-examples)

(export '(icons-example))

(defun create-colored-square-image (width height &key (r #xff) (g #xff) (b #xff))
  (let ((pixels (make-array (* 4 width height) :initial-element #xff)))
    (loop for i from 0 below (* 4 width height) by 4 do
          (setf (aref pixels (+ i 0)) r);red
          (setf (aref pixels (+ i 1)) g);green
          (setf (aref pixels (+ i 2)) b);blue
          (setf (aref pixels (+ i 3)) #xff));alpha
    (make-image :width width :height height :pixels pixels)))

(defparameter *red-image* (create-colored-square-image 48 48 :r #xff :g 0 :b 0))
(defparameter *green-image* (create-colored-square-image 48 48 :r 0 :g #xff :b 0))
(defparameter *blue-image* (create-colored-square-image 48 48 :r 0 :g 0 :b #xff))

(defun icons-example ()
  ;; Graphics calls on OS X must occur in the main thread
  (with-body-in-main-thread ()
    (with-init-window (:title "Icon test" :width 600 :height 400)
      (set-window-icon *green-image*)
      (let ((cursors (make-array 9 :initial-contents `(,(create-cursor *red-image* 0 0)
                                                        ,(create-cursor *green-image* 0 0)
                                                        ,(create-cursor *blue-image* 0 0)
                                                        ,(create-standard-cursor :arrow)
                                                        ,(create-standard-cursor :ibeam)
                                                        ,(create-standard-cursor :crosshair)
                                                        ,(create-standard-cursor :hand)
                                                        ,(create-standard-cursor :hresize)
                                                        ,(create-standard-cursor :vresize))))
            (cursor-num 0))
        (def-mouse-button-callback mouse-cursor-change-callback (window button action mod-keys)
          (declare (ignore window mod-keys))
          (if (eq action :press)
              (setf cursor-num (mod (+ cursor-num
                                       (cond ((eq button :left) 1)
                                             ((eq button :right) -1)
                                             (t 0)))
                                    9))
              (set-cursor (aref cursors cursor-num))))
        (set-mouse-button-callback 'mouse-cursor-change-callback)
        (set-cursor (aref cursors cursor-num))
        (loop until (window-should-close-p)
              ;do (swap-buffers)
              do (poll-events))
        (loop for i from 0 below 9 do (destroy-cursor (aref cursors i)))))))
