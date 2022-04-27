;;;; cl-glfw3.lisp

;;;; # GLFW functions
;;;; Access almost all of the GLFW bindings without the need to use CFFI or %GLFW. Also included are helper functions. Functions are named directly after their GLFW counterparts. GLFW enums are replaced with keywords.

;;;; Much of the convenience in this library comes from the presence of a default window value, *WINDOW*. *WINDOW* can be set by MAKE-CONTEXT-CURRENT, or by CREATE-WINDOW.

;;;; WITH- macros (WITH-INIT, WITH-WINDOW WITH-INIT-WINDOW) are provided for convenience (and inspired by cl-glfw). WITH-CONTEXT, is another convenience function (although not present in cl-glfw).

;;;; Callback creation macros are also provided. These macros ask for the name of the callback to be created, a list of symbols which correspond to the arguments of the callback and the body. Callback setter functions in this package require the (quoted) name of the callback.

;;;; Full documentation for GLFW3 can be found at http://www.glfw.org/docs/3.0/index.html

(in-package #:glfw)

(export
 '(def-error-callback
   set-error-callback
   initialize
   with-init
    get-monitor-work-area
   def-monitor-callback
   *window*
   create-window
   destroy-window
   with-window
   with-init-window
   window-should-close-p
   set-window-should-close
   set-window-title
   set-window-icon
   get-window-opacity
   set-window-opacity
   get-window-position
   set-window-position
   get-window-size
   set-window-size
   set-window-size-limits
   set-window-aspect-ratio
   get-window-frame-size
   set-window-monitor
   get-window-content-scale
   get-framebuffer-size
   iconify-window
   restore-window
    maximize-window
   show-window
   hide-window
focus-window
    request-window-attention
   get-window-monitor
   get-window-attribute
    set-window-attribute
   get-context-version
   def-window-position-callback
   def-window-size-callback
   def-window-close-callback
   def-window-refresh-callback
   def-window-focus-callback
   def-window-iconify-callback
def-window-maximize-callback
   def-framebuffer-size-callback
def-window-content-scale-callback
   set-window-position-callback
   set-window-size-callback
   set-window-close-callback
   set-window-refresh-callback
   set-window-focus-callback
   set-window-iconify-callback
set-window-maximize-callback
   set-framebuffer-size-callback
set-window-content-scale-callback
   get-input-mode
   set-input-mode
   get-key
   get-mouse-button
   get-cursor-position
   set-cursor-position
create-cursor
set-cursor
   def-key-callback
   def-char-callback
def-char-mods-callback
   def-mouse-button-callback
   def-cursor-pos-callback
   def-cursor-enter-callback
   def-scroll-callback
    def-drop-callback
   set-key-callback
   set-char-callback
set-char-mods-callback
   set-mouse-button-callback
   set-cursor-position-callback
   set-cursor-enter-callback
   set-scroll-callback
    set-drop-callback
def-joystick-callback
   set-clipboard-string
   get-clipboard-string
   make-context-current
   get-current-context
   with-context
   swap-buffers
   create-window-surface))

(multiple-value-bind (major minor rev) (%glfw:get-version)
  (when (/= major 3)
    (error "Local GLFW is ~a.~a.~a, should be above 3.x" major minor rev)))

(defmacro with-image-pointer ((&rest bind*) &body body)
  "Internal function"
  (let ((gensym-pixels-vars (mapcar (lambda (x)
				      (gensym (format nil "~a" x)))
				    (mapcar #'first bind*)));マクロ中で使う変数sをリストにまとめて保管
	(img-binding-list (gensym "IMG-BINDING-LIST")));画像の評価を束縛する変数
    ;;画像の評価を1度だけにするため,letで束縛。上は束縛するための変数sをリストで保存している
    `(let ((,img-binding-list (list ,@(mapcar (lambda (x)
						(cadr x))
					      bind*))))
       (mapc (lambda (img)
	       (assert (typep img '(simple-array (unsigned-byte 8) (* * 4)))))
	     ,img-binding-list)
       ;;画像のピクセルデータの大きさの配列をallocしてgensym変数sに束縛
       (destructuring-bind ,gensym-pixels-vars
	 (mapcar (lambda (img-array)
		   (cffi:foreign-alloc :unsigned-char :count (array-total-size img-array)))
		 ,img-binding-list)
	 (unwind-protect
	   (progn
	     ;;画像のピクセルデータの中身を詰める
	     (mapc (lambda (img-array pixel-array-ptr)
		     (let ((height (array-dimension img-array 0))
			   (width (array-dimension img-array 1)))
		       (loop for i from 0 below width do
			     (loop for j from 0 below height do
				   (loop for k from 0 below (array-dimension img-array 2) do
					 (setf (cffi:mem-ref pixel-array-ptr :unsigned-char (+ k (* i 4)
											       (* j (* width 4))))
					       (aref img-array j i k)))))))
		   ,img-binding-list
		   (list ,@gensym-pixels-vars))
	     ;;画像ストラクチャをallocしてbind*で示された変数に束縛
	     (destructuring-bind ,(mapcar #'first bind*)
	       (loop for i from 0 below ,(length bind*) collect (cffi:foreign-alloc :int :count 3))
	       (unwind-protect
		 (progn
		   ;;画像ストラクチャに中身を詰める
		   (mapc (lambda (img-structure-ptr image-array pixel-array-ptr)
			   (setf (cffi:mem-ref img-structure-ptr :int) (array-dimension image-array 1);width
				 (cffi:mem-ref img-structure-ptr :int 4) (array-dimension image-array 0);height
				 (cffi:mem-ref img-structure-ptr :pointer 8) pixel-array-ptr))
			  (list ,@(mapcar #'first bind*))
			  ,img-binding-list
			  (list ,@gensym-pixels-vars))
		   ,@body)
		 (mapc #'cffi:foreign-free (list ,@(mapcar #'first bind*))))))
	     (mapc #'cffi:foreign-free (list ,@gensym-pixels-vars)))))))

#|
(defmacro with-image-pointer ((var image) &body body)
  "Internal function"
  ;;マクロの準備
  (alexandria:with-gensyms (width height pixels image-ptr)
    (alexandria:once-only (image)
      `(let ((,width (image-width ,image))
             (,height (image-height ,image)))
         ;;ポインタを取得し中身をalloc
         (cffi:with-foreign-pointer (,image-ptr ,(* 2 3));int*2+pointer=int*3=2*3 bytes
           ;;中身を詰める
           (cffi:with-foreign-pointer (,pixels (* 1 ,width ,height 4));4=rgba
             (loop for i from 0 below (* ,width ,height 4) do
                   (setf (cffi:mem-ref ,pixels :uchar i)
                         (aref (image-pixels ,image) i)))
             (setf (cffi:mem-ref ,image-ptr :int) ,width
                   (cffi:mem-ref ,image-ptr :int 4) ,height
                   (cffi:mem-ref ,image-ptr :pointer 8) ,pixels)
             ;;ポインタを変数に束縛しbody展開
             (let ((,var ,image-ptr))
               ,@body)))))))
|#

;;;; ## Window and monitor functions
(defmacro import-export (&rest symbols)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (import ',symbols)
     (export ',symbols)))

(defmacro def-error-callback (name (message) &body body)
  (let ((error-code (gensym "error-code")))
    `(%glfw:define-glfw-callback ,name
	 ((,error-code :int) (,message :string))
       (declare (ignore ,error-code))
       ,@body)))

(def-error-callback default-error-fun (message)
  (error message))

(defun set-error-callback (callback-name)
  (%glfw:set-error-callback (cffi:get-callback callback-name)))

(defun initialize (&key (joystick-hat-buttons t)
                        (cocoa-chdid-resources t)
                        (cocoa-menubar t))
  "Start GLFW"
  (%glfw:init-hint :joystick-hat-buttons joystick-hat-buttons)
  (%glfw:init-hint :cocoa-chdid-resources cocoa-chdid-resources)
  (%glfw:init-hint :cocoa-menubar cocoa-menubar)
  (let ((result (%glfw:init)))
    (unless result
      (error "Error initializing glfw."))
    result))

(defmacro with-init ((&rest init-keys) &body body)
  "Wrap BODY with an initialized GLFW instance, ensuring proper termination. If no error callback is set when this is called, a default error callback is set."
  `(progn
     (let ((prev-error-fun (set-error-callback 'default-error-fun)))
       (unless (cffi:null-pointer-p prev-error-fun)
	 (%glfw:set-error-callback prev-error-fun)))
     (initialize ,@init-keys)
     (unwind-protect (progn ,@body)
       (%glfw:terminate))))

(import-export %glfw:get-monitors %glfw:get-primary-monitor %glfw:get-monitor-position %glfw:get-monitor-workarea %glfw:get-monitor-physical-size %glfw:get-monitor-content-scale %glfw:get-monitor-name %glfw:set-monitor-callback %glfw:get-video-modes %glfw:get-video-mode %glfw:set-gamma %glfw:get-gamma-ramp %glfw:set-gamma-ramp %glfw:terminate)

(defun get-monitor-work-area (monitor)
  "Inconsistent name of get-monitor-workarea. old-version used this name"
  (warn "get-monitor-work-area is inconsistent name of foreign function.~% get-monitor-workarea is recommended")
  (%glfw:get-monitor-workarea monitor))

(defmacro def-monitor-callback (name (monitor event) &body body)
  `(%glfw:define-glfw-callback ,name
       ((,monitor :pointer) (,event %glfw::connection-event))
     ,@body))

(defvar *window* nil
  "The window that is currently the default for this library. Can be set through MAKE-CONTEXT-CURRENT.")

(defun create-window (&key
			(width 0) (height 0)
			title
			(monitor (cffi:null-pointer))
			(shared (cffi:null-pointer))
			;; Hints
			(resizable t)
			(visible t)
			(decorated t)
                        (focused t)
                        (auto-iconify t)
                        (floating nil)
                        (maximized nil)
                        (center-cursor t)
                        (transparent-framebuffer nil)
                        (focus-on-show t)
                        (scale-to-monitor nil)
			(red-bits 8) (green-bits 8) (blue-bits 8) (alpha-bits 8)
			(depth-bits 24) (stencil-bits 8)
			(accum-red-bits 0) (accum-green-bits 0) (accum-blue-bits 0)
			(accum-alpha-bits 0)
			(aux-buffers 0)
			(samples 0)
			(refresh-rate 0)
			(stereo nil)
			(srgb-capable nil)
                        (doublebuffer t)
			(client-api :opengl-api)
                        (context-creation-api :native-context-api)
			(context-version-major 1)
			(context-version-minor 0)
			(context-robustness :no-robustness)
                        (context-release-behavior :any-release-behavior)
			(opengl-forward-compat nil)
			(opengl-debug-context nil)
			(opengl-profile :opengl-any-profile)
                        (cocoa-retina-framebuffer t)
                        (cocoa-frame-name "")
                        (cocoa-graphics-switching nil)
                        (x11-class-name "")
                        (x11-instance-name "")
                        )
  "This function handles all window hints.

MONITOR: The monitor on which the window should be full-screen.
SHARED: The window whose context to share resources with."
  (macrolet ((output-hints (&rest hints)
               (flet ((stringhint-p (hintname)
                         (member hintname '(cocoa-frame-name x11-class-name x11-instance-name))))
                 `(progn
                    ,@(loop for (name type) in hints collect
                            `(,(if (stringhint-p (intern (string-upcase
                                                           (symbol-name name))))
                                   '%glfw:window-hint-string
                                   '%glfw:window-hint)
                               ,(intern (string-upcase
                                         (symbol-name name)) :keyword)
                               (cffi:convert-to-foreign ,name ,type)))))))
      (output-hints
        (resizable :boolean)
        (visible :boolean)
        (decorated :boolean)
        (focused :boolean)
        (auto-iconify :boolean)
        (floating :boolean)
        (maximized :boolean)
        (center-cursor :boolean)
        (transparent-framebuffer :boolean)
        (focus-on-show :boolean)
        (scale-to-monitor :boolean)
        (red-bits :int) (green-bits :int) (blue-bits :int) (alpha-bits :int)
        (depth-bits :int) (stencil-bits :int)
        (accum-red-bits :int) (accum-green-bits :int) (accum-blue-bits :int)
        (accum-alpha-bits :int)
        (aux-buffers :int)
        (samples :int)
        (refresh-rate :int)
        (stereo :boolean)
        (srgb-capable :boolean)
        (doublebuffer :boolean)
        (client-api '%glfw::opengl-api)
        (context-creation-api '%glfw::context-creation)
        (context-version-major :int)
        (context-version-minor :int)
        (context-robustness '%glfw::robustness)
        (context-release-behavior '%glfw::release-behavior)
        (opengl-forward-compat :boolean)
        (opengl-debug-context :boolean)
        (opengl-profile '%glfw::opengl-profile)
        (cocoa-retina-framebuffer :boolean)
        (cocoa-frame-name :string)
        (cocoa-graphics-switching :boolean)
        (x11-class-name :string)
        (x11-instance-name :string)
        ))
  (let ((window (%glfw:create-window width height title monitor shared)))
    (if (cffi:null-pointer-p window)
        (error "Error creating window.")
        (if (eq client-api :no-api)
            (setf *window* window)
            (make-context-current window)))))

(defun destroy-window (&optional (window *window*))
  (when window (%glfw:destroy-window window))
  (when (eq window *window*)
    (setf *window* nil)))

(defmacro with-window ((&rest window-keys) &body body)
  "Convenience macro for using windows."
  `(unwind-protect
	(progn
	  (create-window ,@window-keys)
	  ,@body)
     (destroy-window)))

(defmacro with-init-window ((&rest window-keys) &body body)
  "Convenience macro for setting up GLFW and opening a window."
  `(with-init ()
     (with-window ,window-keys ,@body)))

(defun window-should-close-p (&optional (window *window*))
  (%glfw:window-should-close-p window))

(defun set-window-should-close (&optional (window *window*) (closep t))
  (%glfw:set-window-should-close window closep))

(defun set-window-title (title &optional (window *window*))
  (%glfw:set-window-title window title))

(defun get-window-opacity (&optional (window *window*))
  (%glfw:get-window-opacity window))

(defun set-window-opacity (x &optional (window *window*))
  (%glfw:set-window-opacity window x))

(defun get-window-position (&optional (window *window*))
  (%glfw:get-window-position window))

(defun set-window-position (x y &optional (window *window*))
  (%glfw:set-window-position window x y))

(defun get-window-size (&optional (window *window*))
  (%glfw:get-window-size window))

(defun set-window-size (w h &optional (window *window*))
  (%glfw:set-window-size window w h))

(defun set-window-size-limits (minwidth minheight maxwidth maxheight &optional (window *window*))
  (%glfw:set-window-size-limits window minwidth minheight maxwidth maxheight))

(defun set-window-aspect-ratio (width height &optional (window *window*))
  (%glfw:set-window-aspect-ratio window width height))

(defun get-window-frame-size (&optional (window *window*))
  (%glfw:get-window-frame-size window))

(defun get-window-content-scale (&optional (window *window*))
  (%glfw:get-window-content-scale window))

(defun get-framebuffer-size (&optional (window *window*))
  (%glfw:get-framebuffer-size window))

(defun set-window-monitor (monitor width height &key (window *window*)
                                                  (x-position 0) (y-position 0)
                                                  (refresh-rate %glfw:+dont-care+))
  (let ((monitor (if (null monitor) (cffi:null-pointer) monitor)))
    (%glfw:set-window-monitor window monitor x-position y-position width height refresh-rate)))

(defun iconify-window (&optional (window *window*))
  (%glfw:iconify-window window))

(defun set-window-icon (image &optional (window *window*))
  (cond ((null image) (%glfw:set-window-icon window 0 (cffi:null-pointer)))
	(t (with-image-pointer ((pointer image))
	     (%glfw:set-window-icon window 1 pointer)))))

(defun restore-window (&optional (window *window*))
  (%glfw:restore-window window))

(defun maximize-window (&optional (window *window*))
  (%glfw:maximize-window window))

(defun show-window (&optional (window *window*))
  (%glfw:show-window window))

(defun hide-window (&optional (window *window*))
  (%glfw:hide-window window))

(defun focus-window (&optional (window *window*))
  (%glfw:focus-window window))

(defun request-window-attention (&optional (window *window*))
  (%glfw:request-window-attention window))

(defun get-window-monitor (&optional (window *window*))
  (let ((monitor (%glfw:get-window-monitor window)))
    (unless (cffi:null-pointer-p monitor)
      monitor)))

(defun get-window-attribute (attribute &optional (window *window*))
  (let ((value (%glfw:get-window-attribute window attribute)))
    (ccase attribute
      ((:focused :iconified :maximized :hovered :visible :resizable :decorated :auto-iconify :floating :transparent-framebuffer :focus-on-show :opengl-forward-compat :opengl-debug-context :context-no-error)
       (cffi:convert-from-foreign value :boolean))
      (:client-api (cffi:foreign-enum-keyword '%glfw::opengl-api value))
      (:context-creation-api (cffi:foreign-enum-keyword '%glfw::context-creation value))
      ((:context-version-major :context-version-minor :context-revision) value)
      (:opengl-profile (cffi:foreign-enum-keyword '%glfw::opengl-profile value))
      (:context-release-behavior (cffi:foreign-enum-keyword '%glfw::release-behavior value))
      (:context-robustness (cffi:foreign-enum-keyword '%glfw::robustness value)))))

(defun set-window-attribute (attribute value &optional (window *window*))
  (%glfw:set-window-attribute window attribute value))

(defun get-context-version (&optional (window *window*))
  "Convenience function returning (opengl-context-major-version opengl-context-minor-version opengl-context-revision)."
  (list (get-window-attribute :context-version-major window)
        (get-window-attribute :context-version-minor window)
        (get-window-attribute :context-revision window)))

(defmacro def-window-position-callback (name (window x y) &body body)
  `(%glfw:define-glfw-callback ,name
       ((,window :pointer) (,x :int) (,y :int))
     ,@body))

(defmacro def-window-size-callback (name (window w h) &body body)
  `(%glfw:define-glfw-callback ,name
       ((,window :pointer) (,w :int) (,h :int))
     ,@body))

(defmacro def-window-close-callback (name (window) &body body)
  `(%glfw:define-glfw-callback ,name
       ((,window :pointer))
     ,@body))

(defmacro def-window-refresh-callback (name (window) &body body)
  `(%glfw:define-glfw-callback ,name
       ((,window :pointer))
     ,@body))

(defmacro def-window-focus-callback (name (window focusedp) &body body)
  `(%glfw:define-glfw-callback ,name
       ((,window :pointer) (,focusedp :boolean))
     ,@body))

(defmacro def-window-iconify-callback (name (window iconifiedp) &body body)
  `(%glfw:define-glfw-callback ,name
       ((,window :pointer) (,iconifiedp :boolean))
     ,@body))

(defmacro def-window-maximize-callback (name (window maximizedp) &body body)
  `(%glfw:define-glfw-callback ,name
     ((,window :pointer) (,maximizedp :boolean))
     ,@body))

(defmacro def-framebuffer-size-callback (name (window w h) &body body)
  `(%glfw:define-glfw-callback ,name
       ((,window :pointer) (,w :int) (,h :int))
     ,@body))

(defmacro def-window-content-scale-callback (name (window xscale yscale) &body body)
  `(%glfw:define-glfw-callback ,name
     ((,window :pointer) (,xscale :float) (,yscale ,:float))
     ,@body))

(defun set-window-position-callback (callback-name &optional (window *window*))
  (%glfw:set-window-position-callback window (cffi:get-callback callback-name)))

(defun set-window-size-callback (callback-name &optional (window *window*))
  (%glfw:set-window-size-callback window (cffi:get-callback callback-name)))

(defun set-window-close-callback (callback-name &optional (window *window*))
  (%glfw:set-window-close-callback window (cffi:get-callback callback-name)))

(defun set-window-refresh-callback (callback-name &optional (window *window*))
  (%glfw:set-window-refresh-callback window (cffi:get-callback callback-name)))

(defun set-window-focus-callback (callback-name &optional (window *window*))
  (%glfw:set-window-focus-callback window (cffi:get-callback callback-name)))

(defun set-window-iconify-callback (callback-name &optional (window *window*))
  (%glfw:set-window-iconify-callback window (cffi:get-callback callback-name)))

(defun set-window-maximize-callback (callback-name &optional (window *window*))
  (%glfw:set-window-maximize-callback window (cffi:get-callback callback-name)))

(defun set-framebuffer-size-callback (callback-name &optional (window *window*))
  (%glfw:set-framebuffer-size-callback window (cffi:get-callback callback-name)))

(defun set-window-content-scale-callback (callback-name &optional (window *window*))
  (%glfw:set-window-content-scale-callback window (cffi:get-callback callback-name)))

;;;; ## Events and input
(import-export %glfw:poll-events %glfw:wait-events %glfw:wait-events-timeout %glfw:post-empty-event)

(defun get-input-mode (mode &optional (window *window*))
  "Mode is one of :CURSOR :STICKY-KEYS or :STICKY-MOUSE-BUTTONS."
  (let ((value (%glfw:get-input-mode window mode)))
    (ccase mode
      (:cursor
       (cffi:convert-from-foreign value '%glfw::cursor-mode))
      ((:sticky-keys :sticky-mouse-buttons)
       (cffi:convert-from-foreign value :boolean)))))

(defun set-input-mode (mode value &optional (window *window*))
  (let ((value (ccase mode
		 (:cursor
		  (cffi:convert-to-foreign value '%glfw::cursor-mode))
		 ((:sticky-keys :sticky-mouse-buttons)
		  (cffi:convert-to-foreign value :boolean)))))
    (%glfw:set-input-mode window mode value)))

(defun get-key (key &optional (window *window*))
  (%glfw:get-key window key))

(defun get-mouse-button (button &optional (window *window*))
  (%glfw:get-mouse-button window button))

(defun get-cursor-position (&optional (window *window*))
  (%glfw:get-cursor-position window))

(defun set-cursor-position (x y &optional (window *window*))
  (%glfw:set-cursor-position window x y))

(defun create-cursor (image xhot yhot)
  (cond ((null image) (%glfw:create-cursor (cffi:null-pointer) xhot yhot))
        (t (with-image-pointer ((pointer image)) (%glfw:create-cursor pointer xhot yhot)))))

(defun set-cursor (cursor &optional (window *window*))
  (%glfw:set-cursor window cursor))

(defmacro def-key-callback (name (window key scancode action mod-keys) &body body)
  `(%glfw:define-glfw-callback ,name
       ((,window :pointer) (,key %glfw::key) (,scancode :int)
	(,action %glfw::key-action) (,mod-keys %glfw::mod-keys))
     ,@body))

(defmacro def-char-callback (name (window char) &body body)
  (let ((char-code (gensym "char")))
    `(%glfw:define-glfw-callback ,name
	((,window :pointer) (,char-code :unsigned-int))
      (let ((,char (code-char ,char-code)))
	,@body))))

(defmacro def-char-mods-callback (name (window char mod-keys) &body body)
  (let ((char-code (gensym "char")))
    `(%glfw:define-glfw-callback ,name
       ((,window :pointer) (,char-code :unsigned-int) (,mod-keys %glfw::mod-keys))
       (let ((,char (code-char ,char-code)))
         ,@body))))

(defmacro def-mouse-button-callback (name (window button action mod-keys) &body body)
  `(%glfw:define-glfw-callback ,name
       ((,window :pointer) (,button %glfw::mouse)
	(,action %glfw::key-action) (,mod-keys %glfw::mod-keys))
     ,@body))

(defmacro def-cursor-pos-callback (name (window x y) &body body)
  `(%glfw:define-glfw-callback ,name
       ((,window :pointer) (,x :double) (,y :double))
     ,@body))

(defmacro def-cursor-enter-callback (name (window enterp) &body body)
  `(%glfw:define-glfw-callback ,name
       ((,window :pointer) (,enterp :boolean))
     ,@body))

(defmacro def-scroll-callback (name (window x y) &body body)
  `(%glfw:define-glfw-callback ,name
       ((,window :pointer) (,x :double) (,y :double))
     ,@body))

;;added
;;must: support function
(defmacro def-drop-callback (name (window number-of-pathes pathes) &body body)
  `(%glfw:define-glfw-callback ,name
     ((,window :pointer) (,number-of-pathes :int) (,pathes (:pointer (:pointer :string))))
     ,@body))

(defmacro def-joystick-callback (name (joystick event) &body body)
  `(%glfw:define-glfw-callback ,name
     ((,joystick int) (,event %glfw::connection-event))
     ,@body))

(defun set-key-callback (callback-name &optional (window *window*))
  (%glfw:set-key-callback window (cffi:get-callback callback-name)))

(defun set-char-callback (callback-name &optional (window *window*))
  (%glfw:set-char-callback window (cffi:get-callback callback-name)))

(defun set-char-mods-callback (callback-name &optional (window *window*))
  (%glfw:set-char-mods-callback window (cffi:get-callback callback-name)))

(defun set-mouse-button-callback (callback-name &optional (window *window*))
  (%glfw:set-mouse-button-callback window (cffi:get-callback callback-name)))

(defun set-cursor-position-callback (callback-name &optional (window *window*))
  (%glfw:set-cursor-position-callback window (cffi:get-callback callback-name)))

(defun set-cursor-enter-callback (callback-name &optional (window *window*))
  (%glfw:set-cursor-enter-callback window (cffi:get-callback callback-name)))

(defun set-scroll-callback (callback-name &optional (window *window*))
  (%glfw:set-scroll-callback window (cffi:get-callback callback-name)))

(defun set-drop-callback (callback-name)
  (%glfw:set-drop-callback (cffi:get-callback callback-name)))

(import-export %glfw:raw-mouse-motion-supported-p %glfw:get-key-name %glfw:get-key-scancode %glfw:destroy-cursor
               %glfw:create-standard-cursor
               %glfw:joystick-present-p %glfw:get-joystick-axes %glfw:get-joystick-buttons %glfw:get-joystick-hats
               %glfw:get-joystick-name %glfw:get-joystick-guid %glfw:joystick-is-gamepad-p %glfw:update-gamepad-mappings
               %glfw:get-gamepad-name %glfw:get-gamepad-state %glfw:get-timer-value %glfw:get-timer-frequency)

;;added
(deftype joystick-id () '(integer 0 15))

;;;; ## Clipboard

(defun set-clipboard-string (string &optional (window *window*))
  (%glfw:set-clipboard-string window string))

(defun get-clipboard-string (&optional (window *window*))
  (%glfw:get-clipboard-string window))

;;;; ## Time
(import-export %glfw:get-time  %glfw:set-time)

;;;; ## Context
(defun make-context-current (window)
  (setf *window* window)
  (%glfw:make-context-current window)
  window)

(defun get-current-context ()
  (%glfw:get-current-context))

(defmacro with-context (window &body body)
  `(let* ((*window* ,window))
     ,@body))

(defun swap-buffers (&optional (window *window*))
  (%glfw:swap-buffers window))

(defun create-window-surface (instance &optional (window *window*) (allocator (cffi:null-pointer)))
  (%glfw:create-window-surface instance window allocator))

(import-export %glfw:swap-interval
               %glfw:extension-supported-p
               %glfw:get-proc-address
               %glfw:vulkan-supported-p
               %glfw:get-required-instance-extensions
               %glfw:get-instance-proc-address
               %glfw:physical-device-presentation-support-p)
