;;;; glfw-bindings.lisp

;;;; # GLFW Bindings
;;;; Bindings to the GLFW 3.x library. The functions defined in this file attempt to provide the canonical library with minimal conveniences. Full documentation for the library can be found at http://www.glfw.org/docs/3.0/index.html
(in-package #:%glfw)

(export
 '(+dont-care+
   init
   terminate
   get-version
   get-version-string
   set-error-callback
   get-monitors
   get-primary-monitor
   get-monitor-position
   get-monitor-work-area
   get-monitor-physical-size
   get-monitor-content-scale
   get-monitor-name
   set-monitor-callback
   get-video-modes
   get-video-mode
   video-mode
   width
   height
   set-gamma
   get-gamma-ramp
   set-gamma-ramp
   default-window-hints
   window-hint
   create-window
   destroy-window
   window-should-close-p
   set-window-should-close
   set-window-title
   get-window-opacity
   set-window-opacity
   get-window-position
   set-window-position
   get-window-size
   set-window-size
   set-window-size-limits
   set-window-aspect-ratio
   get-window-content-scale
   get-framebuffer-size
   iconify-window
   restore-window
   show-window
   hide-window
   get-window-monitor
   get-window-attribute
   set-window-user-pointer
   get-window-user-pointer
   set-window-position-callback
   set-window-size-callback
   set-window-close-callback
   set-window-refresh-callback
   set-window-focus-callback
   set-window-iconify-callback
   set-framebuffer-size-callback
   set-window-monitor
   poll-events
   wait-events
   post-empty-event
   get-input-mode
   set-input-mode
   get-key
   get-mouse-button
   get-cursor-position
   set-cursor-position
   set-key-callback
   set-char-callback
   set-mouse-button-callback
   set-cursor-position-callback
   set-cursor-enter-callback
   set-scroll-callback
   joystick-present-p
   get-joystick-axes
   get-joystick-buttons
   get-joystick-name
   set-clipboard-string
   get-clipboard-string
   get-time
   set-time
   make-context-current
   get-current-context
   swap-buffers
   swap-interval
   extension-supported-p
   get-proc-address
   vulkan-supported-p
   get-required-instance-extensions
   get-instance-proc-address
   physical-device-presentation-support-p
   create-window-surface

   ;;added
   set-window-icon
   get-window-frame-size
   maximize-window
   focus-window
   request-window-attention
   set-window-maximize-callback
   set-window-content-scale-callback
   wait-events-timeout
   ))

;; internal stuff
(export
  '(define-glfw-callback))

(define-foreign-library (glfw)
     (:darwin (:or
               ; homebrew naming
               "libglfw3.1.dylib" "libglfw3.dylib"
               ; cmake build naming
               "libglfw.3.1.dylib" "libglfw.3.dylib"))
     (:unix (:or "libglfw.so.3.1" "libglfw.so.3"))
     (:windows "glfw3.dll")
     (t (:or (:default "libglfw3") (:default "libglfw"))))

(use-foreign-library glfw)

;;;; ## Float trap masking for OS X

;; Floating points traps need to be masked around certain
;; foreign calls on sbcl/darwin. Some private part of Cocoa
;; (Apple's GUI Framework) generates a SIGFPE that
;; invokes SBCLs signal handler if they're not masked.
;;
;; Traps also need to be restored during lisp callback execution
;; because SBCL relies on them to check division by zero, etc.
;; This logic is encapsulated in DEFINE-GLFW-CALLBACK.
;;
;; It might become necessary to do this for other implementations, too.

(defparameter *saved-lisp-fpu-modes* :unset)

(defmacro with-float-traps-saved-and-masked (&body body)
  "Turn off floating point traps and stash them
during execution of the given BODY. Expands into a PROGN if
this is not required for the current implementation."
  #+(and sbcl darwin)
    `(let ((*saved-lisp-fpu-modes* (sb-int:get-floating-point-modes)))
       (sb-int:with-float-traps-masked (:inexact :invalid
                                        :divide-by-zero :overflow
                                        :underflow)
         ,@body))
  #-(and sbcl darwin)
    `(progn ,@body))

(defmacro with-float-traps-restored (&body body)
  "Temporarily restore the saved float traps during execution
of the given BODY. Expands into a PROGN if this is not required
for the current implementation."
  #+(and sbcl darwin)
      (with-gensyms (modes)
        `(let ((,modes (sb-int:get-floating-point-modes)))
           (unwind-protect
                (progn
                  (when (not (eq *saved-lisp-fpu-modes* :unset))
                    (apply #'sb-int:set-floating-point-modes
                           *saved-lisp-fpu-modes*))
                  ,@body)
             (apply #'sb-int:set-floating-point-modes ,modes))))
  #-(and sbcl darwin)
     `(progn ,@body))

;; CFFI type wrapper
(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-foreign-type float-traps-masked-type () ())

  (define-parse-method float-traps-masked (actual-type)
    (make-instance 'float-traps-masked-type :actual-type actual-type))

  (defmethod expand-to-foreign (value (type float-traps-masked-type))
    value)

  (defmethod expand-from-foreign (value (type float-traps-masked-type))
    `(with-float-traps-saved-and-masked ,value)))

;;;; ## Helpers
(defmacro define-glfw-callback (&whole whole name args &body body)
  "Define a foreign callback. This macro is a thin wrapper around
CFFI's defcallback that takes care of GLFW specifics."
  (multiple-value-bind (actual-body decls doc)
      (parse-body body :documentation t :whole whole)
    `(defcallback ,name :void ,args
       ,@(or doc)
       ,@decls
       (with-float-traps-restored
         ,@actual-body))))

(defun c-array->list (array count &optional (type :pointer))
  (loop for i below count collect (mem-aref array type i)))

(alexandria:define-constant +dont-care+ -1)

;;;; ## GLFW Types
(defcenum (key-action)
  :release
  :press
  :repeat)

(defcenum (key)
  (:unknown -1)
  (:space 32)
  (:apostrophe 39)
  (:comma 44)
  (:minus 45)
  (:period 46)
  (:slash 47)
  (:0 48)
  (:1 49)
  (:2 50)
  (:3 51)
  (:4 52)
  (:5 53)
  (:6 54)
  (:7 55)
  (:8 56)
  (:9 57)
  (:semicolon 59)
  (:equal 61)
  (:a 65)
  (:b 66)
  (:c 67)
  (:d 68)
  (:e 69)
  (:f 70)
  (:g 71)
  (:h 72)
  (:i 73)
  (:j 74)
  (:k 75)
  (:l 76)
  (:m 77)
  (:n 78)
  (:o 79)
  (:p 80)
  (:q 81)
  (:r 82)
  (:s 83)
  (:t 84)
  (:u 85)
  (:v 86)
  (:w 87)
  (:x 88)
  (:y 89)
  (:z 90)
  (:left-bracket 91)
  (:backslash 92)
  (:right-bracket 93)
  (:grave-accent 96)
  (:world-1 161)
  (:world-2 162)
  (:escape 256)
  (:enter 257)
  (:tab 258)
  (:backspace 259)
  (:insert 260)
  (:delete 261)
  (:right 262)
  (:left 263)
  (:down 264)
  (:up 265)
  (:page-up 266)
  (:page-down 267)
  (:home 268)
  (:end 269)
  (:caps-lock 280)
  (:scroll-lock 281)
  (:num-lock 282)
  (:print-screen 283)
  (:pause 284)
  (:f1 290)
  (:f2 291)
  (:f3 292)
  (:f4 293)
  (:f5 294)
  (:f6 295)
  (:f7 296)
  (:f8 297)
  (:f9 298)
  (:f10 299)
  (:f11 300)
  (:f12 301)
  (:f13 302)
  (:f14 303)
  (:f15 304)
  (:f16 305)
  (:f17 306)
  (:f18 307)
  (:f19 308)
  (:f20 309)
  (:f21 310)
  (:f22 311)
  (:f23 312)
  (:f24 313)
  (:f25 314)
  (:kp-0 320)
  (:kp-1 321)
  (:kp-2 322)
  (:kp-3 323)
  (:kp-4 324)
  (:kp-5 325)
  (:kp-6 326)
  (:kp-7 327)
  (:kp-8 328)
  (:kp-9 329)
  (:kp-decimal 330)
  (:kp-divide 331)
  (:kp-multiply 332)
  (:kp-subtract 333)
  (:kp-add 334)
  (:kp-enter 335)
  (:kp-equal 336)
  (:left-shift 340)
  (:left-control 341)
  (:left-alt 342)
  (:left-super 343)
  (:right-shift 344)
  (:right-control 345)
  (:right-alt 346)
  (:right-super 347)
  (:menu 348))

(defbitfield (mod-keys)
  :shift
  :control
  :alt
  :super)

(defcenum (mouse)
  (:1 0)
  (:2 1)
  (:3 2)
  (:4 3)
  (:5 4)
  (:6 5)
  (:7 6)
  (:8 7)
  (:last 7)
  (:left 0)
  (:right 1))

(defcenum (joystick)
  :1
  :2
  :3
  :4
  :5
  :6
  :7
  :8
  :9
  :10
  :11
  :12
  :13
  :14
  :15
  :16
  (:last 15))

(defcenum (errors)
  (:not-initialized #x00010001)
  (:no-current-context #x00010002)
  (:invalid-enum #X00010003)
  (:invalid-value #X00010004)
  (:out-of-memory #X00010005)
  (:api-unavailable #X00010006)
  (:version-unavailable #x00010007)
  (:platform-error #X00010008)
  (:format-unavailable #x00010009))

(defcenum (window-hint)
  (:focused #X00020001)
  (:iconified #X00020002)
  (:resizable #X00020003)
  (:visible #X00020004)
  (:decorated #X00020005)
  (:red-bits #X00021001)
  (:green-bits #X00021002)
  (:blue-bits #X00021003)
  (:alpha-bits #X00021004)
  (:depth-bits #X00021005)
  (:stencil-bits #X00021006)
  (:accum-red-bits #X00021007)
  (:accum-green-bits #x00021008)
  (:accum-blue-bits #X00021009)
  (:accum-alpha-bits #x0002100A)
  (:aux-buffers #X0002100B)
  (:stereo #X0002100c)
  (:samples #X0002100d)
  (:srgb-capable #X0002100E)
  (:refresh-rate #X0002100F)
  (:client-api #X00022001)
  (:context-version-major #x00022002)
  (:context-version-minor #x00022003)
  (:context-revision #x00022004)
  (:context-robustness #x00022005)
  (:opengl-forward-compat #x00022006)
  (:opengl-debug-context #x00022007)
  (:opengl-profile #X00022008)
  ;;added
  ;;window-hint
  (:doublebuffer #x00021010)
  (:context-creation-api #x0002200b)
  (:auto-iconify #x00020006)
  (:maximized #x00020008)
  (:center-cursor #x00020009)
  (:transparent-framebuffer #x0002000a)
  (:focus-on-show #x0002000c)
  (:scale-to-monitor #x0002200c)
  ;;context
  (:context-release-behavior #x00022009)
  (:context-no-error #x0002200a)
  ;;not-tested
  #+darwin (:cocoa-retina-framebuffer #x00023001)
  #+darwin (:cocoa-frame-name #x00023002)
  #+darwin (:cocoa-graphics-switching #x00023003)
  #+linux (:x11-class-name #x00024001)
  #+linux (:x11-instance-name #x00024002)
  )

(defcenum (opengl-api)
  (:no-api 0)
  (:opengl-api #X00030001)
  (:opengl-es-api #X00030002))

(defcenum (robustness)
  (:no-robustness 0)
  (:no-reset-notification #x00031001)
  (:lose-context-on-reset #x00031002))

(defcenum (opengl-profile)
  (:opengl-any-profile 0)
  (:opengl-core-profile #x00032001)
  (:opengl-compat-profile #x00032002))

(defcenum (monitor-event)
  (:connected #X00040001)
  (:disconnected #X00040002))

(defcenum (input-mode)
  (:cursor #X00033001)
  (:sticky-keys #X00033002)
  (:sticky-mouse-buttons #x00033003))

(defcenum (cursor-mode)
  (:normal #X00034001)
  (:hidden #X00034002)
  (:disabled #X00034003))

(defcenum (vk-result :int)
  (:error-native-window-in-use-khr -1000000001) ;; returned by glfwCreateWindowSurface if the window has not been created with GLFW_NO_API
  (:error-extension-not-present -7) ;; returned by glfwCreateWindowSurface if the required extensions have not been enabled on the VkInstance
  (:error-initialization-failed -3) ;; returned by glfwCreateWindowSurface if Vulkan is not supported on the system
  (:success #x0)) ;; returned by glfwCreateWindowSurface if the VkSurfaceKHR has been created successfully

(defcstruct video-mode
  (width :int)
  (height :int)
  (red-bits :int)
  (green-bits :int)
  (blue-bits :int)
  (refresh-rate :int))

(defcstruct gamma-ramp
  (red :pointer)
  (green :pointer)
  (blue :pointer)
  (size :unsigned-int))

;;added
(defcstruct image
  (width :int)
  (height :int)
  (pixels :string))

(defctype window :pointer)
(defctype monitor :pointer)

;; vulkan handles
(defctype vk-instance :pointer)
;; VkSurfaceKHR is a non-dispatchable handle - type depends on the system
;; see: https://www.khronos.org/registry/vulkan/specs/1.2-extensions/man/html/VK_DEFINE_NON_DISPATCHABLE_HANDLE.html
#.(if (= 8 (foreign-type-size :pointer))
      '(defctype vk-surface-khr :pointer)
      '(defctype vk-surface-khr :uint64))
(defctype vk-physical-device :pointer)
(defctype vk-allocation-callbacks :pointer)

;;;; ## GLFW Functions
(defcfun ("glfwInit" init) :boolean)
(defcfun ("glfwTerminate" terminate) :void)

(defun get-version ()
  "Returns major, minor, and revison numbers of GLFW library. May be called before INIT."
  (with-foreign-objects ((major :int) (minor :int) (rev :int))
    (foreign-funcall "glfwGetVersion" :pointer major :pointer minor :pointer rev)
    (values (mem-ref major :int) (mem-ref minor :int) (mem-ref rev :int))))

(defcfun ("glfwGetVersionString" get-version-string) :string)

(defcfun ("glfwSetErrorCallback" set-error-callback) :pointer
  "ERROR-FUN is of type 'void (* GLFWerrorfun)(int,const char*)'.
Returns the previous error callback."
  (error-fun :pointer))

;;;; ### Window and monitor functions
(defun get-monitors ()
  "Returns list of pointers to opaque monitor objects."
  (with-foreign-object (count :int)
    (c-array->list (foreign-funcall "glfwGetMonitors" :pointer count :pointer)
        (mem-ref count :int)
        'monitor)))

(defcfun ("glfwGetPrimaryMonitor" get-primary-monitor) :pointer
  "Return the main monitor.")

(defun get-monitor-position (monitor)
  "Returned position is (x y) in screen coordinates."
  (with-foreign-objects ((x :int) (y :int))
    (foreign-funcall "glfwGetMonitorPos"
		     monitor monitor :pointer x :pointer y :void)
    (list (mem-ref x :int) (mem-ref y :int))))

(defun get-monitor-work-area (monitor)
  "Returned work area is (x y w h) in screen coordinates."
  (with-foreign-objects ((x :int) (y :int) (w :int) (h :int))
    (foreign-funcall "glfwGetMonitorWorkarea"
                     monitor monitor :pointer x :pointer y :pointer w :pointer h :void)
    (list (mem-ref x :int) (mem-ref y :int) (mem-ref w :int) (mem-ref h :int))))

(defun get-monitor-physical-size (monitor)
  "Returned size is (w h) in mm."
  (with-foreign-objects ((w :int) (h :int))
    (foreign-funcall "glfwGetMonitorPhysicalSize"
		     monitor monitor :pointer w :pointer h :void)
    (list (mem-ref w :int) (mem-ref h :int))))

(defun get-monitor-content-scale (monitor)
  "Returned scale is (x-scale y-scale)."
  (with-foreign-objects ((x-scale :float) (y-scale :float))
    (foreign-funcall  "glfwGetMonitorContentScale"
                      monitor monitor :pointer x-scale :pointer y-scale :void)
    (list (mem-ref x-scale :float) (mem-ref y-scale :float))))

(defcfun ("glfwGetMonitorName" get-monitor-name) :string
  (monitor monitor))

(defcfun ("glfwSetMonitorCallback" set-monitor-callback) :pointer
  "MONITOR-FUN is a callback of type 'void (* GLFWmonitorfun)(GLFWmonitor*,int)'.
Returns previously set callback."
  (monitor-fun :pointer))

(defun get-video-modes (monitor)
  "Returns list of available video modes for the supplied monitor."
  (with-foreign-object (count :int)
    (c-array->list (foreign-funcall "glfwGetVideoModes" monitor monitor :pointer count
				    :pointer)
        (mem-ref count :int)
        '(:struct video-mode))))

(defun get-video-mode (monitor)
  (mem-ref (foreign-funcall "glfwGetVideoMode"
                            monitor monitor (:pointer (:struct video-mode)))
           '(:struct video-mode)))

(defcfun ("glfwSetGamma" set-gamma) :void
"Generates a 256-element gamma ramp from the specified exponent and then calls SET-GAMMA-RAMP with it."
  (monitor monitor) (gamma :float))

(defun get-gamma-ramp (monitor)
  (mem-ref (foreign-funcall "glfwGetGammaRamp"
                            monitor monitor (:pointer (:struct gamma-ramp)))
           '(:struct gamma-ramp)))

(defcfun ("glfwSetGammaRamp" set-gamma-ramp) :void
  (monitor monitor) (ramp gamma-ramp))

(defcfun ("glfwDefaultWindowHints" default-window-hints) :void
  "Reset all window hints to defaults.")

(defcfun ("glfwWindowHint" window-hint) :void
  (target window-hint) (hint :int))

(defcfun ("glfwWindowHintString" window-hint-string) :void
  (target window-hint) (hint (:pointer :char)))

(defcfun ("glfwCreateWindow" create-window) (float-traps-masked window)
  "Returns a window pointer that shares resources with the window SHARED or NULL."
  (width :int) (height :int) (title :string) (monitor monitor) (shared window))

(defcfun ("glfwDestroyWindow" destroy-window) :void
  (window window))

(defcfun ("glfwWindowShouldClose" window-should-close-p) :boolean
    (window window))

(defcfun ("glfwSetWindowShouldClose" set-window-should-close) :void
  (window window) (value :boolean))

(defcfun ("glfwSetWindowTitle" set-window-title) :void
  (window window) (title :string))

;;added pointer?
(defcfun ("glfwSetWindowIcon" set-window-icon) :void
  (window window) (image-count :int) (images (:pointer (:struct image))))

(defcfun ("glfwSetWindowMonitor" set-window-monitor) :void
    (window window) (monitor monitor)
    (x-position :int) (y-position :int)
    (width :int) (height :int) (refresh-rate :int))

(defun get-window-position (window)
  "Returns position of upper left corner of window (x y) in screen coordinates."
  (with-foreign-objects ((x :int) (y :int))
    (foreign-funcall "glfwGetWindowPos"
		     window window :pointer x :pointer y :void)
    (list (mem-ref x :int) (mem-ref y :int))))

(defcfun ("glfwSetWindowPos" set-window-position) :void
  (window window) (x :int) (y :int))

(defun get-window-opacity (window)
  "Returns opacity of window."
  (with-foreign-objects ((x :float))
    (foreign-funcall "glfwGetWindowOpacity"
                     window window :pointer x :void)
    (mem-ref x :float)))

(defcfun ("glfwSetWindowOpacity" set-window-opacity) :void
  (window window) (x :float))

(defun get-window-size (window)
  "Returns size (w h) in screen coordinates."
  (with-foreign-objects ((w :int) (h :int))
    (foreign-funcall "glfwGetWindowSize"
		     window window :pointer w :pointer h :void)
    (list (mem-ref w :int) (mem-ref h :int))))

(defcfun ("glfwSetWindowSize" set-window-size) :void
  (window window) (w :int) (h :int))

(defcfun ("glfwSetWindowSizeLimits" set-window-size-limits) :void
  (window window) (minwidth :int) (minheight :int) (maxwidth :int) (maxheight :int))

(defcfun ("glfwSetWindowAspectRatio" set-window-aspect-ratio) :void
  (window window) (width :int) (height :int))

(defun get-window-content-scale (window)
  "Returned scale is (x-scale y-scale)."
  (with-foreign-objects ((x-scale :float) (y-scale :float))
    (foreign-funcall  "glfwGetWindowContentScale"
                      window window :pointer x-scale :pointer y-scale :void)
    (list (mem-ref x-scale :float) (mem-ref y-scale :float))))

(defun get-framebuffer-size (window)
  "Returns size (w h) of framebuffer in pixels."
  (with-foreign-objects ((w :int) (h :int))
    (foreign-funcall "glfwGetFramebufferSize"
		     window window :pointer w :pointer h :void)
    (list (mem-ref w :int) (mem-ref h :int))))

;;added
(defun get-window-framesize (window)
  "returns size (left top right bottom) of frame size."
  (with-foreign-objects ((left :int) (top :int) (right :int) (bottom :int))
    (foreign-funcall "glfwGetWindowFrameSize"
                     window window
                     :pointer left :pointer top :pointer right :pointer bottom :void)
    (list (mem-ref left :int) (mem-ref top :int) (mem-ref right :int) (mem-ref bottom :int))))

(defcfun ("glfwIconifyWindow" iconify-window) :void
  (window window))

(defcfun ("glfwRestoreWindow" restore-window) :void
  (window window))

;;added
(defcfun ("glfwMaximizeWindow" maximize-window) :void
  (window window))

(defcfun ("glfwShowWindow" show-window) :void
  (window window))

(defcfun ("glfwHideWindow" hide-window) :void
  (window window))

;;added
(defcfun ("glfwFocusWindow" focus-window) :void
  (window window))

;;added
(defcfun ("glfwRequestWindowAttention" request-window-attention) :void
  (window window))

(defcfun ("glfwGetWindowMonitor" get-window-monitor) monitor
  (window window))

(defcfun ("glfwGetWindowAttrib" get-window-attribute) :int
  (window window) (attribute window-hint))

(defcfun ("glfwSetWindowUserPointer" set-window-user-pointer) :void
  (window window) (pointer :pointer))

(defcfun ("glfwGetWindowUserPointer" get-window-user-pointer) :pointer
  (window window))

(defcfun ("glfwSetWindowPosCallback" set-window-position-callback) :pointer
  "POSITION-FUN is a callback of type 'void (* GLFWwindowposfun)(GLFWwindow*,int,int)'.
Returns previously set callback."
  (window window) (position-fun :pointer))

(defcfun ("glfwSetWindowSizeCallback" set-window-size-callback) :pointer
  "SIZE-FUN is a callback of type 'void (* GLFWwindowsizefun)(GLFWwindow*,int,int)'.
Returns previously set callback."
  (window window) (size-fun :pointer))

(defcfun ("glfwSetWindowCloseCallback" set-window-close-callback) :pointer
  "CLOSE-FUN is a callback of type 'void (* GLFWwindowclosefun)(GLFWwindow*)'.
Returns previously set callback."
  (window window) (close-fun :pointer))

(defcfun ("glfwSetWindowRefreshCallback" set-window-refresh-callback) :pointer
  "REFRESH-FUN is a callback of type 'void (* GLFWwindowrefreshfun)(GLFWwindow*)'.
Returns previously set callback."
  (window window) (refresh-fun :pointer))

(defcfun ("glfwSetWindowFocusCallback" set-window-focus-callback) :pointer
  "FOCUS-FUN is a callback of type 'void (* GLFWwindowfocusfun)(GLFWwindow*,int)'.
Returns previously set callback."
  (window window) (focus-fun :pointer))

(defcfun ("glfwSetWindowIconifyCallback" set-window-iconify-callback) :pointer
  "ICONIFY-FUN is a callback of type 'void (* GLFWwindowiconifyfun)(GLFWwindow*,int)'.
Returns previously set callback."
  (window window) (iconify-fun :pointer))

;;added
(defcfun ("glfwsetWindowMaximizeCallback" set-window-maximize-callback) :pointer
  "MAXIMIZE-FUN is a callback of type 'void (* GLFWwindowmaximizefun)(GLFWwindow*,int)'.
  Returns previously set callback."
  (window window) (maximize-fun :pointer))
;;added
(defcfun ("glfwSetWindowContentScaleCallback" set-window-content-scale-callback) :pointer
  "CONTENTS-SCALE-FUN is a callback of type 'void (* GLFWwindowContentsScalefun)(GLFWwindow*,float,float)'.
  Returns previously set callback."
  (window window) (contents-scale-fun :pointer))

(defcfun ("glfwSetFramebufferSizeCallback" set-framebuffer-size-callback) :pointer
  "FRAMEBUFFER-SIZE-FUN is a callback of type 'void (* GLFWframebuffersizefun)(GLFWwindow*,int,int)'.
Returns previously set callback."
  (window window) (framebuffer-size-fun :pointer))

;;;; ### Events and input
(defcfun ("glfwPollEvents" poll-events) (float-traps-masked :void))

(defcfun ("glfwWaitEvents" wait-events) (float-traps-masked :void))

;;added trapps-masked?
(defcfun ("glfwWaitEventsTimeout" wait-events-timeout) :void
  (timeout :double))

(defcfun ("glfwPostEmptyEvent" post-empty-event) :void)

(defcfun ("glfwGetInputMode" get-input-mode) :int
  (window window) (mode input-mode))

(defcfun ("glfwSetInputMode" set-input-mode) :void
  (window window) (mode input-mode) (value :int))

(defcfun ("glfwGetKey" get-key) key-action
  (window window) (key key))

(defcfun ("glfwGetMouseButton" get-mouse-button) key-action
  (window window) (button mouse))

(defun get-cursor-position (window)
  "Returns position of cursor (x y) realive to client area of window."
  (with-foreign-objects ((x :int) (y :int))
    (foreign-funcall "glfwGetCursorPos"
		     window window :pointer x :pointer y :void)
   (list (mem-ref x :double) (mem-ref y :double))))

(defcfun ("glfwSetCursorPos" set-cursor-position) :void
  (window window) (x :double) (y :double))

(defcfun ("glfwSetKeyCallback" set-key-callback) :pointer
  "KEY-FUN is a callback of type 'void (* GLFWkeyfun)(GLFWwindow*,int,int,int,int)'.
Returns previously set callback."
  (window window) (key-fun :pointer))

(defcfun ("glfwSetCharCallback" set-char-callback) :pointer
  "CHAR-FUN is a callback of type 'void (* GLFWcharfun)(GLFWwindow*,unsigned int)'.
Returns previously set callback."
  (window window) (char-fun :pointer))

(defcfun ("glfwSetMouseButtonCallback" set-mouse-button-callback) :pointer
  "MOUSE-BUTTON-FUN is a callback of type 'void (* GLFWmousebuttonfun)(GLFWwindow*,int,int,int)'.
Returns previously set callback."
  (window window) (mouse-button-fun :pointer))

(defcfun ("glfwSetCursorPosCallback" set-cursor-position-callback) :pointer
  "CURSOR-POS-FUN is a callback of type 'void (* GLFWcursorposfun)(GLFWwindow*,double,double)'.
Returns previously set callback."
  (window window) (CURSOR-POS-FUN :pointer))

(defcfun ("glfwSetCursorEnterCallback" set-cursor-enter-callback) :pointer
  "CURSOR-ENTER-FUN is a callback of type 'void (* GLFWcursorenterfun)(GLFWwindow*,int)'.
Returns previously set callback."
  (window window) (CURSOR-ENTER-FUN :pointer))

(defcfun ("glfwSetScrollCallback" set-scroll-callback) :pointer
  "SCROLL-FUN is a callback of type 'void (* GLFWscrollfun)(GLFWwindow*,double,double)'.
Returns previously set callback."
  (window window) (SCROLL-FUN :pointer))

(defcfun ("glfwJoystickPresent" joystick-present-p) :boolean
  (joystick :int))

(defun get-joystick-axes (joystick)
  "Returns list of values for each axes of the joystick."
  (with-foreign-object (count :int)
    (c-array->list (foreign-funcall "glfwGetJoystickAxes"
				    :int joystick :pointer count
				    :pointer)
        (mem-ref count :int)
        :float)))

(defun get-joystick-buttons (joystick)
  "Returns list of values for each button of the joystick."
  (with-foreign-object (count :int)
    (c-array->list (foreign-funcall "glfwGetJoystickButtons"
				    :int joystick :pointer count
				    :pointer)
        (mem-ref count :int)
        'key-action)))

(defcfun ("glfwGetJoystickName" get-joystick-name) :string
  (joystick :int))

;;;; ### Clipboard
(defcfun ("glfwSetClipboardString" set-clipboard-string) :void
  (window window) (string :string))

(defcfun ("glfwGetClipboardString" get-clipboard-string) :string
  (window window))

;;;; ### Time
(defcfun ("glfwGetTime" get-time) :double)

(defcfun ("glfwSetTime" set-time) :void
  (time :double))

;;;; ### Context
(defcfun ("glfwMakeContextCurrent" make-context-current) :void
  (window window))

(defcfun ("glfwGetCurrentContext" get-current-context) window)

(defcfun ("glfwSwapBuffers" swap-buffers) :void
  (window window))

(defcfun ("glfwSwapInterval" swap-interval) :void
  (interval :int))

(defcfun ("glfwExtensionSupported" extension-supported-p) :boolean
  (extension :string))

(defcfun ("glfwGetProcAddress" get-proc-address) :pointer
  (proc-name :string))

(defcfun ("glfwVulkanSupported" vulkan-supported-p) :boolean)

(defun get-required-instance-extensions ()
  "Returns a all names of required Vulkan extensions in a list."
  (with-foreign-object (count :int)
    (c-array->list (foreign-funcall "glfwGetRequiredInstanceExtensions"
				    :pointer count
                                    :pointer)
        (mem-ref count :int)
        :string)))

(defcfun ("glfwGetInstanceProcAddress" get-instance-proc-address) :pointer
  (instance vk-instance)
  (proc-name :string))

(defcfun ("glfwGetPhysicalDevicePresentationSupport" physical-device-presentation-support-p) :boolean
  (instance vk-instance)
  (device vk-physical-device)
  (queue-family :uint32))

(defun create-window-surface (instance window allocator)
  (cffi:with-foreign-object (surface-khr 'vk-surface-khr)
    (let ((result (foreign-funcall "glfwCreateWindowSurface"
                                   vk-instance instance
                                   window window
                                   vk-allocation-callbacks allocator
                                   vk-surface-khr surface-khr
                                   vk-result)))
      (if (eq result :success)
          (cffi:mem-aref surface-khr 'vk-surface-khr)
          (error "Error creating VkSurfaceKHR: ~a" result)))))
