;;;; glfw-bindings.lisp

;;;; # GLFW Bindings
;;;; Bindings to the GLFW 3.x library. The functions defined in this file attempt to provide the canonical library with minimal conveniences. Full documentation for the library can be found at http://www.glfw.org/docs/3.0/index.html
(in-package #:%glfw)

(export
 '(+dont-care+
   ;;initialize
   init
   terminate
init-hint
get-version
   get-version-string
   set-error-callback
   ;;monitor
   get-monitors
   get-primary-monitor
   get-monitor-position
   get-monitor-workarea
   get-monitor-physical-size
   get-monitor-content-scale
   get-monitor-name
   set-monitor-user-pointer
   get-monitor-user-pointer
   set-monitor-callback
   get-video-modes
   get-video-mode
   video-mode
   width
   height
   set-gamma
   get-gamma-ramp
   set-gamma-ramp
   ;;window
   default-window-hints
   window-hint
   window-hint-string
   create-window
   destroy-window
   window-should-close-p
   set-window-should-close
   set-window-title
   set-window-icon
   get-window-position
   set-window-position
   get-window-size
   set-window-size-limits
   set-window-aspect-ratio
   set-window-size
   get-framebuffer-size
   get-window-frame-size
   get-window-content-scale
   get-window-opacity
   set-window-opacity
   iconify-window
   restore-window
   maximize-window
   show-window
   hide-window
   focus-window
   request-window-attention
   get-window-monitor
   set-window-monitor
   get-window-attribute
   set-window-attribute
   set-window-user-pointer
   get-window-user-pointer
   set-window-position-callback
   set-window-size-callback
   set-window-close-callback
   set-window-refresh-callback
   set-window-focus-callback
   set-window-iconify-callback
   set-window-maximize-callback
   set-framebuffer-size-callback
   set-window-content-scale-callback
   poll-events
   wait-events
   wait-events-timeout
   post-empty-event
   ;;input
   get-input-mode
   set-input-mode
   raw-mouse-motion-supported-p
   get-key-name
   get-key-scancode
   get-key
   get-mouse-button
   get-cursor-position
   set-cursor-position
   create-cursor
   create-standard-cursor
   destroy-cursor
   set-cursor
   set-key-callback
   set-char-callback
   set-char-mods-callback
   set-mouse-button-callback
   set-cursor-position-callback
   set-cursor-enter-callback
   set-scroll-callback
   set-drop-callback
   joystick-present-p
   get-joystick-axes
   get-joystick-buttons
   get-joystick-hats
   get-joystick-name
   get-joystick-guid
   set-joystick-user-pointer
   get-joystick-user-pointer
   joystick-is-gamepad-p
   set-joystick-callback
   update-gamepad-mappings
   get-gamepad-name
   get-gamepad-state
   set-clipboard-string
   get-clipboard-string
   get-time
   set-time
   get-timer-value
   get-timer-frequency
   ;;context
   make-context-current
   get-current-context
   swap-buffers
   swap-interval
   extension-supported-p
   get-proc-address
   ;;vulkan
   vulkan-supported-p
   get-required-instance-extensions
   get-instance-proc-address
   physical-device-presentation-support-p
   create-window-surface
 ))

;; internal stuff
(export
  '(define-glfw-callback))

(define-foreign-library (glfw)
     (:darwin (:or
               ; homebrew naming
               "libglfw3.3.dylib" "libglfw3.2.dylib" "libglfw3.1.dylib" "libglfw3.dylib"
               ; cmake build naming
               "libglfw.3.3.dylib" "libglfw.3.2.dylib" "libglfw.3.1.dylib" "libglfw.3.dylib"
               ;;glfw-blob
               "libglfw.dylib.bodged"))
     (:unix (:or "libglfw.so.3.3" "libglfw.wo.3.2" "libglfw.so.3.1" "libglfw.so.3" "libglfw.so"
             ;;glfw-blob
             "libglfw.so.bodged"))
     (:windows (:or "glfw3.dll"
                ;;glfw-blob
                "libglfw.dll.bodged"))
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
(defcenum (initialize-hint)
  (:joystick-hat-buttons #x00050001)
  (:cocoa-chdid-resources #x00051001)
  (:cocoa-menubar #x00051002))

(defcenum (key-action)
  :release
  :press
  :repeat)

;;; # Gamepad axes
;;added
(defcenum (gamepad-axes)
  (:left-x 0)
  (:left-y 1)
  (:right-x 2)
  (:right-y 3)
  (:left-trigger 4)
  (:right-trigger 5)
  (:last 5))

;;added
;;; # Gamepad buttons
(defcenum (gamepad-buttons)
  (:a 0)
  (:b 1)
  (:x 2)
  (:y 3)
  (:left-bumper 4)
  (:right-bumper 5)
  (:back 6)
  (:start 7)
  (:guide 8)
  (:left-thumb 9)
  (:right-thumb 10)
  (:dpad-up 11)
  (:dpad-right 12)
  (:dpad-down 13)
  (:dpad-left)
  (:last 14)
  (:cross 0)
  (:circle 1)
  (:square 2)
  (:triangle 3))

;;added
;;; # joystick hat states
(defbitfield (hat)
  (:centered #x0000)
  (:up #x0001)
  (:right #x0002)
  (:down #x0004)
  (:left #x0008)
  (:right-up #x0003)
  (:right-down #x0006)
  (:left-up #x0009)
  (:left-down #x000c))

;;; # Joysticks
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

;;; # KeyBoard Keys
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

;;added caps-lock#x10 num-lock#x20
;;; # Modifier key flags
(defbitfield (mod-keys)
  :shift
  :control
  :alt
  :super
  :caps-lock
  :num-lock)

;;; # Mouse buttons
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

;;added
;;Standard cursor shapes
(defcenum (cursor-shape)
  (:arrow #x00036001)
  (:ibeam #x00036002)
  (:crosshair #x00036003)
  (:hand #x00036004)
  (:hresize #x00036005)
  (:vresize #x00036006))

;;; # Error codes
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

;;; # window-hint
(defcenum (window-hint)
  (:focused #X00020001)
  (:iconified #X00020002)
  (:resizable #X00020003)
  (:visible #X00020004)
  (:decorated #X00020005)
  (:auto-iconify #x00020006) ;added
  (:floating #x00020007) ;added
  (:maximized #x00020008) ;added
  (:center-cursor #x00020009) ;added
  (:transparent-framebuffer #x0002000a) ;added
  (:hovered #x0002000b) ;added
  (:focus-on-show #x0002000c) ;added
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
  (:doublebuffer #x00021010) ;added
  (:client-api #X00022001)
  (:context-version-major #x00022002)
  (:context-version-minor #x00022003)
  (:context-revision #x00022004)
  (:context-robustness #x00022005)
  (:opengl-forward-compat #x00022006)
  (:opengl-debug-context #x00022007)
  (:opengl-profile #X00022008)
  (:context-release-behavior #x00022009) ;added
  (:context-no-error #x0002200a) ;added
  (:context-creation-api #x0002200b) ;added
  (:scale-to-monitor #x0002200c) ;added
  (:cocoa-retina-framebuffer #x00023001) ;added
  (:cocoa-frame-name #x00023002) ;added
  (:cocoa-graphics-switching #x00023003) ;added
  (:x11-class-name #x00024001) ;added
  (:x11-instance-name #x00024002) ;added
  )

;;window attributes for set-window-attributes
(defcenum (window-attribute)
  (:decorated #x00020005)
  (:resizeable #x00020003)
  (:floating #x00020007)
  (:auto-iconify #x00020005)
  (focus-on-show #x0002000c))

;; # for client-api hit
(defcenum (opengl-api)
  (:no-api 0)
  (:opengl-api #X00030001)
  (:opengl-es-api #X00030002))

;; # for context-creation-api hint
(defcenum (context-creation)
  (:native-context-api #x00036001)
  (:egl-context-api #x00036002)
  (osmesa-context #x00036003))

;; # for context-robustness hint
(defcenum (robustness)
  (:no-robustness 0)
  (:no-reset-notification #x00031001)
  (:lose-context-on-reset #x00031002))

;; # for context-release-behavior hint
(defcenum (release-behavior)
  (:any-release-behavior 0)
  (:release-behavior-flush #x00035001)
  (:release-behavior-none #x00035002))

;; # for opengl-profile hint
(defcenum (opengl-profile)
  (:opengl-any-profile 0)
  (:opengl-core-profile #x00032001)
  (:opengl-compat-profile #x00032002))

;; # for monitor callbacks
(defcenum (connection-event)
  (:connected #X00040001)
  (:disconnected #X00040002))

;; # for get-input-mode and set-input-mode
(defcenum (input-mode)
  (:cursor #X00033001)
  (:sticky-keys #X00033002)
  (:sticky-mouse-buttons #x00033003)
  ;;added
  (:lock-key-mods #x00033004)
  (:raw-mouse-motion #x00033005))

;; # for set-input-mode function
(defcenum (cursor-mode)
  (:normal #X00034001)
  (:hidden #X00034002)
  (:disabled #X00034003))

;; # for create-window-surface
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
  (pixels (:pointer :uchar)))

;;added
(defcstruct gamepad-state
  (buttons (:pointer :char))
  (axes (:pointer :float)))

(defctype window :pointer)
(defctype monitor :pointer)
;;added
(defctype cursor :pointer)

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

(defcfun ("glfwInitHint" init-hint) :void
  (hint initialize-hint) (value :boolean))

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
;;;; ### Monitor function
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

(defun get-monitor-workarea (monitor)
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

;;added
(defcfun ("glfwSetMonitorUserPointer" set-monitor-user-pointer) :void
  (monitor monitor) (pointer :pointer))
;;added
(defcfun ("glfwGetMonitorUserPointer" get-monitor-user-pointer) :pointer
  (monitor monitor))

(defcfun ("glfwSetMonitorCallback" set-monitor-callback) :pointer
  "MONITOR-FUN is a callback of type 'void (* GLFWmonitorfun)(GLFWmonitor* monitor,int event)'.
  event is one of the connection-event
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
  (monitor monitor) (ramp (:pointer (:struct gamma-ramp))))

;;;; ### Monitor function
(defcfun ("glfwDefaultWindowHints" default-window-hints) :void
  "Reset all window hints to defaults.")

(defcfun ("glfwWindowHint" window-hint) :void
  (target window-hint) (hint :int))

(defcfun ("glfwWindowHintString" window-hint-string) :void
  (target window-hint) (window-hint-string :string))

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

;;added
(defcfun ("glfwSetWindowIcon" set-window-icon) :void
  (window window) (image-count :int) (images (:pointer (:struct image))))

(defun get-window-position (window)
  "Returns position of upper left corner of window (x y) in screen coordinates."
  (with-foreign-objects ((x :int) (y :int))
    (foreign-funcall "glfwGetWindowPos"
		     window window :pointer x :pointer y :void)
    (list (mem-ref x :int) (mem-ref y :int))))

(defcfun ("glfwSetWindowPos" set-window-position) :void
  (window window) (x :int) (y :int))

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

;;added
(defun get-window-frame-size (window)
  "returns size (left top right bottom) of frame size."
  (with-foreign-objects ((left :int) (top :int) (right :int) (bottom :int))
    (foreign-funcall "glfwGetWindowFrameSize"
                     window window
                     :pointer left :pointer top :pointer right :pointer bottom :void)
    (list (mem-ref left :int) (mem-ref top :int) (mem-ref right :int) (mem-ref bottom :int))))

(defun get-window-content-scale (window)
  "Returned scale is (x-scale y-scale)."
  (with-foreign-objects ((x-scale :float) (y-scale :float))
    (foreign-funcall  "glfwGetWindowContentScale"
                      window window :pointer x-scale :pointer y-scale :void)
    (list (mem-ref x-scale :float) (mem-ref y-scale :float))))

(defun get-window-opacity (window)
  "Returns opacity of window."
  (with-foreign-objects ((x :float))
    (foreign-funcall "glfwGetWindowOpacity"
                     window window :pointer x :void)
    (mem-ref x :float)))

(defcfun ("glfwSetWindowOpacity" set-window-opacity) :void
  (window window) (x :float))

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

(defcfun ("glfwSetWindowMonitor" set-window-monitor) :void
    (window window) (monitor monitor)
    (x-position :int) (y-position :int)
    (width :int) (height :int) (refresh-rate :int))

(defun get-framebuffer-size (window)
  "Returns size (w h) of framebuffer in pixels."
  (with-foreign-objects ((w :int) (h :int))
    (foreign-funcall "glfwGetFramebufferSize"
		     window window :pointer w :pointer h :void)
    (list (mem-ref w :int) (mem-ref h :int))))

(defcfun ("glfwGetWindowAttrib" get-window-attribute) :int
  (window window) (attribute window-hint))

(defcfun ("glfwSetWindowAttrib" set-window-attribute) :void
  (window window) (attrib window-attribute) (value :boolean))

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
(defcfun ("glfwSetWindowMaximizeCallback" set-window-maximize-callback) :pointer
  "MAXIMIZE-FUN is a callback of type 'void (* GLFWwindowmaximizefun)(GLFWwindow*,int)'.
  Returns previously set callback."
  (window window) (maximize-fun :pointer))

(defcfun ("glfwSetFramebufferSizeCallback" set-framebuffer-size-callback) :pointer
  "FRAMEBUFFER-SIZE-FUN is a callback of type 'void (* GLFWframebuffersizefun)(GLFWwindow*,int,int)'.
Returns previously set callback."
  (window window) (framebuffer-size-fun :pointer))

;;added
(defcfun ("glfwSetWindowContentScaleCallback" set-window-content-scale-callback) :pointer
  "CONTENTS-SCALE-FUN is a callback of type 'void (* GLFWwindowContentsScalefun)(GLFWwindow*,float,float)'.
  Returns previously set callback."
  (window window) (contents-scale-fun :pointer))

;;;; ### Events and input
(defcfun ("glfwPollEvents" poll-events) (float-traps-masked :void))

(defcfun ("glfwWaitEvents" wait-events) (float-traps-masked :void))

;;added trapps-masked?
(defcfun ("glfwWaitEventsTimeout" wait-events-timeout) :void
  (timeout :double))

(defcfun ("glfwPostEmptyEvent" post-empty-event) :void)

(defcfun ("glfwSwapBuffers" swap-buffers) :void
  (window window))

;;;; ### Input function
(defcfun ("glfwGetInputMode" get-input-mode) :int
  (window window) (mode input-mode))

(defcfun ("glfwSetInputMode" set-input-mode) :void
  "if mode is :cursor value is cursor-mode
  else value is true or false"
  (window window) (mode input-mode) (value :int))

;;added
(defcfun ("glfwRawMouseMotionSupported" raw-mouse-motion-supported-p) :int)

;;added
(defcfun ("glfwGetKeyName" get-key-name) :string
  (key key) (scancode :int))
;;added
(defcfun ("glfwGetKeyScancode" get-key-scancode) :int
  (key key))

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

;;added
(defcfun ("glfwCreateCursor" create-cursor) cursor
  (image (:pointer (:struct image))) (xhot :int) (yhot :int))

;;added
(defcfun ("glfwCreateStandardCursor" create-standard-cursor) cursor
  (shape cursor-shape))

;;added
(defcfun ("glfwDestroyCursor" destroy-cursor) :void
  (cursor cursor))

;;added
(defcfun ("glfwSetCursor" set-cursor) :void
  (window window) (cursor cursor))

(defcfun ("glfwSetKeyCallback" set-key-callback) :pointer
  "KEY-FUN is a callback of type 'void (* GLFWkeyfun)(GLFWwindow*,int,int,int,int)'.
Returns previously set callback."
  (window window) (key-fun :pointer))

(defcfun ("glfwSetCharCallback" set-char-callback) :pointer
  "CHAR-FUN is a callback of type 'void (* GLFWcharfun)(GLFWwindow*,unsigned int)'.
Returns previously set callback."
  (window window) (char-fun :pointer))

;;added
(defcfun ("glfwSetCharModsCallback" set-char-mods-callback) :pointer
  "CHAR-MODS-FUN is a callback of type 'void (* GLFWCharModsfun)(GLFWwindow*,pointer)'.
Returns previously set callback."
  (window window) (char-mods-fun :pointer))

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

;;;; ### files
;;added
(defcfun ("glfwSetDropCallback" set-drop-callback) :pointer
  "DROP-FUN is a callback of type 'void (* GLFWdropfun)(GLFWwindow*,int path_count,struct char** path_names)'.
Returns previously set callback."
  (DROP-FUN :pointer))

;;;; ### joystick
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

;;added
(defun get-joystick-hats (joystick)
  "Returns list of values for direction of the joystick."
  (with-foreign-object (count :int)
    (c-array->list (foreign-funcall "glfwGetJoystickHats"
                                    :int joystick :pointer count
                                    :pointer)
                   (mem-ref count :int)
                   'hats)))

(defcfun ("glfwGetJoystickName" get-joystick-name) :string
  (joystick :int));jid

;;added
(defcfun ("glfwGetJoystickGUID" get-joystick-guid) :string
  (joystick :int))

;;added
(defcfun ("glfwSetJoystickUserPointer" set-joystick-user-pointer) :void
  (joystick :int) (pointer :pointer))

;;added
(defcfun ("glfwGetJoystickUserPointer" get-joystick-user-pointer) :pointer
  (joystick :int))

;;added
(defcfun ("glfwJoystickIsGamepad" joystick-is-gamepad-p) :boolean
  (joystick :int))

;;added
(defcfun ("glfwSetJoystickCallback" set-joystick-callback) :pointer
  "JOYSTICK-FUN is a callback of type 'void (* GLFWjoystickfun)(int joystick,int event)'.
Returns previously set callback."
  (JOYSTICK-FUN :pointer))

;;added
(defcfun ("glfwUpdateGamepadMappings" update-gamepad-mappings) :boolean
  (string :string))

;;added
(defcfun ("glfwGetGamepadName" get-gamepad-name) :string
  (joystick :int))

;;added
(defcfun ("glfwGetGamepadState" get-gamepad-state) :boolean
  (joystick :int) (gamepad-state (:pointer (:struct gamepad-state))))

;;;; ### Clipboard
(defcfun ("glfwSetClipboardString" set-clipboard-string) :void
  (window window) (string :string))

(defcfun ("glfwGetClipboardString" get-clipboard-string) :string
  (window window))

;;;; ### Time
(defcfun ("glfwGetTime" get-time) :double)

(defcfun ("glfwSetTime" set-time) :void
  (time :double))

;;added
(defcfun ("glfwGetTimerValue" get-timer-value) :uint64)

;;added
(defcfun ("glfwGetTimerFrequency" get-timer-frequency) :uint64)

;;;; ### Context
(defcfun ("glfwMakeContextCurrent" make-context-current) :void
  (window window))

(defcfun ("glfwGetCurrentContext" get-current-context) window)

(defcfun ("glfwSwapInterval" swap-interval) :void
  (interval :int))

(defcfun ("glfwExtensionSupported" extension-supported-p) :boolean
  (extension :string))

(defcfun ("glfwGetProcAddress" get-proc-address) :pointer
  (proc-name :string))

;;;; ### Vulkan function
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
