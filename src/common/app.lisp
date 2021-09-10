(in-package :ve.common.app)

(defclass app ()
  ((instance
    :initarg :instance
    :initform nil
    :accessor instance)
   (debug-reporter
    :initarg :debug-reporter
    :initform nil
    :accessor debug-reporter)
   (surface
    :initarg :surface
    :initform nil
    :accessor surface)
   (gpu
    :initarg :gpu
    :initform nil
    :accessor gpu)
   (device
    :initarg :device
    :initform nil
    :accessor device)
   (swapchain
    :initarg :swapchain
    :initform nil
    :accessor swapchain))
  (:documentation
"
instance type of cffi pointer
debug-reporter type of cffi pointer
surface type of cffi pointer
gpu type of gpu-info in physical-device.lisp
device tyoe of device in device.lisp
swapchain type of cffi pointer
"
   ))

;; main function
(defun make-app (window &key
			  (instance-lays nil)
			  (instance-exts nil)
			  (device-lays nil)
			  (device-exts nil)
			  (format :r8g8b8a8-unorm)
			  (debug nil))
"
ret -> app class
arg
window              <=> sdl window handle
instance-lays       <=> list of string
instance-exts       <=> list of string
device-lays         <=> list of string
device-exts         <=> list of string
debug               <=> t of nil
"
  (multiple-value-bind (instance debug-reporter) (create-instance window :debug debug :layers instance-lays :extensions instance-exts)
    (let* ((surface (create-surface instance window))
	   (gpu (select-gpu instance surface))
	   (device (create-device gpu :extensions device-exts :layers device-lays))
	   (swapchain (create-swapchain window gpu device surface format)))
      (make-instance 'app
		     :instance instance
		     :debug-reporter debug-reporter
		     :surface surface
		     :gpu gpu
		     :device device
		     :swapchain swapchain))))

(defun destroy-app (app)
  (let ((instance (instance app))
	(debug-reporter (debug-reporter app))
	(surface (surface app))
	(device (device app))
	(swapchain (swapchain app)))
    (destroy-swapchain device swapchain)
    (destroy-device device)
    (vk:destroy-surface-khr instance surface)
    (when debug-reporter
      (vk:destroy-debug-utils-messenger-ext instance debug-reporter))
    (vk:destroy-instance instance)))

;; for sdl window action
(defmacro def-key-action  (fun (key) &body body)
  `(defun ,fun (,key)
     ,@body))

(defmacro def-mouse-action (fun (x y xrel yrel state) &body body)
  `(defun ,fun (,x ,y ,xrel ,yrel ,state)
     ,@body))

(def-key-action key-down (key)
  (format t "key-down ~a~%" key))

(def-key-action key-up (key)
  (format t "key-up ~a~%" key))

(def-mouse-action mouse-action (x y xrel yrel state)
  (format t "moduse ~a ~a ~a ~a ~a~%" x y xrel yrel state))

(defmacro with-app ((window
		     app &key
			   (title "sdl window")
			   (w 600)
			   (h 600)
			   (x 0)
			   (y 0)
			   (instance-lays nil)
			   (instance-exts nil)
			   (device-lays nil)
			   (device-exts nil)
			   (format :r8g8b8a8-unorm)
			   (debug nil))
		    &body body)
  `(sdl2:with-init (:everything)
     (sdl2:with-window (,window :title ,title :w ,w :h ,h :x ,x :y ,y :flags '(:vulkan))
       (let ((,app (make-app ,window
			     :instance-lays ,instance-lays
			     :instance-exts ,instance-exts
			     :device-lays ,device-lays
			     :device-exts ,device-exts
			     :format ,format
			     :debug ,debug)))
	 ,@body
	 (destroy-app ,app)))))

(defmacro with-main-loop ((&key
			     (keydown nil)
			     (keyup nil)
			     (mouse nil))
			  &body body)
  `(sdl2:with-event-loop (:method :poll)
     (:keydown (:keysym key) (when ,keydown
			       (funcall ,keydown key)))
     (:keyup (:keysym key) (when ,keyup
			     (funcall ,keyup key)))
     (:mousemotion (:x x :y y :xrel xrel :yrel yrel :state state)
		   (when ,mouse
		     (funcall ,mouse x y xrel yrel state)))
     (:idle () ,@body)
     (:quit () t)))
