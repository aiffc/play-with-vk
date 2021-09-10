(in-package :ve.common.app)

(defclass swapchain ()
  ((swapchain
    :initarg :swapchain
    :initform nil
    :accessor swapchain-handle)    ;; pointer of cffi pointer [type -> vkSwapchainKhr]
   (images
    :initarg :images
    :initform nil
    :accessor swapchain-images)    ;; list of cffi pointer [type -> vkImage]
   (format
    :initarg :format
    :initarg nil
    :accessor swapchain-format)
   ;; (semaphores
   ;;  :initarg :semaphores
   ;;  :initform nil
   ;;  :accessor swapchain-semaphores)
   ))

(defun make-swapchain (device handle format)
  (let* ((d-handle (device-handle device))
	 (images (vk:get-swapchain-images-khr d-handle handle))
	 ;(image-count (length images))
	 ;; (semaphores (loop :for i :from 0 :below image-count
	 ;; 		   :collect (check-result #'vk:create-semaphore
	 ;; 					  d-handle
	 ;; 					  (make-instance 'vk:semaphore-create-info))))
	 )
    (make-instance 'swapchain
		   ; :semaphores semaphores
		   :swapchain handle
		   :images images
		   :format format)))

(defun select-present-mode (gpu)
  (let ((modes (gpu-present-mode gpu)))
    (common-dbg "present-modes: ~a~%" modes)
    (cond ((find :mailbox-khr modes) :mailbox-khr)
	  ((find :immediate-khr modes) :immediate-khr)
	  (t :fifo-khr))))

(defun get-min-image-count (capability)
  (let* ((max-image-count (vk:max-image-count capability))
	 (min-image-count (vk:min-image-count capability))
	 (image-count (1+ min-image-count)))
    (if (and (> max-image-count 0)
	     (> image-count max-image-count))
	max-image-count
	min-image-count)))

(defun get-pre-transform (capability)
  (let ((current-transform (vk:current-transform capability))
	(supported-transform (vk:supported-transforms capability)))
    (if (member :identity supported-transform)
	:identity
	current-transform)))

(defun get-format (format formats)
  (let* ((f (find format formats :key #'vk:format)))
    (if f f (first formats))))

(defun get-composite-alpha (capability)
  (let ((default :inherit)
	(composite-alphas (vk:supported-composite-alpha capability)))
    (if (member :opaque composite-alphas)
	:opaque
	default)))

(defun create-swapchain (window gpu device surface &optional (format :r8g8b8a8-unorm))
"
ret -> class of swapchain
arg
window         <=> sdl window handle
gpu            <=> class of gpu
device         <=> class of device
surface        <=> cffi pointer
"
  (let* ((present-mode (select-present-mode gpu))
	 (sharing-mode (device-sharing-mode device))
	 (window-size (sdl-vulkan:sdl-get-drawable-size window))
	 (capability (gpu-capabilities gpu))
	 (image-count (get-min-image-count capability))
	 (pre-transform (get-pre-transform capability))
	 (formats (gpu-formats gpu))
	 (format (get-format format formats))
	 (composite-alpha (get-composite-alpha capability))
	 (extent (make-instance 'vk:extent-2d
				:height (getf window-size :height)
				:width (getf window-size :width)))
	 (create-info (make-instance 'vk:swapchain-create-info-khr
				     :pre-transform pre-transform
				     :present-mode present-mode
				     :image-sharing-mode sharing-mode
				     :surface surface
				     :image-array-layers 1
				     :image-extent extent
				     :image-usage '(:transfer-dst :color-attachment :sampled)
				     :image-format (vk:format format)
				     :image-color-space (vk:color-space format)
				     :min-image-count image-count
				     :composite-alpha composite-alpha
				     :clipped %vk:+true+))
	 (swapchain-handle (check-result #'vk:create-swapchain-khr (device-handle device) create-info)))
    (common-dbg "present-mode: ~a~%" present-mode)
    (common-dbg "image-count: ~a~%" image-count)
    (common-dbg "format: ~a~%" (vk:format format))
    (common-dbg "color-space: ~a~%" (vk:color-space format))
    (common-dbg "composite-alpha: ~a~%" composite-alpha)
    (make-swapchain device swapchain-handle (vk:format format))))

(defun destroy-swapchain (device swapchain)
  (let ((d (device-handle device))
	;; (semaphores (swapchain-semaphores swapchain))
	(s (swapchain-handle swapchain)))
    ;; (mapcar #'(lambda (semaphore)
    ;; 		(vk:destroy-semaphore d semaphore))
    ;; 	    semaphores)
    (vk:device-wait-idle d)
    (vk:destroy-swapchain-khr d s)))

(defun acquire-image (app &key (semaphore (cffi:null-pointer)) (fence (cffi:null-pointer)))
  "
ret -> next image index
argument 
device        <=>  class of device
swapchain     <=>  class of swapchain
"
  (let ((swapchain-handle (swapchain-handle (swapchain app)))
	(device-handle (device-handle (device app))))
    (vk:acquire-next-image-khr device-handle swapchain-handle 10000000 semaphore fence)
    ;; ready to do
    ;; (multiple-value-bind (next-image-index result)
    ;; 	(vk:acquire-next-image-khr device-handle swapchain-handle 100000 semaphore fence)
    ;;   ;; not support swapchain resize
    ;;   next-image-index)
    ))

(defun present-image (swapchain queue semaphores)
"
ret -> vk resutl
argument 
swapchain                <=> class of swapchain
queue                    <=> vkDeviceQueue cffi pointer
"  
  (let* ((swapchain-handle (list (swapchain-handle swapchain)))
	 ;; (semaphores (swapchain-semaphores swapchain))
	 (images (swapchain-images swapchain))
	 (create-info (make-instance 'vk:present-info-khr
				     :results (cffi:null-pointer)
				     :image-indices images
				     :swapchains swapchain-handle
				     :wait-semaphores semaphores)))
    (vk:queue-present-khr queue create-info)))
