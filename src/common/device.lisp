(in-package :ve.common.app)

(defclass device ()
  ((device
    :initarg :device
    :initform nil
    :accessor device-handle)
   (present-queue-family
    :initarg :present-queue-family
    :initform nil
    :accessor device-present-queue-family)
   (present-queue
    :initarg :present-queue
    :initform nil
    :accessor device-present-queue)
   (graphics-queue-family
    :initarg :graphics-queue-family
    :initform nil
    :accessor device-graphics-queue-family)
   (graphics-queue
    :initarg :graphics-queue
    :initform nil
    :accessor device-graphics-queue)
   (transfer-queue-family
    :initarg :transfer-queue-family
    :initform nil
    :accessor device-transfer-queue-family)
   (transfer-queue
    :initarg :transfer-queue
    :initform nil
    :accessor device-transfer-queue)
   (compute-queue-family
    :initarg :compute-queue-family
    :initform nil
    :accessor device-compute-queue-family)
   (compute-queue
    :initarg :compute-queue
    :initform nil
    :accessor device-compute-queue)
   (sharing-mode
    :initarg :sharing-mode
    :initform nil
    :accessor device-sharing-mode)))


(defun get-all-gpu-layers (gpu)
  (remove-duplicates (mapcar #'vk:layer-name (vk:enumerate-device-layer-properties gpu))
		     :test #'string=))

(defun get-all-gpu-extensions (gpu)
  (remove-duplicates (append (mapcar #'vk:extension-name
				     (vk:enumerate-device-extension-properties gpu))
			     (apply #'append
				    (mapcar #'(lambda (layer)
						(mapcar #'vk:extension-name
							(vk:enumerate-device-extension-properties gpu layer)))
					    (get-all-gpu-layers gpu))))
		     :test #'string=))

(defun make-device-queue-create-info (queue-info)
  "
ret -> vk:device-queue-create-info
arg
queue-info <=> slot by (gpu-XXX-queues gpu)
"  
  (let* ((properties (second queue-info))
	 (index (first queue-info))
	 (count (vk:queue-count properties))
	 (properties (loop :for i :from 0 :below count
			   :collect 1.0)))
    (make-instance 'vk:device-queue-create-info
		   :queue-family-index index
		   :queue-priorities properties)))

(defun make-device-queue (queue-families transfer compute graphics present)
  (let* ((graphics-index (first (mapcar #'first graphics)))
	 (transfer-index (first (mapcar #'first transfer)))
	 (compute-index (first (mapcar #'first compute)))
	 (present-index (first (mapcar #'first present)))
	 (indexs (remove-duplicates (list graphics-index transfer-index compute-index present-index))))
    (values (mapcar #'(lambda (index)
			(unless (null index)
			  (make-device-queue-create-info (nth index queue-families))))
		    indexs)
	    graphics-index transfer-index compute-index present-index)))

(defun create-device (gpu &key
			    (extensions nil)
			    (layers nil)
			    (debug nil))
  "
ret -> class of device
arg
gpu            <=> class of gpu
extensions     <=> list of string
layers         <=> list of string
"  
  (let* ((transfer-queues (gpu-transfer-queues gpu))
	 (graphics-queues (gpu-graphics-queues gpu))
	 (compute-queues (gpu-compute-queues gpu))
	 (present-queues (gpu-present-queues gpu))
	 (queue-families (gpu-queue-family gpu))
	 (physical-device (gpu-handle gpu))
	 (all-extensions (get-all-gpu-extensions physical-device))
	 (all-layers (get-all-gpu-layers physical-device))
	 (features (gpu-features gpu)))
    (pushnew "VK_KHR_swapchain" extensions :test #'string=)
    (when debug
      (if (find "VK_LAYER_KHRONOS_validation" all-layers :test #'string=)
	  (pushnew "VK_LAYER_KHRONOS_validation" layers)))
    (multiple-value-bind (device-queue-create-info gindex tindex cindex pindex) (make-device-queue queue-families transfer-queues compute-queues graphics-queues present-queues)
      (let* ((create-info (make-instance 'vk:device-create-info
					 :enabled-features features
					 :enabled-extension-names (intersection extensions all-extensions :test #'string=)
					 :enabled-layer-names (intersection layers all-layers :test #'string=)
					 :queue-create-infos device-queue-create-info))
	     (device-handle (check-result #'vk:create-device physical-device create-info)))
	(make-instance 'device
		       :device device-handle
		       :sharing-mode :exclusive
		       :present-queue-family pindex
		       :present-queue (vk:get-device-queue device-handle pindex 0)
		       :graphics-queue-family gindex
		       :graphics-queue (vk:get-device-queue device-handle gindex 0)
		       :transfer-queue-family tindex
		       :transfer-queue (vk:get-device-queue device-handle tindex 0)
		       :compute-queue-family cindex
		       :compute-queue (vk:get-device-queue device-handle cindex 0))))))

(defun destroy-device (device)
  (let ((d (device-handle device)))
    (vk:destroy-device d)))
