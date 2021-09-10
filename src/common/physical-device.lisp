(in-package :ve.common.app)

;; all queue is (list index properties)
(defclass gpu-info ()
  ((handle
    :initarg :handle
    :initform nil
    :accessor gpu-handle)
   (queue-family
    :initarg :queue-family
    :initform nil
    :accessor gpu-queue-family)
   (present-queues
    :initarg :present-queues
    :initform nil
    :accessor gpu-present-queues)
   (graphics-queues
    :initarg :graphics-queues
    :initform nil
    :accessor gpu-graphics-queues)
   (transfer-queues
    :initarg :transfer-queues
    :initarg nil
    :accessor gpu-transfer-queues)
   (compute-queues
    :initarg :compute-queues
    :initarg nil
    :accessor gpu-compute-queues)
   (capabilities
    :initarg :capabilities
    :initform nil
    :accessor gpu-capabilities)
   (formats
    :initarg :formats
    :initform nil
    :accessor gpu-formats)
   (present-mode
    :initarg :present-mode
    :initform nil
    :accessor gpu-present-mode) 
   (features
    :initarg :features
    :initform nil
    :accessor gpu-features)
   (properties
    :initarg :properties
    :initform nil
    :accessor gpu-properties)
   (score
    :initarg :score
    :initform 0
    :accessor gpu-score)))

;; for for-vulakn physical device
(defun make-gpu-info (handle surface queue-family capabilities format present-mode features properties)
  (let* ((score 0)
	 (type (vk:device-type properties))
	 (flags (mapcar #'vk:queue-flags queue-family))
	 (graphics-queues (loop :for i :from 0 :upto (1- (length flags))
				:for f := (nth i flags)
				:for c := (nth i queue-family)
				:when (find :graphics f :test #'eql)
				  :collect (list i c)))
	 (compute-queues (loop :for i :from 0 :upto (1- (length flags))
				:for f := (nth i flags)
				:for c := (nth i queue-family)
				:when (find :compute f :test #'eql)
				  :collect (list i c)))
	 (transfer-queues (loop :for i :from 0 :upto (1- (length flags))
				:for f := (nth i flags)
				:for c := (nth i queue-family)
				:when (find :transfer f :test #'eql)
				  :collect (list i c)))
	 (present-queues (loop :for i :from 0 :upto (1- (length flags))
			       :for c := (nth i queue-family)
			       :when (vk:get-physical-device-surface-support-khr handle i surface)
				 :collect (list i c)))
	 (queue-families (loop :for i :from 0 :upto (1- (length flags))
			       :for c := (nth i queue-family)
			       :collect (list i c))))
    (case type
      (:other (incf score 50))
      (:integrated-gpu (incf score 150))
      (:discrete-gpu (incf score 250))
      (:virtual-gpu (incf score 100))
      (:cpu (incf score 200)))
    (common-dbg "graphics-queues: ~a~%" graphics-queues)
    (common-dbg "present-queues: ~a~%" present-queues)
    (common-dbg "transfer-queues: ~a~%" transfer-queues)
    (common-dbg "compute-queues: ~a~%" compute-queues)
    (common-dbg "queue-families: ~a~2%" queue-families)
    
    (make-instance 'gpu-info
		   :handle handle
		   :queue-family queue-families
		   :capabilities capabilities
		   :formats format
		   :present-mode present-mode
		   :features features
		   :properties properties
		   :present-queues present-queues
		   :graphics-queues graphics-queues
		   :transfer-queues transfer-queues
		   :compute-queues compute-queues
		   :score score)))

(defun get-gpu-info (gpu surface)
  "select best gpu"
  (let ((capabilities (vk:get-physical-device-surface-capabilities-khr gpu surface))
	(format (vk:get-physical-device-surface-formats-khr gpu surface))
	(present-mode (vk:get-physical-device-surface-present-modes-khr gpu surface))
	(features (vk:get-physical-device-features gpu))
	(properties (vk:get-physical-device-properties gpu))
	(queue-family (vk:get-physical-device-queue-family-properties gpu)))
    (make-gpu-info gpu surface queue-family capabilities format present-mode features properties)))

(defun pickup-gpu (gpus surface)
  "return first gpu ready to do"
  (let* ((infos (mapcar #'(lambda (gpu)
			    (get-gpu-info gpu surface))
			gpus))
	 (select-gpu (if (= 1 (length gpus))
			 (first infos)
			 (find-if #'max infos :key #'gpu-score))))
    (let ((properties (gpu-properties select-gpu))
	  (queue-family (mapcar #'second (gpu-queue-family select-gpu))))
      (common-dbg "apiVersion: ~a~%" (vk-utils:format-api-version (vk:api-version properties)))
      (common-dbg "driverVersion: ~a~%" (vk-utils:format-api-version (vk:driver-version properties)))
      (common-dbg "vendorID: ~a~%" (vk:vendor-id properties))
      (common-dbg "deviceID: ~a~%" (vk:device-id properties))
      (common-dbg "deviceType: ~a~%" (vk:device-type properties))
      (common-dbg "deviceName: ~a~%" (vk:device-name properties))
      (dolist (q queue-family)
	(common-dbg "~tqueueFlags: ~a~%" (vk:queue-flags q))
	(common-dbg "~tqueueCount: ~a~%" (vk:queue-count q))))
    select-gpu))

(defun select-gpu (instance surface)
"
ret -> class of gpu
argument 
instance <=> cffi pointer
surface <=> cffi pointer
"
  (let ((gpus (check-result #'vk:enumerate-physical-devices instance)))
    (pickup-gpu gpus surface)))
