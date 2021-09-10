(in-package :ve.common.app)

;; for vulkan debug
;; (cffi:defcallback debug-callback %vk:bool32 ((flags %vk:flags)
;; 					     (type %vk:debug-report-object-type-ext obj-type)
;; 					     (src-obj :uint64)
;; 					     (loacation %vk:size-t)
;; 					     (msg-code :int32)
;; 					     (layer-prefix (:pointer :char))
;; 					     (puser-data (:pointer)))
;;   (format t "flags: ~a~%" flags)
;;   (format t "type: ~a~%" type)
;;   (format t "src-obj: ~a~%" src-obj)
;;   (format t "location: ~a~%" loacation)
;;   (format t "msg-code: ~a~%" msg-code)
;;   (format t "layer-prefix: ~a~%" (cffi:foreign-string-to-lisp layer-prefix))
;;   (format t "puser-data: ~a~2%" (cffi:foreign-string-to-lisp puser-data))
;;   (format t "------------------------------------------------------------------------------~%")
;;   nil)

(cffi:defcallback debug-callback %vk:bool32 ((message-severity %vk:debug-utils-message-severity-flag-bits-ext)
					     (message-type %vk:debug-utils-message-type-flags-ext)
					     (call-back-data %vk:debug-utils-messenger-callback-data-ext)
					     (p-user-data :pointer))
  (format t "message-severity: ~a~%" message-severity)
  (format t "message-type: ~a~%" message-type)
  (format t "debug message: ~a~%" (vk:message call-back-data))
  (when p-user-data
    (format t "user-dataL: ~a~%" (cffi:foreign-string-to-lisp p-user-data)))
  (format t "------------------------------------------------------------------------------~%")
  nil)

(defun create-debug-report-callback (instance create-info)
  (check-result #'vk:create-debug-utils-messenger-ext instance create-info))

;; for vulkan instance
(defun get-all-instance-layers ()
  "get all instance layers"
  (remove-duplicates (mapcar #'vk:layer-name (vk:enumerate-instance-layer-properties))
		     :test #'string=))

(defun get-all-instance-extensions ()
  "get all instance extensions"
  (remove-duplicates  (append (mapcar #'vk:extension-name
				      (vk:enumerate-instance-extension-properties)) 
			      (apply #'append
				     (mapcar #'(lambda (layer)
						 (mapcar #'vk:extension-name
							 (vk:enumerate-instance-extension-properties layer)))
					     (get-all-instance-layers))))
		      :test #'string=))

(defun create-instance (window &key (debug nil) (layers nil) (extensions nil))
  (let* ((debug-create-info (make-instance 'vk:debug-utils-messenger-create-info-ext
					   :message-type '(:validation :general :performance)
					   :message-severity '(:warning :error :info)
					   :pfn-user-callback (cffi:callback debug-callback) 
					   :user-data (cffi:null-pointer)))
	 (all-extensions (get-all-instance-extensions))
	 (all-layers (get-all-instance-layers))
	 (sdl-extensions (sdl-vulkan:sdl-get-instance-extensions window))
	 (useable-extensions (remove-duplicates (append sdl-extensions extensions)
						:test #'string=))
	 (useable-layers layers)
	 (application-info (make-instance 'vk:application-info
					  :application-name "vulkan-demo"
					  :application-version (vk:make-api-version 0 0 0)
					  :engine-name "vulkan-demo"
					  :engine-version (vk:make-api-version 0 0 0)
					  :api-version (vk:make-api-version 1 2 0)))
	 (instance-create-info (make-instance 'vk:instance-create-info
					      :application-info application-info
					      :enabled-layer-names useable-layers
					      :enabled-extension-names useable-extensions))
	 (debug-reporter nil))
    (when debug
      (if (find "VK_EXT_debug_utils" all-extensions :test #'string=)
	  (pushnew "VK_EXT_debug_utils" (vk:enabled-extension-names instance-create-info))
	  (error "not support vk_ext_debug_report"))
      (if (find "VK_LAYER_KHRONOS_validation" all-layers :test #'string=)
	  (pushnew "VK_LAYER_KHRONOS_validation" (vk:enabled-layer-names instance-create-info)))
      (setf (vk:next instance-create-info) debug-create-info))
    
    (multiple-value-bind (handle result) (vk:create-instance instance-create-info)
      (if (eql result :success)
	  (progn
	    (setf vk:*default-extension-loader* (vk:make-extension-loader :instance handle))
	    (when debug
	      (setf debug-reporter (create-debug-report-callback handle debug-create-info)))
	    (values handle debug-reporter))
	  (error "create-instance fail error ~a~%" result)))))

