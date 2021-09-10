(in-package :ve.resource.image)

(defun create-image-view (device image &key
					 (view-type :2d)
					 (format :r8g8b8a8-snorm)
					 (r :identity)
					 (g :identity)
					 (b :identity)
					 (a :identity)
					 (aspect-mask :color)
					 (base-mip-level 0)
					 (level-count 1)
					 (base-array-layer 0)
					 (layer-count 1))
  (let* ((device-handle (ve.common.app::device-handle device))
	 (component-info (make-instance 'vk:component-mapping
					:r r :g g :b b :a a))
	 (subresource-info (make-instance 'vk:image-subresource-range
					  :layer-count layer-count
					  :base-array-layer base-array-layer
					  :level-count level-count
					  :base-mip-level base-mip-level
					  :aspect-mask aspect-mask))
	 (create-info (make-instance 'vk:image-view-create-info
				     :subresource-range subresource-info
				     :components component-info
				     :format format
				     :view-type view-type
				     :image image)))
    (check-result #'vk:create-image-view device-handle create-info)))

(defun create-image-view-with-swapchain (device swapchain &key
							    (view-type :2d)
							    (format :r8g8b8a8-snorm)
							    (r :identity)
							    (g :identity)
							    (b :identity)
							    (a :identity)
							    (aspect-mask :color)
							    (base-mip-level 0)
							    (level-count 1)
							    (base-array-layer 0)
							    (layer-count 1))
  (let* ((device-handle (ve.common.app::device-handle device))
	 (component-info (make-instance 'vk:component-mapping
					:r r :g g :b b :a a))
	 (subresource-info (make-instance 'vk:image-subresource-range
					  :layer-count layer-count
					  :base-array-layer base-array-layer
					  :level-count level-count
					  :base-mip-level base-mip-level
					  :aspect-mask aspect-mask))
	 (images (swapchain-images swapchain)))
    (loop :for i :in images
	  :collect (vk:create-image-view device-handle
					 (make-instance 'vk:image-view-create-info
							:subresource-range subresource-info
							:components component-info
							:format format
							:view-type view-type
							:image i)))))

(defun destroy-image-view (device image-view)
  (let ((device-handle (ve.common.app::device-handle device)))
    (common-dbg "destroy image view ~a~%" image-view)
    (if (listp image-view)
	(mapc #'(lambda (view)
		  (vk:destroy-image-view device-handle view))
	      image-view)
	(vk:destroy-image-view device-handle image-view))))

;; (defmacro with-image-views ((device &rest args) &body body)
;;   (let* ((handle-names (mapcar #'first args))
;; 	 (arguments (mapcar #'second args))
;; 	 (handles (mapcar #'(lambda (arg)
;; 			      `(apply #'create-image-view (list ,device ,@arg)))
;; 			  arguments)))
;;     `((lambda (,@handle-names)
;; 	,@body
;; 	(mapcar #'(lambda (image-view)
;; 		    (destroy-image-view ,device image-view))
;; 		(list ,@handle-names)))
;;       ,@handles)))

(def-util-union-with with-image-views 'create-image-view 'destroy-image-view)

(defmacro with-swapchain-image-views ((image-views device swapchain &key
								  (view-type :2d)
								  (format :r8g8b8a8-snorm)
								  (r :identity)
								  (g :identity)
								  (b :identity)
								  (a :identity)
								  (aspect-mask :color)
								  (base-mip-level 0)
								  (level-count 1)
								  (base-array-layer 0)
								  (layer-count 1))
				      &body body)
  `(let ((,image-views (create-image-view-with-swapchain ,device
							 ,swapchain
							 :view-type ,view-type
							 :format ,format
							 :r ,r
							 :g ,g
							 :b ,b
							 :a ,a
							 :aspect-mask ,aspect-mask
							 :base-mip-level ,base-mip-level
							 :level-count ,level-count
							 :base-array-layer ,base-array-layer
							 :layer-count ,layer-count)))
     ,@body
     (destroy-image-view ,device ,image-views)))

