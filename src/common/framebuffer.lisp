(in-package :ve.common.app)

(defun create-framebuffers (device swapchain render-pass width height image-views)
  (let ((image-count (length (swapchain-images swapchain)))
	(device-handle (device-handle device)))
    (loop :for i :from 0 :below image-count
	  :for image-view := (nth i image-views)
	  :collect (check-result #'vk:create-framebuffer device-handle (make-instance 'vk:framebuffer-create-info
										      :render-pass render-pass
										      :attachments (list image-view)
										      :width width
										      :height height
										      :layers 1)))))

(defun destroy-framebuffer (device framebuffers)
  (let ((device-handle (device-handle device)))
    (loop :for f :in framebuffers
	  :do (vk:destroy-framebuffer device-handle f))))

(defmacro with-framebuffers ((framebuffers device swapchain render-pass width height image-views) &body body)
  `(let ((,framebuffers (create-framebuffers ,device ,swapchain ,render-pass ,width ,height ,image-views)))
     ,@body
     (destroy-framebuffer ,device ,framebuffers)))
