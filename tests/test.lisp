(defpackage :test.initapp
  (:use
   :cl
   :ve.common.app
   :ve.resource.image
   :ve.pipeline))

(in-package :test.initapp)

(defparameter *frag* (namestring (asdf:system-relative-pathname :ve "tests/shader/fragment-shader.spv")))
(defparameter *vert* (namestring (asdf:system-relative-pathname :ve "tests/shader/vertex-shader.spv")))

(defun shader-module (device)
  (with-shader-stages (stages (device *frag* :stage :fragment) (device *vert* :stage :vertex))
    stages))

(defun vertex-input ()
  (with-bindings (bindings (0 (* (cffi:foreign-type-size :float) 8) :vertex))
    (with-attributes (attribute (0 0 :r32g32b32a32-sfloat 0)
				(1 0 :r32g32b32a32-sfloat 16))
      (create-vertex-stage :bindings bindings :attributes attribute))))

(defun viewput ()
  (with-viewports (viewports (0.0 0.0 600.0 600.0 0.0 0.0))
    (with-rect2ds (rects (0 0 600 600))
      (create-viewport-stage viewports rects))))

(defun color-blend ()
  (with-color-blend-attachments (colors ())
    (create-color-blend-stage :attachments colors)))

(defun gpipeline-info (device layout render-pass)
  (with-graphics-pipeline-infos (create-info
				 (layout
				  render-pass
				  :stages (shader-module device)
				  :vertex-input-state (create-vertex-stage) ;; (vertex-input)
				  :input-assembly-state (create-assembly-stage :triangle-list nil)
				  :viewport-state (viewput)
				  :rasterization-state (create-rasterization-stage)
				  :multisample-state (create-multisample-stage)
				  :depth-stencil-state (create-depth-stencil-stage)
				  :color-blend-state (color-blend)
				  ;:dynamic-state (create-dynamic-stage :viewport :scissor)
				  ))
    create-info))

(defmacro with-test-render-pass ((render-pass device format) &body body)
  `(with-attachment-descriptions (color-attachments (:format ,format))
     (with-attachment-references (color-refer (0 :color-attachment-optimal))
       (with-subpass-descriptions (subpass (:color-attachments color-refer))
	 (with-subpass-dependency (dependency (:dst-subpass 0
					       :src-stage-mask :color-attachment-output
					       :src-access-mask 0
					       :dst-stage-mask :color-attachment-output
					       :dst-access-mask '(:color-attachment-read :color-attachment-write)))
	   (with-render-pass (,render-pass ,device :attachments color-attachments
						   :subpasses subpass
						   :dependencies dependency)
	     ,@body))))))

(defmacro with-test-pipelayout ((pipelayout device) &body body)
  `(with-pipeline-layout (,pipelayout ,device)
     ,@body))

(defmacro with-test-pipeline ((pipeline device render-pass pipelayout format) &body body)
  `(with-test-render-pass (,render-pass ,device ,format)
     (with-test-pipelayout (,pipelayout ,device)
       (with-graphics-pipeline (,pipeline ,device (gpipeline-info ,device ,pipelayout ,render-pass))
	 ,@body))))

(defmacro with-test-framebuffers ((framebuffers image-views device swapchain render-pass width height format) &body body)
  `(with-swapchain-image-views (,image-views ,device ,swapchain :format ,format)
     (with-framebuffers (,framebuffers ,device ,swapchain ,render-pass ,width ,height ,image-views)
       ,@body)))

(defmacro with-test-command-pool ((command-pool cmds render-pass framebuffers pipeline device) &body body)
  `(with-command-pool (,command-pool ,device (device-graphics-queue-family ,device))
     (with-allocate-cmds  (,cmds ,device ,command-pool ,framebuffers ,render-pass ,pipeline
			   :level :primary
			   :count (length ,framebuffers)
			   :vertex-count 3
			   :first-vertex 0
			   :r 1.0
			   :g 0.75
			   :b 0.8
			   :a 0.0)
       ,@body)))

(defun draw-frame (app image-available-semaphore render-finish-semaphore cmds)
  
  (let* ((image-index (acquire-image app :semaphore image-available-semaphore))
	 (submit-info (make-instance 'vk:submit-info
				     :wait-semaphores (list image-available-semaphore)
				     :wait-dst-stage-mask (list :color-attachment-output)
				     :command-buffers (list (nth image-index cmds))
				     :signal-semaphores (list render-finish-semaphore)))
	 (present-info (make-instance 'vk:present-info-khr
				      :wait-semaphores (list render-finish-semaphore)
				      :swapchains (list (ve.common.app::swapchain-handle (swapchain app)))
				      :image-indices (list image-index))))
    (vk:queue-submit (device-graphics-queue (device app)) (list submit-info))
    (vk:queue-present-khr (device-present-queue (device app)) present-info)
    (vk:queue-wait-idle (device-present-queue (device app)))))

(defun test (&optional (debug nil))
  (with-app (window app :debug debug)
    (let* ((device (device app))
	   (swapchain (swapchain app))
	   (swapchain-format (swapchain-format swapchain)))
      (with-semaphores (device (image-available-semaphore) (render-finish-semaphore))
	(with-test-pipeline (pipeline device render-pass pipelayout swapchain-format)
	  (with-test-framebuffers (framebuffers image-views device swapchain render-pass 600 600 swapchain-format)
	    (with-test-command-pool (command-pool cmds render-pass framebuffers (first pipeline) device)
	      (with-main-loop ()
		(draw-frame app image-available-semaphore render-finish-semaphore cmds)))))))))

