(in-package :ve.pipeline)

(defun create-graphics-pipeline-info (layout render-pass &key
							   (stages nil)
							   (vertex-input-state nil)
							   (input-assembly-state nil)
							   (tessellation-state nil)
							   (viewport-state nil)
							   (rasterization-state nil)
							   (multisample-state nil)
							   (depth-stencil-state nil)
							   (color-blend-state nil)
							   (dynamic-state nil)
							   (subpass 0)
							   (base-pipeline-handle nil)
							   (base-pipeline-index 0))
  (make-instance 'vk:graphics-pipeline-create-info
		 :stages stages
		 :vertex-input-state vertex-input-state
		 :input-assembly-state input-assembly-state
		 :tessellation-state tessellation-state
		 :viewport-state viewport-state
		 :rasterization-state rasterization-state
		 :multisample-state multisample-state
		 :depth-stencil-state depth-stencil-state
		 :color-blend-state color-blend-state
		 :dynamic-state dynamic-state
		 :layout layout
		 :render-pass render-pass
		 :subpass subpass
		 :base-pipeline-handle base-pipeline-handle
		 :base-pipeline-index base-pipeline-index))

(defun create-graphics-pipeline (device create-info &optional (cache nil))
  (let* ((device-handle (ve.common.app::device-handle device))
	 (pipeline-handle (check-result #'vk:create-graphics-pipelines device-handle create-info (if cache cache (cffi:null-pointer))))
	 (shader-modules (mapcar #'vk:stages create-info)))
    
    (when shader-modules
      (mapc #'(lambda (cmodules)
		(when cmodules
		  (mapc #'(lambda (m)
			    (destroy-shader-module device (vk:module m)))
			cmodules)))
	    shader-modules))
    pipeline-handle))

(defun destroy-graphics-pipeline (device pipeline)
  (let ((device-handle (ve.common.app::device-handle device)))
    (mapc #'(lambda (handle)
	      (vk:destroy-pipeline device-handle handle))
	  pipeline)))

(defmacro with-graphics-pipeline ((pipeline device cinfo &optional (cache nil)) &body body)
  `(let ((,pipeline (create-graphics-pipeline ,device ,cinfo ,cache)))
     ,@body
     (destroy-graphics-pipeline ,device ,pipeline)))

(def-util-list-with with-graphics-pipeline-infos 'create-graphics-pipeline-info)

