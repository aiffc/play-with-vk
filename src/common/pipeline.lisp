(in-package :ve.common.app)

;; (defclass pipeline ()
;;   ((input-assembly-state
;;    :initarg :input-assembly-state
;;    :initform nil)
;;   (rasterization-state
;;    :initarg :rasterization-state
;;    :initform nil)
;;   (blend-attachment-state
;;    :initarg :blend-attachment-state
;;    :initform nil)
;;   (depth-stencil-state
;;    :initarg :depth-stencil-state
;;    :initform nil)
;;   (multiple-sample-state
;;    :initarg :multiple-sample-state
;;    :initform nil)
;;   (tessellation-state
;;    :initarg :tessellation-state
;;    :initform nil)
;;    (vert-shader-module
;;    :initarg :vert-shader-module
;;    :initform nil)
;;   (frag-shader-module
;;    :initarg :frag-shader-module
;;    :initform nil)
;;   (comp-shader-module
;;    :initarg :comp-shader-module
;;    :initform nil)
;;   (tesc-shader-module
;;    :initarg :tesc-shader-module
;;    :initform nil)
;;   (tese-shader-module
;;    :initarg :tese-shader-module
;;    :initform nil)
;;   (gemo-shader-module
;;    :initarg :gemo-shader-module
;;    :initform nil)))


(defun load-shader-module (path)
  (with-open-file (in path
		      :direction :input
		      :element-type '(unsigned-byte 32))
    (let ((shader-code (make-array 1024 :fill-pointer 0 :adjustable t :element-type '(unsigned-byte 32))))
      (loop :for b := (read-byte in nil nil)
	    :while b
	    :do (vector-push-extend b shader-code)
	    :finally (return (adjust-array shader-code (length shader-code)))))))

(defun create-shader-module (device path)
  "
ret -> cffi pointer
args
device    <=> class of device
path      <=> string
"
  (let* ((device-handle (ve.common.app::device-handle device))
	 (create-info (make-instance 'vk:shader-module-create-info :code (load-shader-module path))))
    (check-result #'vk:create-shader-module device-handle create-info)))

(defun destroy-shader-module (device module)
  "do this after create pipeline"
  (let* ((device-handle (ve.common.app::device-handle device)))
    (common-dbg "destroy shader module ~a~%" module)
    (vk:destroy-shader-module device-handle module)))

(defun create-pipeline-shader-stage (device path &key
						   (stage :vertex)
						   (name "main"))
  (make-instance 'vk:pipeline-shader-stage-create-info
		 :name name
		 :module (create-shader-module device path)
		 :stage stage))

;; for pipeline color blend
(defun create-color-blend-stage (&key
				   (logic-op-enable nil)
				   (logic-op :no-op)
				   (attachments nil)
				   (blend-constants '(1.0 1.0 1.0 1.0)))
  (make-instance 'vk:pipeline-color-blend-state-create-info
		 :logic-op-enable logic-op-enable
		 :logic-op logic-op
		 :attachments attachments
		 :blend-constants (make-array 4 :element-type 'float :initial-contents blend-constants)))

(def-util-list-with with-color-blend-attachments 'create-color-blend-attachment)


(defun make-pipeline (device
		      pipeline-layout
		      render-pass &key
				    (vertex-input-bindings nil)
				    (vertex-inpute-attributes nil)
				    (pipeline-cache nil)
				    (subpass 0)
				    (vert-path nil)
				    (frag-path nil)
				    (comp-path nil)
				    (tesc-path nil)
				    (tese-path nil)
				    (gemo-path nil))
  (let* ((shader-modules nil)
	 (vertex-inpute-state (make-instance 'vk:pipeline-vertex-input-state-create-info
					     :vertex-binding-descriptions vertex-input-bindings
					     :vertex-attribute-descriptions vertex-inpute-attributes))
	 (inpute-assembly-state (make-instance 'vk:pipeline-input-assembly-state-create-info
					       :topology :triangle-list))
	 (rasterization-state (make-instance 'vk:pipeline-rasterization-state-create-info
					     :polygon-mode :fill
					     :cull-mode :back
					     :front-face :clockwise
					     :depth-clamp-enable nil
					     :rasterizer-discard-enable nil
					     :depth-bias-enable nil
					     :line-width 1.0))
	 (blend-attachments (loop :repeat 8
				  :collect (make-instance 'vk:pipeline-color-blend-attachment-state
							  :color-write-mask '(:r :g :b :a)
							  :blend-enable nil
							  :src-color-blend-factor :one
							  :dst-color-blend-factor :zero
							  :color-blend-op :add
							  :src-alpha-blend-factor :one
							  :dst-alpha-blend-factor :zero
							  :alpha-blend-op :add)))
	 (color-blend-state (make-instance 'vk:pipeline-color-blend-state-create-info
					   :attachments blend-attachments))
	 (depth-stencil-state (make-instance 'vk:pipeline-depth-stencil-state-create-info
					     :depth-test-enable 1
					     :depth-write-enable 1
					     :depth-compare-op :less-or-equal
					     :depth-bounds-test-enable nil
					     :back (make-instance 'vk:stencil-op-state
								  :fail-op :keep
								  :pass-op :keep
								  :depth-fail-op :keep
								  :compare-op :always)
					     :front (make-instance 'vk:stencil-op-state
								   :fail-op :keep
								   :pass-op :keep
								   :depth-fail-op :keep
								   :compare-op :always)
					     :stencil-test-enable 1))
	 (multisample-state (make-instance 'vk:pipeline-multisample-state-create-info
					       :rasterization-samples :1
					       :sample-mask nil))
	 (tessellation-state (make-instance 'vk:pipeline-tessellation-state-create-info
					    :patch-control-points 0))
	 (viewport-state (make-instance 'vk:pipeline-viewport-state-create-info
					:scissors (list (make-instance 'vk:rect-2d
								       :extent (make-instance 'vk:extent-2d
											      :width 0
											      :height 0)
								       :offset (make-instance 'vk:offset-2d
											      :x 0
											      :y 0)))
					:viewports (list (make-instance 'vk:viewport))))
	 (dynamic-state (make-instance 'vk:pipeline-dynamic-state-create-info
				       :dynamic-states '(:viewport :scissor)))
	 (create-info (make-instance 'vk:graphics-pipeline-create-info
				     :vertex-input-state vertex-inpute-state
				     :input-assembly-state inpute-assembly-state
				     :tessellation-state tessellation-state
				     :viewport-state viewport-state
				     :rasterization-state rasterization-state
				     :multisample-state multisample-state
				     :depth-stencil-state depth-stencil-state
				     :color-blend-state color-blend-state
				     :dynamic-state dynamic-state
				     :layout pipeline-layout
				     :render-pass render-pass
				     :subpass subpass
				     :base-pipeline-handle nil
				     :base-pipeline-index 0)))
    (when vert-path
      (push (create-pipeline-shader-stage device vert-path :stage :vertex) shader-modules))
    (when frag-path
      (push (create-pipeline-shader-stage device frag-path :stage :fragment) shader-modules))
    (when comp-path
      (push (create-pipeline-shader-stage device comp-path :stage :compute) shader-modules))
    (when tesc-path
      (push (create-pipeline-shader-stage device tesc-path :stage :tessellation-control) shader-modules))
    (when tese-path
      (push (create-pipeline-shader-stage device tese-path :stage :tessellation-evaluation) shader-modules))
    (when gemo-path
      (push (create-pipeline-shader-stage device gemo-path :stage :geometry) shader-modules))
    (setf (vk:stages create-info) shader-modules)
    (let* ((device-handle (device-handle device))
	  (pipeline (first (check-result #'vk:create-graphics-pipelines device-handle create-info (if pipeline-cache
													       pipeline-cache
													       (cffi:null-pointer))))))
      (mapcar #'(lambda (module)
		  (when module
		    (destroy-shader-module device-handle module)))
	      (vk:stage create-info))
      pipeline)))
