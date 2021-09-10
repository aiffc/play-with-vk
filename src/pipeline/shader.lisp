(in-package :ve.pipeline)

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

(def-util-list-with with-shader-stages 'create-pipeline-shader-stage)
