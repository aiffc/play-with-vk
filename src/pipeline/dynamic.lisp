(in-package :ve.pipeline)

(defun create-dynamic-stage (&rest stages)
  (make-instance 'vk:pipeline-dynamic-state-create-info
		 :dynamic-states stages))
