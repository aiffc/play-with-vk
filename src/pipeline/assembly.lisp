(in-package :ve.pipeline)

(defun create-assembly-stage (topology primitive-restart-enable)
  (make-instance 'vk:pipeline-input-assembly-state-create-info
		 :topology topology
		 :primitive-restart-enable primitive-restart-enable))
