(in-package :ve.pipeline)

(defun create-multisample-stage (&key
				   (sample-shading-enable nil)
				   (rasterization-samples :1)
				   (min-sample-shading 0.0)
				   (sample-mask nil)
				   (atc-enable nil)
				   (ato-enable nil))
  (make-instance 'vk:pipeline-multisample-state-create-info
		 :rasterization-samples rasterization-samples
		 :sample-shading-enable sample-shading-enable
		 :min-sample-shading  min-sample-shading
		 :sample-mask  sample-mask
		 :alpha-to-coverage-enable atc-enable 
		 :alpha-to-one-enable ato-enable))
