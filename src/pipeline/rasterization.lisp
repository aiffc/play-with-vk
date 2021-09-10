(in-package :ve.pipeline)

(defun create-rasterization-stage (&key
				     (depth-clamp-enable nil)
				     (rasterizer-discard-enable nil)
				     (polygon-mode :fill)
				     (cull-mode :back)
				     (front-face :clockwise)
				     (depth-bias-enable nil)
				     (depth-bias-constant-factor 0.0)
				     (depth-bias-clamp 0.0)
				     (depth-bias-slope-factor 0.0)
				     (line-width 1.0))
  (make-instance 'vk:pipeline-rasterization-state-create-info
		 :line-width line-width
		 :depth-bias-slope-factor depth-bias-slope-factor
		 :depth-bias-clamp depth-bias-clamp
		 :depth-bias-constant-factor depth-bias-constant-factor
		 :depth-bias-enable depth-bias-enable
		 :front-face front-face
		 :cull-mode cull-mode
		 :polygon-mode polygon-mode
		 :rasterizer-discard-enable rasterizer-discard-enable
		 :depth-clamp-enable depth-clamp-enable))
