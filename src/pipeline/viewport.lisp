(in-package :ve.pipeline)

(defun create-viewport (x y w h mind maxd)
  (make-instance 'vk:viewport
		 :x x
		 :y y
		 :width w
		 :height h
		 :min-depth mind
		 :max-depth maxd))

(defun create-rect2d (offset-x offset-y extent-w extent-h)
  (let ((offset (make-instance 'vk:offset-2d
			       :x offset-x
			       :y offset-y))
	(extent (make-instance 'vk:extent-2d
			       :width extent-w
			       :height extent-h)))
    (make-instance 'vk:rect-2d
		   :extent extent
		   :offset offset)))

(def-util-list-with with-viewports 'create-viewport)
(def-util-list-with with-rect2ds 'create-rect2d)

(defun create-viewport-stage (viewports rects)
  (make-instance 'vk:pipeline-viewport-state-create-info
		 :scissors rects
		 :viewports viewports))
