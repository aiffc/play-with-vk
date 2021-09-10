(in-package :ve.pipeline)

(defun create-attachment-description (&key
					(flags nil)
					(format :r8g8b8a8-snorm)
					(samples :1) 
					(load-op :clear)
					(store-op :store)
					(stencil-load-op :dont-care)
					(stencil-store-op :dont-care)
					(initial-layout :undefined)
					(final-layout :present-src-khr))
  (make-instance 'vk:attachment-description
		 :flags flags
		 :format format
		 :samples samples
		 :load-op load-op
		 :store-op store-op
		 :stencil-load-op stencil-load-op
		 :stencil-store-op stencil-store-op
		 :initial-layout initial-layout
		 :final-layout final-layout))

(defun create-subpss-description (&key
				    (flags nil)
				    (pipeline-bind-point :graphics)
				    (input-attachments nil)
				    (color-attachments nil)
				    (resolve-attachments nil)
				    (depth-stencil-attachment nil)
				    (preserve-attachments nil))
  (make-instance 'vk:subpass-description
		 :flags flags
		 :pipeline-bind-point pipeline-bind-point
		 :input-attachments input-attachments
		 :color-attachments color-attachments
		 :resolve-attachments resolve-attachments
		 :depth-stencil-attachment depth-stencil-attachment
		 :preserve-attachments preserve-attachments))

(defun create-attachment-reference (attachments layout)
  (make-instance 'vk:attachment-reference
		 :attachment attachments
		 :layout layout))

(defun create-subpass-dependency (&key
				    (src-subpass vk:+subpass-external+)
				    (dst-subpass 0)
				    (src-stage-mask :top-of-pipe)
				    (dst-stage-mask :top-of-pipe)
				    (src-access-mask :indirect-command-read)
				    (dst-access-mask :indirect-command-read)
				    (dependency-flags :by-region))
  (make-instance 'vk:subpass-dependency
		 :src-subpass src-subpass
		 :dst-subpass dst-subpass
		 :src-stage-mask src-stage-mask
		 :dst-stage-mask dst-stage-mask
		 :src-access-mask src-access-mask
		 :dst-access-mask dst-access-mask
		 :dependency-flags dependency-flags))

(defun create-render-pass (device attachments subpasses dependencies)
  (let ((device-handle (ve.common.app::device-handle device))
	(create-info (make-instance 'vk:render-pass-create-info
				    :dependencies dependencies
				    :subpasses subpasses
				    :attachments attachments)))
    (vk:create-render-pass device-handle create-info)))

(defun destroy-render-pass (device render-pass)
  (let ((device-handle (ve.common.app::device-handle device)))
    (vk:destroy-render-pass device-handle render-pass)))

(def-util-list-with with-attachment-descriptions 'create-attachment-description)
(def-util-list-with with-subpass-descriptions 'create-subpss-description)
(def-util-list-with with-attachment-references 'create-attachment-reference)
(def-util-list-with with-subpass-dependency 'create-subpass-dependency)

(defmacro with-render-pass ((render-pass device &key
						  (attachments nil)
						  (subpasses nil)
						  (dependencies nil)) &body body)
  `(let ((,render-pass (create-render-pass ,device ,attachments ,subpasses ,dependencies)))
     ,@body
     (destroy-render-pass ,device ,render-pass)))
