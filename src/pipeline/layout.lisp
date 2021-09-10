(in-package :ve.pipeline)

(defun create-pipeline-layout (device set-layouts ranges)
  (let ((create-info (make-instance 'vk:pipeline-layout-create-info
				    :set-layouts set-layouts
				    :push-constant-ranges ranges))
	(device-handle (ve.common.app::device-handle device)))
    (check-result #'vk:create-pipeline-layout device-handle create-info)))

(defun destroy-pipeline-layout (device pipeline-layout)
  (let ((device-handle (ve.common.app::device-handle device)))
    (vk:destroy-pipeline-layout device-handle pipeline-layout)))

(defmacro with-pipeline-layout ((pipeline-layput device &key
							  (set-layouts nil)
							  (ranges nil))
				&body body)
  `(let ((,pipeline-layput (create-pipeline-layout ,device ,set-layouts ,ranges)))
     ,@body
     (destroy-pipeline-layout ,device ,pipeline-layput)))

(defun create-push-constant-range (flags offset size)
  (make-instance 'vk:push-constant-range
		 :stage-flags flags
		 :offset offset
		 :size size))

(def-util-list-with with-push-constant-ranges 'create-push-constant-range)
