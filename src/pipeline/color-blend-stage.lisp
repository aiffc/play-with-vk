(in-package :ve.pipeline)

(defun create-color-blend-attachment (&key
					(blend-enable nil)
					(src-color-blend-factory :zero)
					(dst-color-blend-factory :zero)
					(color-blend-op :add)
					(src-alpha-blend-factory :zero)
					(dst-alpha-blend-factory :zero)
					(alpha-blend-op :add)
					(write-mask '(:r :g :b :a)))
  (make-instance 'vk:pipeline-color-blend-attachment-state
		 :blend-enable blend-enable
		 :src-color-blend-factor src-color-blend-factory
		 :dst-color-blend-factor dst-color-blend-factory
		 :color-blend-op color-blend-op
		 :src-alpha-blend-factor src-alpha-blend-factory
		 :dst-alpha-blend-factor dst-alpha-blend-factory
		 :alpha-blend-op alpha-blend-op
		 :color-write-mask write-mask))

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
