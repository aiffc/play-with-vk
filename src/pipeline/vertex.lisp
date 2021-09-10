(in-package :ve.pipeline)

(defun create-vertex-stage (&key (bindings nil) (attributes nil))
  (make-instance 'vk:pipeline-vertex-input-state-create-info
		 :vertex-attribute-descriptions attributes
		 :vertex-binding-descriptions bindings))

(defun create-binding-description (binding stride input-rate)
  (make-instance 'vk:vertex-input-binding-description
		 :input-rate input-rate
		 :binding binding
		 :stride stride))

(defun create-attribute-description (location binding format offset)
  (make-instance 'vk:vertex-input-attribute-description
		 :location location
		 :binding binding
		 :format format
		 :offset offset))

(def-util-list-with with-bindings 'create-binding-description)
(def-util-list-with with-attributes 'create-attribute-description)
