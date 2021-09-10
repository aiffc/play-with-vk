(in-package :ve.common.app)

(defun create-command-pool (device queue-family-index &optional (flags :reset-command-buffer))
  "
ret -> vkCommandPool cffi pointer
arguments
device             <=> class of device
queue-faily-index  <=> integer
flags              <=> VkCommandPoolCreateFlagBits
"
  (let ((device-handle (device-handle device))
	(create-info (make-instance 'vk:command-pool-create-info
				    :flags flags
				    :queue-family-index queue-family-index)))
    (check-result #'vk:create-command-pool device-handle create-info)))

(defun destroy-command-pool (device command-pool)
  (let ((device-handle (device-handle device)))
    (vk:destroy-command-pool device-handle command-pool)))

(defmacro with-command-pool ((pool device queue-family-index &optional (flags :reset-command-buffer)) &body body)
  `(let ((,pool (create-command-pool ,device ,queue-family-index ,flags)))
     ,@body
     (destroy-command-pool ,device ,pool)))

(defun allocate-commands (device command-pool &optional (level :primary) (count 1))
  "
ret -> list of vkCommand (type of cffi pointer)
argument
device       <=> class of device
command-pool <=> cffi pointer
level        <=> VkCommandBufferLevel
count        <=> integer 
"
  (let ((device-handle (device-handle device))
	(create-info (make-instance 'vk:command-buffer-allocate-info
				    :command-buffer-count count
				    :level level
				    :command-pool command-pool)))
    (check-result #'vk:allocate-command-buffers device-handle create-info)))

(defun free-commands (device command-pool cmds)
  (let ((device-handle (device-handle device)))
    (vk:free-command-buffers device-handle command-pool cmds)))

(defun begin-cmd (cmds
		  framebuffers
		  render-pass
		  pipeline
		  &key
		    (offset-x 0)
		    (offset-y 0)
		    (extent-width 600)
		    (extent-height 600)
		    (r 0.0)
		    (g 0.0)
		    (b 0.0)
		    (a 1.0)
		    (vertex-count 0)
		    (first-vertex 0)
		    (contents :inline)
		    (bind-point :graphics)
		    (flags :one-time-submit)
		    (inheritance-p nil)
		    (inheritance-render-pass nil)
		    (subpass 0)
		    (framebuffer nil)
		    (query-enable %vk:+false+)
		    (query-flags :precise)
		    (pipeline-statistics :input-assembly-vertices))
  "
ret -> vkResult
arguments
flags                <=> vkCommandBegineflag
inheritance-p        <=> t or nil, t if cmd need inheritance
render-pass          <=> for inheritance render pass cffi pointer
subpass              <=> for inheritance subpass 
framebuffer          <=> for inheritance framebuffer cffi pointer
query-enable         <=> for inheritance occlusion-query-enable %vk:+true+ or %vk:+false+
query-flags          <=> for inheritance query-flags
pipeline-statistics  <=> for inheritance pipeline-statistics
"  
  (let* ((inheritance-info (make-instance 'vk:command-buffer-inheritance-info
					  :pipeline-statistics pipeline-statistics
					  :query-flags query-flags
					  :occlusion-query-enable query-enable
					  :framebuffer framebuffer
					  :subpass subpass
					  :render-pass inheritance-render-pass))
	 (create-info (make-instance 'vk:command-buffer-begin-info
				     :flags flags
				     :inheritance-info (if inheritance-p inheritance-info nil)))
	 (render-area (make-instance 'vk:rect-2d
				     :extent (make-instance 'vk:extent-2d :width extent-width :height extent-height)
				     :offset (make-instance 'vk:offset-2d :x offset-x :y offset-y))))
    (loop :for i :from 0 :below (length cmds)
	  :for f := (nth i framebuffers)
	  :for c := (nth i cmds)
	  :do (progn
		(check-result #'vk:begin-command-buffer c create-info)
		(vk:cmd-begin-render-pass c
					  (make-instance 'vk:render-pass-begin-info
							 :render-pass render-pass
							 :framebuffer f
							 :render-area render-area
							 :clear-values (list (make-instance 'vk:clear-color-value
											    :float-32
											    (make-array 4
													:initial-contents
													(list r g b a)))))
					  contents)
		(vk:cmd-bind-pipeline c bind-point pipeline)
		(vk:cmd-draw c vertex-count 1 first-vertex 0)
		(vk:cmd-end-render-pass c)
		(check-result #'vk:end-command-buffer c)))))

(defmacro with-allocate-cmds ((cmds
			       device
			       pool
			       framebuffers
			       render-pass
			       pipeline
			       &key
				 (level :primary)
				 (count 1)
				 (offset-x 0)
				 (offset-y 0)
				 (extent-width 600)
				 (extent-height 600)
				 (r 0.0)
				 (g 0.0)
				 (b 0.0)
				 (a 1.0)
				 (vertex-count 0)
				 (first-vertex 0)
				 (contents :inline)
				 (bind-point :graphics)
				 (flags :one-time-submit)
				 (inheritance-p nil)
				 (inheritance-render-pass nil)
				 (subpass 0)
				 (framebuffer nil)
				 (query-enable %vk:+false+)
				 (query-flags :precise)
				 (pipeline-statistics :input-assembly-vertices)) &body body)
  `(let ((,cmds (allocate-commands ,device ,pool ,level ,count)))
     (begin-cmd ,cmds ,framebuffers ,render-pass ,pipeline
		:offset-x ,offset-x
		:offset-y ,offset-y
		:extent-width ,extent-width
		:extent-height ,extent-height
		:r ,r
		:g ,g
		:b ,b
		:a ,a
		:vertex-count ,vertex-count
		:first-vertex ,first-vertex
		:contents ,contents
		:bind-point ,bind-point
		:flags ,flags
		:inheritance-p ,inheritance-p
		:inheritance-render-pass ,inheritance-render-pass
		:subpass ,subpass
		:framebuffer ,framebuffer
		:query-enable ,query-enable
		:query-flags ,query-flags
		:pipeline-statistics ,pipeline-statistics)
     ,@body
     (free-commands ,device ,pool ,cmds)))

;; (defmacro with-cmd ((cmd &key
;; 			  (flags :one-time-submit)
;; 			  (inheritance-p nil)
;; 			  (render-pass (cffi:null-pointer))
;; 			  (subpass 0)
;; 			  (framebuffer (cffi:null-pointer))
;; 			  (query-enable %vk:+false+)
;; 			  (query-flags :precise)
;; 			  (pipeline-statistics :input-assembly-vertices)) &body body)
;;   `(progn
;;      (start-cmd ,cmd
;; 		:flags ,flags
;; 		:inheritance-p ,inheritance-p
;; 		:render-pass ,render-pass
;; 		:subpass ,subpass
;; 		:framebuffer ,framebuffer
;; 		:query-enable ,query-enable
;; 		:query-flags ,query-flags
;; 		:pipeline-statistics ,pipeline-statistics)
;;      ,@body
;;      (end-cmd ,cmd)))

(defun reset-command-pool (device command-pool)
  "
ret -> VkResult
argument 
device         <=> class of device 
command-pool   <=> vkCommandPool (cffi:pointer)
"
  (let ((device-handle (device-handle device)))
    (check-result #'vk:reset-command-pool device-handle command-pool)))
