(in-package :ve.util)

;; for check result

(defparameter +success-code+
  '(:success
    :not-ready
    :timeout
    :event-set
    :event-reset
    :incomplete
    :suboptimal-khr
    :thread-idle-khr
    :thread-done-khr
    :operation-deferred-khr
    :operation-not-deferred-khr
    :pipeline-compile-required-ext)
  "success list")

(defparameter +error-code+
  '(:error-out-of-host-memory
    :error-out-of-device-memory
    :error-initialization-failed
    :error-device-lost
    :error-memory-map-failed
    :error-layer-not-present
    :error-extension-not-present
    :error-feature-not-present
    :error-incompatible-driver
    :error-too-many-objects
    :error-format-not-supported
    :error-fragmented-pool
    :error-surface-lost-khr
    :error-native-window-in-use-khr
    :error-out-of-date-khr
    :error-incompatible-display-khr
    :error-invalid-shader-nv
    :error-out-of-pool-memory
    :error-fragmented-pool
    :error-invalid-external-handle
    :error-fragmentation
    :error-invalid-opaque-capture-address
    :error-full-screen-exclusive-mode-lost-ext
    :error-unknown)
  "error list")

(defun check-result (fn &rest args)
  "check-result -> function function arguments"
  (multiple-value-bind (handle result) (apply fn args)
    (cond ((member result +success-code+)
	   (common-dbg "do fn ~a success code: ~a~%" fn result))
	  ((member result +error-code+)
	   (error "do ~a failed code: ~a" fn result))
	  (t (common-dbg t "unknow error~a~%" fn)))
    (values handle result)))

(defmacro def-util-create-list-with (mname fn &optional (dfn nil))
  `(defmacro ,mname ((handles chandle &rest args) &body body)
     (let ((h (mapcar #'(lambda (arg)
			  `(apply ',,fn (list ,chandle ,@create-arg)))
		      args)))
       `((lambda (,handles)
	   ,@body
	   (when ',,dfn
	     (progn
	       ,@(mapcar #'(lambda (h)
			     `(apply ',,dfn (list ,chandle ,h)))
			 handle-names))))
	 (list ,@h)))))

(defmacro def-util-list-with (mname fn)
  `(defmacro ,mname ((handles &rest args) &body body)
     (let ((h (mapcar #'(lambda (arg)
			  `(apply ',,fn (list ,@arg)))
		      args)))
       `((lambda (,handles)
	   ,@body)
	 (list ,@h)))))

(defmacro def-util-union-with (mname cfn &optional (dfn nil))
  `(defmacro ,mname ((chandle &rest args) &body body)
     (let* ((handle-names (mapcar #'first args))
	    (create-args (mapcar #'second args))
	    (handles (mapcar #'(lambda (arg)
				 `(apply ',,cfn (list ,chandle ,@arg)))
			     create-args)))
       `((lambda (,@handle-names)
	   ,@body
	   (when ',,dfn
	     (progn
	       ,@(mapcar #'(lambda (h)
			     `(apply ',,dfn (list ,chandle ,h)))
			 handle-names))))
	 ,@handles))))
