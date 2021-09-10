(in-package :ve.common.app)

(defun create-semaphore (device)
  (let ((device-handle (device-handle device)))
    (check-result #'vk:create-semaphore device-handle (make-instance 'vk:semaphore-create-info))))

(defun destroy-semaphore (device semaphre)
  (common-dbg "destroy semaphore ~a~%" semaphre)
  (vk:destroy-semaphore (device-handle device) semaphre))

;; (defmacro with-semaphores ((device &rest args) &body body)
;;   (let ((argv (loop :for i :from 0 :below (length args)
;; 		    :collect `(create-semaphore ,device))))
;;     `((lambda (,@args)
;; 	,@body
;; 	(mapcar #'(lambda (arg)
;; 		    (destroy-semaphore ,device arg))
;; 		(list ,@args)))
;;       ,@argv)))

(def-util-union-with with-semaphores 'create-semaphore 'destroy-semaphore)

(defun create-fence (device)
  (let ((device-handle (device-handle device)))
    (check-result #'vk:create-fence device-handle (make-instance 'vk:fence-create-info))))

(defun destroy-fence (device fence)
  (common-dbg "destroy fence ~a~%" fence)
  (vk:destroy-fence (device-handle device) fence))

;; (defmacro with-fences ((device &rest args) &body body)
;;   (let ((argv (loop :for i :from 0 :below (length args)
;; 		    :collect `(create-fence ,device))))
;;     `((lambda (,@args)
;; 	,@body
;; 	(mapcar #'(lambda (arg)
;; 		    (destroy-fence ,device arg))
;; 		(list ,@args)))
;;       ,@argv)))

(def-util-union-with with-fences 'create-fence 'destroy-fence)
