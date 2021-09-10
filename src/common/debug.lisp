(in-package :ve.common.debug)

(defparameter *common-dbg* nil)

(defun common-dbg (str &rest args)
  (when *common-dbg*
    (apply #'format (list* *debug-io* str args))))

(defun common-dbg-on () (setf *common-dbg* t))
(defun common-dbg-off () (setf *common-dbg* nil))
