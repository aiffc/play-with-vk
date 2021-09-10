(in-package :ve.common.app)

;; for vulakn surface
(defun create-surface (instance window)
"
ret -> cffi pointer
argument 
instance  <=> cffi pointer
window    <=> sdl window handle
"
  (sdl-vulkan:sdl-create-surface instance window))

