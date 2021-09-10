(defpackage :ve.common
  (:use :cl))

(defpackage :ve.common.debug
  (:use :cl)
  (:export
   :common-dbg
   :common-dbg-on
   :common-dbg-off))

(defpackage :ve.util
  (:use
   :cl
   :ve.common.debug)
  (:export
   :check-result
   :def-util-list-with
   :def-util-union-with))

(defpackage :ve.common.app
  (:use
   :cl
   :ve.util
   :ve.common.debug)
  (:export
   ;; app class export
   :instance
   :surface
   :gpu
   :device
   :swapchain
   ;; app common export 
   :def-key-action
   :def-mouse-actioin
   :with-app
   :with-main-loop
   ;; device class export
   :device-present-queue-family
   :device-graphics-queue-family
   :device-transfer-queue-family
   :device-compute-queue-family
   :device-present-queue
   :device-graphics-queue
   :device-transfer-queue
   :device-compute-queue
   ;; for physical device export nothing
   
   ;; swapchain export
   :swapchain-images
   :acquire-image
   :present-image
   :swapchain-format
   ;; for command export
   :with-command-pool
   :with-allocate-cmds
   ;; :with-cmd
   :reset-command-pool
   ;; for  signal export
   :with-semaphores
   :with-fences
   ;; for frame buffer
   :with-framebuffers))

(defpackage :ve.resource.image
  (:use
   :cl
   :ve.util
   :ve.common.debug
   :ve.common.app)
  (:export
   :with-image-views
   :with-swapchain-image-views))

(defpackage :ve.pipeline
  (:use
   :cl
   :ve.util
   :ve.common.debug
   :ve.common.app)
  (:export
   ;; for shader stage
   :with-shader-stages
   ;; for vertex stage
   :create-vertex-stage
   :with-bindings
   :with-attributes
   ;; for assembly stage
   :create-assembly-stage
   ;; for viewport stage
   :with-viewports
   :with-rect2ds
   :create-viewport-stage
   ;; for rasterization-stage
   :create-rasterization-stage
   ;; for multisample stage
   :create-multisample-stage
   ;; for depth stencil stage
   :create-depth-stencil-stage
   ;; for color blend stage
   :create-color-blend-stage
   :with-color-blend-attachments
   ;; for dynamic-stage
   :create-dynamic-stage
   ;; for pipeline layout 
   :with-push-constant-ranges
   :with-pipeline-layout
   ;; for render pass
   :with-attachment-descriptions
   :with-subpass-descriptions
   :with-attachment-references
   :with-subpass-dependency
   :with-render-pass
   ;; for graphics pipeline
   :with-graphics-pipeline-infos
   :with-graphics-pipeline))
