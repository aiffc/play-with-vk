(defsystem "ve"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (:vk :sdl2 :sdl-vulkan :cffi)
  :components ((:module "src"
                :components
                ((:file "package")
		 (:file "util")
		 (:file "common/debug")
		 (:file "common/instance")
		 (:file "common/surface")
		 (:file "common/physical-device")
		 (:file "common/device")
		 (:file "common/swapchain")
		 (:file "common/command")
		 (:file "common/signal")
		 (:file "common/framebuffer")
		 (:file "common/app")
		 (:file "resource/image")
		 (:file "pipeline/shader")
		 (:file "pipeline/vertex")
		 (:file "pipeline/assembly")
		 (:file "pipeline/viewport")
		 (:file "pipeline/rasterization")
		 (:file "pipeline/multisample")
		 (:file "pipeline/depth-stencil")
		 (:file "pipeline/color-blend-stage")
		 (:file "pipeline/dynamic")
		 (:file "pipeline/layout")
		 (:file "pipeline/renderpass")
		 (:file "pipeline/graphics-pipeline"))))
  :description ""
  :in-order-to ((test-op (test-op "ve/tests"))))

(defsystem "ve/tests"
  :author ""
  :license ""
  :depends-on (:ve)
  :components ((:module "tests"
                :components
                ((:file "test"))))
  :description "Test system for ve")
