;;;; flight-sim.asd

(asdf:defsystem #:flight-sim
  :serial t
  :depends-on (#:cl-opengl
               #:cl-glu
               #:lispbuilder-sdl)
  :components ((:file "package")
	       (:file "util")
	       (:file "math")
	       (:file "graphics")
	       (:file "model")
	       (:file "physics")
	       (:file "objects")
	       (:file "engine")
               (:file "flight-sim")))

