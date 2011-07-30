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
	       (:file "phsyics")
	       (:file "objects")
               (:file "flight-sim")))

