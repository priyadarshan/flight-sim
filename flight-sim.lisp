;;;; flight-sim.lisp

(in-package #:flight-sim)

;;; "flight-sim" goes here. Hacks and glory await!



;(defclass engine-object (game-object)
;  ((active :initarg :active :reader active :initform nil)
;   (start-time :initarg :start-time :reader start-time :initform nil)
;   (forces :initarg :forces :accessor forces :initform '())))

;(defgeneric activate-engine (object engine-sym))

;(defmethod activate-engine ((object powered-object) engine-sym)
;  (push :engine-sym (getf (engines object) :active))
;  (engine-start (getf (getf (engines object) :engines) engine-sym) (wall-time)))

;(defgeneric engine-start (engine time))
;(defmethod engine-start ((engine engine-object) time)
;  (setf (slot-value engine 'active) t)
;  (setf (slot-value engine 'start-time) time))

;(defgeneric engine-stop (engine))
;(defmethod engine-stop ((engine engine-object))
;  (setf (slot-value engine 'active) nil))


; take 2 seconds to fully fire
;(defmethod engine-genmodel ((engine engine-object))
;  (let ((time (- (wall-time) (start-time engine))))
;    (setf (model engine)
;	(make-model-3pyramid (make-2d-array 4 3 
;					    `((0.0 0.5 0.0) (-2.0 -0.5 0.0) (2.0 -0.5 0.0) 
;					      ; z goes from 0 to 1 in 2 seconds
;					      (0.0 0.0 ,(converge 0 1 2 time))))
;			     :point-colors (make-2d-array 4 3 `(
;								(,(converge 16 64 2 time) ,(converge 0 132 2 time) ,(converge 32 164 2 time))
;								(,(converge 16 64 2 time) ,(converge 0 132 2 time) ,(converge 32 164 2 time))
;								(,(converge 16 64 2 time) ,(converge 0 132 2 time) ,(converge 32 164 2 time))
;								(,(converge 0 255 2 time) ,(converge 0 255 2 time) ,(converge 64 255 2 time))))))))


;(defclass powered-object (game-object)
;  ;; plist :: ( :objects (plist models) :active (list symbols))
;  ((engines :initarg :engines :accessor engines :initform '(:engines () :active ()))))
;;  ((engine :initarg :engine :accessor engine :initform nil)))
;
;
;   ;(attachments :initarg :attachments :accessor attachments :initform nil)))




; time is time elapsed in seconds (with decimal for sub seconds)
;(defmethod time-step ((engine engine) object time)
;  ; f = ma
;  (let ((accel (/ (force engine) (mass object)))) 
;  ; x = x +v*t + 1/2 * a * t^2
;  (dotimes (i 3) (progn
;		   (incf (aref (coords motion) i) 
;			 (+ (* (aref (velocity motion) i) time) (* .5 (aref (acceleration motion) i) (expt time 2))))
;		   (incf (aref (velocity motion) i)
;			 (* time (aref (acceleration motion) i))))))



;(defmethod time-step ((object powered-object) time)  
;  (loop for engine in (loop for engine-sym in (getf (engines object) :active) collecting (getf (engines engines) engine-sym)) do
;       (time-step engine object time)))	 
; ; (motion-step (motion *self*) time))





;(defclass engine () 
;  (

;  (make-instance 'model
;		 :vertices (make-2d-array 4 3 '((0 0 0) (0 1 3) (-2 0 3) (2 0 3)))
;		 :faces (make-2d-array 4 3 '((0 1 3) (0 2 1) (0 3 2) (1 2 3)))
;		 :colors (make-2d-array 2 3 '((196 196 196) (32 32 32)))
;		 :face-colors (make-2d-array 4 3 '((0 0 0) (0 0 0) (0 0 0) (1 1 1)))))

 
(defparameter *world* nil)

(defparameter *self* nil)

(defparameter *velocity* 2) ; 1 unit / second
(defparameter *acceleration* 2) ; 1 unit /second
(defparameter *controls-active* '())



(defparameter *start-time* (wall-time))

(defparameter *last-time* nil)
(defparameter *num-frames* 0)




;(defmethod object-draw ((object powered-object))
;  (draw-entity object)
;  (if (eql (active (engine object)) t)
;      (progn
;	(setf (model (engine object)) (engine-genmodel (engine object)))
;	(gl:translate 0 0 0)
;	(object-draw (engine object)))))
 

(defun draw-world ()
  ;; clear the buffer
  (gl:clear :color-buffer-bit :depth-buffer-bit)      
  ;; move to eye position
  ;;(object-draw (make-instance 'powered-object :motion (make-instance 'motion :coords (vector 0 0 -3)) :model *ship-model* :engine (engine *self*)))
  (format t "draw self~%")
  (draw *self*)
  (format t "move to coords~%")
  (gl:translate  (- (aref (coords (body *self*)) 0)) (- (aref (coords (body *self*)) 1)) (- (aref (coords (body *self*)) 2))) ;; eye    
  (format t "draw all else~%")
  (loop for entity across *world* do
       ; only draw if its infront of me
       (if (< (aref (coords (body entity)) 2) (+ 10  (aref (coords (body *self*)) 2)))
	   (draw entity)))
  (format t "look at~%")
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  
  (glu:look-at 0 6 10 ;; pos
		0 0 0 ;; center
		0 1 0 ;; up in y pos
		)
      
    ;; finish the frame
   (gl:flush)
   (sdl:update-display))

;(defun phys-step (time)
;  (time-step *self* time))


(defun thruster-on (key)
  (case key 
  ;  ((:sdl-key-w) ; + z
   ;  (progn
       ;(setf (aref (acceleration (motion *self*)) 2) (- *acceleration*))
       ;(engine-start (engine *self*) (wall-time))))
 ;      (activate-engine *self* :thrust))) 
     
    ((:sdl-key-s) ; - z
     (setf (aref (acceleration (motion *self*)) 2) *acceleration*))
    ((:sdl-key-q) ; + x
     (setf (aref (acceleration (motion *self*)) 0) *acceleration*))
    ((:sdl-key-a) ; - x
     (setf (aref (acceleration (motion *self*)) 0) (- *acceleration*)))
    ((:sdl-key-e) ; + y
     (setf (aref (acceleration (motion *self*)) 1) *acceleration*))
    ((:sdl-key-d) ; - y
     (setf (aref (acceleration (motion *self*)) 1) (- *acceleration*)))
    (otherwise (format t "~a~%" key))))

(defun thruster-off (key)
  (case key 
    ;((:sdl-key-w) ; + z
    ; (progn
    ;   (setf (aref (acceleration (motion *self*)) 2) 0)
    ;   (engine-stop (engine *self*))))
     
    ((:sdl-key-s) ; - z
     (setf (aref (acceleration (motion *self*)) 2) 0))
    ((:sdl-key-q) ; + q
     (setf (aref (acceleration (motion *self*)) 0) 0))
    ((:sdl-key-a) ; - a
     (setf (aref (acceleration (motion *self*)) 0) 0))
    ((:sdl-key-e) ; + e
     (setf (aref (acceleration (motion *self*)) 1) 0))
    ((:sdl-key-d) ; - d
     (setf (aref (acceleration (motion *self*)) 1) 0))
    (otherwise (format t "~a~%" key))))


(defun sim-step ()
  "draw a frame"
  (let* ((start-time (wall-time))
	 (time (- start-time *last-time*)))
	

     ; (phys-step time)
      (draw-world)
      

      (incf *num-frames*)
      (if (not (eql (floor *last-time*) (floor time)))
	  (let* ((short-interval time)
		 (long-interval (- start-time *start-time*) )
		 (short-fps (floor (if (zerop short-interval) 0 (/ 1 short-interval))))
		 (long-fps (floor (if (zerop long-interval) 0  (/ *num-frames* long-interval)))))
	    
	    (format t "FPS since last:~a since start:~a (~a frames in ~a seconds)~%" short-fps long-fps *num-frames* long-interval)))
  
      (setf *last-time* start-time)))


(defun reshape () 
  (gl:shade-model :smooth)
  (gl:clear-color 0 0 0 0)
  (gl:clear-depth 1)
  (gl:enable :depth-test)
  (gl:depth-func :lequal)
  (gl:enable :cull-face)
  (gl:hint :perspective-correction-hint :nicest)

  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:perspective 50; 45 ;; FOV
		   1.0 ;; aspect ratio(/ width (max height 1))
		   1/10 ;; z near
		   1000 ;; z far
		   )

  (gl:matrix-mode :modelview)
)

(defun populate-world ()
  (setf *world* 
	(make-array 101 :initial-contents
		    (loop for i from 0 to 100 collecting
			 (let ((e (make-instance 'game-object 
					:model (make-instance 'model
							      :vertices (vertices *diamond-model*)
							      :faces (faces *diamond-model*))
					
					:body (make-instance 'body
							     :coords (vector (- (random 75) 37) (- (random 75) 37) (- (random 200) ))
							     :angles (vector (random 360) (random 360) (random 360))))))
			   (setf (colors (model e)) (make-2d-array 3 3 `((,(random 255) ,(random 255) ,(random 255)) (,(random 255) ,(random 255) ,(random 255)) (,(random 255) ,(random 255) ,(random 255)))))
			   (setf (face-colors (model e)) (make-2d-array 8 3 '((0 1 1) (0 1 1) (0 1 1) (0 1 1) (1 2 1) (1 2 1) (1 2 1) (1 2 1))))
			   e)))))

(defun init () 
  (setf *start-time* (wall-time))
  (setf *num-frames* 0)
  (setf *last-time* *start-time*)
  (setf *controls-active* '())
  (setf *self* (make-instance 'game-object 
			      :body (make-instance 'body :coords (vector 0 0 11))
			      :model *ship-model*))
			      ;:engines (list :engines (list :thrust 
				;		       (make-instance 'engine-object 
				;				      :motion (make-instance 'motion :coords (vector 0 0.5 3.0))
				;				      :forces (list (make-instance 'force :newtons 10 :direction '(0 0 1))))))))
					     
  (populate-world)
)

(defun main-loop () 
  (init)
  (sdl:with-init ()
    (sdl:window 640 480 :flags sdl:sdl-opengl)
    ;; cl-opengl needs platform specific support to be able to load GL
    ;; extensions, so we need to tell it how to do so in lispbuilder-sdl
    (reshape)
    (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address)
    (sdl:with-events () 
      (:quit-event () t)
      (:key-down-event (:key key) (thruster-on key)) ;(push key  *controls-active*))
      (:key-up-event (:key key) (thruster-off key)) ;(setf *controls-active* (remove key *controls-active*)))
		       
      (:idle ()
	     ;; this lets slime keep working while the main loop is running
             ;; in sbcl using the :fd-handler swank:*communication-style*
             ;; (something similar might help in some other lisps, not sure which though)
	     #+(and sbcl (not sb-thread)) (restartable
                                           (sb-sys:serve-all-events 0))
             (restartable (sim-step))))))
	     ;(draw)))))