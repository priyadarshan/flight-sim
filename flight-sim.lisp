;;;; flight-sim.lisp

(in-package #:flight-sim)

;;; "flight-sim" goes here. Hacks and glory await!



   


(defclass game-object ()
  ((model :initarg :model :accessor model :initform (make-instance 'model))
   (bosy :initarg :body :accessor body :inotform (make-instance 'body))))


(defclass engine-object (game-object)
  ((active :initarg :active :reader active :initform nil)
   (start-time :initarg :start-time :reader start-time :initform nil)
   (forces :initarg :forces :accessor forces :initform '())))

(defgeneric activate-engine (object engine-sym))

(defmethod activate-engine ((object powered-object) engine-sym)
  (push :engine-sym (getf (engines object) :active))
  (engine-start (getf (getf (engines object) :engines) engine-sym) (wall-time)))

(defgeneric engine-start (engine time))
(defmethod engine-start ((engine engine-object) time)
  (setf (slot-value engine 'active) t)
  (setf (slot-value engine 'start-time) time))

(defgeneric engine-stop (engine))
(defmethod engine-stop ((engine engine-object))
  (setf (slot-value engine 'active) nil))

;; function to determine value lying on start to end taking time duration at now
(defun converge (start end duration now)
  (float (+ start (* (- end start) (if (eql now 0.0) 0.0 (/ (min now duration) duration))))))

; take 2 seconds to fully fire
(defmethod engine-genmodel ((engine engine-object))
  (let ((time (- (wall-time) (start-time engine))))
    (setf (model engine)
	(make-model-3pyramid (make-2d-array 4 3 
					    `((0.0 0.5 0.0) (-2.0 -0.5 0.0) (2.0 -0.5 0.0) 
					      ; z goes from 0 to 1 in 2 seconds
					      (0.0 0.0 ,(converge 0 1 2 time))))
			     :point-colors (make-2d-array 4 3 `(
								(,(converge 16 64 2 time) ,(converge 0 132 2 time) ,(converge 32 164 2 time))
								(,(converge 16 64 2 time) ,(converge 0 132 2 time) ,(converge 32 164 2 time))
								(,(converge 16 64 2 time) ,(converge 0 132 2 time) ,(converge 32 164 2 time))
								(,(converge 0 255 2 time) ,(converge 0 255 2 time) ,(converge 64 255 2 time))))))))


(defclass powered-object (game-object)
  ;; plist :: ( :objects (plist models) :active (list symbols))
  ((engines :initarg :engines :accessor engines :initform '(:engines () :active ()))))
;  ((engine :initarg :engine :accessor engine :initform nil)))


   ;(attachments :initarg :attachments :accessor attachments :initform nil)))

;; time is time elapsed in seconds (with decimal for sub seconds)
(defmethod time-step ((engine engine) object time)
  ; f = ma
  (let ((accel (/ (force engine) (mass object)))) 
  ; x = x +v*t + 1/2 * a * t^2
  (dotimes (i 3) (progn
		   (incf (aref (coords motion) i) 
			 (+ (* (aref (velocity motion) i) time) (* .5 (aref (acceleration motion) i) (expt time 2))))
		   (incf (aref (velocity motion) i)
			 (* time (aref (acceleration motion) i))))))



(defmethod time-step ((object powered-object) time)  
  (loop for engine in (loop for engine-sym in (getf (engines object) :active) collecting (getf (engines engines) engine-sym)) do
       (time-step engine object time)))	 
 ; (motion-step (motion *self*) time))




(defparameter *diamond-model* 
  (make-instance 'model
		 :vertices (make-2d-array 6 3 '((0.0 1.0 0.0) (0.5 0.0 0.5) (0.5 0.0 -0.5) 
						(-0.5 0.0 0.5) (-0.5 0.0 -0.5) (0.0 -1.0 0.0)))
		 :faces (make-2d-array 8 3 '((0 3 1) (0 2 4) (0 1 2) (0 4 3)
					     (3 5 1) (2 5 4) (1 5 2) (4 5 3)))))

(defun make-model-3pyramid (points &key (face-colors nil) (point-colors nil))
  (make-instance 'model
		 :vertices points
		 :faces (make-2d-array 4 3 '((0 1 3) (0 2 1) (0 3 2) (1 2 3)))
		 :colors (if face-colors face-colors point-colors)
		 :face-colors (if face-colors 
				  (make-2d-array 4 3 '((0 0 0) (1 1 1) (2 2 2) (3 3 3)))
				  (make-2d-array 4 3 '((0 1 3) (0 2 1) (0 3 2) (1 2 3))))))
	 
(defparameter *ship-model*
  (make-model-3pyramid (make-2d-array 4 3 '((0.0 0.0 0.0) (0.0 1.0 3.0) (-2.0 0.0 3.0) (2.0 0.0 3.0)))
		       :face-colors (make-2d-array 4 3 '((196 196 196) (196 196 196) (196 196 196) (32 32 32)))))


;(defclass engine () 
;  (

;  (make-instance 'model
;		 :vertices (make-2d-array 4 3 '((0 0 0) (0 1 3) (-2 0 3) (2 0 3)))
;		 :faces (make-2d-array 4 3 '((0 1 3) (0 2 1) (0 3 2) (1 2 3)))
;		 :colors (make-2d-array 2 3 '((196 196 196) (32 32 32)))
;		 :face-colors (make-2d-array 4 3 '((0 0 0) (0 0 0) (0 0 0) (1 1 1)))))


(defparameter *world* nil)

;(defparameter *origin* (vector 0 0 -7))
(defparameter *self* nil) ; (make-instance 'motion :coords (vector 0 0 -11)))
;(defparameter *orientation* (vector 0 1 0))

(defparameter *velocity* 2) ; 1 unit / second
(defparameter *acceleration* 2) ; 1 unit /second
(defparameter *controls-active* '())

(let ((time-units (/ 1.0 internal-time-units-per-second)))
  (defun wall-time (&key (offset 0))
    (+ (* (get-internal-real-time) time-units)
       offset)))


(defparameter *start-time* (wall-time))

(defparameter *last-time* nil)
(defparameter *num-frames* 0)

;;(defparameter *t1* '( (-0.5 -0.5 0) (0 0.5 0) (0.5 -0.5 0)))

(defun get-vertecies (faces vertices) 
  (make-array (length faces) :initial-contents
	      (loop for i across faces collecting (aref vertices i))))



(defun shift-color (time) 
  (values 
   ;;; red
   (/ (+ (* (sin (+ (* 0.3 time) 0)) 127) 128) 255)
   ;;; green
   (/ (+ (* (sin (+ (* 0.3 time) (* 2/3 PI))) 127 ) 128) 255)
   ;;; blue
   (/ (+ (* (sin (+ (* 0.3 time) (* 4/3 PI))) 127) 128) 255)))


;; returns a real lisp 2d array
(defun make-rotation-matrix (xa ya za) 
  (let ((sxa (sin xa))
	(cxa (cos xa))
	(sya (sin ya))
	(cya (cos ya))
	(sza (sin za))
	(cza (cos za)))
    (make-array '(3 3) :initial-contents (list (list (* cya cza) (+ (- (* cxa sza)) (* sxa sya cza)) (+ (* sxa sza) (* cxa sya cza)))
					   (list (* cya sza) (+ (* cxa cza) (* sxa sya sza)) (+ (- (* sxa cza)) (* cxa sya sza)))
					   (list (- sya) (* sxa cya) (* cxa cya))))))
					   
(defun rotate* (m v)
  (let ((result (make-array 3 :initial-element 0)))
    (dotimes (x 3)
      (dotimes (y 3)
	(incf (aref result x) (* (aref v y) (aref m x y)))))
    result))
	
(defun translate-point (v1 v2 &optional (fn #'+)) 
  (let ((result (make-array 3)))
    (dotimes (i 3)
      (setf (aref result i) (funcall fn (aref v1 i) (aref v2 i))))
    result))
  

(defun translate-triangle (tri position)
  (make-array (length tri) :initial-contents
	      (loop for v across tri collecting (translate-point position v))))

;(defun rotate-vertex-2d (v rM)
;  v)
 ;; (let ((result (lm:* rM (lm:vector (first v) (second v)))))
 ;;   (list (lm:elt result 0) (lm:elt result 1))))
 
;; (let* ((x (first v))
;;	 (y (second v))
;;	 (theta (atan (if (eql 0 x) 1000000 (/ y x))))
;;	 (hyp (sqrt (+ (* x x) (* y y)))))
 ;;   (list (/ (cos (+ theta time)) hyp) (/ (sin (+ theta time)) hyp) (third v))))
;    (list (+ (first v) (/ (sin time) 2)) (+ (second v) (/ (cos time) 2))   (third v)))

(defun rotate-triangle (tri m)
  (make-array (length tri) :initial-contents
	      (loop for v across tri collecting (rotate* m v))))

;  (let* ((angle (/ time 1000))
;	 (cos-a (cos angle))
;	 (sin-a (sin angle))
;	 (rM nil)) ;lm:make-matrix 2 2 :initial-elements 
;		;	     '(cos-a sin-a
;		;	       (- sin-a) cos-a))))
 ;   (list (append (rotate-vertex-2d (first tri) rM) '((third (firt tri))))
;	  (append (rotate-vertex-2d (second tri) rM) '((third (second tri))))
;	  (append (rotate-vertex-2d (third tri) rM) (third (third tri))))))
;
;(defun scale-colors (c) 
;  (make-array 3 :initial-contents (loop for ci across c collecting (/ ci 255))))
  

(defun draw-triangle (tri colors)
  (declare (type shape-vector tri))
  (declare (type shape-vector colors))
  (gl:with-primitive :triangles
    (let ((c (aref colors 0)))
      (gl:color (aref c 0) (aref c 1) (aref c 2)))
    (let ((v (aref tri 0)))
      (gl:vertex (aref v 0) (aref v 1) (aref v 2)))
    
    (let ((c (aref colors 1)))
      (gl:color (aref c 0) (aref c 1) (aref c 2)))
    (let ((v (aref tri 1)))
      (gl:vertex (aref v 0) (aref v 1) (aref v 2)))
    
    (let ((c (aref colors 2)))
      (gl:color (aref c 0) (aref c 1) (aref c 2)))
    (let ((v (aref tri 2)))
      (gl:vertex (aref v 0) (aref v 1) (aref v 2)))))

(defun draw-entity (entity)
  (gl:translate (aref (coords (motion entity)) 0) (aref (coords (motion entity)) 1) (aref (coords (motion entity)) 2))
  (gl:rotate (aref (angles entity) 0) 1 0 0)
  (gl:rotate (aref (angles entity) 1) 0 1 0)
  (gl:rotate (aref (angles entity) 2) 0 0 1)
  (loop for i from 0 to (1- (length (faces (model entity)))) do
       (draw-triangle (get-vertecies (aref (faces (model entity)) i) (vertices (model entity)))
		      (get-vertecies (aref (face-colors (model entity)) i) (colors (model entity))))))

(defgeneric object-draw (object))

(defmethod object-draw :before ((object game-object))
  (gl:push-matrix))

(defmethod object-draw :after ((object game-object))
  (gl:pop-matrix))

(defmethod object-draw ((object game-object))
  (draw-entity object))

(defmethod object-draw ((object powered-object))
  (draw-entity object)
  (if (eql (active (engine object)) t)
      (progn
	(setf (model (engine object)) (engine-genmodel (engine object)))
	(gl:translate 0 0 0)
	(object-draw (engine object)))))
 

(defun draw ()
  ;; clear the buffer
  (gl:clear :color-buffer-bit :depth-buffer-bit)      
  ;; move to eye position
  (object-draw (make-instance 'powered-object :motion (make-instance 'motion :coords (vector 0 0 -3)) :model *ship-model* :engine (engine *self*)))

  (gl:translate  (- (aref (coords (motion *self*)) 0)) (- (aref (coords (motion *self*)) 1)) (- (aref (coords (motion *self*)) 2))) ;; eye    
  
  (loop for entity across *world* do
       ; only draw if its infront of me
       (if (< (aref (coords (motion entity)) 2) (+ 10  (aref (coords (motion *self*)) 2)))
	   (object-draw entity)))
       
  


  
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  
       
;      (gl:translate 0 -2 -7)
 ;     (gl:rotate 16 1  0 0)
   (glu:look-at 0 6 10 ;(aref *origin* 0) (aref *origin* 1) (aref *origin* 2) ;; eye
		0 0 0 ;; center
		0 1 0 ;; up in y pos
		)
	   
      
    ;; finish the frame
   (gl:flush)
   (sdl:update-display))

(defun phys-step (time)
  (time-step *self* time))

;  (format t "z-position: ~a z-velocity: ~a z-acceleration: ~a~%" (aref (coords *self*) 2) (aref (velocity *self*) 2) (aref (acceleration *self*) 2))
;  (format t "y-position: ~a y-velocity: ~a y-acceleration: ~a~%" (aref (coords *self*) 1) (aref (velocity *self*) 1) (aref (acceleration *self*) 1))
;  (format t "x-position: ~a x-velocity: ~a x-acceleration: ~a~%" (aref (coords *self*) 0) (aref (velocity *self*) 0) (aref (acceleration *self*) 0)))
  ;(loop for entity across *world* do
  ;     (motion-step (motion entity) time)))
;       (accel (accelerator (motion entity)) entity time)
;       (let ((velocities (velocities (motion entity)))
;	     (coords (coords (motion entity))))
;	 (incf (aref coords 0) (* time (aref velocities 0)))
;	 (incf (aref coords 1) (* time (aref velocities 1)))
;	 (incf (aref coords 2) (* time (aref velocities 2))))))

(defun thruster-on (key)
  (case key 
    ((:sdl-key-w) ; + z
     (progn
       ;(setf (aref (acceleration (motion *self*)) 2) (- *acceleration*))
       ;(engine-start (engine *self*) (wall-time))))
       (activate-engine *self* :thrust))) 
     
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
    ((:sdl-key-w) ; + z
     (progn
       (setf (aref (acceleration (motion *self*)) 2) 0)
       (engine-stop (engine *self*))))
     
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
	

      (phys-step time)
      (draw)
      

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
 ; (gl:load-identity)
  ;(glu:look-at 0 2 7 ;; eye
;	       0 0 0 ;; center
;	       0 1 0 ;; up in y pos
;	       )
  
)

(defun populate-world ()
  (setf *world* 
	(make-array 101 :initial-contents
		    (loop for i from 0 to 100 collecting
			 (let ((e (make-instance 'game-object 
					:model (make-instance 'model
							      :vertices (vertices *diamond-model*)
							      :faces (faces *diamond-model*))
					:angles (vector (random 360) (random 360) (random 360))
					:motion (make-instance 'motion
							       :coords (vector (- (random 75) 37) (- (random 75) 37) (- (random 200) ))))))
			   (setf (colors (model e)) (make-2d-array 3 3 `((,(random 255) ,(random 255) ,(random 255)) (,(random 255) ,(random 255) ,(random 255)) (,(random 255) ,(random 255) ,(random 255)))))
			   (setf (face-colors (model e)) (make-2d-array 8 3 '((0 1 1) (0 1 1) (0 1 1) (0 1 1) (1 2 1) (1 2 1) (1 2 1) (1 2 1))))
			   e)))))
							     
							     
									
		     
			    

(defun init () 
  (setf *start-time* (wall-time))
  (setf *num-frames* 0)
  (setf *last-time* *start-time*)
  (setf *controls-active* '())
  (setf *self* (make-instance 'powered-object 
			      :motion (make-instance 'motion :coords (vector 0 0 11))
			      :model *ship-model*
			      :engines (list :engines (list :thrust 
						       (make-instance 'engine-object 
								      :motion (make-instance 'motion :coords (vector 0 0.5 3.0))
								      :forces (list (make-instance 'force :newtons 10 :direction '(0 0 1))))))))
					     
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