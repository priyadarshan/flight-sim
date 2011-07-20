;;;; flight-sim.lisp

(in-package #:flight-sim)

;;; "flight-sim" goes here. Hacks and glory await!

(defmacro restartable (&body body)
  "Helper macro since we use continue restarts a lot 
   (remember to hit C in slime or pick the restart so errors don't kill the app"
  `(restart-case
       (progn ,@body)
     (continue () :report "Continue")))

;;; degrees to radians
(defmacro dtr (d)
  `(/ (* ,d pi) 180))

;;; radians to degress
(defmacro rtd (r)
  `(/ (* ,r 180) pi))

(defun make-2d-array (h w contents)
  (let ((arr (make-array h)))
    (do ((i 0 (incf i))
	 (rest-list contents (rest rest-list)))
	((eql i h)) 
      (setf (aref arr i) (make-array w :initial-contents (car rest-list))))
    arr))

(defclass accelerator () nil)

(defclass circle-accel (accelerator)
  ((origin :initarg :origin :accessor origin :initform (vector 0 0 0))))

(defgeneric accel (accelerator motion time)
  (:documentation "apply some modifications to a motion class based on some rules"))

(defmethod accel ((accelerator accelerator) entity time)
  nil)

(defmethod accel ((accelerator circle-accel) entity time)
  (format t "(circle-accel::accel)~%")
  (let* ((o-vector (vector 
		    (- (aref (origin accelerator) 0) (aref (coords entity) 0))
		    (- (aref (origin accelerator) 1) (aref (coords entity) 1))
		    (- (aref (origin accelerator) 2) (aref (coords entity) 2))))
	 (t-vector (rotate* (make-rotation-matrix (/ pi 2) (/ pi 2) (/ pi 2)) o-vector)))
    (setf (velocities (motion entity)) t-vector)))
;    (setf (aref (velocities (motion entity) 0) (* time (aref t-vector 0)))
;    (setf (aref (velocities (motion entity)) 0) (* time (aref t-vector 0)))
;    (setf (aref (velocities (motion entity)) 0) (* time (aref t-vector 0)))))
	 

(defclass model ()
  ((vertices :initarg :vertices :accessor vertices :initform (vector))
   (faces :initarg :faces :accessor faces :initform (vector))))

(defclass motion ()
  ((coords :initarg :coords :accessor coords :initform (vector 0 0 0))
   (velocity :initarg :velocity :accessor velocity :initform (vector 0 0 0))
   (acceleration :initarg :acceleration :accessor acceleration :initform (vector 0 0 0))
   (jerk :initarg :jerk :accessor jerk :initform (vector 0 0 0))))
   
;; time is time elapsed in seconds (with decimal for sub seconds)
(defmethod motion-step ((motion motion) time)
  ; x = x +v*t + 1/2 * a * t^2
  (dotimes (i 3) (progn
		   (incf (aref (coords motion) i) 
			 (+ (* (aref (velocity motion) i) time) (* .5 (aref (acceleration motion) i) (expt time 2))))
		   (incf (aref (velocity motion) i)
			 (* time (aref (acceleration motion) i))))))

(defclass game-object ()
  ((model :initarg :model :accessor model :initform (make-instance 'model))
   (motion :initarg :motion :accessor motion :initform (make-instance 'motion))
   (angles :initarg :angles :accessor angles :initform (vector 0 0 0))))


(defparameter *diamond-model* 
  (make-instance 'model
		 :vertices (make-2d-array 6 3 '((0.0 1 0) (0.5 0 0.5) (0.5 0 -0.5) 
						(-0.5 0 0.5) (-0.5 0 -0.5) (0.0 -1 0)))
		 :faces (make-2d-array 8 3 '((0 3 1) (0 2 4) (0 1 2) (0 4 3)
					     (3 5 1) (2 5 4) (1 5 2) (4 5 3)))))



(defparameter *world* nil)

(defparameter *origin* (vector 0 0 -7))
(defparameter *self* nil) ; (make-instance 'motion :coords (vector 0 0 -11)))
(defparameter *orientation* (vector 0 1 0))

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

(defun draw-triangle (tri time) 
  (let ((time (- (wall-time) *start-time*)))
  (gl:with-primitive :triangles
    (multiple-value-bind (red green blue) (shift-color time)
      (gl:color red green blue))
    (let ((v (aref tri 0)))
      (gl:vertex (aref v 0) (aref v 1) (aref v 2)))
    
    (multiple-value-bind (green blue red) (shift-color time)
      (gl:color red green blue))
    (let ((v (aref tri 1)))
      (gl:vertex (aref v 0) (aref v 1) (aref v 2)))
    
    (multiple-value-bind (blue green red) (shift-color time)
      (gl:color red green blue))
    (let ((v (aref tri 2)))
      (gl:vertex (aref v 0) (aref v 1) (aref v 2))))))

(defun draw (time)
  ;; clear the buffer
  (gl:clear :color-buffer-bit :depth-buffer-bit)      
  ;; move to eye position
  (gl:translate (aref (coords *self*) 0) (aref (coords *self*) 1) (aref (coords *self*) 2)) ;; eye
  (loop for entity across *world* do
       ;(let ((entity (aref *world* i)))
       (progn
	 (gl:push-matrix)
	 (gl:translate (aref (coords (motion entity)) 0) (aref (coords (motion entity)) 1) (aref (coords (motion entity)) 2))
   (gl:rotate (aref (angles entity) 0) 1 0 0)
   (gl:rotate (aref (angles entity) 1) 0 1 0)
   (gl:rotate (aref (angles entity) 2) 0 0 1)
   (loop for face across (faces (model entity)) do
	(draw-triangle (get-vertecies face (vertices (model entity))) time))
   (gl:pop-matrix)
      ))
      
   (gl:matrix-mode :modelview)
   (gl:load-identity)
;      (gl:translate 0 -2 -7)
 ;     (gl:rotate 16 1  0 0)
   (glu:look-at 0 0 1 ;(aref *origin* 0) (aref *origin* 1) (aref *origin* 2) ;; eye
		0 0 0 ;; center
		0 1 0 ;; up in y pos
		)
	   
      
    ;; finish the frame
   (gl:flush)
   (sdl:update-display))

(defun phys-step (time)
  (motion-step *self* time)
  (format t "z-position: ~a z-velocity: ~a z-acceleration: ~a~%" (aref (coords *self*) 2) (aref (velocity *self*) 2) (aref (acceleration *self*) 2)))
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
     (setf (aref (acceleration *self*) 2) *acceleration*))
    ((:sdl-key-s) ; - z
     (setf (aref (acceleration *self*) 2) (- *acceleration*)))
    ((:sdl-key-q) ; + x
     (setf (aref (acceleration *self*) 0) *acceleration*))
    ((:sdl-key-a) ; - x
     (setf (aref (acceleration *self*) 0) (- *acceleration*)))
    ((:sdl-key-d) ; + y
     (setf (aref (acceleration *self*) 1) *acceleration*))
    ((:sdl-key-e) ; - y
     (setf (aref (acceleration *self*) 1) (- *acceleration*)))
    (otherwise (format t "~a~%" key))))

(defun thruster-off (key)
  (case key 
    ((:sdl-key-w) ; + z
     (setf (aref (acceleration *self*) 2) 0))
    ((:sdl-key-s) ; - z
     (setf (aref (acceleration *self*) 2) 0))
    ((:sdl-key-q) ; + q
     (setf (aref (acceleration *self*) 0) 0))
    ((:sdl-key-a) ; - a
     (setf (aref (acceleration *self*) 0) 0))
    ((:sdl-key-e) ; + e
     (setf (aref (acceleration *self*) 1) 0))
    ((:sdl-key-d) ; - d
     (setf (aref (acceleration *self*) 1) 0))
    (otherwise (format t "~a~%" key))))


(defun sim-step ()
  "draw a frame"
  (let* ((start-time (wall-time))
	 (time (- start-time *last-time*)))
	

      (phys-step time)
      (draw time)
      

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
	(make-array 10 :initial-contents
		    (loop for i from 0 to 9 collecting
			 (make-instance 'game-object 
					:model *diamond-model*
					:angles (vector (random 360) (random 360) (random 360))
					:motion (make-instance 'motion
							       :coords (vector (- (random 10) 5) (- (random 10) 5) (- (random 10) 5))))))))
							     
							     
									
		     
			    

(defun init () 
  (setf *start-time* (wall-time))
  (setf *num-frames* 0)
  (setf *last-time* *start-time*)
  (setf *controls-active* '())
  (setf *self* (make-instance 'motion :coords (vector 0 0 -11)))
;  (reshape)
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