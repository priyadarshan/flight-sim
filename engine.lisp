(in-package #:flight-sim)


(defclass engine-object (game-object)
 ((start-time :initarg :start-time :accessor start-time :initform 0)
  ;; time till fully active
  (activation-time :initarg :activation-time :accessor activation-time :initform 0)
  (force :initarg :force :accessor force :initform (make-instance 'force))))


(defmethod activate ((object engine-object) start-time)
  (setf (start-time object) start-time))

;; Engine Vertices &
;; Engine colors
;; array of color transforms for engine vertices
;;   Each cell if an RGB array of transforms for the vertex
;;     Each subcell is (Start-color Final-color Transform-time)
;; 4 vertices
;; each of 3 coords/colors
;; each either:
;;   Value
;;   (start end time)

(defclass engine-model (model)
  ((template-vertices :initarg :template-vertices :accessor template-vertices :initform nil)
   (template-colors :initarg :template-colors :accessor template-colors :initform nil)))


(defun generate-step-2d-array (2darr time)
  (let ((len-arr (length 2darr))
	(len-row (length (first 2darr))))
    (make-2d-array len-arr len-row
		   (loop for row in 2darr collecting 
			(loop for item in row collecting
			     ;(let ((item (aref (aref 2darr i) j)))
			     (if (listp item)
				 (converge (first item) (second item) (third item) time)
				 item))))))

; take 2 seconds to fully fire
(defmethod regen-model ((model engine-model) time)
  (setf (vertices model) (generate-step-2d-array (template-vertices model) time))
  (setf (colors model) (generate-step-2d-array (template-colors model) time)))

(defun make-thruster-vertices (start-model final-model duration)
  (loop for i from 0 to (1- (length start-model)) collect
       (let ((start (elt start-model i))
	     (final (elt final-model i)))
	 (loop for x from 0 to 2 collect
	      (if (eql (elt start x) (elt final x))
		  (elt start x)
		  (list (elt start x) (elt final x) duration))))))


(defun make-thruster-colors (base-color-start base-color-final tip-color-start tip-color-final duration)
  (append (loop for i from 1 to 3 collect 
	     (loop for x from 0 to 2 collect
		  (list (elt base-color-start x) (elt base-color-final x) duration)))
	(list (loop for x from 0 to 2 collect
		   (if (eql (elt tip-color-start x) (elt tip-color-final x)) 
		       (elt tip-color-start x)
		       (list (elt tip-color-start x) (elt tip-color-final x) duration))))))
		    


(defmethod draw ((object engine-object) time) 
  (if (< (- time (start-time object)) (activation-time object)) ;; hack since times are in templates!!!
      (regen-model (model object) (- time (start-time object))))
  (call-next-method))
	    
      
(defmethod get-accel ((src engine-object) (target game-object))
  (let* ((force-used (scalar-proj (scale-vector-1 (direction (force src))) (scale-vector-1 (coords (body src)))))
	 (accel (/ (* (newtons (force src)) force-used) (mass (body target))))
	 (accel-vec (scale-vector (scale-vector-1 (direction (force src))) (- accel))))
    accel-vec))
    

(defparameter *rear-thruster-vertices* 
  (make-thruster-vertices                                                                                                                          
   (transform-points (translate-points *3pyramid-points* (vector 0 0 0.5)) (vector 4 1 1.01))
   ;'( (0.0 0.5 0.0) (-2.0 -0.5 0.0) (2.0 -0.5 0.0) (0.0 0.0 0.0))
   (transform-points (translate-points (rotate-points *3pyramid-points* (vector 0 0 0)) (vector 0 0 0.5)) (vector 4 1 1.5))
  ; '( (0.0 0.5 0.0) (-2.0 -0.5 0.0) (2.0 -0.5 0.0) (0.0 0.0 1.5))
   2))
;  '((0.0 0.5 0.0) (-2.0 -0.5 0.0) (2.0 -0.5 0.0) 
;    ; z goes from 0 to 1 in 2 seconds
;    (0.0 0.0 (0 1.5 2))))

(defparameter *rear-thruster-colors*
  (make-thruster-colors '(32 32 32) '(64 132 164) '(0 0 64) '(255 255 255) 2))
;  '(((32 64 2) (32 132 2) (32 164 2))  ;; vertex1 : Red (32 -> 62 in 2 sec) Green (32 -> 132 in 2 sec) Blue (32 -> 164 in 2 sec
;    ((32 64 2) (32 132 2) (32 164 2))
;    ((32 64 2) (32 132 2) (32 164 2))
;    ((0 255 2) (0 255 2) (64 255 2))))

(defparameter *left-jet-vertices*
  (make-thruster-vertices
   (rotate-points (transform-points *3pyramid-points* (vector 0.25 0.25 0.2)) (vector (- (+ (/ pi 2) .5)) 0 .5))
   (rotate-points (transform-points *3pyramid-points* (vector 0.25 0.25 0.4)) (vector (- (+ (/ pi 2) .5)) 0 .5))
   2))

(defparameter *left-jet-colors*
  (make-thruster-colors '(40 40 40) '(255 255 0) '(80 80 80) '(255 255 255) 2))
;  (make-thruster-colors '(196 196 196) '(255 255 196) '(196 196 196) '(255 255 255) 2))

