(in-package #:flight-sim)

(defclass engine-object (game-object)
 ((start-time :initarg :start-time :accessor start-time :initform 0)
  ;; time till fully active
  (activation-time :initarg :activation-time :accessor activation-time :initform 0)
  (force :initarg :force :accessor force :initform (make-instance 'force))))


(defmethod activate ((object engine-object) start-time)
  (setf (start-time object) start-time))

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


(defparameter *thruster-vertices* 
  '((0.0 0.5 0.0) (-2.0 -0.5 0.0) (2.0 -0.5 0.0) 
    ; z goes from 0 to 1 in 2 seconds
    (0.0 0.0 (0 1 2))))

(defparameter *thruster-colors*
  '(((32 64 2) (32 132 2) (32 164 2))
    ((32 64 2) (32 132 2) (32 164 2))
    ((32 64 2) (32 132 2) (32 164 2))
    ((0 255 2) (0 255 2) (64 255 2))))


(defmethod draw ((object engine-object) time) 
  (if (< (- time (start-time object)) (activation-time object)) ;; hack since times are in templates!!!
      (regen-model (model object) (- time (start-time object))))
  (call-next-method))
	    
      
(defmethod phys-act ((src engine-object) (target game-object) time)
  (let* ((scalar-proj (scalar-proj (vector-scale-1 (direction (force (src)))) (vector-scale-1 (position src))))
	 (accel (/ (newtons (force src)) (mass (body target))))
	 (accel-vec (scale-vector scalar-proj accel)))
    


; time is time elapsed in seconds (with decimal for sub seconds)
;(defmethod time-step ((engine engine) object time)
;  ; f = ma
;  (let ((accel (/ (force engine) (mass object)))) 
;  ; x = x +v*t + 1/2 * a * t^2
;  (dotimes (i 3) (progn
;                  (incf (aref (coords motion) i) 
;                        (+ (* (aref (velocity motion) i) time) (* .5 (aref (acceleration motion) i) (expt time 2))))
;                  (incf (aref (velocity motion) i)
;                        (* time (aref (acceleration motion) i))))))

    
    

