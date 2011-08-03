(in-package #:flight-sim)

(defclass engine-object (game-object)
 ((start-time :initarg :start-time :accessor start-time :initform 0)
  (activation-time :initarg :activation-time :accessor activation-time :initform 0)))


(defclass engine-model (model)
  ((template-vertices :initarg :template-vertices :accessor template-vertices :initform nil)
   (template-colors :initarg :template-colors :accessor template-colors :initform nil)))

(defun generate-step-2d-array (2darr time)
  (let ((len-arr (length 2darr))
	(len-row (length (aref 2darr 0))))
    (make-2d-array len-arr len-row
		   (loop for i from 0 to len-arr collecting 
			(loop for j from 0 to len-row collecting
			     (let ((item (aref (aref 2darr i) j)))
			       (if (listp item)
				   (converge (first item) (second item) (third item) time)
				   item)))))))

; take 2 seconds to fully fire
(defmethod regen-model ((model engine-model) time)
  (setf (vertices model) (generate-step-2d-array *thruster-vertices* time))
  (setf (colors model) (generate-step-2d-array *thruster-colors* time)))


(defparameter *thruster-vertices* 
  '((0.0 0.5 0.0) (-2.0 -0.5 0.0) (2.0 -0.5 0.0) 
    ; z goes from 0 to 1 in 2 seconds
    (0.0 0.0 (0 1 2))))

(defparameter *thruster-colors*
  '(((16 64 2) (0 132 2) (32 164 2))
    ((16 64 2) (0 132 2) (32 164 2))
    ((16 64 2) (0 132 2) (32 164 2))
    ((0 255 2) (0 255 2) (64 255 2))))


(defmethod draw ((model engine-model) time) 
  (if (and (generated-model model) (< (- time (start-time model)) (activation-time model)))
      (regen-model model time))
  (call-next-method))
	    
      
