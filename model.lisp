(in-package #:flight-sim)

(defclass model ()
  ((vertices :initarg :vertices :accessor vertices :initform (vector) :type shape-vector)
   (faces :initarg :faces :accessor faces :initform (vector) :type shape-ref-vector )
   (colors :initarg :colors :reader colors :initform (vector) :type shape-vector)
   (face-colors :initarg :face-colors :accessor face-colors :initform (vector) :type shape-ref-vector)))

(defmethod scale-colors ((model model))
  (let ((colors (colors model)))
    (loop for i from 0 to (1- (length colors)) do 
	 (loop for j from 0 to 2 do
	      (setf (aref (aref colors i) j) (float (/ (aref (aref colors i) j) 255)))))))

(defmethod initialize-instance :after ((model model) &key)
  (scale-colors model))

(defgeneric (setf colors) (colors model))

(defmethod (setf colors) (colors (model model))
  (setf (slot-value model 'colors) colors)
  (scale-colors model))


(defun get-vertecies (face vertices) 
  (make-array (length face) :initial-contents
	      (loop for i across face collecting (aref vertices i))))

(defun shift-color (time) 
  (values 
   ;;; red
   (/ (+ (* (sin (+ (* 0.3 time) 0)) 127) 128) 255)
   ;;; green
   (/ (+ (* (sin (+ (* 0.3 time) (* 2/3 PI))) 127 ) 128) 255)
   ;;; blue
   (/ (+ (* (sin (+ (* 0.3 time) (* 4/3 PI))) 127) 128) 255)))



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

