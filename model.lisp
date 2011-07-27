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
