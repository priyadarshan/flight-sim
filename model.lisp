(in-package #:flight-sim)

(defclass model ()
  ((vertices :initarg :vertices :accessor vertices :initform (vector) :type shape-vector)
   (faces :initarg :faces :accessor faces :initform (vector) :type shape-ref-vector )
   (colors :initarg :colors :reader colors :initform (vector) :type shape-vector)
   (face-colors :initarg :face-colors :accessor face-colors :initform (vector) :type shape-ref-vector)))

(defmethod draw ((model model) time)
  (loop for i from 0 to (1- (length (faces model))) do
       (draw-triangle (get-vertecies (aref (faces model) i) (vertices model))
		      (get-vertecies (aref (face-colors model) i) (colors model)))))

(defclass animated-model ()
  ((start-time :initarg :start-time :accessor start-time :initform 0.0)))

(defgeneric scale-colors (model))

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
; point up along +z
(defparameter *3pyramid-points*
  (make-2d-array 4 3 '((0.0 0.5 -0.5) (-0.5 -0.5 -0.5) (0.5 -0.5 -0.5) (0.0 0.0 0.5))))

; point up along +z, flat facing +y (into)
(defparameter *3pyramid-flat-points*
  (make-2d-array 4 3 '((0.0 -0.5 -0.5) (0.0 0.5 0.5) (-0.5 -0.5 0.5) (0.5 -0.5 0.5))))

(defparameter *colors* (make-hash-table :test 'equal))
(setf (gethash "red" *colors*) '(255 0 0))
(setf (gethash "darkred" *colors*) '(139 0 0))
;(setf (gethash "lightred" *colors*) '(255 0 0))

(setf (gethash "cyan" *colors*) '(0 255 255))

(setf (gethash "blue" *colors*) '(0 0 255))
(setf (gethash "darkblue" *colors*) '(0 0 139))
(setf (gethash "lightblue" *colors*) '(173 216 230))

(setf (gethash "pink" *colors*) '(255 20 147))

(setf (gethash "green" *colors*) '(0 128 0))
(setf (gethash "darkgreen" *colors*) '(0 100 0))
(setf (gethash "lightgreen" *colors*) '(144 238 144))
(setf (gethash "forestgreen" *colors*) '(34 140 34))

(setf (gethash "lime" *colors*) '(0 255 0))

(setf (gethash "orange" *colors*) '(255 165 0))
(setf (gethash "yellow" *colors*) '(255 255 0))
(setf (gethash "purple" *colors*) '(128 0 128))

(setf (gethash "black" *colors*) '(0 0 0))
(setf (gethash "white" *colors*) '(255 255 255))
(setf (gethash "grey" *colors*) '(128 128 128))


;; returns a model of a 3 pyramid from points and colors
(defun make-model-3pyramid (points &key (face-colors nil) (point-colors nil))
  (make-instance 'model
		 :vertices (if (listp points) (make-2d-array 4 3 points) points)
		 :faces (make-2d-array 4 3 '((0 1 3) (0 2 1) (0 3 2) (1 2 3)))
		 :colors (if face-colors 
			     (if (listp face-colors) (make-2d-array 4 3 face-colors) face-colors)
			     (if (listp point-colors) (make-2d-array 4 3 point-colors) point-colors))
		 :face-colors (if face-colors 
				  (make-2d-array 4 3 '((0 0 0) (1 1 1) (2 2 2) (3 3 3)))
				  (make-2d-array 4 3 '((0 1 3) (0 2 1) (0 3 2) (1 2 3))))))





(defparameter *ship-model*
  (make-model-3pyramid ;*3pyramid-flat-points*
   (transform-points
    (rotate-points  *3pyramid-flat-points* (make-rotation-matrix (vector 0 0 0)))
    (vector 4 1 3))
   :face-colors '((196 196 196) (196 196 196) (196 196 196) (32 32 32))))

;(defparameter *ship-model*
;  (make-model-3pyramid '((0.0 -0.5 -1.5) (0.0 0.5 1.5) (-2.0 -0.5 1.5) (2.0 -0.5 1.5))
;		       :face-colors '((196 196 196) (196 196 196) (196 196 196) (32 32 32))))
  
