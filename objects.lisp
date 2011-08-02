(in-package #:flight-sim)



(defclass game-object ()
  ((model :initarg :model :accessor model :initform (make-instance 'model))
   (body :initarg :body :accessor body :initform (make-instance 'body))
   (attachments :initarg :attachments :accessor attachments :initform '())
   (active-attachments :initarg :active-attachments :accessor active-attachments :initform '())))


;(defmethod coords ((object game-object))
;  (coords (body object)))
		   

(defmethod draw :before ((object game-object))
  (gl:push-matrix)
  (gl:translate (aref (coords (body object)) 0) (aref (coords (body object)) 1) (aref (coords (body object)) 2))
  (gl:rotate (aref (angles (body object)) 0) 1 0 0)
  (gl:rotate (aref (angles (body object)) 1) 0 1 0)
  (gl:rotate (aref (angles (body object)) 2) 0 0 1))

(defmethod draw :after ((object game-object))
  (gl:pop-matrix))

(defmethod draw ((object game-object))
  (draw (model object)))
