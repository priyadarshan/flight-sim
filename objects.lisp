(in-package #:flight-sim)



(defclass game-object ()
  ((model :initarg :model :accessor model :initform (make-instance 'model))
   (body :initarg :body :accessor body :initform (make-instance 'body))
   (attachments :initarg :attachments :accessor attachments :initform '())
   (active-attachments :initarg :active-attachments :accessor active-attachments :initform '())))


;(defmethod coords ((object game-object))
;  (coords (body object)))

(defgeneric activate (object start-time))
(defgeneric activate-attachment (object sym start-time))

(defgeneric deactivate (object))
(defgeneric deactivate-attachment (object sym))

(defmethod activate-attachment ((object game-object) sym start-time)
  (push sym (active-attachments object))
  (activate (getf (attachments object) sym) start-time))

(defmethod deactivate-attachment ((object game-object) sym)
  (setf (active-attachments object) (remove sym (active-attachments object))))

(defmethod draw :before ((object game-object) time)
  (gl:push-matrix)
  (gl:translate (aref (coords (body object)) 0) (aref (coords (body object)) 1) (aref (coords (body object)) 2))
  (gl:rotate (aref (angles (body object)) 0) 1 0 0)
  (gl:rotate (aref (angles (body object)) 1) 0 1 0)
  (gl:rotate (aref (angles (body object)) 2) 0 0 1))

(defmethod draw :after ((object game-object) time)
  (gl:pop-matrix))

(defmethod draw ((object game-object) time)
  (draw (model object) time)
  (loop for a in (active-attachments object) do
       (draw (getf (attachments object) a) time)))

;; get the vector of accel src object exerts on target object
(defgeneric get-accel (src target))

(defgeneric apply-accel (object accel time))

; time is time elapsed in seconds (with decimal for sub seconds)
(defmethod apply-accel ((object game-object) accel time)
  ; x = x +v*t + 1/2 * a * t^2
  (dotimes (i 3) (progn
                  (incf (aref (coords (body object)) i) 
                        (+ (* (aref (velocity (body object)) i) time) (* .5 (aref accel i) (expt time 2))))
                  (incf (aref (velocity (body object)) i)
                        (* time (aref accel i))))))

