(in-package #:flight-sim)

(defclass motion ()
  ((velocity :initarg :velocity :accessor velocity :initform (vector 0 0 0))
   (acceleration :initarg :acceleration :accessor acceleration :initform (vector 0 0 0))
   (jerk :initarg :jerk :accessor jerk :initform (vector 0 0 0))))


(defclass body ()
  ((motion :initarg :motion :accessor motion :initform (make-instance 'motion))
   (coords :initarg :coords :accessor coords :initform (vector 0 0 0))
   (mass :initarg :mass :accessor mass :initform 0.0)
   (angles :initarg :angles :accessor angles :initform (vector 0 0 0))))


(defclass force ()
  ((newtons :initarg :newtons :accessor newtons :initform 0)
   (direction :initarg :direction :accessor direction :initform (vector))))
