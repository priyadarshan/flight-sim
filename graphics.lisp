(in-package #:flight-sim)

(defun draw-triangle (tri colors)
  (declare (type shape-vector tri))
  (declare (type shape-vector colors))
  (gl:with-primitive :triangles
    (let ((c (aref colors 0)))
      (gl:color (aref c 0) (aref c 1) (aref c 2)))
    (let ((v (aref tri 0)))
      (gl:vertex (aref v 0) (aref v 1) (aref v 2)))
    
    (let ((c (aref colors 1)))
      (gl:color (aref c 0) (aref c 1) (aref c 2)))
    (let ((v (aref tri 1)))
      (gl:vertex (aref v 0) (aref v 1) (aref v 2)))
    
    (let ((c (aref colors 2)))
      (gl:color (aref c 0) (aref c 1) (aref c 2)))
    (let ((v (aref tri 2)))
      (gl:vertex (aref v 0) (aref v 1) (aref v 2)))))

;(defun draw-entity (entity)
  

(defgeneric draw (object))