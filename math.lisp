(in-package #:flight-sim)

;;; degrees to radians
(defmacro dtr (d)
  `(/ (* ,d pi) 180))

;;; radians to degress
(defmacro rtd (r)
  `(/ (* ,r 180) pi))

(deftype point-vector () '(simple-array float (*)))
(deftype shape-vector () '(simple-array point-vector (*)))

(deftype pos-int () '(integer 0 *))
(deftype ref-vector () '(simple-array pos-int (*)))
(deftype shape-ref-vector () '(simple-array ref-vector (*)))

;; function to determine value lying on start to end taking time duration at now
(defun converge (start end duration now)
  (if (> now duration) 
      end
      (float (+ start (* (- end start) (if (eql now 0.0) 0.0 (/ (min now duration) duration)))))))


;; returns a real lisp 2d array: args in radians
(defun make-rotation-matrix (xyz) 
  (let ((sxa (sin (aref xyz 0))) ;x
	(cxa (cos (aref xyz 0))) ;x
	(sya (sin (aref xyz 1))) ;y 
	(cya (cos (aref xyz 1))) ;y
	(sza (sin (aref xyz 2))) ;z
	(cza (cos (aref xyz 2)))) ;z
    (make-array '(3 3) :initial-contents (list (list (* cya cza) (+ (- (* cxa sza)) (* sxa sya cza)) (+ (* sxa sza) (* cxa sya cza)))
					   (list (* cya sza) (+ (* cxa cza) (* sxa sya sza)) (+ (- (* sxa cza)) (* cxa sya sza)))
					   (list (- sya) (* sxa cya) (* cxa cya))))))
					   
(defun rotate* (m v)
  (let ((result (make-array 3 :initial-element 0)))
    (dotimes (x 3)
      (dotimes (y 3)
	(incf (aref result x) (* (aref v y) (aref m x y)))))
    result))
	
(defun translate-point (v1 v2 &optional (fn #'+)) 
  (let ((result (make-array 3)))
    (dotimes (i 3)
      (setf (aref result i) (funcall fn (aref v1 i) (aref v2 i))))
    result))
  

(defun translate-points (tri position)
  (make-array (length tri) :initial-contents
	      (loop for v across tri collecting (translate-point position v))))

(defun rotate-triangle (points m)
  (if (not (eql (second (type-of m)) t))
      (rotate-triangle points (make-rotation-matrix m))
      (make-array (length points) :initial-contents
		  (loop for v across points collecting (rotate* m v)))))

(defun rotate-points (points m)
  (if (not (eql (second (type-of m)) t))
      (rotate-points points (make-rotation-matrix m))
      (make-array (length points) :initial-contents (loop for tri across points collecting (rotate* m tri)))))


(defun scale-vector (v a)
  (make-array (length v) :initial-contents (loop for i across v collecting (* i a))))

; scale points by a
(defun scale-points (points a)
  (make-array (length points) :initial-contents (loop for v across points collecting (scale-vector v a))))

; scale poitns by v (x y z)
(defun transform-points (points xyz)
  (make-array (length points) :initial-contents 
	      (loop for v across points collecting 
		   (make-array 3 :initial-contents
			       (list (* (aref v 0) (aref xyz 0)) (* (aref v 1) (aref xyz 1)) (* (aref v 2) (aref xyz 2)))))))

; returns a vector with all elemts scaled to biggest 1 which is scaled to 1
; e.x. (scale-vector (8 4 2)) -> (1 .5 .25)
(defun scale-vector-1 (v)
  (let ((max (loop for i across v maximize (abs i) into result finally (return result))))
    (make-array (length v) :initial-contents (loop for i across v collecting (float (/ i max))))))

(defun dot (v1 v2)
  (loop for i from 0 to (1- (length v1)) summing (* (aref v1 i) (aref v2 i))))

(defun vector-length (v)
  (sqrt (dot v v)))

(defun scalar-proj (vector direction)
  (let ((length (vector-length direction)))
    (if (eql 0 length)
	0
	(/ (dot vector direction) length))))

(defun vector- (v1 v2)
  (make-array (length v1) :initial-contents (loop for i from 0 to (1- (length v1)) collecting (- (aref v1 i) (aref v2 i)))))

(defun vector+ (v1 v2)
  (make-array (length v1) :initial-contents (loop for i from 0 to (1- (length v1)) collecting (+ (aref v1 i) (aref v2 i)))))

