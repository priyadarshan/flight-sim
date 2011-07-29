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
  (float (+ start (* (- end start) (if (eql now 0.0) 0.0 (/ (min now duration) duration))))))


;; returns a real lisp 2d array
(defun make-rotation-matrix (xa ya za) 
  (let ((sxa (sin xa))
	(cxa (cos xa))
	(sya (sin ya))
	(cya (cos ya))
	(sza (sin za))
	(cza (cos za)))
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
  

(defun translate-triangle (tri position)
  (make-array (length tri) :initial-contents
	      (loop for v across tri collecting (translate-point position v))))

(defun rotate-triangle (tri m)
  (make-array (length tri) :initial-contents
	      (loop for v across tri collecting (rotate* m v))))
