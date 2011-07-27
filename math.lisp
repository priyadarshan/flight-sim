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



