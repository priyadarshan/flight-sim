(in-package #:flight-sim)

(defmacro restartable (&body body)
  "Helper macro since we use continue restarts a lot 
   (remember to hit C in slime or pick the restart so errors don't kill the app"
  `(restart-case
       (progn ,@body)
     (continue () :report "Continue")))

(defun make-2d-array (h w contents)
  (let ((arr (make-array h)))
    (do ((i 0 (incf i))
	 (rest-list contents (rest rest-list)))
	((eql i h)) 
      (setf (aref arr i) (make-array w :initial-contents (car rest-list))))
    arr))
