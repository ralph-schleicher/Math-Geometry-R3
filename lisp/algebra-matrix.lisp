;;; algebra-matrix.lisp --- matrices.

;; Copyright (C) 2012, 2013 Ralph Schleicher

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;;    * Redistributions of source code must retain the above copyright
;;      notice, this list of conditions and the following disclaimer.
;;
;;    * Redistributions in binary form must reproduce the above copyright
;;      notice, this list of conditions and the following disclaimer in
;;      the documentation and/or other materials provided with the
;;      distribution.
;;
;;    * The name of the author may not be used to endorse or promote
;;      products derived from this software without specific prior
;;      written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS
;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
;; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
;; IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;;; Code:

(in-package :math-geometry-r3)

(deftype matrix-r3 ()
  `(simple-array real (3 3)))

(export 'make-matrix)
(defun make-matrix (&rest arg)
  "Create a matrix with elements A11, A12, A13, A21, A22, A23,
A31, A32, and A33."
  (let ((a (make-array '(3 3) :element-type 'real)))
    (ecase (length arg)
      (9 (multiple-value-bind (a11 a12 a13 a21 a22 a23 a31 a32 a33)
	     (values-list arg)
	   (setf (aref a 0 0) a11 (aref a 0 1) a12 (aref a 0 2) a13
		 (aref a 1 0) a21 (aref a 1 1) a22 (aref a 1 2) a23
		 (aref a 2 0) a31 (aref a 2 1) a32 (aref a 2 2) a33)))
      (0))
    a))

(export 'copy-matrix)
(defun copy-matrix (a)
  "Return a copy of matrix A."
  (declare (type matrix-r3 a))
  (make-matrix (aref a 0 0) (aref a 0 1) (aref a 0 2)
	       (aref a 1 0) (aref a 1 1) (aref a 1 2)
	       (aref a 2 0) (aref a 2 1) (aref a 2 2)))

(export 'null-matrix-p)
(defun null-matrix-p (a)
  "True if matrix A is a null matrix."
  (declare (type matrix-r3 a))
  (and (zerop (aref a 0 0)) (zerop (aref a 0 1)) (zerop (aref a 0 2))
       (zerop (aref a 1 0)) (zerop (aref a 1 1)) (zerop (aref a 1 2))
       (zerop (aref a 2 0)) (zerop (aref a 2 1)) (zerop (aref a 2 2))))

(export 'identity-matrix-p)
(defun identity-matrix-p (a)
  "True if matrix A is an identity matrix."
  (declare (type matrix-r3 a))
  (and (= (aref a 0 0) 1) (zerop (aref a 0 1)) (zerop (aref a 0 2))
       (zerop (aref a 1 0)) (= (aref a 1 1) 1) (zerop (aref a 1 2))
       (zerop (aref a 2 0)) (zerop (aref a 2 1)) (= (aref a 2 2) 1)))

(export 'diagonal-matrix-p)
(defun diagonal-matrix-p (a)
  "True if matrix A is a diagonal matrix."
  (declare (type matrix-r3 a))
  (and (zerop (aref a 0 1)) (zerop (aref a 0 2))
       (zerop (aref a 1 0)) (zerop (aref a 1 2))
       (zerop (aref a 2 0)) (zerop (aref a 2 1))))

(export 'symmetric-matrix-p)
(defun symmetric-matrix-p (a)
  "True if matrix A is a symmetric matrix."
  (declare (type matrix-r3 a))
  (and (= (aref a 1 0) (aref a 0 1))
       (= (aref a 2 0) (aref a 0 2))
       (= (aref a 2 1) (aref a 1 2))))

(export 'determinant)
(defun determinant (a)
  "Return the determinant of matrix A.
Value is a scalar."
  (declare (type matrix-r3 a))
  (+ (* (aref a 0 0) (- (* (aref a 1 1) (aref a 2 2))
			(* (aref a 1 2) (aref a 2 1))))
     (* (aref a 0 1) (- (* (aref a 1 2) (aref a 2 0))
			(* (aref a 1 0) (aref a 2 2))))
     (* (aref a 0 2) (- (* (aref a 1 0) (aref a 2 1))
			(* (aref a 1 1) (aref a 2 0))))))

(export 'transpose)
(defun transpose (a)
  "Transpose matrix A in place."
  (declare (type matrix-r3 a))
  (rotatef (aref a 1 0) (aref a 0 1))
  (rotatef (aref a 2 0) (aref a 0 2))
  (rotatef (aref a 2 1) (aref a 1 2))
  a)

(export 'cofactors)
(defun cofactors (a)
  "Calculate matrix of cofactors of matrix A in place."
  (declare (type matrix-r3 a))
  (psetf (aref a 0 0) (- (* (aref a 1 1) (aref a 2 2))
			 (* (aref a 1 2) (aref a 2 1)))
	 (aref a 0 1) (- (* (aref a 1 2) (aref a 2 0))
			 (* (aref a 1 0) (aref a 2 2)))
	 (aref a 0 2) (- (* (aref a 1 0) (aref a 2 1))
			 (* (aref a 1 1) (aref a 2 0)))
	 (aref a 1 0) (- (* (aref a 0 2) (aref a 2 1))
			 (* (aref a 0 1) (aref a 2 2)))
	 (aref a 1 1) (- (* (aref a 0 0) (aref a 2 2))
			 (* (aref a 0 2) (aref a 2 0)))
	 (aref a 1 2) (- (* (aref a 0 1) (aref a 2 0))
			 (* (aref a 0 0) (aref a 2 1)))
	 (aref a 2 0) (- (* (aref a 0 1) (aref a 1 2))
			 (* (aref a 0 2) (aref a 1 1)))
	 (aref a 2 1) (- (* (aref a 0 2) (aref a 1 0))
			 (* (aref a 0 0) (aref a 1 2)))
	 (aref a 2 2) (- (* (aref a 0 0) (aref a 1 1))
			 (* (aref a 0 1) (aref a 1 0))))
  a)

(export 'adjugate)
(defun adjugate (a)
  "Calculate adjugate matrix of matrix A in place."
  (declare (type matrix-r3 a))
  (transpose (cofactors a)))

(export 'inverse)
(defun inverse (a)
  "Calculate inverse matrix of matrix A in place."
  (declare (type matrix-r3 a))
  (let ((det (determinant a)))
    (when (zerop det)
      (error (make-condition 'division-by-zero
			     :operation 'inverse
			     :operands (list a))))
    (adjugate a)
    (%rscale 9 a det)
    a))

(export 'rotation-matrix)
(defun rotation-matrix (roll pitch yaw)
  "Calculate the rotation matrix for the Euler angle sequence (1,2,3).

The angles for the Euler angle sequence (1,2,3) are also called
Tait-Bryan angles or nautical angles and are usually referred to
as roll (bank), pitch (attitude), and yaw (heading).

First argument ROLL is the rotation about the x-axis in radian.
Second argument PITCH is the rotation about the y-axis in radian.
Third argument YAW is the rotation about the z-axis in radian.

Return value is an orthogonal matrix."
  (flet ((%sin (arg)
	   (if (zerop (mod arg pi))
	       0
	     (let ((val (sin arg)))
	       (if (= (abs val) 1)
		   (round val)
		 val))))
	 (%cos (arg)
	   (if (zerop (mod (- arg pi/2) pi))
	       0
	     (let ((val (cos arg)))
	       (if (= (abs val) 1)
		   (round val)
		 val)))))
    (let ((sin-phi (%sin roll))
	  (cos-phi (%cos roll))
	  (sin-theta (%sin pitch))
	  (cos-theta (%cos pitch))
	  (sin-psi (%sin yaw))
	  (cos-psi (%cos yaw)))
      (make-matrix (* cos-theta cos-psi)
		   (* cos-theta sin-psi)
		   (- sin-theta)
		   (- (* sin-phi sin-theta cos-psi) (* cos-phi sin-psi))
		   (+ (* sin-phi sin-theta sin-psi) (* cos-phi cos-psi))
		   (* sin-phi cos-theta)
		   (+ (* cos-phi sin-theta cos-psi) (* sin-phi sin-psi))
		   (- (* cos-phi sin-theta sin-psi) (* sin-phi cos-psi))
		   (* cos-phi cos-theta)))))

(export 'rotation-angles)
(defun rotation-angles (r &optional yaw)
  "Calculate the rotation angles from a rotation matrix.

First argument R is the rotation matrix.
Optional second argument YAW is the rotation about the z-axis in
 radian.  If a singularity arises, that is the cosine of the pitch
 angle is equal to zero, it is not possible to distinguish roll and
 yaw.  In that case the yaw angle is set to zero unless an alternative
 value is supplied as the second argument.

Return values are the rotation angles for the Euler angle sequence
\(1,2,3), that is roll, pitch, and yaw.  The return values always lie
in the half-closed interval (-pi, pi]."
  (declare (type matrix-r3 r))
  (let (phi theta psi)
    (let* ((sin-theta (- (aref r 0 2)))
	   (cos-theta (if (/= (abs sin-theta) 1)
			  (hypot (aref r 1 2) (aref r 2 2))
			0)))
      (setf theta (atan sin-theta cos-theta))
      (if (/= cos-theta 0)
	  (setf psi (atan (aref r 0 1) (aref r 0 0))
		phi (atan (aref r 1 2) (aref r 2 2)))
	;; Handle singularity.
	(progn
	  (setf psi (if yaw (angle yaw) 0)
		phi (atan (aref r 1 0) (aref r 2 0)))
	  (when (/= psi 0)
	    (if (plusp sin-theta)
		(incf phi psi)
	      (decf phi psi))))))
    (values phi theta psi)))

;;; algebra-matrix.lisp ends here
