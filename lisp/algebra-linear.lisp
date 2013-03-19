;;; algebra-linear.lisp --- linear algebra.

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

;;; Eigenvalues and eigenvectors.

(defun characteristic-polynomial (a)
  "Return the coefficients of the characteristic polynomial.
The highest order coefficient is implicit one.  That is, for
the third-degree polynomial

     f(x) = x³ + p·x² + q·x + r

value is a list with the numbers P, Q, and R."
  (declare (type matrix-r3 a))
  (list	(- (+ (aref a 0 0)
	      (aref a 1 1)
	      (aref a 2 2)))
	(- (+ (* (aref a 0 0) (aref a 1 1))
	      (* (aref a 0 0) (aref a 2 2))
	      (* (aref a 1 1) (aref a 2 2)))
	   (+ (* (aref a 0 1) (aref a 1 0))
	      (* (aref a 0 2) (aref a 2 0))
	      (* (aref a 1 2) (aref a 2 1))))
	(- (+ (* (aref a 0 0) (aref a 1 2) (aref a 2 1))
	      (* (aref a 0 1) (aref a 1 0) (aref a 2 2))
	      (* (aref a 0 2) (aref a 1 1) (aref a 2 0)))
	   (+ (* (aref a 0 0) (aref a 1 1) (aref a 2 2))
	      (* (aref a 0 1) (aref a 1 2) (aref a 2 0))
	      (* (aref a 0 2) (aref a 1 0) (aref a 2 1))))))

(export 'eigenvalues)
(defun eigenvalues (a)
  "Calculate the eigenvalues of the matrix A.
Value is a list with three numbers."
  (declare (type matrix-r3 a))
  (apply #'cubic-formula-1 (characteristic-polynomial a)))

(export 'eigenvector)
(defun eigenvector (a e)
  "Calculate the eigenvector for the matrix A and eigenvalue E."
  (declare (type matrix-r3 a)
	   (type number e))
  (let (z p q v)
    ;; Iterated vectors of the matrix A.  Starting with the random
    ;; vector #(1 0 0), the first iterated vector is equal to the
    ;; first column vector of the matrix A.  Vector Z is the second
    ;; iterated vector.
    (setf z (make-vector (%dot 3 a a :start1 0 :step1 1 :start2 0 :step2 3)
			 (%dot 3 a a :start1 3 :step1 1 :start2 0 :step2 3)
			 (%dot 3 a a :start1 6 :step1 1 :start2 0 :step2 3)))
    ;; Coefficients of the characteristic polynomial P(x) of the
    ;; matrix A.
    (setf p (coerce (characteristic-polynomial a) 'vector))
    ;; Coefficients of the second-degree polynomial, that is the
    ;; quotient of P(x) on division by (x - E) where E is a root,
    ;; that is an eigenvalue, of P(x).
    (setf q (vector 0 0))
    ;; Use Horner's method to divide P(x) by (x - E).
    (setf (svref q 0) (+ (svref p 0) e)
	  (svref q 1) (+ (svref p 1) (* e (svref q 0))))
    ;; The eigenvector associated with eigenvalue E.
    (setf v (make-vector (+ (svref z 0) (* (aref a 0 0) (svref q 0)) (svref q 1))
			 (+ (svref z 1) (* (aref a 1 0) (svref q 0)))
			 (+ (svref z 2) (* (aref a 2 0) (svref q 0)))))
    ;; Return value.
    (normalize-vector v)))

;;; algebra-linear.lisp ends here
