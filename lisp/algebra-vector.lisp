;;; algebra-vector.lisp --- vectors.

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

(deftype vector-r3 ()
  `(simple-array real (3)))

(export 'make-vector)
(defun make-vector (&rest arg)
  "Create a vector with elements X1, X2, and X3."
  (let ((x (make-array '(3) :element-type 'real)))
    (ecase (length arg)
      (3 (multiple-value-bind (x1 x2 x3)
	     (values-list arg)
	   (setf (svref x 0) x1
		 (svref x 1) x2
		 (svref x 2) x3)))
      (0))
    x))

(export 'copy-vector)
(defun copy-vector (x)
  "Return a copy of vector X."
  (declare (type vector-r3 x))
  (copy-seq x))

(export 'null-vector-p)
(defun null-vector-p (x)
  "True if vector X is a null vector."
  (declare (type vector-r3 x))
  (and (zerop (svref x 0))
       (zerop (svref x 1))
       (zerop (svref x 2))))

(export 'vector-length)
(defun vector-length (x)
  "Return the vector length (Euclidean norm) of vector X.
Value is a scalar."
  (declare (type vector-r3 x))
  (hypot3 (svref x 0)
	  (svref x 1)
	  (svref x 2)))

(export 'reverse-vector)
(defun reverse-vector (x)
  "Reverse the vector X."
  (declare (type vector-r3 x))
  (setf (svref x 0) (- (svref x 0))
	(svref x 1) (- (svref x 1))
	(svref x 2) (- (svref x 2)))
  x)

(export 'normalize-vector)
(defun normalize-vector (x)
  "Normalize the vector X."
  (declare (type vector-r3 x))
  (let ((len (vector-length x)))
    (if (zerop len)
	(%set 3 x 0)
      (%rscale 3 x len)))
  x)

(export 'distance-vector)
(defun distance-vector (x y &optional (w (make-vector)))
  "Distance vector.

     w ← y - x

Calculate the distance between the two vectors X and Y and store the
result in the vector W.  It is safe to specify either X or Y for W.
"
  (declare (type vector-r3 x y w))
  (setf (svref w 0) (- (svref y 0) (svref x 0))
	(svref w 1) (- (svref y 1) (svref x 1))
	(svref w 2) (- (svref y 2) (svref x 2)))
  w)

(export 'dot)
(defun dot (x y)
  "Dot product."
  (declare (type vector-r3 x y))
  (%dot 3 x y))

(export 'cross)
(defun cross (x y &optional (w (make-vector)))
  "Cross product.

     w ← x × y

Build the cross product of the two vectors X and Y and store the result
in the vector W.  It is safe to specify either X or Y for W.

First argument X is the left vector operand.
Second argument Y is the right vector operand.
Optional third argument W is the result vector.
 Default is to allocate a new vector.

Return value is the vector W."
  (declare (type vector-r3 x y w))
  (psetf (svref w 0) (- (* (svref x 1) (svref y 2)) (* (svref x 2) (svref y 1)))
	 (svref w 1) (- (* (svref x 2) (svref y 0)) (* (svref x 0) (svref y 2)))
	 (svref w 2) (- (* (svref x 0) (svref y 1)) (* (svref x 1) (svref y 0))))
  w)

;;; algebra-vector.lisp ends here
