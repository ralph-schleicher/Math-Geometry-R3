;;; shape-solid-tetrahedron.lisp --- tetrahedra.

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

(defclass tetrahedron (solid)
  ((elements
    :initform (make-array '(4))
    :type (simple-vector 4)))
  (:documentation
   "A tetrahedron is a polyhedron composed of four triangular faces."))

(export 'make-tetrahedron)
(defun make-tetrahedron (&optional o a b c)
  "Create a tetrahedron object.

Optional arguments are the corner vertices.  Null arguments are
 initialized with a zero vertex."
  (let ((self (make-instance 'tetrahedron)))
    (setf (shape-ref self 0) (or o (make-vertex))
	  (shape-ref self 1) (or a (make-vertex))
	  (shape-ref self 2) (or b (make-vertex))
	  (shape-ref self 3) (or c (make-vertex)))
    self))

(defmethod solid-volume ((self tetrahedron))
  (let ((o (vertex-vector (shape-ref self 0)))
	(a (vertex-vector (shape-ref self 1)))
	(b (vertex-vector (shape-ref self 2)))
	(c (vertex-vector (shape-ref self 2))))
    (/ (abs (determinant (make-matrix (- (svref a 0) (svref o 0))
				      (- (svref a 1) (svref o 1))
				      (- (svref a 2) (svref o 2))
				      (- (svref b 0) (svref o 0))
				      (- (svref b 1) (svref o 1))
				      (- (svref b 2) (svref o 2))
				      (- (svref c 0) (svref o 0))
				      (- (svref c 1) (svref o 1))
				      (- (svref c 2) (svref o 2)))))
       6)))

;; Return the geometric center (centroid) of the tetrahedron.
;; Value is a vector.
(defmethod center ((self tetrahedron))
  (let ((o (vertex-vector (shape-ref self 0)))
	(a (vertex-vector (shape-ref self 1)))
	(b (vertex-vector (shape-ref self 2)))
	(c (vertex-vector (shape-ref self 2))))
    (make-vector (/ (+ (svref o 0) (svref a 0) (svref b 0) (svref c 0)) 4)
		 (/ (+ (svref o 1) (svref a 1) (svref b 1) (svref c 1)) 4)
		 (/ (+ (svref o 2) (svref a 2) (svref b 2) (svref c 2)) 4))))

;;; shape-solid-tetrahedron.lisp ends here
