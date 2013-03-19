;;; shape-face-triangle.lisp --- triangles.

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

(defclass triangle (face)
  ((elements
    :initform (make-array '(3))
    :type (simple-vector 3)))
  (:documentation
   "A triangle is a polygon with three corners."))

(export 'make-triangle)
(defun make-triangle (&optional o a b)
  "Create a triangle object.

Optional arguments are the corner vertices.  Null arguments are
 initialized with a zero vertex."
  (let ((self (make-instance 'triangle)))
    (setf (shape-ref self 0) (or o (make-vertex))
	  (shape-ref self 1) (or a (make-vertex))
	  (shape-ref self 2) (or b (make-vertex)))
    self))

(defun %triangle-normal (self)
  "Return vector perpendicular to the plane."
  (let ((o (vertex-vector (shape-ref self 0)))
	(a (vertex-vector (shape-ref self 1)))
	(b (vertex-vector (shape-ref self 2))))
    (unless (null-vector-p o)
      (setf a (distance-vector o a)
	    b (distance-vector o b)))
    (cross a b)))

(defmethod face-area ((self triangle))
  (/ (vector-length (%triangle-normal self)) 2))

(defmethod face-normal ((self triangle))
  (normalize-vector (%triangle-normal self)))

;; Return the geometric center (centroid) of the triangle.
;; Value is a vector.
(defmethod center ((self triangle))
  (let ((o (vertex-vector (shape-ref self 0)))
	(a (vertex-vector (shape-ref self 1)))
	(b (vertex-vector (shape-ref self 2))))
    (make-vector (/ (+ (svref o 0) (svref a 0) (svref b 0)) 3)
		 (/ (+ (svref o 1) (svref a 1) (svref b 1)) 3)
		 (/ (+ (svref o 2) (svref a 2) (svref b 2)) 3))))

;;; shape-face-triangle.lisp ends here
