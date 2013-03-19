;;; shape-edge.lisp --- edges.

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

(defclass edge (shape)
  ((elements
    :initform (make-array '(2))
    :type (simple-vector 2)))
  (:documentation
   "An edge is a line segment joining two vertices.
A planar closed sequence of edges form a face."))

(export 'make-edge)
(defun make-edge (&optional o a)
  "Create an edge object.

Optional first argument O is the start vertex.
Optional second argument A is the end vertex.

Null arguments are initialized with a zero vertex."
  (let ((self (make-instance 'edge)))
    (setf (shape-ref self 0) (or o (make-vertex))
	  (shape-ref self 1) (or a (make-vertex)))
    self))

(export 'edge-length)
(defun edge-length (self)
  "Return the length of the edge.
Value is a scalar."
  (let ((o (vertex-vector (shape-ref self 0)))
	(a (vertex-vector (shape-ref self 1))))
    (hypot3 (- (svref a 0) (svref o 0))
	       (- (svref a 1) (svref o 1))
	       (- (svref a 2) (svref o 2)))))

(export 'edge-direction)
(defun edge-direction (self)
  "Return the direction vector of the line.
Value is a unit vector."
  (let ((o (vertex-vector (shape-ref self 0)))
	(a (vertex-vector (shape-ref self 1))))
    (normalize-vector (make-vector (- (svref a 0) (svref o 0))
				   (- (svref a 1) (svref o 1))
				   (- (svref a 2) (svref o 2))))))

(export 'edge-reverse)
(defun edge-reverse (self)
  "Change the direction of the edge, that is exchange the start and
end vertex."
  (rotatef (shape-ref self 0) (shape-ref self 1))
  self)

;; Return the geometric center (centroid) of the edge.
;; Value is a vector.
(defmethod center ((self edge))
  (let ((o (vertex-vector (shape-ref self 0)))
	(a (vertex-vector (shape-ref self 1))))
    (make-vector (/ (+ (svref o 0) (svref a 0)) 2)
		 (/ (+ (svref o 1) (svref a 1)) 2)
		 (/ (+ (svref o 2) (svref a 2)) 2))))

;; Return the unit mass of the edge.  Value is a scalar.
;; The mass function of an edge is its length (which see).
(defmethod mass ((self edge))
  (edge-length self))

;;; shape-edge.lisp ends here
