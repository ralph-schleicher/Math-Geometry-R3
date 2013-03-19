;;; shape-vertex.lisp --- vertices.

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

(export 'vertex-vector)
(defclass vertex (shape)
  ((elements
    :accessor vertex-vector
    :initarg :vector
    :initform (make-vector)
    :type vector-r3))
  (:documentation
   "A vertex is a special kind of point describing a corner
or intersection of geometric shapes in space.  Any vertex
can be interpreted as an origin vector, too.  Although the
terms \"vertex\" and \"vector\" can be used interchangeably,
they are usually choosen according to the context."))

(export 'vertex-ref)
(defun vertex-ref (self index)
  "Return the element of vertex SELF specified by INDEX."
  (svref (vertex-vector self) index))

(defun (setf vertex-ref) (value self index)
  "Set the element of vertex SELF specified by INDEX to value."
  (setf (svref (vertex-vector self) index) value))

;; All methods referring to a vertex's vector
;; shall work with built-in vectors, too.
(defmethod vertex-vector ((self vector))
  (declare (type vector-r3 self))
  self)

(export 'make-vertex)
(defun make-vertex (&rest arg)
  "Create a vertex object.
Arguments are either three real numbers, a vertex, or a vector."
  (make-instance 'vertex :vector (ecase (length arg)
				   (3 (apply #'make-vector arg))
				   (1 (vertex-vector (first arg)))
				   (0 (make-vector)))))

;; Return the geometric center (centroid) of the vertex.
;; Value is a vector.
(defmethod center ((self vertex))
  (copy-vector (vertex-vector self)))

;; Return the unit mass of the vertex.  Value is a scalar.
;; The mass function of a vertex always returns one (unity).
(defmethod mass ((self vertex))
  (declare (ignore self))
  1)

;;; shape-vertex.lisp ends here
