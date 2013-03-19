;;; shape.lisp --- shapes.

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

(export '(shape-tag
	  shape-data))
(defclass shape ()
  ((elements
    :documentation "Elements defining the shape."
    :accessor shape-elements
    :initform (make-array '(0))
    :type simple-vector)
   (tag
    :documentation "User-defined shape identifier."
    :accessor shape-tag
    :initform nil)
   (data
    :documentation "User-defined shape data."
    :accessor shape-data
    :initform nil))
  (:documentation
   "Base class for shapes."))

(defun shape-ref (self index)
  "Return the element of shape SELF specified by INDEX."
  (svref (shape-elements self) index))

(defun (setf shape-ref) (value self index)
  "Set the element of shape SELF specified by INDEX to VALUE."
  (setf (svref (shape-elements self) index) value))

(defmethod print-object ((self shape) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (when (shape-tag self)
      (prin1 (shape-tag self) stream)
      (princ #\Space stream))
    (princ #\( stream)
    (iter (for index :from 0 :below (length (shape-elements self)))
	  (unless (first-time-p)
	    (princ #\Space stream))
	  (prin1 (shape-ref self index) stream))
    (princ #\) stream)))

(export 'center)
(defgeneric center (self)
  (:documentation
   "Return the geometric center (centroid) of the shape.
Value is a vector."))

(export 'mass)
(defgeneric mass (self)
  (:documentation
   "Return the unit mass of the shape.
Value is a scalar."))

;;; shape.lisp ends here
