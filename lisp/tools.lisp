;;; tools.lisp --- tools and algorithms.

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

;;;; Tools and algorithms in space.

(defun %best-fit (c p)
  (let ((xo 0) (yo 0) (zo 0))
    ;; Mean values.
    (map nil (lambda (v)
	       (setf v (vertex-vector v))
	       (incf xo (svref v 0))
	       (incf yo (svref v 1))
	       (incf zo (svref v 2)))
	 p)
    (divf xo c)
    (divf yo c)
    (divf zo c)
    ;; Coefficients of the system of linear equations.
    (let ((xx 0) (xy 0) (xz 0) (yy 0) (yz 0) (zz 0) (x 0) (y 0) (z 0))
      (map nil (lambda (v)
		 (setf v (vertex-vector v)
		       x (- (xvref v 0) xo)
		       y (- (xvref v 1) yo)
		       z (- (xvref v 2) zo))
		 (incf xx (* x x))
		 (incf xy (* x y))
		 (incf xz (* x z))
		 (incf yy (* y y))
		 (incf yz (* y z))
		 (incf zz (* z z)))
	   p)
      ;; Return coefficient matrix.
      (make-matrix xx xy xz xy yy yz xz yz zz))))

(defun best-fit-line (p)
  "Return the direction vector of the best fit line for a given point set."
  (let ((c (length p)))
    (when (< c 2)
      (error "Too few vertices."))
    (if (= c 2)
	(edge-direction (make-edge (elt p 0) (elt p 1)))
      ;; Return eigenvector of the largest eigenvalue.
      (let ((a (%best-fit c p)))
	(eigenvector a (first (sort (eigenvalues a) #'absolute-descending)))))))

(defun best-fit-plane (p)
  "Return the normal vector of the best fit plane for a given point set."
  (let ((c (length p)))
    (when (< c 3)
      (error "Too few vertices."))
    (if (= c 3)
	(face-normal (make-triangle (elt p 0) (elt p 1) (elt p 2)))
      ;; Return eigenvector of the smallest eigenvalue.
      (let ((a (%best-fit c p)))
	(eigenvector a (first (sort (eigenvalues a) #'absolute-ascending)))))))

(defun bounding-box (p &key corners)
  "Return the axis-aligned minimum bounding box for a given point set."
  (let ((c (length p)))
    (when (< c 1)
      (error "Too few vertices."))
    (let ((lo (copy-vector (vertex-vector (elt p 0))))
	  (hi (copy-vector (vertex-vector (elt p 0)))))
      (map nil (lambda (v)
		 (setf v (vertex-vector v))
		 (minf (svref lo 0) (svref v 0))
		 (maxf (svref hi 0) (svref v 0))
		 (minf (svref lo 1) (svref v 1))
		 (maxf (svref hi 1) (svref v 1))
		 (minf (svref lo 2) (svref v 2))
		 (maxf (svref hi 2) (svref v 2)))
	   p)
      (if (not corners)
	  (%accu 3 lo hi :alpha -1)
	(values lo hi)))))

;;; tools.lisp ends here
