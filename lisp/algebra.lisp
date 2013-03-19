;;; algebra.lisp --- basic definitions.

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

(defmacro %set (n x r &key (start 0) (step 1))
  "Set elements of X to the scalar value R."
  `(setf ,@(iter (repeat n)
		 (for x-index :from start :by step)
		 (collect `(row-major-aref ,x ,x-index))
		 (collect r))))
#-(and)
(progn
  (macroexpand-1 '(%set 3 y 0))
  (macroexpand-1 '(%set 3 y s :start 2 :step 3))
  (macroexpand-1 '(%set 3 y 2 :start 3))
  (values))

(defmacro %scale (n x r &key (start 0) (step 1))
  "Multiply elements of X by the scalar value R."
  `(progn ,@(iter (repeat n)
		  (for x-index :from start :by step)
		  (collect `(mulf (row-major-aref ,x ,x-index) ,r)))))
#-(and)
(progn
  (macroexpand-1 '(%scale 3 y 0.5))
  (macroexpand-1 '(%scale 3 y s :start 2 :step 3))
  (macroexpand-1 '(%scale 3 y 2 :start 3))
  (values))

(defmacro %rscale (n x r &key (start 0) (step 1))
  "Divide elements of X by the scalar value R."
  `(progn ,@(iter (repeat n)
		  (for x-index :from start :by step)
		  (collect `(divf (row-major-aref ,x ,x-index) ,r)))))
#-(and)
(progn
  (macroexpand-1 '(%rscale 3 y 0.5))
  (macroexpand-1 '(%rscale 3 y s :start 2 :step 3))
  (macroexpand-1 '(%rscale 3 y 2 :start 3))
  (values))

(defmacro %dot (n x y &key (start1 0) (step1 1) (start2 0) (step2 1))
  "Dot product of X and Y."
  `(+ ,@(iter (repeat n)
	      (for x-index :from start1 :by step1)
	      (for y-index :from start2 :by step2)
	      (collect `(* (row-major-aref ,x ,x-index)
			   (row-major-aref ,y ,y-index))))))
#-(and)
(progn
  (macroexpand-1 '(%dot 3 u v))
  (macroexpand-1 '(%dot 3 a b :start1 3 :start2 1 :step2 3))
  (values))

(defmacro %accu (n x y &key (alpha 1 alpha-supplied-p) (start1 0) (step1 1) (start2 0) (step2 1))
  "Add scaled elements of X to Y."
  `(progn ,@(iter (repeat n)
		  (for x-index :from start1 :by step1)
		  (for y-index :from start2 :by step2)
		  (collect `(incf (row-major-aref ,y ,y-index)
				  ,(if alpha-supplied-p
				       `(* ,alpha (row-major-aref ,x ,x-index))
				       `(row-major-aref ,x ,x-index)))))))
#-(and)
(progn
  (macroexpand-1 '(%accu 3 u v))
  (macroexpand-1 '(%accu 3 u v :alpha s))
  (values))

(defmacro %copy (n x y &key (start1 0) (step1 1) (start2 0) (step2 1))
  "Copy elements from X to Y (vectors must not overlap)."
  `(setf ,@(iter (repeat n)
		 (for x-index :from start1 :by step1)
		 (for y-index :from start2 :by step2)
		 (collect `(row-major-aref ,y ,y-index))
		 (collect `(row-major-aref ,x ,x-index)))))
#-(and)
(progn
  (macroexpand-1 '(%copy 3 u v))
  (macroexpand-1 '(%copy 3 a a :start1 3 :start2 2 :step2 3))
  (values))

(defmacro %swap (n x y &key (start1 0) (step1 1) (start2 0) (step2 1))
  "Interchange elements of X with Y (vectors must not overlap)."
  `(progn ,@(iter (repeat n)
		  (for x-index :from start1 :by step1)
		  (for y-index :from start2 :by step2)
		  (collect `(rotatef (row-major-aref ,y ,y-index)
				     (row-major-aref ,x ,x-index))))))
#-(and)
(progn
  (macroexpand-1 '(%swap 3 u v))
  (macroexpand-1 '(%swap 3 a a :start1 3 :start2 2 :step2 3))
  (values))

;;; algebra.lisp ends here
