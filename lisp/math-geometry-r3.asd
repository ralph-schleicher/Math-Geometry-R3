;;; math-geometry-r3.asd --- ASDF system definition.

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

(in-package :common-lisp-user)

(asdf:defsystem :math-geometry-r3
  :description "Objects and tools for analytic geometry."
  :author "Ralph Schleicher <rs@ralph-schleicher.de>"
  :license "Modified BSD License"
  :version "0.0"
  :depends-on (:iterate :rs-cll)
  :serial t
  :components ((:file "math-geometry-r3")
	       (:file "algebra")
	       (:file "algebra-vector")
	       (:file "algebra-matrix")
	       (:file "algebra-linear")
	       (:file "shape")
	       (:file "shape-vertex")
	       (:file "shape-edge")
	       (:file "shape-face")
	       (:file "shape-face-triangle")
	       (:file "shape-solid")
	       (:file "shape-solid-tetrahedron")
	       (:file "tools")
	       ))

;; local variables:
;; time-stamp-time-zone: "UTC"
;; time-stamp-format: "%:y%02m%02d.%02H%02M"
;; time-stamp-start: ":version\\s-+\""
;; time-stamp-end: "\""
;; end:

;;; math-geometry-r3.asd ends here
