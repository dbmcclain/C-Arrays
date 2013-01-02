
(in-package :cl-user)

(defpackage #:c-arrays
  (:use #:common-lisp)
  (:nicknames #:ca)
  (:export 
   #:<carray>
   #:<float-carray>
   #:<double-carray>
   #:carray-data
   #:carray-element-type
   #:carray-dimensions
   #:carray-dimension
   #:carray-reduced-dimensions
   #:carray-rank
   #:carray-reduced-rank
   #:carray-total-size
   #:make-carray
   #:make-overlay-carray
   #:make-float-carray
   #:make-double-carray
   #:with-dynamic-carray
   #:with-dynamic-float-carray
   #:with-dynamic-double-carray
   #:discard-carray
   #:caref
   #:row-major-caref
   #:carray-row-major-index
   #:elementwise
   #:copy-converting
   #:convert-to-lisp-object
   #:round-to-integer
   #:truncate-to-integer
   #:convert-to-float
   #:with-row-major-access
   #:copy-lisp-array-to-truncated-int-carray
   #:copy-lisp-array-to-rounded-int-carray
   #:copy-lisp-array-to-float-carray
   #:copy-lisp-array-to-double-carray
   #:is-float-array
   #:is-double-array
   #:with-overlay-carray
   #:carray-data-address
   #:sfloat
   #:dfloat
   ))

