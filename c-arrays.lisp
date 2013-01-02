;; carray.lsp -- Generalized C Arrays
;;
;; DM 10/96
;; -------------------------------------------------------------------

(in-package "C-ARRAYS")

;; ------------------------------------------------------------------------
;; CARRAY -- the abstract base class for all C Arrays
;;
(defclass <carray> ()
  ((data    :accessor carray-data
            :initform nil)
   (nels    :accessor carray-total-size
            :initarg  :nelems)))

(defmethod is-float-array ((arr <carray>))
  (member (carray-element-type arr)
          '(:FLOAT :SINGLE-FLOAT :LISP-SINGLE-FLOAT)))

(defmethod is-double-array ((arr <carray>))
  (member (carray-element-type arr)
          '(:DOUBLE :DOUBLE-FLOAT :LISP-DOUBLE-FLOAT)))

;; ---------------------------------------------
(defclass <float-mixin> ()
  ())

(defmethod is-float-array ((arr <float-mixin>))
  (declare (ignore arr))
  t)

(defmethod is-double-array ((arr <float-mixin>))
  (declare (ignore arr))
  nil)

;; --------------------------------------------
(defclass <double-mixin> ()
  ())

(defmethod is-float-array ((arr <double-mixin>))
  (declare (ignore arr))
  nil)

(defmethod is-double-array ((arr <double-mixin>))
  (declare (ignore arr))
  t)

;; --------------------------------------------
(defclass <float-carray> (<float-mixin> <carray>)
  ())

(defclass <double-carray> (<double-mixin> <carray>)
  ())

;; --------------------------------------------
(defclass <static-mixin> ()
  ())

(defclass <static-carray> (<static-mixin> <carray>)
  ())

(defclass <static-float-carray> (<static-mixin> <float-carray>)
  ())

(defclass <static-double-carray> (<static-mixin> <double-carray>)
  ())

;; --------------------------------------------
(defclass <shadowing-carray> (<carray>)
  ((data :initarg :ptr)))

(defclass <shadowing-float-carray> (<float-carray>)
  ((data :initarg :ptr)))

(defclass <shadowing-double-carray> (<double-carray>)
  ((data :initarg :ptr)))
  
;; ----------------------------------------------------------------------
;; Instance Creation
;;

(defmethod discard-carray (obj)
  (declare (ignore obj))
  nil)

(defmethod discard-carray ((obj <static-mixin>))
  (hcl:flag-not-special-free-action obj)
  (fli:free-foreign-object (carray-data obj)))

;; -------------------------------
;;
(defmethod initialize-instance :after ((arr <static-mixin>) &key type dims
                                       &allow-other-keys)
  (setf (carray-data arr)
        (fli:allocate-foreign-object
         :type `(:foreign-array ,type ,dims)))
  (hcl:add-special-free-action 'discard-carray)
  (hcl:flag-special-free-action arr))

;; -------------------------------
;;
(defun make-carray (typ dims)
   ;;
   ;; dims should be an integer vector length or a list of dimensions
   ;;
   ;; data can either be a callocate'd area or NIL.
   ;;      If NIL is supplied then the initialize-instance method
   ;;      will go ahead and callocate an area of the required size.
   ;;
   ;; typ  should be one of the types here in the ECASE form for
   ;;      the element type of the array.
   ;;
   ;; copy when non-NIL forces the new carray to use a copy of
   ;;      the provided data area
   ;;
   (let* ((dims     (um:mklist dims))
          (nelems   (reduce #'* dims)))
     (make-instance (case typ
                      (:float    '<static-float-carray>)
                      (:double   '<static-double-carray>)
                      (otherwise '<static-carray>))
                    :nelems nelems
                    :type   typ
                    :dims   dims)))

(defun make-float-carray (dims)
  (make-carray :float dims))

(defun make-double-carray (dims)
  (make-carray :double dims))

(defmacro with-dynamic-carray ((p typ dims) &body body)
  `(let ((,p (make-carray ,typ ,dims)))
     (unwind-protect
         (locally
           ,@body)
       (discard-carray ,p))))

(defmacro with-dynamic-float-carray ((p dims) &body body)
  `(with-dynamic-carray (,p :float ,dims) ,@body))

(defmacro with-dynamic-double-carray ((p dims) &body body)
  `(with-dynamic-carray (,p :double ,dims) ,@body))

(defun carray-data-address (ca &rest indices)
  (apply #'fli:foreign-array-pointer (carray-data ca) indices))

;; -------------------------------
(defun carray-element-type (arr)
  (fli:foreign-array-element-type (carray-data arr)))

(defun carray-dimensions (arr)
  (fli:foreign-array-dimensions (carray-data arr)))

(defun carray-rank (arr)
  (length (carray-dimensions arr)))

(defun carray-reduced-dimensions (arr)
  (or (remove 1 (carray-dimensions arr))
      1))

(defun carray-reduced-rank (arr)
  (length (carray-reduced-dimensions arr)))

(defun carray-dimension (arr ix)
  ;; dimensions are indexed from zero
  (nth ix (carray-dimensions arr)))


;; ----------------------------------------------------------------------
;; REF/SET Methods
;;
;; NOTE: you must provide data of the appropriate type for (setf caref).
;; No data coercion is performed here.
;;

(defun caref (arr &rest ixlist)
  (apply #'fli:foreign-aref (carray-data arr) ixlist))

(defun (setf caref) (val arr &rest ixlist)
  (setf (apply #'fli:foreign-aref (carray-data arr) ixlist) val))


;; -------------------------------
;; iterators
#|
;; DM/MCFA  06/02 -- Recode to remove the use of :foreign-array types.
;; this appears, as per R.Laning, to cause trouble with delivery and
;; dynamically sized arrays.
;;
(defmacro with-row-major-access ((p arr) &body body)
  (let ((the-arr (gensym)))
    `(let ((,the-arr ,arr))
       (fli:with-coerced-pointer
           (,p :pointer-type `(:foreign-array 
                               ,(carray-element-type ,the-arr)
                               (,(carray-total-size  ,the-arr))))
           (carray-data ,the-arr)
         ,@body))))

(defun row-major-caref (arr ix)
  (with-row-major-access (p arr)
    (fli:foreign-aref p ix)))

(defun (setf row-major-caref) (val arr ix)
  (with-row-major-access (p arr)
    (setf (fli:foreign-aref p ix) val)))
|#

;; Recoded row-major accessing with raw pointers and dereference with :index args
(defmacro with-row-major-access ((p arr) &body body)
  (let ((the-arr (gensym)))
    `(let ((,the-arr ,arr))
       (fli:with-coerced-pointer           
           (,p :type `(,(carray-element-type ,the-arr)))
           (carray-data ,the-arr)
         ,@body))))

(defun row-major-caref (arr ix)
  (with-row-major-access (p arr)
    (fli:dereference p :index ix)))

(defun (setf row-major-caref) (val arr ix)
  (with-row-major-access (p arr)
      (setf (fli:dereference p :index ix) val)))

(defun carray-row-major-index (arr &rest indices)
  (let ((dims (carray-dimensions arr)))
    (unless (= (length indices)
	       (length dims))
      (error "Wrong number of dimensions supplied for carray-row-major-index. Wanted ~A but got ~A"
             (length dims) (length indices)))
    (let ((strides (do ((strides (list 1))
			(dims (reverse dims) (rest dims)))
		       ((endp dims) (rest strides))
		     (push (* (first strides) (first dims)) strides))))
      (loop for index in indices
	  and stride in strides sum
	    (* index stride))
      )))
	      

#|
(defun elementwise (fn arr)
  (with-row-major-access (p arr)
    (dotimes (ix (carray-total-size arr))
      (funcall fn ix (fli:foreign-aref p ix)))))
|#
(defun elementwise (fn arr)
  (with-row-major-access (p arr)
    (dotimes (ix (carray-total-size arr))
      (funcall fn ix (fli:dereference p :index ix)))))

;; -------------------------------
;; Overlay C-Arrays -- like displaced arrays in Lisp
;; These are unsafe in the sense that unconstrained use of them
;; can lead to fatal progam errors. E.g., if an overlay is created
;; and its underlying array gets discared and GC'd then future accesses
;; through the overlay become invalid.
;;
;; Suggested use is to use (WITH-OVERLAY-CARRAY ...) and be careful
;; not to discard the parent array until this macro exits.
;;

(defun make-overlay-carray (arr
                            &key
                            (dims   (carray-dimensions arr))
                            offset)
  (let* ((offset (um:mklist (or offset
                                (mapcar (constantly 0) (carray-dimensions arr)))))
         (dims   (um:mklist dims))
         (nelems (reduce #'* dims))
         (carr   (carray-data arr)))
    (fli:with-coerced-pointer
        (p :pointer-type `(:foreign-array
                           ,(carray-element-type arr)
                           (,@dims)))
        (apply #'fli:foreign-array-pointer carr offset)
      (make-instance (cond
                      ((is-float-array arr)  '<shadowing-float-carray>)
                      ((is-double-array arr) '<shadowing-double-carray>)
                      (t                     '<shadowing-carray>))
                     :ptr    p
                     :nelems nelems)
      )))

(defmacro with-overlay-carray ((p &key dims offset) arr &body body)
  `(let ((,p (make-overlay-carray ,arr :dims ,dims :offset ,offset)))
     ,@body))

;; -------------------------------
;; converters and copying
;;
#|
(defun copy-converting (arr &key 
                            (type (carray-element-type arr)) 
                            (dims (carray-dimensions arr))
                            (fn   #'identity))
  (let ((newarr (make-carray type dims)))
    (with-row-major-access (p newarr)
      (elementwise #'(lambda (ix val)
                       (setf (fli:foreign-aref p ix) (funcall fn val)))
                   arr))
    newarr))
|#
(defun copy-converting (arr &key 
                            (type (carray-element-type arr)) 
                            (dims (carray-dimensions arr))
                            (fn   #'identity))
  (let ((newarr (make-carray type dims)))
    (with-row-major-access (p newarr)
      (elementwise #'(lambda (ix val)
                       (setf (fli:dereference p :index ix) (funcall fn val)))
                   arr))
    newarr))

(defun sfloat (x)
  (coerce x 'single-float))

(defun dfloat (x)
  (coerce x 'double-float))

(defun convert-to-float (arr &key (type :double))
  (copy-converting arr :type type :fn #'sfloat))

(defun round-to-integer (arr &key (type :int))
  (copy-converting arr :type type :fn #'round))

(defun truncate-to-integer (arr &key (type :int))
  (copy-converting arr :type type :fn #'truncate))


(defun convert-to-lisp-object (arr)
  (let ((newarr (make-array (carray-dimensions arr))))
    (elementwise #'(lambda (ix val)
                     (setf (row-major-aref newarr ix) val))
                 arr)
    newarr))

(defun copy-lisp-array-converting-to-carray (conv-fn lisp-arr c-arr)
  (let* ((nel (min (carray-total-size c-arr)
                   (array-total-size lisp-arr)))
         (v   (make-array nel
                          :displaced-to lisp-arr
                          :element-type (array-element-type lisp-arr))))
    (with-row-major-access (p c-arr)
      (dotimes (ix nel)
        (setf (fli:dereference p :index ix)
              (funcall conv-fn (aref v ix))))
      )))

(defun copy-lisp-array-to-double-carray (&rest args)
  (apply #'copy-lisp-array-converting-to-carray #'dfloat args))

(defun copy-lisp-array-to-float-carray (&rest args)
  (apply #'copy-lisp-array-converting-to-carray #'sfloat args))

(defun copy-lisp-array-to-rounded-int-carray (&rest args)
  (apply #'copy-lisp-array-converting-to-carray #'round args))

(defun copy-lisp-array-to-truncated-int-carray (&rest args)
  (apply #'copy-lisp-array-converting-to-carray #'truncate args))


#|
(defun simple-integerp (val)
  (and (integerp val)
       (cond ((<= 0 val 255) :BYTE)
             ((<= -32768 val 32767) :SHORT)
             ((<= #x-80000000 val #x7fffffff) :int))))

(defun c-numeric-type (val)
  (and (realp val)
       (if (integerp val)
           (cond ((<= 0 val 255) :BYTE)
                 ((<= -32768 val 32767) :SHORT)
                 ((<= #x-80000000 val #x7fffffff) :int)
                 (t :DOUBLE))
         :DOUBLE)))

(defun c-integer-type-p (type)
  (case type ((:BYTE :SHORT :INT) t)))


(defmethod coerce-to-carray ((obj carray))
  obj)

(defmethod coerce-to-carray ((obj cons))
  (cond ((every #'integerp obj)
         (coerce-integer-list-to-carray obj))
        ((every #'realp obj)
         (coerce-numeric-list-to-carray obj))
        ((some #'complexp obj)
         (coerce-complex-list-to-carray obj)))
        
        
  (let* ((size (length obj))
         (type (if (every #'integerp obj)
                   :int
                 :double))
         (arr  (make-carray type size)))
    (with-row-major-access (p arr)
      (do ((ix 0  (1+ ix))
           (l  obj (cdr l)))
          ((endp l))
        (setf (fli:foreign-aref p ix) (car l))))
    arr))

(defmethod coerce-to-carray ((obj array))
  (let* ((size   (array-total-size obj))
         (shadow (make-array (array-total-size obj) :displaced-to obj))
         (type   (if (every #'integerp shadow)
                     :int
                   :double))
         (arr    (make-carray type (array-dimensions obj))))
    (with-row-major-access (p arr)
      (dotimes (ix size)
        (setf (fli:foreign-aref p ix) (aref shadow ix))))
    arr))

(defmethod coerce-to-carray ((obj number))
  (let ((arr (make-carray :double 1)))
    (with-row-major-access (p arr)
      (setf (fli:foreign-aref p 0) obj))
    arr))

(defmethod coerce-to-carray ((obj integer))
  (let ((arr (make-carray :int 1)))
    (with-row-major-access (p arr)
      (setf (fli:foreign-aref p 0) obj))
    arr))

(defmethod coerce-to-carray ((obj complex))
  (let ((arr (make-carray :double 2)))
    (with-row-major-access (p arr)
      (setf (fli:foreign-aref p 0) (realpart obj)
            (fli:foreign-aref p 1) (imagpart obj)))
    arr))

(defmethod coerce-to-carray ((obj string))
  (let* ((len (length obj))
         (arr (make-carray :char (1+ len))))
    (with-row-major-access (p arr)
      (dotimes (ix len)
        (setf (fli:foreign-aref p ix) (char obj ix)))
      (setf (fli:foreign-aref p len) (code-char 0)))
    arr))


(defun indgen (n)
  (let ((arr (make-carray :int n)))
    (with-row-major-access (p arr)
      (dotimes (ix n)
        (setf (fli:foreign-aref p ix) ix)))
    arr))

|#

#|  ;; some test code...

(defun tst (n)
  (fli:with-dynamic-foreign-objects ()
    (let* ((nel (* 240 320))
           (cx (make-carray :float nel :dynamic t))
           (x  (vm:unoise nel 10)))
      (time
       (dotimes (ix n)
         (dotimes (ix nel)
           (setf (caref cx ix) (float (aref x ix))))
         (dotimes (ix nel)
           (setf (aref x ix) (caref cx ix))))
       ))))

(defun tst2 (n)
  (fli:with-dynamic-foreign-objects ()
    (let* ((nel (* 240 320))
           (cx (make-carray :float nel :dynamic t))
           (cy (make-carray :float nel :dynamic t))
           (x  (vm:unoise nel 10)))
      (dotimes (ix nel)
        (setf (caref cx ix) (float (aref x ix))))
      (time
       (dotimes (ix n)
         (dotimes (ix nel)
           (setf (caref cy ix)
                 (log (abs (caref cx ix)))))
         )))))

(defun tst3 (n)
  (fli:with-dynamic-foreign-objects ()
    (let* ((nel (* 240 320))
           (cx (make-carray :float nel :dynamic t))
           (cy (make-carray :float nel :dynamic t))
           (x  (vm:unoise nel 10))
           (fn #'(lambda (ix v)
                   (setf (caref cy ix)
                         (log (abs v))))))
      (dotimes (ix nel)
        (setf (caref cx ix) (float (aref x ix))))
      (time
       (dotimes (ix n)
         (elementwise fn cx)))
      )))

(defun tst4 (n)
  (fli:with-dynamic-foreign-objects ()
    (let* ((nel (* 240 320))
           (cx (make-carray :float nel :dynamic t))
           (x  (vm:unoise nel 10)))
      (time
       (dotimes (ix n)
         (dotimes (ix nel)
           (setf (caref cx ix) (float (aref x ix))))
         (vm:elementwise (x) (log (abs x)))
         (convert-to-lisp-object cx)))
      )))

|#

(provide "c-arrays")

;; -- end of carray.lsp -- ;;
