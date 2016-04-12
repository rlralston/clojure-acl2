(include-book "../mc/mc")
(include-book "java/lang/Number")
(include-book "java/lang/Integer")
(include-book "java/lang/Long")
(include-book "java/math/BigInteger")
(include-book "clojure/lang/BigInt")
(include-book "clojure/lang/Numbers$OpsP")
(include-book "clojure/lang/Numbers$LongOps")
(include-book "clojure/lang/Numbers$BigIntOps")
(include-book "clojure/lang/Numbers")

(in-package "MC")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  The Clojure +' operator is executed as an addP operation that takes 2 
;;  java.lang.Object parameters. The parameters must be of numeric types. For
;;  this project, we are interested in: Integer, Long, BigInt, and BigInteger 
;;  numeric types. The smaller precision parameter is upcast to the larger 
;;  precision. To test, we will need 16 test cases. 
;;
;;     1. Integer, Integer
;;     2. Long, Long
;;     3. BigInt, BigInt
;;     4. BigInteger, BigInteger
;;     5. Integer, Long
;;     6. Integer, BigInt
;;     7. Integer, BigInteger
;;     8. Long, Integer
;;     9. Long, BigInt
;;    10. Long, BigInteger
;;    11. BigInt, Integer
;;    12. BigInt, Long
;;    13. BigInt, BigInteger
;;    14. BigInteger, Integer
;;    15. BigInteger, Long
;;    16. BigInteger, BigInt
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst *addP-heap*
  '((0 . (("java.lang.Class" ("<name>" . "java.math.BigInteger"))
          ("java.lang.Object" ("monitor" . 0) ("mcount" . 0) ("wait-set" . 0))))
    ;(1 . (("ARRAY" ("<array>" *ARRAY* NIL 2 (2147483647 2147483647)))))
    ;(2 . (("ARRAY" ("<array>" *ARRAY* NIL 2 (2147483647 2147483647)))))
    (1 . (("ARRAY" ("<array>" *ARRAY* NIL 2 (3 -1)))))
    (2 . (("ARRAY" ("<array>" *ARRAY* NIL 1 (1)))))
    ;(2 . (("ARRAY" ("<array>" *ARRAY* NIL 2 (-1 -1)))))    
    (3 . (("java.lang.Integer" ("value" . 12))))
    (4 . (("java.lang.Integer" ("value" . 327))))
    (5 . (("java.lang.Long" ("value" . 500))))
    (6 . (("java.lang.Long" ("value" . 1024))))
    (7 . (("java.math.BigInteger" ("signum" . 1)
                                 ("mag" . (REF 1))
                                 ("bitCount" . 0)
                                 ("bitLength" . 0)
                                 ("lowestSetBit" . 0)
                                 ("firstNonzeroIntNum" . 0))
          ("java.lang.Number")))
    (8 . (("java.math.BigInteger" ("signum" . 1)
                                 ("mag" . (REF 2))
                                 ("bitCount" . 0)
                                 ("bitLength" . 0)
                                 ("lowestSetBit" . 0)
                                 ("firstNonzeroIntNum" . 0))
          ("java.lang.Number")))
    ;; The bpart will need to be set to references to bigintegers, but this is 
    ;; the last test case to fix. 
    (9 . (("clojure.lang.BigInt" ("lpart" 0)
                                 ("bpart" nil))))
    (10 . (("clojure.lang.BigInt" ("lpart" 0)
                                  ("bpart" nil))))
    (11 . (("clojure.lang.Numbers$LongOps")))))

(defconst *clinit-state*
  (make-state 
    (push 
     (make-frame 
      0 
      nil       
      nil
      '((INVOKESTATIC "clojure.lang.Numbers" "<clinit>" 0)
        (INVOKESTATIC "java.math.BigInteger" "<clinit>" 0))      
      "clojure.lang.Numbers")
     nil)
    *addP-heap*
    (make-class-def (list *java.lang.Number* *java.lang.Integer* *java.lang.Long* *clojure.lang.BigInt* *clojure.lang.Numbers$OpsP* *clojure.lang.Numbers$LongOps* *clojure.lang.Numbers$BigIntOps* *java.math.BigInteger* *clojure.lang.Numbers*))))

(defconst *base-state* 
  (run-to-return 0 1500 (m5_load *clinit-state*)))

(defconst *BigIntegerState*
  (make-state 
    (push 
     (make-frame 
      0 
      nil       
      nil
      '((HALT))      
      "java.math.BigInteger")
     nil)
    nil
    (make-class-def (list *java.math.BigInteger*))))

(defun init-state () 
  (m5_load *BigIntegerState*))    

(defthm init-BigInteger-1
  (implies (and (equal (next-inst jstate)
                       ;'(invokestatic "java.math.BigInteger" "add_ia.ia" 0))
                       '(invokestatic "java.math.BigInteger" "<init>" 3))
                (equal (class-table jstate) 
                       (class-table (init-state))))
           (equal (next-inst (step jstate))
                  '(ALOAD_0))))

(defun new-step-n (n s)
  (if (zp n)
    s
    (new-step-n (- n 1) (step s))))#|ACL2s-ToDo-Line|#


(defthm init-BigInteger-1-p2
  (implies (and (equal (next-inst jstate)
                       ;'(invokestatic "java.math.BigInteger" "add_ia.ia" 0))
                       '(invokestatic "java.math.BigInteger" "<init>" 3))
                (equal (class-table jstate) 
                       (class-table (init-state))))
           (equal (next-inst (new-step-n 1 jstate))
                  '(ALOAD_0))))
    
(defthm init-BigInteger-1
  (let* ((stack (stack (top-frame jstate)))
         (x2 (top stack))
         (x1 (top (pop stack)))
         (x0 (top (pop (pop stack)))))
  (implies (and (pre-<init> jstate x0 x1 x2)
                (equal (class-table jstate) 
                       (class-table (init-state))))
           (equal (next-inst (step-n 1 jstate))
                  '(ALOAD_0)))))
                  ;'(invokestatic "java.math.BigInteger" "<init>" 3)))))
    

(defconst *makearray*
  (make-state 
    (push 
     (make-frame 
      0 
      nil 
      (push 7 nil)
      '((newarray))       
      "clojure.lang.Numbers")
     nil)
    (heap *base-state*)
    (class-table *base-state*)))

(defconst *make-new-BigInteger*
  (make-state 
    (push 
     (make-frame 
      0       
      (push '(REF 1) (push 0 nil))
      nil
      '((new "java.math.BigInteger")                           
        (dup)                                       
        (aload_0)
        (iload_1)
        (invokespecial "java.math.BigInteger" "<init>" 2))       
      "clojure.lang.Numbers")
     nil)
    (heap *base-state*)
    (class-table *base-state*)))

(defconst *addp-Integer.Integer*
  (make-state 
    (push 
     (make-frame 
      0 
      nil 
      (push '(REF 3) (push '(REF 4) nil)) 
      '((INVOKESTATIC "clojure.lang.Numbers" "addP" 2))       
      "clojure.lang.Numbers")
     nil)
    (heap *base-state*)
    (class-table *base-state*)))

(defconst *addp-BigInteger.BigInteger*
  (make-state 
    (push 
     (make-frame 
      0 
      nil 
      (push '(REF 7) (push '(REF 8) nil)) 
      '((INVOKESTATIC "clojure.lang.Numbers" "addP" 2))       
      "clojure.lang.Numbers")
     nil)
    (heap *base-state*)
    (class-table *base-state*)))

(defconst *add-arrays*
  (make-state 
    (push 
     (make-frame 
      0 
      nil 
      (push '(REF 1) (push '(REF 2) nil)) 
      '((INVOKESTATIC "java.math.BigInteger" "add_ia.ia" 2))       
      "java.math.BigInteger")
     nil)
    (heap *base-state*)
    (class-table *base-state*)))

(defun inst-= (inst1 inst2)
  (equal inst1 inst2))

(defun refType? (ref heap type)
  (equal (class-name-of-ref ref heap) type))

(defun poised-for-execute-add (s)
  (and
   ; Next instruction is BigInt:add
   (inst-= (next-inst s)
           '(invokevirtual "clojure.lang.BigInt" "add" 1))
   ; Top 2 stack elements are Refs to BigInts
   (refType? (top (stack (top-frame s))) (heap s) "clojure.lang.BigInt")
   (refType? (top (pop (stack (top-frame s)))) (heap s) "clojure.lang.BigInt")))
      
(defun refp (r)
  (and
   (consp r)
   (equal (car r) 'REF)))

;(defun fieldValue (ref heap class-name field-name)
;  (field-value class-name field-name (instance ref heap)))

(defun jmBigIntegerp (ref heap)
  (refType? ref heap "java.math.BigInteger"))

(defun coerce-reasnum (iarr idx)
  (if (endp iarr)
    0
    (+ (ash (logand (car iarr) 4294967295) 
            (* 32 idx))
       (coerce-reasnum (cdr iarr) (1- idx)))))

(defun jmBigIntegerValue (ref heap)
  (let* ((bi (deref ref heap))
         (signum (field-value "java.math.BigInteger" "signum" bi))
         (magref (field-value "java.math.BigInteger" "mag" bi))
         (array (deref magref heap))
         (ridx (array-bound array))
         (iarr (array-data array)))
    (if (zp signum)
      0
      (* signum (+ (coerce-reasnum iarr (1- ridx)))))))
      
(defun intArrayp (ref heap)
  (refType? ref heap "ARRAY"))

(defconst *BigIntegerState*
  (make-state 
    (push 
     (make-frame 
      0 
      nil       
      nil
      '((HALT))      
      "java.math.BigInteger")
     nil)
    nil
    (make-class-def (list *java.math.BigInteger*))))

(defun init-state () 
  (m5_load *BigIntegerState*))

(defun reasnum-iarr (iarr)
  (coerce-reasnum iarr (length iarr)))

; Since add always swaps xs and ys so that xs is longer, 
; this function is logically equivalent by terminating on decreasing xs.
; 
(defun add-lendian-iarr (xs ys carry)
  (let* 
    ((x (logand (int-fix (car xs)) #xffffffff))
     (y (logand (int-fix (car ys)) #xffffffff))
     (sum (+ x y carry)))
    (if (endp xs)
      (list carry)
      (cons (int-fix sum)
            (add-lendian-iarr (cdr xs)
                              (cdr ys)                      
                              (ash sum -32))))))

(defun coerce-lendian-reasnum (iarr idx)
  (if (endp iarr)
    0
    (+ (ash (logand (int-fix (car iarr)) #xffffffff) 
            (* 32 idx))
       (coerce-lendian-reasnum (cdr iarr) (1+ idx)))))

;(include-book "arithmetic/natp-posp" :dir :system)
(include-book "arithmetic/top-with-meta" :dir :system)

(defthm distributivity-of-ash
  (implies
   (and (integerp x)
        (integerp y)
        (posp i))
   (equal 
    (ash (+ x y) i)
    (+ (ash x i) (ash y i)))))

(defun add-lendian-iarr-2 (xs ys carry)
  (let* 
    ((x (uint-fix (car xs)))
     (y (uint-fix (car ys)))
     (sum (+ x y carry)))
    (if (endp xs)
      (list carry)
      (cons (int-fix sum)
            (add-lendian-iarr-2 (cdr xs)
                              (cdr ys)                      
                              (ash sum -32))))))

(defthm ash-is-ordinal
  (implies (posp x)
  (o-p (ash x y))))

(defthm ash-is-decreasing-ordinal-helper
  (implies 
   (and (posp x)
        (integerp y)
        (< y 0))
  (o< (* x (expt 2 y)) x)))

(defthm ash-is-integerp
  (integerp (ash x y)))

;(include-book "arithmetic/natp-posp" :dir :system)

(defthm floor-half-decreases-1
  (implies (posp x)
           (o< (* 1/2 x) x)))

(include-book "arithmetic-3/top" :dir :system)

(defthm floor-half-decreases-2
  (implies (posp x)
           (o< (floor (* 1/2 x) 1) x)))

(defun negp (x) 
  (not (natp x)))

(defthm floor-half-decreases
  (implies (posp x)
           (o< (floor (* x (expt 2 -32)) 1) x)))

(defthm ash-is-decreasing-ordinal
  (implies (posp x)
  (o< (ash x -32) x)))


;(defthm ash-decreases-x
  ;(implies (natp x)
   ;        (<= (ash x -32) x))
  ;:hints (("Goal" :in-theory (disable ash floor nonnegative-integer-quotient))))

(defun expand-int (x)
  ; add guard that x is positive
  (let* ((lsi (int-fix x)))
    (if (<= x 2147483647)
      (list x)
      (cons lsi
            (expand-int (ash x -32))))))

(defthm int-fix-range
  (implies (integerp x)
           (>= (int-fix x) -2147483648))) 

(defthm int-fix-range-2
  (implies (integerp x)
           (<= (int-fix x) 2147483647))) 



(defthm ash-always-positive
  (implies (integerp x) 
           (natp (logand (int-fix x) #xffffffff))))

(defthm coerce-lendian-is-positive
  (implies (integer-listp xs)
           (posp (coerce-lendian-reasnum xs 0))))



(defthm add-lendian-is-correct
  (implies 
   (and (integer-listp xs)           
        (integer-listp ys)
        (= (length ys) (length xs))
        (= x (coerce-lendian-reasnum xs 0))
        (= y (coerce-lendian-reasnum ys 0))
        (posp x)
        (posp y))
   (equal 
    (coerce-lendian-reasnum (add-lendian-iarr-2 xs ys 0) 0)
    (+ x y))))
           
           


(defun add-iarr (xs ys carry)
  (let* 
    ((x (logand (int-fix (car xs)) #xffffffff))
     (y (logand (int-fix (car ys)) #xffffffff))
     (sum (+ x y carry)))
    (cond 
     ((and (integer-listp xs) (integer-listp ys))      
      (cons (int-fix sum)
            (add-iarr (cdr xs)
                      (cdr ys)
                      (ash sum -32))))
     ((integer-listp xs)
      (cons (int-fix sum)
            (add-iarr (cdr xs)
                      nil
                      (ash sum -32))))
     ((integer-listp ys)
      (cons (int-fix sum)
            (add-iarr nil
                      (cdr ys)
                      (ash sum -32))))     
      (t carry))))

;(in-theory (disable init-state (init-state)))

; Loop Invariant 2 java.bigInteger.add(int[], int[]
;     x0: The value of the x parameter at method invocation
;     y0: The value of the y parameter at method invocation
;      x: The value of x which shall be either x0 or y0 depending on the swap
; xIndex: The loop variant, which is decremented 
; result: The value of result

; <code>
;        while (xIndex > 0)
;            result[--xIndex] = x[xIndex];
; <code>
(defun R2 (x0 y0 x xIndex result)
  (and (or (equal x x0)
           (equal x y0))
       (intp xIndex)       
       (if (< 0 xIndex)
         (equal (nth xIndex result)
                (nth xIndex x))
         )))


; Postcondition java.BigInteger.add(int[], int[])
;     x0: The value of the x parameter at method invocation
;     y0: The value of the y parameter at method invocation
; result: The value of result
; 
;   reasval(result) = reasval(x0) + reasval(y0)
(defun Q (x0 y0 result) 
  (equal (reasnum-iarr result)
         (+ (reasnum-iarr x0)
            (reasnum-iarr y0))))

(defthm jmBigInteger-add-arrays-is-correct
  (implies
   (and
    ; Poised to execute add(int[], int[])
    (equal (next-inst s)
           '(invokestatic "java.math.BigInteger" "add_ia.ia" 2))
    (equal (class-table s) (class-table (init-state)))
    ; Top 2 stack elements are int array references
     (intArrayp (top (stack (top-frame s))) (heap s))
     (intArrayp (top (pop (stack (top-frame s)))) (heap s))
    )
    ; Execute state results in int array reference on stack
    (equal (next-inst (step-n 1 s)) '(ALOAD_0))))

;(defthm add-is-correct
;  (implies 
;    (and 
;     (poised-for-execute-add s)
;     ; The instance and parameter references 
;     (refp i)     
;     (refp p)     
;     (equal i (top (stack (top-frame s))))
;     (equal p (top (pop (stack (top-frame s)))))     
;     ; Neither is a big integer (fits in a long)
;     (equal (field-value "clojure.lang.BigInt" "bpart" (deref i (heap s)))
;            nil)
;     (equal (field-value "clojure.lang.BigInt" "bpart" (deref p (heap s)))
;            nil)
;     ; Define the long portions
;     (integerp x)
;     (integerp y)
;     (equal (field-value "clojure.lang.BigInt" "lpart" (deref i (heap s)))
;            x)
;     (equal (field-value "clojure.lang.BigInt" "lpart" (deref p (heap s)))
;            y))    
;        
;    (equal 
;     (field-value 
;      "clojure.lang.BigInt" 
;      "lpart"
;      (deref 
;       (top (stack (top-frame (step-n 22 s))))
;       (heap (step-n 22 s))))
;     (+ x y))))
     


; Arrays 1, 1: (deref '(REF 63) (heap (step-n 267 *addp-BigInteger.BigInteger*)))
; Arrays 1, 2: (deref '(REF 63) (heap (step-n 276 *addp-BigInteger.BigInteger*)))

; current step: (next-inst (step-n 409 *addp-BigInteger.BigInteger*))

; (deref '(REF 92) (heap (step-n 91 *add-arrays*)))
; (state-pp (step-n 687 *addp-BigInteger.BigInteger*))