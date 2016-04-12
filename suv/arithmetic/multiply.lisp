(include-book "../../mc/mc")
(include-book "../java/lang/Number")
(include-book "../java/lang/Integer")
(include-book "../java/lang/Long")
(include-book "../java/math/BigInteger")
(include-book "../clojure/lang/BigInt")
(include-book "../clojure/lang/Numbers$OpsP")
(include-book "../clojure/lang/Numbers$LongOps")
(include-book "../clojure/lang/Numbers$BigIntOps")
(include-book "../clojure/lang/Numbers")

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
    (1 . (("ARRAY" ("<array>" *ARRAY* NIL 2 (-1 -1)))))
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

(defconst *multiplyp-Integer.Integer*
  (make-state 
    (push 
     (make-frame 
      0 
      nil 
      (push '(REF 3) (push '(REF 4) nil)) 
      '((INVOKESTATIC "clojure.lang.Numbers" "multiplyP" 2))       
      "clojure.lang.Numbers")
     nil)
    (heap *base-state*)
    (class-table *base-state*)))

(defconst *multiplyp-BigInteger.BigInteger*
  (make-state 
    (push 
     (make-frame 
      0 
      nil 
      (push '(REF 7) (push '(REF 8) nil)) 
      '((INVOKESTATIC "clojure.lang.Numbers" "multiplyP" 2))       
      "clojure.lang.Numbers")
     nil)
    (heap *base-state*)
    (class-table *base-state*)))#|ACL2s-ToDo-Line|#


;; (state-pp (step-n 572 *multiplyp-BigInteger.BigInteger*))