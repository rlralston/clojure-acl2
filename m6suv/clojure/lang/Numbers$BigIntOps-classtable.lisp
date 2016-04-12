; Numbers$BigIntOps-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:52 CDT 2014.
;

(include-book "../../model-translation")

(defconst *clojure.lang.Numbers$BigIntOps*
 (make-class-def
      '(class "clojure.lang.Numbers$BigIntOps"
            "clojure.lang.Numbers$OpsP"
            (constant_pool)
            (fields)
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
                    (methodCP "<init>" "clojure.lang.Numbers$OpsP" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "combine"
                              (parameters (class "clojure.lang.Numbers$Ops"))
                              (returntype . (class "clojure.lang.Numbers$Ops"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (aload_0))
                                      (2 (invokeinterface
                    (methodCP "opsWith" "clojure.lang.Numbers$Ops" ((class "clojure.lang.Numbers$BigIntOps")) (class "clojure.lang.Numbers$Ops")) 2))
                                      (7 (areturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "opsWith"
                              (parameters (class "clojure.lang.Numbers$LongOps"))
                              (returntype . (class "clojure.lang.Numbers$Ops"))
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 2)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (areturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap )))
                        (method "opsWith"
                              (parameters (class "clojure.lang.Numbers$DoubleOps"))
                              (returntype . (class "clojure.lang.Numbers$Ops"))
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 4)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "DOUBLE_OPS" "clojure.lang.Numbers" (class "clojure.lang.Numbers$DoubleOps"))))
                                      (3 (areturn))
                                      (endofcode 4))
                                   (Exceptions )
                                   (StackMap )))
                        (method "opsWith"
                              (parameters (class "clojure.lang.Numbers$RatioOps"))
                              (returntype . (class "clojure.lang.Numbers$Ops"))
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 4)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "RATIO_OPS" "clojure.lang.Numbers" (class "clojure.lang.Numbers$RatioOps"))))
                                      (3 (areturn))
                                      (endofcode 4))
                                   (Exceptions )
                                   (StackMap )))
                        (method "opsWith"
                              (parameters (class "clojure.lang.Numbers$BigIntOps"))
                              (returntype . (class "clojure.lang.Numbers$Ops"))
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 2)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (areturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap )))
                        (method "opsWith"
                              (parameters (class "clojure.lang.Numbers$BigDecimalOps"))
                              (returntype . (class "clojure.lang.Numbers$Ops"))
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 4)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "BIGDECIMAL_OPS" "clojure.lang.Numbers" (class "clojure.lang.Numbers$BigDecimalOps"))))
                                      (3 (areturn))
                                      (endofcode 4))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isZero"
                              (parameters (class "java.lang.Number"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 43)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (invokestatic (methodCP "toBigInt" "clojure.lang.Numbers" ((class "java.lang.Object")) (class "clojure.lang.BigInt")))) 
                                      (4 (astore_2)) 
                                      (5 (aload_2)) 
                                      (6 (getfield (fieldCP "bipart" "clojure.lang.BigInt" (class "java.math.BigInteger")))) 
                                      (9 (ifnonnull 27)) ;;to TAG_0
                                      (12 (aload_2)) 
                                      (13 (getfield (fieldCP "lpart" "clojure.lang.BigInt" long))) 
                                      (16 (lconst_0)) 
                                      (17 (lcmp)) 
                                      (18 (ifne 25)) ;;to TAG_1
                                      (21 (iconst_1)) 
                                      (22 (goto 26))  ;;to TAG_2
                                      (25 (iconst_0)) ;;at TAG_1
                                      (26 (ireturn)) ;;at TAG_2
                                      (27 (aload_2)) ;;at TAG_0
                                      (28 (getfield (fieldCP "bipart" "clojure.lang.BigInt" (class "java.math.BigInteger")))) 
                                      (31 (invokevirtual (methodCP "signum" "java.math.BigInteger" () int))) 
                                      (34 (ifne 41)) ;;to TAG_3
                                      (37 (iconst_1)) 
                                      (38 (goto 42)) ;;to TAG_4
                                      (41 (iconst_0)) ;;at TAG_3
                                      (42 (ireturn)) ;;at TAG_4
                                      (endofcode 43))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isPos"
                              (parameters (class "java.lang.Number"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 43)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (invokestatic (methodCP "toBigInt" "clojure.lang.Numbers" ((class "java.lang.Object")) (class "clojure.lang.BigInt")))) 
                                      (4 (astore_2)) 
                                      (5 (aload_2)) 
                                      (6 (getfield (fieldCP "bipart" "clojure.lang.BigInt" (class "java.math.BigInteger")))) 
                                      (9 (ifnonnull 27)) ;;to TAG_0
                                      (12 (aload_2)) 
                                      (13 (getfield (fieldCP "lpart" "clojure.lang.BigInt" long))) 
                                      (16 (lconst_0)) 
                                      (17 (lcmp)) 
                                      (18 (ifle 25)) ;;to TAG_1
                                      (21 (iconst_1)) 
                                      (22 (goto 26))  ;;to TAG_2
                                      (25 (iconst_0)) ;;at TAG_1
                                      (26 (ireturn)) ;;at TAG_2
                                      (27 (aload_2)) ;;at TAG_0
                                      (28 (getfield (fieldCP "bipart" "clojure.lang.BigInt" (class "java.math.BigInteger")))) 
                                      (31 (invokevirtual (methodCP "signum" "java.math.BigInteger" () int))) 
                                      (34 (ifle 41)) ;;to TAG_3
                                      (37 (iconst_1)) 
                                      (38 (goto 42)) ;;to TAG_4
                                      (41 (iconst_0)) ;;at TAG_3
                                      (42 (ireturn)) ;;at TAG_4
                                      (endofcode 43))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isNeg"
                              (parameters (class "java.lang.Number"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 43)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (invokestatic (methodCP "toBigInt" "clojure.lang.Numbers" ((class "java.lang.Object")) (class "clojure.lang.BigInt")))) 
                                      (4 (astore_2)) 
                                      (5 (aload_2)) 
                                      (6 (getfield (fieldCP "bipart" "clojure.lang.BigInt" (class "java.math.BigInteger")))) 
                                      (9 (ifnonnull 27)) ;;to TAG_0
                                      (12 (aload_2)) 
                                      (13 (getfield (fieldCP "lpart" "clojure.lang.BigInt" long))) 
                                      (16 (lconst_0)) 
                                      (17 (lcmp)) 
                                      (18 (ifge 25)) ;;to TAG_1
                                      (21 (iconst_1)) 
                                      (22 (goto 26))  ;;to TAG_2
                                      (25 (iconst_0)) ;;at TAG_1
                                      (26 (ireturn)) ;;at TAG_2
                                      (27 (aload_2)) ;;at TAG_0
                                      (28 (getfield (fieldCP "bipart" "clojure.lang.BigInt" (class "java.math.BigInteger")))) 
                                      (31 (invokevirtual (methodCP "signum" "java.math.BigInteger" () int))) 
                                      (34 (ifge 41)) ;;to TAG_3
                                      (37 (iconst_1)) 
                                      (38 (goto 42)) ;;to TAG_4
                                      (41 (iconst_0)) ;;at TAG_3
                                      (42 (ireturn)) ;;at TAG_4
                                      (endofcode 43))
                                   (Exceptions )
                                   (StackMap )))
                        (method "add"
                              (parameters (class "java.lang.Number") (class "java.lang.Number"))
                              (returntype . (class "java.lang.Number"))
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 12)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (invokestatic
                    (methodCP "toBigInt" "clojure.lang.Numbers" ((class "java.lang.Object")) (class "clojure.lang.BigInt"))))
                                      (4 (aload_2))
                                      (5 (invokestatic
                    (methodCP "toBigInt" "clojure.lang.Numbers" ((class "java.lang.Object")) (class "clojure.lang.BigInt"))))
                                      (8 (invokevirtual
                    (methodCP "add" "clojure.lang.BigInt" ((class "clojure.lang.BigInt")) (class "clojure.lang.BigInt"))))
                                      (11 (areturn))
                                      (endofcode 12))
                                   (Exceptions )
                                   (StackMap )))
                        (method "multiply"
                              (parameters (class "java.lang.Number") (class "java.lang.Number"))
                              (returntype . (class "java.lang.Number"))
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 12)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (invokestatic
                    (methodCP "toBigInt" "clojure.lang.Numbers" ((class "java.lang.Object")) (class "clojure.lang.BigInt"))))
                                      (4 (aload_2))
                                      (5 (invokestatic
                    (methodCP "toBigInt" "clojure.lang.Numbers" ((class "java.lang.Object")) (class "clojure.lang.BigInt"))))
                                      (8 (invokevirtual
                    (methodCP "multiply" "clojure.lang.BigInt" ((class "clojure.lang.BigInt")) (class "clojure.lang.BigInt"))))
                                      (11 (areturn))
                                      (endofcode 12))
                                   (Exceptions )
                                   (StackMap )))
                        (method "divide"
                              (parameters (class "java.lang.Number") (class "java.lang.Number"))
                              (returntype . (class "java.lang.Number"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 12)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (invokestatic
                    (methodCP "toBigInteger" "clojure.lang.Numbers" ((class "java.lang.Object")) (class "java.math.BigInteger"))))
                                      (4 (aload_2))
                                      (5 (invokestatic
                    (methodCP "toBigInteger" "clojure.lang.Numbers" ((class "java.lang.Object")) (class "java.math.BigInteger"))))
                                      (8 (invokestatic
                    (methodCP "divide" "clojure.lang.Numbers" ((class "java.math.BigInteger") (class "java.math.BigInteger")) (class "java.lang.Number"))))
                                      (11 (areturn))
                                      (endofcode 12))
                                   (Exceptions )
                                   (StackMap )))
                        (method "quotient"
                              (parameters (class "java.lang.Number") (class "java.lang.Number"))
                              (returntype . (class "java.lang.Number"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 12)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (invokestatic
                    (methodCP "toBigInt" "clojure.lang.Numbers" ((class "java.lang.Object")) (class "clojure.lang.BigInt"))))
                                      (4 (aload_2))
                                      (5 (invokestatic
                    (methodCP "toBigInt" "clojure.lang.Numbers" ((class "java.lang.Object")) (class "clojure.lang.BigInt"))))
                                      (8 (invokevirtual
                    (methodCP "quotient" "clojure.lang.BigInt" ((class "clojure.lang.BigInt")) (class "clojure.lang.BigInt"))))
                                      (11 (areturn))
                                      (endofcode 12))
                                   (Exceptions )
                                   (StackMap )))
                        (method "remainder"
                              (parameters (class "java.lang.Number") (class "java.lang.Number"))
                              (returntype . (class "java.lang.Number"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 12)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (invokestatic
                    (methodCP "toBigInt" "clojure.lang.Numbers" ((class "java.lang.Object")) (class "clojure.lang.BigInt"))))
                                      (4 (aload_2))
                                      (5 (invokestatic
                    (methodCP "toBigInt" "clojure.lang.Numbers" ((class "java.lang.Object")) (class "clojure.lang.BigInt"))))
                                      (8 (invokevirtual
                    (methodCP "remainder" "clojure.lang.BigInt" ((class "clojure.lang.BigInt")) (class "clojure.lang.BigInt"))))
                                      (11 (areturn))
                                      (endofcode 12))
                                   (Exceptions )
                                   (StackMap )))
                        (method "equiv"
                              (parameters (class "java.lang.Number") (class "java.lang.Number"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 12)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (invokestatic
                    (methodCP "toBigInt" "clojure.lang.Numbers" ((class "java.lang.Object")) (class "clojure.lang.BigInt"))))
                                      (4 (aload_2))
                                      (5 (invokestatic
                    (methodCP "toBigInt" "clojure.lang.Numbers" ((class "java.lang.Object")) (class "clojure.lang.BigInt"))))
                                      (8 (invokevirtual
                    (methodCP "equals" "clojure.lang.BigInt" ((class "java.lang.Object")) boolean)))
                                      (11 (ireturn))
                                      (endofcode 12))
                                   (Exceptions )
                                   (StackMap )))
                        (method "lt"
                              (parameters (class "java.lang.Number") (class "java.lang.Number"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 12)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (invokestatic
                    (methodCP "toBigInt" "clojure.lang.Numbers" ((class "java.lang.Object")) (class "clojure.lang.BigInt"))))
                                      (4 (aload_2))
                                      (5 (invokestatic
                    (methodCP "toBigInt" "clojure.lang.Numbers" ((class "java.lang.Object")) (class "clojure.lang.BigInt"))))
                                      (8 (invokevirtual
                    (methodCP "lt" "clojure.lang.BigInt" ((class "clojure.lang.BigInt")) boolean)))
                                      (11 (ireturn))
                                      (endofcode 12))
                                   (Exceptions )
                                   (StackMap )))
                        (method "negate"
                              (parameters (class "java.lang.Number"))
                              (returntype . (class "java.lang.Number"))
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (invokestatic
                    (methodCP "toBigInteger" "clojure.lang.Numbers" ((class "java.lang.Object")) (class "java.math.BigInteger"))))
                                      (4 (invokevirtual
                    (methodCP "negate" "java.math.BigInteger" () (class "java.math.BigInteger"))))
                                      (7 (invokestatic
                    (methodCP "fromBigInteger" "clojure.lang.BigInt" ((class "java.math.BigInteger")) (class "clojure.lang.BigInt"))))
                                      (10 (areturn))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "inc"
                              (parameters (class "java.lang.Number"))
                              (returntype . (class "java.lang.Number"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 16)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (invokestatic
                    (methodCP "toBigInteger" "clojure.lang.Numbers" ((class "java.lang.Object")) (class "java.math.BigInteger"))))
                                      (4 (astore_2))
                                      (5 (aload_2))
                                      (6 (getstatic (fieldCP "ONE" "java.math.BigInteger" (class "java.math.BigInteger"))))
                                      (9 (invokevirtual
                    (methodCP "add" "java.math.BigInteger" ((class "java.math.BigInteger")) (class "java.math.BigInteger"))))
                                      (12 (invokestatic
                    (methodCP "fromBigInteger" "clojure.lang.BigInt" ((class "java.math.BigInteger")) (class "clojure.lang.BigInt"))))
                                      (15 (areturn))
                                      (endofcode 16))
                                   (Exceptions )
                                   (StackMap )))
                        (method "dec"
                              (parameters (class "java.lang.Number"))
                              (returntype . (class "java.lang.Number"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 16)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (invokestatic
                    (methodCP "toBigInteger" "clojure.lang.Numbers" ((class "java.lang.Object")) (class "java.math.BigInteger"))))
                                      (4 (astore_2))
                                      (5 (aload_2))
                                      (6 (getstatic (fieldCP "ONE" "java.math.BigInteger" (class "java.math.BigInteger"))))
                                      (9 (invokevirtual
                    (methodCP "subtract" "java.math.BigInteger" ((class "java.math.BigInteger")) (class "java.math.BigInteger"))))
                                      (12 (invokestatic
                    (methodCP "fromBigInteger" "clojure.lang.BigInt" ((class "java.math.BigInteger")) (class "clojure.lang.BigInt"))))
                                      (15 (areturn))
                                      (endofcode 16))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))#|ACL2s-ToDo-Line|#



;(defconst *Numbers$BigIntOps-class-table*
;  (make-static-class-decls 
;   *clojure.lang.Numbers$BigIntOps*))

;(defconst *package-name-map* 
;  ("clojure.lang.Numbers$BigIntOps" . "clojure.lang"))

