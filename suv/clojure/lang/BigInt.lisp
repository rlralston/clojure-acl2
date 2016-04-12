#|$ACL2s-Preamble$;
(include-book "../../../mc/mc")

(acl2::begin-book t :ttags :all);$ACL2s-Preamble$|#


(in-package "MC")

(defconst *clojure.lang.BigInt*
  (make-class-decl
   "clojure.lang.BigInt"  ; class name
   '("java.lang.Number" "java.lang.Object")  ; superclass
   ; interfaces
   '()   
   '("lpart" "bipart")    ; fields
   '()                    ; static fields
   '((LONG 4294967295)) ; constant pool (Seems to be numbered by index)                            
   (list ; methods
    '("<init>" (long java.lang.BigInteger)                           
      (ALOAD_0)                                     ; 0: aload_0
      (INVOKESPECIAL "java.lang.Number" "<init>" 0)
      (ALOAD_0)                                     ; (4 (aload_0))
      (LLOAD_1)                                     ; (5 (lload_1))
      (PUTFIELD "clojure.lang.BigInt" "lpart" t)    
      (ALOAD_0)                                     ; (9 (aload_0))
      (ALOAD_3)                                     ; (10 (aload_3))
      (PUTFIELD "clojure.lang.BigInt" "bipart" nil)  
      (RETURN))                                     ; (14 (return))
    
    '("fromLong" (long)                           
      (NEW "clojure.lang.BigInt") ; (0 (new (class "clojure.lang.BigInt")))
      (DUP)                       ; (3 (dup))
      (LLOAD_0)                   ; (4 (lload_0))
      (ACONST_NULL)               ; (5 (aconst_null))
      (INVOKESPECIAL "clojure.lang.BigInt" "<init>" 3)
      (ARETURN))                  ; (9 (areturn))      
      
    '("bitLength" ()                           
      (ALOAD_0) ; (0 (aload_0))
      (INVOKEVIRTUAL "clojure.lang.BigInt" "toBigInteger" 0)
      (INVOKEVIRTUAL "clojure.lang.BigInt" "bitLength" 0)
      (IRETURN)) ; (7 (ireturn))
    
    '("valueOf" (long)                           
      (NEW "clojure.lang.BigInt")
      (DUP)                       ; (3 (dup))
      (LLOAD_0)                   ; (4 (lload_0))
      (ACONST_NULL)               ; (5 (aconst_null))
      (INVOKESPECIAL "clojure.lang.BigInt" "<init>" 3) 
      (ARETURN))                  ; (9 (areturn))
      
    '("toBigInteger" ()                           
      (ALOAD_0)                   ; (0 (aload_0)) 
      (GETFIELD "clojure.lang.BigInt" "bipart" nil) 
      (IFNONNULL 11)              ; (4 (ifnonnull 15))  ;;to TAG_0
      (ALOAD_0)                   ; (7 (aload_0)) 
      (GETFIELD "clojure.lang.BigInt" "lpart" 1) ; (8 (getfield (fieldCP "lpart" "clojure.lang.BigInt" long))) 
      (INVOKESTATIC "java.math.BigInteger" "valueOf" 2) 
      (ARETURN)                   ; (14 (areturn)) 
      (ALOAD_0)                   ; (15 (aload_0)) ;;at TAG_0
      (GETFIELD "clojure.lang.BigInt" "bipart" nil) 
      (ARETURN))                  ; (19 (areturn))
      
    '("fromBigInteger" (java.math.BigInteger)                           
      (ALOAD_0)                                            ; 0 : aload_0 
      (INVOKEVIRTUAL "java.math.BigInteger" "bitLength" 0)
      (BIPUSH 64)                                          ; 4 : bipush 64 
      (IF_ICMPGE 16)                                       ; 6 : if_icmpge 22  ;;to TAG_0
      (NEW "clojure.lang.BigInt") 
      (DUP)                                                ; 12: dup 
      (ALOAD_0)                                            ; 13: aload_0 
      (INVOKEVIRTUAL "java.math.BigInteger" "longValue" 0)  
      (ACONST_NULL)                                        ; 17: aconst_null 
      (INVOKESPECIAL "clojure.lang.BigInt" "<init>" 3) 
      (ARETURN)                                            ; 21: areturn 
      (NEW "clojure.lang.BigInt")                          ; 22: new (class "clojure.lang.BigInt") ;;at TAG_0
      (DUP)                                                ; 25: dup 
      (LCONST_0)                                           ; 26: lconst_0 
      (ALOAD_0)                                            ; 27: aload_0 
      (INVOKESPECIAL "clojure.lang.BigInt" "<init>" 3) 
      (ARETURN))                                           ; 31: areturn 

    '("add" (clojure.lang.BigInt)                           
      (aload_0)                                               ; 0 : aload_0 
      (getfield "clojure.lang.BigInt" "bipart" nil)           ; 1 : getfield "bipart" "clojure.lang.BigInt" (class "java.math.BigInteger") 
      (ifnonnull 47)                                          ; 4 : ifnonnull 51  ;;to TAG_0
      (aload_1)                                               ; 7 : aload_1 
      (getfield "clojure.lang.BigInt" "bipart" nil)           ; 8 : getfield "bipart" "clojure.lang.BigInt" (class "java.math.BigInteger") 
      (ifnonnull 40)                                          ; 11: ifnonnull 51  ;;to TAG_0
      (aload_0)                                               ; 14: aload_0 
      (getfield "clojure.lang.BigInt" "lpart" t)              ; 15: getfield "lpart" "clojure.lang.BigInt" long 
      (aload_1)                                               ; 18: aload_1 
      (getfield "clojure.lang.BigInt" "lpart" t)              ; 19: getfield "lpart" "clojure.lang.BigInt" long 
      (ladd)                                                  ; 22: ladd 
      (lstore_2)                                              ; 23: lstore_2 
      (lload_2)                                               ; 24: lload_2 
      (aload_0)                                               ; 25: aload_0 
      (getfield "clojure.lang.BigInt" "lpart" t)              ; 26: getfield "lpart" "clojure.lang.BigInt" long) 
      (lxor)                                                  ; 29: lxor 
      (lconst_0)                                              ; 30: lconst_0 
      (lcmp)                                                  ; 31: lcmp 
      (ifge 14)                                               ; 32: ifge 46 ;;to TAG_1
      (lload_2)                                               ; 35: lload_2 
      (aload_1)                                               ; 36: aload_1 
      (getfield "clojure.lang.BigInt" "lpart" t)              ; 37: getfield (fieldCP "lpart" "clojure.lang.BigInt" long) 
      (lxor)                                                  ; 40: lxor 
      (lconst_0)                                              ; 41: lconst_0 
      (lcmp)                                                  ; 42: lcmp 
      (iflt 8)                                                ; 43: iflt 51  ;;to TAG_0
      (lload_2)                                               ; 46: lload_2 ;;at TAG_1
      (invokestatic "clojure.lang.BigInt" "valueOf" 2)        ; 47: invokestatic "valueOf" "clojure.lang.BigInt" (long) (class "clojure.lang.BigInt") 
      (areturn)                                               ; 50: areturn 
      (aload_0)                                               ; 51: aload_0 ;;at TAG_0
      (invokevirtual "clojure.lang.BigInt" "toBigInteger" 0)  ; 52: invokevirtual "toBigInteger" "clojure.lang.BigInt" () (class "java.math.BigInteger") 
      (aload_1)                                               ; 55: aload_1 
      (invokevirtual "clojure.lang.BigInt" "toBigInteger" 0)  ; 56: invokevirtual "toBigInteger" "clojure.lang.BigInt" () (class "java.math.BigInteger") 
      (invokevirtual "java.math.BigInteger" "add" 1)          ; 59: invokevirtual "add" "java.math.BigInteger" ((class "java.math.BigInteger")) (class "java.math.BigInteger") 
      (invokestatic "clojure.lang.BigInt" "fromBigInteger" 1) ; 62: invokestatic "fromBigInteger" "clojure.lang.BigInt" ((class "java.math.BigInteger")) (class "clojure.lang.BigInt") 
      (areturn))                                              ; 65: areturn 
    
    '("multiply" (clojure.lang.BigInt)
      (ALOAD_0)
      (GETFIELD "clojure.lang.BigInt" "bipart" NIL)
      (IFNONNULL 48)
      (ALOAD_1)
      (GETFIELD "clojure.lang.BigInt" "bipart" NIL)
      (IFNONNULL 41)
      (ALOAD_0)
      (GETFIELD "clojure.lang.BigInt" "lpart" T)
      (ALOAD_1)
      (GETFIELD "clojure.lang.BigInt" "lpart" T)
      (LMUL)
      (LSTORE_2)
      (ALOAD_1)
      (GETFIELD "clojure.lang.BigInt" "lpart" T)
      (LCONST_0)
      (LCMP)
      (IFEQ 17)
      (LLOAD_2)
      (ALOAD_1)
      (GETFIELD "clojure.lang.BigInt" "lpart" T)
      (LDIV)
      (ALOAD_0)
      (GETFIELD "clojure.lang.BigInt" "lpart" T)
      (LCMP)
      (IFNE 8)
      (LLOAD_2)
      (INVOKESTATIC "clojure.lang.BigInt" "valueOf" 2)
      (ARETURN)
      (ALOAD_0)
      (INVOKEVIRTUAL "clojure.lang.BigInt" "toBigInteger" 0)
      (ALOAD_1)
      (INVOKEVIRTUAL "clojure.lang.BigInt" "toBigInteger" 0)
      (INVOKEVIRTUAL "java.math.BigInteger" "multiply" 1)
      (INVOKESTATIC "clojure.lang.BigInt" "fromBigInteger" 1)
      (ARETURN))
    
    )
  '(REF -1)))