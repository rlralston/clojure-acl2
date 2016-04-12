#|$ACL2s-Preamble$;
(include-book "../../../mc/mc")

(acl2::begin-book t :ttags :all);$ACL2s-Preamble$|#


(in-package "MC")

(defconst *clojure.lang.Numbers*
  (make-class-decl
   "clojure.lang.Numbers"     ; class name
   '("java.lang.Object")  ; superclass
   ; interfaces
   '()   
   '()    ; fields
   '("LONG_OPS" "BIGINT_OPS")    ; static fields
   '((STRING (REF -1) "Divide by zero")
     (DOUBLE "9.223372036854776E18")
     (DOUBLE "-9.223372036854776E18")
     (STRING (REF -1) "bit operation not supported for: ")
     (STRING (REF -1) "integer overflow")
     (LONG -1)
     (LONG -9223372036854775808)
     (LONG 9223372036854775807)
     
     ; The following are constant pool names for classes that are 
     ; compared to result of getClass
     (CLASS (REF -1) "java.lang.Long")
     (CLASS (REF -1) "java.lang.Double")
     (CLASS (REF -1) "java.lang.Integer")
     (CLASS (REF -1) "java.lang.Float")
     (CLASS (REF -1) "clojure.lang.BigInt")
     (CLASS (REF -1) "java.math.BigInteger")
     (CLASS (REF -1) "clojure.lang.Ratio")
     (CLASS (REF -1) "java.math.BigDecimal")) ; constant pool (Seems to be numbered by index)                            
   (list ; methods

    '("<clinit>" ()                          
      (new "clojure.lang.Numbers$LongOps")                        ; 0 : new (class "clojure.lang.Numbers$LongOps")
      (dup)                                                       ; 3 : dup
      (invokespecial "clojure.lang.Numbers$LongOps" "<init>" 0)   ; 4 : invokespecial (methodCP "<init>" "clojure.lang.Numbers$LongOps" () void)
      (putstatic "clojure.lang.Numbers" "LONG_OPS" nil)           ; 7 : putstatic (fieldCP "LONG_OPS" "clojure.lang.Numbers" (class "clojure.lang.Numbers$LongOps")))
      ; (10 (new (class "clojure.lang.Numbers$DoubleOps")))
      ; (13 (dup))
      ; (14 (invokespecial (methodCP "<init>" "clojure.lang.Numbers$DoubleOps" () void)))
      ; (17 (putstatic (fieldCP "DOUBLE_OPS" "clojure.lang.Numbers" (class "clojure.lang.Numbers$DoubleOps"))))
      ; (20 (new (class "clojure.lang.Numbers$RatioOps")))
      ; (23 (dup))
      ; (24 (invokespecial (methodCP "<init>" "clojure.lang.Numbers$RatioOps" () void)))
      ; (27 (putstatic (fieldCP "RATIO_OPS" "clojure.lang.Numbers" (class "clojure.lang.Numbers$RatioOps"))))
      (new "clojure.lang.Numbers$BigIntOps")                      ; 30: new (class "clojure.lang.Numbers$BigIntOps")
      (dup)                                                       ; 33: dup
      (invokespecial "clojure.lang.Numbers$BigIntOps" "<init>" 0) ; 34: invokespecial (methodCP "<init>" "clojure.lang.Numbers$BigIntOps" () void)
      (putstatic "clojure.lang.Numbers" "BIGINT_OPS" nil)         ; 37: putstatic (fieldCP "BIGINT_OPS" "clojure.lang.Numbers" (class "clojure.lang.Numbers$BigIntOps")))
      ; (40 (new (class "clojure.lang.Numbers$BigDecimalOps")))
      ; (43 (dup))
      ; (44 (invokespecial (methodCP "<init>" "clojure.lang.Numbers$BigDecimalOps" () void)))
      ; (47 (putstatic (fieldCP "BIGDECIMAL_OPS" "clojure.lang.Numbers" (class "clojure.lang.Numbers$BigDecimalOps"))))
      ; (50 (return))
      (RETURN))                                     ; 50: return
    
    '("<init>" ()                          
      (ALOAD_0)                                     ; 0: aload_0
      (INVOKESPECIAL "java.lang.Object" "<init>" 0)
      (RETURN))                                     ; (4 (return))
    
    '("ops" (java.lang.Object)                          
      (ALOAD_0)                                                ; 0 : aload_0
      (INVOKEVIRTUAL "java.lang.Object" "getClass" 0)          ; 1 : invokevirtual "getClass" "java.lang.Object" 
      (ASTORE_1)                                               ; 4 : astore_1 
      (ALOAD_1)                                                ; 5 : aload_1 
      (LDC_W 8)                                                ; 6 : ldc_w  
      (IF_ACMPNE 7)                                            ; 9 : if_acmpne 16 ;;to TAG_0
      (GETSTATIC "clojure.lang.Numbers" "LONG_OPS" nil)        ; 12: getstatic "LONG_OPS" "clojure.lang.Numbers" 
      (ARETURN)                                                ; 15: areturn 
      (ALOAD_1)                                                ; 16: aload_1 ;;at TAG_0
      (LDC_W 9)                                                ; 17: ldc_w  
      (IF_ACMPNE 7)                                            ; 20: if_acmpne 27 ;;to TAG_1
      (GETSTATIC "clojure.lang.Numbers" "DOUBLE_OPS" nil)      ; 23: getstatic "DOUBLE_OPS" "clojure.lang.Numbers" 
      (ARETURN)                                                ; 26: areturn 
      (ALOAD_1)                                                ; 27: aload_1 ;;at TAG_1
      (LDC_W 10)                                               ; 28: ldc_w  
      (IF_ACMPNE 7)                                            ; 31: if_acmpne 38 ;;to TAG_2
      (GETSTATIC "clojure.lang.Numbers" "LONG_OPS" nil)        ; 34: getstatic "LONG_OPS" "clojure.lang.Numbers" 
      (ARETURN)                                                ; 37: areturn 
      (ALOAD_1)                                                ; 38: aload_1 ;;at TAG_2
      (LDC_W 11)                                               ; 39: ldc_w 
      (IF_ACMPNE 7)                                            ; 42: if_acmpne 49 ;;to TAG_3
      (GETSTATIC "clojure.lang.Numbers" "DOUBLE_OPS" nil)      ; 45: getstatic "DOUBLE_OPS" "clojure.lang.Numbers" 
      (ARETURN)                                                ; 48: areturn 
      (ALOAD_1)                                                ; 49: aload_1 ;;at TAG_3
      (LDC_W 12)                                               ; 50: ldc_w  
      (IF_ACMPNE 7)                                            ; 53: if_acmpne 60 ;;to TAG_4
      (GETSTATIC "clojure.lang.Numbers" "BIGINT_OPS" nil)      ; 56: getstatic "BIGINT_OPS" "clojure.lang.Numbers" 
      (ARETURN)                                                ; 59: areturn 
      (ALOAD_1)                                                ; 60: aload_1 ;;at TAG_4
      (LDC_W 13)                                               ; 61: ldc_w  
      (IF_ACMPNE 7)                                            ; 64: if_acmpne 71 ;;to TAG_5
      (GETSTATIC "clojure.lang.Numbers" "BIGINT_OPS" nil)      ; 67: getstatic "BIGINT_OPS" "clojure.lang.Numbers" 
      (ARETURN)                                                ; 70: areturn 
      (ALOAD_1)                                                ; 71: aload_1 ;;at TAG_5
      (LDC_W 14)                                               ; 72: ldc_w 
      (IF_ACMPNE 7)                                            ; 75: if_acmpne 82 ;;to TAG_6
      (GETSTATIC "clojure.lang.Numbers" "RATIO_OPS" nil)       ; 78: getstatic "RATIO_OPS" "clojure.lang.Numbers" 
      (ARETURN)                                                ; 81: areturn 
      (ALOAD_1)                                                ; 82: aload_1 ;;at TAG_6
      (LDC_W 15)                                               ; 83: ldc_w 
      (IF_ACMPNE 7)                                            ; 86: if_acmpne 93 ;;to TAG_7
      (GETSTATIC "clojure.lang.Numbers" "BIGDECIMAL_OPS" nil)  ; 89: getstatic "BIGDECIMAL_OPS" "clojure.lang.Numbers"  
      (ARETURN)                                                ; 92: areturn 
      (GETSTATIC "clojure.lang.Numbers" "LONG_OPS" nil)        ; 93: getstatic "LONG_OPS" "clojure.lang.Numbers" ;;at TAG_7
      (ARETURN))                                               ; 96: areturn 
    
    '("addP" (java.lang.Object java.lang.Object)                          
      (ALOAD_0)                                                ; 0 : aload_0    
      (INVOKESTATIC "clojure.lang.Numbers" "ops" 1)            ; 1 : invokestatic "ops" "clojure.lang.Numbers"
      (ALOAD_1)                                                ; 4 : aload_1
      (INVOKESTATIC "clojure.lang.Numbers" "ops" 1)            ; 5 : invokestatic "ops" "clojure.lang.Numbers"
      (INVOKEINTERFACE "clojure.lang.Numbers$Ops" "combine" 1) ; 8 : invokeinterface "combine" "clojure.lang.Numbers$Ops"
      (ALOAD_0)                                                ; 13: aload_0
      (CHECKCAST "java.lang.Number")                           ; 14: checkcast "java.lang.Number"
      (ALOAD_1)                                                ; 17: aload_1
      (CHECKCAST "java.lang.Number")                           ; 18: checkcast "java.lang.Number"
      (INVOKEINTERFACE "clojure.lang.Numbers$Ops" "addP" 2)    ; 21: invokeinterface "addP" "clojure.lang.Numbers$Ops"
      (ARETURN))                                               ; 26: areturn      
      
    '("minusP" (java.lang.Object java.lang.Object)
      (ALOAD_1)
      (INVOKESTATIC "clojure.lang.Numbers" "ops" 1)
      (ASTORE_2)
      (ALOAD_2)
      (ALOAD_1)
      (CHECKCAST "java.lang.Number")
      (INVOKEINTERFACE "clojure.lang.Numbers$Ops" "negateP" 1)
      (ASTORE_3)
      (ALOAD_3)
      (INVOKESTATIC "clojure.lang.Numbers" "ops" 1)
      (ASTORE 4)
      (ALOAD_0)
      (INVOKESTATIC "clojure.lang.Numbers" "ops" 1)
      (ALOAD 4)
      (INVOKEINTERFACE "clojure.lang.Numbers$Ops" "combine" 1)
      (ALOAD_0)
      (CHECKCAST "java.lang.Number")
      (ALOAD_3)
      (INVOKEINTERFACE "clojure.lang.Numbers$Ops" "addP" 2)
      (ARETURN))    
   
    '("multiplyP" (java.lang.Object java.lang.Object)
      (ALOAD_0)
      (INVOKESTATIC "clojure.lang.Numbers" "ops" 1)
      (ALOAD_1)
      (INVOKESTATIC "clojure.lang.Numbers" "ops" 1)
      (INVOKEINTERFACE "clojure.lang.Numbers$Ops" "combine" 1)
      (ALOAD_0)
      (CHECKCAST "java.lang.Number")
      (ALOAD_1)
      (CHECKCAST "java.lang.Number")
      (INVOKEINTERFACE "clojure.lang.Numbers$Ops" "multiplyP" 2)
      (ARETURN))
    
    '("num" (long)                          
      (lload_0)                                   ; 0: lload_0
      (invokestatic "java.lang.Long" "valueOf" 2) ; 1: invokestatic "valueOf" "java.lang.Long" (long) (class "java.lang.Long")     
      (areturn))                                  ; 4: areturn       
        
    '("toBigInt" (java.lang.Object)                          
      (ALOAD_0)                                               ; 0 : aload_0    
      (instanceof "clojure.lang.BigInt")                      ; 1 : instanceof (class "clojure.lang.BigInt")) 
      (ifeq 8)                                                ; 4 : ifeq 12  ;;to TAG_0                                      
      (aload_0)                                               ; 7 : aload_0 
      (checkcast "clojure.lang.BigInt")                       ; 8 : checkcast (class "clojure.lang.BigInt") 
      (areturn)                                               ; 11: areturn 
      (aload_0)                                               ; 12: aload_0 ;;at TAG_0
      (instanceof "java.math.BigInteger")                     ; 13: instanceof (class "java.math.BigInteger") 
      (ifeq 11)                                               ; 16: ifeq 27 ;;to TAG_1
      (aload_0)                                               ; 19: aload_0 
      (checkcast "java.math.BigInteger")                      ; 20: checkcast (class "java.math.BigInteger") 
      (invokestatic "clojure.lang.BigInt" "fromBigInteger" 1) ; 23: invokestatic "fromBigInteger" "clojure.lang.BigInt" ((class "java.math.BigInteger")) (class "clojure.lang.BigInt") 
      (areturn)                                               ; 26: areturn 
      (aload_0)                                               ; 27: aload_0 ;;at TAG_1
      (checkcast "java.lang.Number")                          ; 28: checkcast (class "java.lang.Number") 
      (invokevirtual "java.lang.Number" "longValue" 0)        ; 31: invokevirtual "longValue" "java.lang.Number" () long 
      (invokestatic "clojure.lang.BigInt" "fromLong" 1)       ; 34: invokestatic "fromLong" "clojure.lang.BigInt" (long) (class "clojure.lang.BigInt") 
      (areturn))                                              ; 37: areturn 
    )
  '(REF -1)))