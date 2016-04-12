#|$ACL2s-Preamble$;
(include-book "../../../mc/mc")

(acl2::begin-book t :ttags :all);$ACL2s-Preamble$|#


(in-package "MC")

(defconst *clojure.lang.Numbers$LongOps*
  (make-class-decl
   "clojure.lang.Numbers$LongOps"     ; class name
   '("java.lang.Object")  ; superclass
   ; interfaces
   '()   
   '()    ; fields
   '()    ; static fields
   '((LONG -9223372036854775808)
     (LONG 9223372036854775807)) ; constant pool (Seems to be numbered by index)                            
   (list ; methods    
    '("<init>" ()                          
      (ALOAD_0)                                     ; 0: aload_0
      (INVOKESPECIAL "java.lang.Object" "<init>" 0)
      (RETURN))                                     ; (4 (return))
    
    '("combine" (java.lang.Numbers$Ops)                          
      (ALOAD_1)                                                        ; 0: aload_1
      (ALOAD_0)                                                        ; 1: aload_0
      (INVOKEINTERFACE "clojure.lang.Numbers$Ops" "opsWith_longops" 1) ; 2: invokeinterface "opsWith" "clojure.lang.Numbers$Ops" 
      (ARETURN))                                                       ; 7: areturn 
    
    '("opsWith_longops" (java.lang.Numbers$LongOps)
      (aload_0)  ; 0: aload_0
      (areturn)) ; 1: areturn
                   
    '("opsWith_bigintops" (java.lang.Numbers$BigIntOps)
      (getstatic "clojure.lang.Numbers" "BIGINT_OPS" nil) ; 0: getstatic "BIGINT_OPS" "clojure.lang.Numbers"
      (areturn))                                          ; 3: areturn                                                        
    
    ;; Need to add opsWith methods
    
    '("addP" (java.lang.Number java.lang.Number)                          
      (ALOAD_1)                                                ; 0 : aload_1        
      (INVOKEVIRTUAL "java.lang.Number" "longValue" 0)         ; 1 : invokevirtual "longValue" "java.lang.Number" () long 
      (LSTORE_3)                                               ; 4 : lstore_3 
      (ALOAD_2)                                                ; 5 : aload_2 
      (INVOKEVIRTUAL "java.lang.Number" "longValue" 0)         ; 6 : invokevirtual "longValue" "java.lang.Number" () long 
      (LSTORE 5)                                               ; 9 : lstore 5 
      (LLOAD_3)                                                ; 11: lload_3 
      (LLOAD 5)                                                ; 12: lload 5 
      (LADD)                                                   ; 14: ladd 
      (LSTORE 7)                                               ; 15: lstore 7 
      (LLOAD 7)                                                ; 17: lload 7 
      (LLOAD_3)                                                ; 19: lload_3 
      (LXOR)                                                   ; 20: lxor 
      (LCONST_0)                                               ; 21: lconst_0 
      (LCMP)                                                   ; 22: lcmp 
      (IFGE 22)                                                ; 23: ifge 45  ;;to TAG_0
      (LLOAD 7)                                                ; 26: lload 7 
      (LLOAD 5)                                                ; 28: lload 5 
      (LXOR)                                                   ; 30: lxor 
      (LCONST_0)                                               ; 31: lconst_0 
      (LCMP)                                                   ; 32: lcmp 
      (IFGE 12)                                                ; 33: ifge 45  ;;to TAG_0
      (GETSTATIC "clojure.lang.Numbers" "BIGINT_OPS" nil)      ; 36: getstatic "BIGINT_OPS" "clojure.lang.Numbers" (class "clojure.lang.Numbers$BigIntOps") 
      (ALOAD_1)                                                ; 39: aload_1 
      (ALOAD_2)                                                ; 40: aload_2 
      (INVOKEVIRTUAL "clojure.lang.Numbers$BigIntOps" "add" 2) ; 41: invokevirtual "add" "clojure.lang.Numbers$BigIntOps" 
      (ARETURN)                                                ; 44: areturn 
      (LLOAD 7)                                                ; 45: lload 7 ;;at TAG_0
      (INVOKESTATIC "clojure.lang.Numbers" "num" 2)            ; 47: invokestatic "num" "clojure.lang.Numbers" 
      (ARETURN))                                               ; 50: areturn               

    '("multiplyP" (java.lang.Number java.lang.Number)
      (ALOAD_1)
      (INVOKEVIRTUAL "java.lang.Number" "longValue" 0)
      (LSTORE_3)
      (ALOAD_2)
      (INVOKEVIRTUAL "java.lang.Number" "longValue" 0)
      (LSTORE 5)
      (LLOAD_3)
      (LLOAD 5)
      (LMUL)
      (LSTORE 7)
      (LLOAD 5)
      (LCONST_0)
      (LCMP)
      (IFEQ 22)
      (LLOAD 7)
      (LLOAD 5)
      (LDIV)
      (LLOAD_3)
      (LCMP)
      (IFEQ 12)
      (GETSTATIC "clojure.lang.Numbers" "BIGINT_OPS" NIL)
      (ALOAD_1)
      (ALOAD_2)
      (INVOKEVIRTUAL "clojure.lang.Numbers$BigIntOps" "multiply" 2)
      (ARETURN)
      (LLOAD 7)
      (INVOKESTATIC "clojure.lang.Numbers" "num" 2)
      (ARETURN))
    )
  '(REF -1)))