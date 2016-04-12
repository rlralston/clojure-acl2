#|$ACL2s-Preamble$;
(include-book "../../../mc/mc")

(acl2::begin-book t :ttags :all);$ACL2s-Preamble$|#


(in-package "MC")

(defconst *clojure.lang.Numbers$BigIntOps*
  (make-class-decl
   "clojure.lang.Numbers$BigIntOps"     ; class name
   '("clojure.lang.Numbers$OpsP" "java.lang.Object")  ; superclass
   ; interfaces
   '()
   '()    ; fields
   '()    ; static fields
   '((LONG -9223372036854775808)
     (LONG 9223372036854775807)) ; constant pool (Seems to be numbered by index)                            
   (list ; methods    
    '("<init>" ()                          
      (ALOAD_0)                                              ; 0: aload_0
      (INVOKESPECIAL "clojure.lang.Numbers$OpsP" "<init>" 0)
      (RETURN))                                              ; (4 (return))
    
    '("combine" (java.lang.Numbers$Ops)                          
      (ALOAD_1)                                                          ; 0: aload_1
      (ALOAD_0)                                                          ; 1: aload_0
      (INVOKEINTERFACE "clojure.lang.Numbers$Ops" "opsWith_bigintops" 1) ; 2: invokeinterface "opsWith" "clojure.lang.Numbers$Ops" 
      (ARETURN))                                                         ; 7: areturn 
    
    '("opsWith_longops" (java.lang.Numbers$LongOps)
      (aload_0)  ; 0: aload_0
      (areturn)) ; 1: areturn
                   
    '("opsWith_bigintops" (java.lang.Numbers$BigIntOps)
      (aload_0)  ; 0: aload_0
      (areturn)) ; 1: areturn
    
    '("add" (java.lang.Number java.lang.Number)                          
      (aload_1)                                          ; 0 : aload_1        
      (invokestatic "clojure.lang.Numbers" "toBigInt" 1) ; 1 : invokestatic "toBigInt" "clojure.lang.Numbers" ((class "java.lang.Object")) (class "clojure.lang.BigInt"))
      (aload_2)                                          ; 4 : aload_2
      (invokestatic "clojure.lang.Numbers" "toBigInt" 1) ; 5 : invokestatic "toBigInt" "clojure.lang.Numbers" ((class "java.lang.Object")) (class "clojure.lang.BigInt"))
      (invokevirtual "clojure.lang.BigInt" "add" 1)      ; 8 : invokevirtual "add" "clojure.lang.BigInt" ((class "clojure.lang.BigInt")) (class "clojure.lang.BigInt")      
      (areturn))                                         ; 11: areturn               
    
    '("multiply" (java.lang.Number java.lang.Number)
      (ALOAD_1)
      (INVOKESTATIC "clojure.lang.Numbers" "toBigInt" 1)
      (ALOAD_2)
      (INVOKESTATIC "clojure.lang.Numbers" "toBigInt" 1)
      (INVOKEVIRTUAL "clojure.lang.BigInt" "multiply" 1)
      (ARETURN))
    
    '("negate" (java.lang.Number)
      (ALOAD_1)
      (INVOKESTATIC "clojure.lang.Numbers" "toBigInteger" 1)
      (INVOKEVIRTUAL "java.math.BigInteger" "negate" 0)
      (INVOKESTATIC "clojure.lang.BigInt" "fromBigInteger" 1)
      (ARETURN))
    )
  '(REF -1)))