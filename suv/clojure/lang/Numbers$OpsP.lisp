#|$ACL2s-Preamble$;
(include-book "../../../mc/mc")

(acl2::begin-book t :ttags :all);$ACL2s-Preamble$|#


(in-package "MC")

(defconst *clojure.lang.Numbers$OpsP*
  (make-class-decl
   "clojure.lang.Numbers$OpsP"     ; class name
   '("java.lang.Object")  ; superclass
   ; interfaces
   '()   
   '()    ; fields
   '()    ; static fields
   '() ; constant pool (Seems to be numbered by index)                            
   (list ; methods    
    '("<init>" ()                          
      (ALOAD_0)                                     ; 0: aload_0
      (INVOKESPECIAL "java.lang.Object" "<init>" 0)
      (RETURN))                                     ; (4 (return))
    
    '("addP" (java.lang.Number java.lang.Number)                          
      (aload_0)                                           ; 0: aload_0
      (aload_1)                                           ; 1: aload_1
      (aload_2)                                           ; 2: aload_2
      (invokevirtual "clojure.lang.Numbers$OpsP" "add" 2) ; 3: invokevirtual "add" "clojure.lang.Numbers$OpsP" ((class "java.lang.Number") (class "java.lang.Number")) (class "java.lang.Number"))
      (areturn))                                          ; 6: areturn 
    
    '("multiplyP" (java.lang.Number java.lang.Number)                          
      (aload_0)                                                ; 0: aload_0
      (aload_1)                                                ; 1: aload_1
      (aload_2)                                                ; 2: aload_2
      (invokevirtual "clojure.lang.Numbers$OpsP" "multiply" 2) ; 3: invokevirtual "multiply" "clojure.lang.Numbers$OpsP" ((class "java.lang.Number") (class "java.lang.Number")) (class "java.lang.Number"))
      (areturn))                                               ; 6: areturn
    
    '("negateP" (java.lang.Number)
      (aload_0)                                              ; 0: aload_0
      (aload_1)                                              ; 1: aload_1
      (invokevirtual "clojure.lang.Numbers$OpsP" "negate" 1) ; 2: invokevirtual "negate" "clojure.lang.Numbers$OpsP" ((class "java.lang.Number")) (class "java.lang.Number"))
      (areturn))                                             ; 5: areturn    
    
    '("incP" (java.lang.Number)
      (aload_0)                                           ; 0: aload_0
      (aload_1)                                           ; 1: aload_1
      (invokevirtual "clojure.lang.Numbers$OpsP" "inc" 1) ; 2: invokevirtual "inc" "clojure.lang.Numbers$OpsP" ((class "java.lang.Number")) (class "java.lang.Number"))
      (areturn))                                          ; 5: areturn    
   
    '("decP" (java.lang.Number)
      (aload_0)                                           ; 0: aload_0
      (aload_1)                                           ; 1: aload_1
      (invokevirtual "clojure.lang.Numbers$OpsP" "dec" 1) ; 2: invokevirtual "dec" "clojure.lang.Numbers$OpsP" ((class "java.lang.Number")) (class "java.lang.Number"))
      (areturn))                                          ; 5: areturn
    )
  '(REF -1)))    