#|$ACL2s-Preamble$;
(include-book "../../../mc/mc")

(acl2::begin-book t :ttags :all);$ACL2s-Preamble$|#


(in-package "MC")

(defconst *java.lang.Number*
  (make-class-decl
   "java.lang.Number"     ; class name
   '("java.lang.Object")  ; superclass
   ; interfaces
   '()   
   '()    ; fields
   '()                    ; static fields
   '((LONG 4294967295)) ; constant pool (Seems to be numbered by index)                            
   (list ; methods
    ; private constructor (int[], int)
    '("<init>" ()                          
      (ALOAD_0)                                     ; 0: aload_0
      (INVOKESPECIAL "java.lang.Object" "<init>" 0)
      (RETURN))                                     ; (4 (return))    
    )
  '(REF -1)))