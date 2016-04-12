#|$ACL2s-Preamble$;
(include-book "../../../mc/mc")

(acl2::begin-book t :ttags :all);$ACL2s-Preamble$|#


(in-package "MC")

(defconst *java.lang.Long*
  (make-class-decl
   "java.lang.Long"  ; class name
   '("java.lang.Number")   ; superclass
   ; interfaces
   '()   
   '()                     ; fields
   '()                     ; static fields
   '((LONG -9223372036854775808)
     (LONG 9223372036854775807)
     (INT 64)
     (LONG 4290774380558885855)
     (STRING (REF -1) "-9223372036854775808")
     (LONG 2147483647)
     (LONG 100)
     (INT 65536)
     (INT 52429)
     (LONG 10)
     (STRING (REF -1) "null")
     (STRING (REF -1) "radix ")
     (STRING (REF -1) " less than Character.MIN_RADIX")
     (STRING (REF -1) " greater than Character.MAX_RADIX")
     (LONG -9223372036854775807)
     (LONG -128)
     (LONG 127)
     (STRING (REF -1) "Zero length string")
     (STRING (REF -1) "0x")
     (STRING (REF -1) "0X")
     (STRING (REF -1) "#")
     (STRING (REF -1) "0")
     (STRING (REF -1) "-")
     (STRING (REF -1) "+")
     (STRING (REF -1) "Sign character in wrong position")
     (LONG 6148914691236517205)
     (LONG 3689348814741910323)
     (LONG 1085102592571150095)
     (LONG 71777214294589695)
     (LONG 4294901760)
     (STRING (REF -1) "long"))                              
   (list ; methods
    '("<init>" (LONG)                           
      (ALOAD_0)                                     ; 0: aload_0
      (INVOKESPECIAL "java.lang.Number" "<init>" 0) ; 1: invokespecial "<init>" "java.lang.Number"
      (ALOAD_0)                                     ; 4: aload_0
      (LLOAD_1)                                     ; 5: lload_1
      (PUTFIELD "java.lang.Long" "value" t)         ; 6: putfield "value" "java.lang.Long" long     
      (RETURN))                                     ; 9: return
    
    '("longValue" ()                          
      (ALOAD_0)                               ; 0: aload_0
      (GETFIELD "java.lang.Long" "value" t)   ; 1: getfield            
      (LRETURN))                              ; 4: lreturn   
    
    '("valueOf" (long)
      ; For now, removed the cached version of values between -128 and 127. 
      ;;(lload_0)                                          ; 0 : lload_0
      ;;(ldc2_w 15)                                        ; 1 : ldc2_w 15 ;; LONG:: "-128"
      ;;(lcmp)                                             ; 4 : lcmp 
      ;;(iflt 22)                                          ; 5 : iflt 27  ;;to TAG_0
      ;;(lload_0)                                          ; 8 : lload_0 
      ;;(ldc2_w 16)                                        ; 9 : ldc2_w 16 ;; LONG:: "127"
      ;;(lcmp)                                             ; 12: lcmp 
      ;;(ifgt 14)                                          ; 13: ifgt 27  ;;to TAG_0
      ;;(getstatic "java.lang.Long$LongCache" "cache" nil) ; 16: getstatic "cache" "java.lang.Long$LongCache" (array (class "java.lang.Long")) 
      ;;(lload_0)                                          ; 19: lload_0 
      ;;(l2i)                                              ; 20: l2i 
      ;;(sipush 128)                                       ; 21: sipush 128 
      ;;(iadd)                                             ; 24: iadd 
      ;;(aaload)                                           ; 25: aaload 
      ;;(areturn)                                          ; 26: areturn 
      (new "java.lang.Long")                             ; 27: new (class "java.lang.Long") ;;at TAG_0
      (dup)                                              ; 30: dup 
      (lload_0)                                          ; 31: lload_0 
      (invokespecial "java.lang.Long" "<init>" 2)        ; 32: invokespecial "<init>" "java.lang.Long" (long) void) 
      (areturn))                                         ; 35: areturn    
    )
   '(REF -1)))

