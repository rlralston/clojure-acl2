#|$ACL2s-Preamble$;
(include-book "../../../mc/mc")

(acl2::begin-book t :ttags :all);$ACL2s-Preamble$|#


;; This is the Integer class

(in-package "MC")

(defconst *java.lang.Integer*
  (make-class-decl
   "java.lang.Integer"  ; class name
   '("java.lang.Number" "java.lang.Object")   ; superclass
   ; interfaces
   '()
   '()                     ; fields
   '()                     ; static fields
   '((INT -2147483648)
     (INT 2147483647)
     (INT 32)
     (LONG 1360826667806852920)
     (STRING  "-2147483648")
     (INT 65536)
     (INT 52429)
     (STRING  "null")
     (STRING  "radix ")
     (STRING  " less than Character.MIN_RADIX")
     (STRING  " greater than Character.MAX_RADIX")
     (INT -2147483647)
     (STRING  "Zero length string")
     (STRING  "0x")
     (STRING  "0X")
     (STRING  "#")
     (STRING  "0")
     (STRING  "-")
     (STRING  "+")
     (STRING  "Sign character in wrong position")
     (INT 1431655765)
     (INT 858993459)
     (INT 252645135)
     (INT 65280)
     (INT 16711680)
     (STRING  "int")
     (INT 99999)
     (INT 999999)
     (INT 9999999)
     (INT 99999999)
     (INT 999999999))                              
   (list ; methods
    '("longValue" ()                           
      (ALOAD_0)                                  ; 0: aload_0
      (GETFIELD "java.lang.Integer" "value" nil) ; 1: getfield      
      (I2L)                                      ; 4: i2l      
      (LRETURN))                                 ; 5: lreturn    

    '("bitCount" (INT)                              
      (ILOAD_0)
      (ILOAD_0)
      (ICONST_1) 
      (IUSHR) 
      (LDC 20)        ;;INT:: "1431655765"
      (IAND) 
      (ISUB) 
      (ISTORE_0) 
      (ILOAD_0) 
      (LDC 21)       ;;INT:: "858993459"
      (IAND) 
      (ILOAD_0) 
      (ICONST_2) 
      (IUSHR) 
      (LDC 21)       ;;INT:: "858993459"
      (IAND) 
      (IADD) 
      (ISTORE_0) 
      (ILOAD_0) 
      (ILOAD_0) 
      (ICONST_4) 
      (IUSHR) 
      (IADD) 
      (LDC 22)       ;;INT:: "252645135"
      (IAND) 
      (ISTORE_0) 
      (ILOAD_0) 
      (ILOAD_0) 
      (BIPUSH 8) 
      (IUSHR) 
      (IADD) 
      (ISTORE_0) 
      (ILOAD_0) 
      (ILOAD_0) 
      (BIPUSH 16) 
      (IUSHR) 
      (IADD) 
      (ISTORE_0) 
      (ILOAD_0) 
      (BIPUSH 63) 
      (IAND) 
      (IRETURN))
    
    '("numberOfLeadingZeros" (INT) 
      (ILOAD_0) 
      (IFNE 6)  ; (1 (ifne 7)) ;;to TAG_0
      (BIPUSH 32) 
      (IRETURN) 
      (ICONST_1) ;;at TAG_0
      (ISTORE_1) 
      (ILOAD_0) 
      (BIPUSH 16) 
      (IUSHR) 
      (IFNE 11) ; (13 (ifne 24)) ;;to TAG_1
      (IINC 1 16) 
      (ILOAD_0) 
      (BIPUSH 16) 
      (ISHL) 
      (ISTORE_0) 
      (ILOAD_0) ;;at TAG_1
      (BIPUSH 24) 
      (IUSHR)
      (IFNE 11) ; (28 (ifne 39))  ;;to TAG_2
      (IINC 1 8) 
      (ILOAD_0)
      (BIPUSH 8) 
      (ISHL) 
      (ISTORE_0) 
      (ILOAD_0) ;;at TAG_2
      (BIPUSH 28) 
      (IUSHR)
      (IFNE 10) ; (43 (ifne 53)) ;;to TAG_3
      (IINC 1 4) 
      (ILOAD_0) 
      (ICONST_4) 
      (ISHL)
      (ISTORE_0) 
      (ILOAD_0) ;;at TAG_3
      (BIPUSH 30) 
      (IUSHR) 
      (IFNE 10)  ; (57 (ifne 67)) ;;to TAG_4
      (IINC 1 2) 
      (ILOAD_0)
      (ICONST_2) 
      (ISHL)
      (ISTORE_0) 
      (ILOAD_1) ;;at TAG_4
      (ILOAD_0) 
      (BIPUSH 31)
      (IUSHR)
      (ISUB) 
      (ISTORE_1) 
      (ILOAD_1)
      (IRETURN))
    )
   '(REF -1)))

(defun intp (x)
  (and (integerp x)
       (<= (- (expt 2 31)) x)
       (< x (expt 2 31))))

(defconst *java.lang.Integer-heap*
  '((0 . (("java.lang.Class" ("<name>" . "java.lang.Integer"))
          ("java.lang.Object" ("monitor" . 0) ("mcount" . 0) ("wait-set" . 0))))))

;; The bitCount-state is a test state for the bitCount function which returns the number of 1's in 
;; the 2's-complement of the name. bitCount has no branching so it always takes 42 steps so it can
;; be tested with: (stack (top-frame 0 (step-n 42 0 *bitCount-state*))) 

(defconst *bitCount-state* 
  (make-state 
   (push 
    (make-frame 
     0 
     nil
     (push 5 nil) ; The passed in parameter
     '((INVOKESTATIC "java.lang.Integer" "bitCount" 1))
     "java.lang.Integer")
    nil)
   *java.lang.Integer-heap*
   (make-class-def (list *java.lang.Integer*))))

; (stack (top-frame 0 (step-n 47 0 *numberOfLeadingZeros-state*))) ; for input 1
; (next-inst 0 (step-n 42 0 *numberOfLeadingZeros-state*)) 

(defconst *numberOfLeadingZeros-state* 
  (make-state 
   (push 
    (make-frame 
     0 
     nil
     (push 1 nil) ; The passed in parameter
     '((INVOKESTATIC "java.lang.Integer" "numberOfLeadingZeros" 1))
     "java.lang.Integer")
    nil)
   *java.lang.Integer-heap*
   (make-class-def (list *java.lang.Integer*))))

;; 

;(defconst *java.lang.Integer-heap*
;  '((0 . (("java.lang.Class" ("<name>" . "java.lang.Integer"))
;          ("java.lang.Object" ("monitor" . 0) ("mcount" . 0) ("wait-set" . 0))))))

(defconst *longValue-heap*
  '((0 . (("java.lang.Integer" ("value" . 12))
          ("java.lang.Object" ("monitor" . 0) ("mcount" . 0) ("wait-set" . 0))))))

; (stack (top-frame 0 (step-n 4 0 *longValue-state*))) 
; (next-inst 0 (step-n 4 0 *numberOfLeadingZeros-state*)) 

(defconst *longValue-state* 
  (make-state 
   (push 
    (make-frame 
     0 
     nil
     (push '(REF 0) nil) ; The passed in parameter
     '((INVOKESPECIAL "java.lang.Integer" "longValue" 0)) 
     "java.lang.Integer")
    nil)
   *longValue-heap*
   (make-class-def (list *java.lang.Integer*))))