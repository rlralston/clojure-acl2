#|$ACL2s-Preamble$;
(include-book "../mc/mc")
(include-book "../mc/utilities")
(include-book "java/lang/Number")

(acl2::begin-book t :ttags :all);$ACL2s-Preamble$|#


(in-package "MC")

(defconst *java.math.BigInteger*
  (make-class-decl
   "java.math.BigInteger"  ; class name
   '("java.lang.Number" "java.lang.Object")   ; superclass
   ; interfaces
   '()
   ; fields
   '("signum"              ; int  
     "mag"                 ; (array int) 
     "bitCount"            ; int
     "bitLength"           ; int 
     "lowestSetBit"        ; int 
     "firstNonzeroIntNum") ; int          
   ; static fields                       
   '()     
   ; constant pool (Seems to be numbered by index)
   '()                               
   (list ; methods
    ; private constructor (int[], int)
    '("<init>" (|INT[]| INT)                           
      (ALOAD_0)                                      ; 0 : aload_0
      (INVOKESPECIAL "java.lang.Number" "<init>" 0)  ; 1 : invokespecial #796 // Method java/lang/Number."<init>":()V
      (ALOAD_0)                                      ; 4 : aload_0
      (ALOAD_1)                                      ; 5 : aload_1
      (ARRAYLENGTH)                                  ; 6 : arraylength
      (IFNE 7)                                       ; 7 : ifne          14
      (ICONST_0)                                     ; 10: iconst_0
      (GOTO 4)                                       ; 11: goto          15
      (ILOAD_2)                                      ; 14: iload_2
      (PUTFIELD "java.math.BigInteger" "signum" nil) ; 15: putfield      #746                // Field signum:I
      (ALOAD_0)                                      ; 18: aload_0       
      (ALOAD_1)                                      ; 19: aload_1       
      (PUTFIELD "java.math.BigInteger" "mag" nil)    ; 20: putfield      #754                // Field mag:[I
      (RETURN))                                      ; 23: return
    )
   '(REF -1)))

(defconst *java.math.BigInteger.init-heap*
  '((0 . (("java.lang.Class" ("<name>" . "java.math.BigInteger"))
          ("java.lang.Object" ("monitor" . 0) ("mcount" . 0) ("wait-set" . 0))))
;; overflow
   (1 ("ARRAY" ("<array>" *ARRAY* "INT" 1 (7))))
   (2 ("java.math.BigInteger" ("signum" . 0)
                              ("mag" . 0)
                              ("bitCount" . 0)
                              ("bitLength" . 0)
                              ("lowestSetBit" . 0)
                              ("firstNonzeroIntNum" . 0)))))

(defconst *init-state2* 
  (make-state 
    (push 
     (make-frame 
      0 
      nil 
      (push 0 (push '(REF 1) (push '(REF 2) nil))) 
      '((INVOKESTATIC "java.math.BigInteger" "<init>" 3))
      "java.math.BigInteger")
     nil)
    *java.math.BigInteger.init-heap*
    (make-class-def (list *java.lang.Number* *java.math.BigInteger*))))  
  
(defun refp (x)
  (equal (car x) 'REF))

(defun pre-<init> (jstate x0 x1 x2) 
  (let* ((heap (heap jstate)))
    (and (refp x0)
         (refp x1)
         (integerp x2)
         (equal (class-name-of-ref x1 heap) "ARRAY")
         (equal (class-name-of-ref x0 heap) "java.math.BigInteger")
         (equal (next-inst jstate)
                '(INVOKESTATIC "java.math.BigInteger" "<init>" 3)))))
         
(defun post-<init> (jstate x0 x1 x2) 
  (let* ((heap (heap jstate)))
    (and (equal (class-name-of-ref x0 heap) "java.math.BigInteger")
         (equal (field-value "java.math.BigInteger" "signum" (deref x0 heap))
                x2)
         (equal (field-value "java.math.BigInteger" "mag" (deref x0 heap))
                x1))))

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
    '()
    (make-class-def (list *java.lang.Number* *java.math.BigInteger*))))

(defconst *base-state* 
  (run-to-return 0 1500 (m5_load *clinit-state*)))

(defconst *BigIntegerState*
  (make-state 
    (push 
     (make-frame 
      0 
      nil       
      nil
      '((HALT))      
      "java.math.BigInteger")
     nil)
    nil
    (make-class-def (list *java.math.BigInteger*))))

(defun init-state () 
  (m5_load *BigIntegerState*))    

(defconst *j.m.BigInteger-signInt*
  '("signInt" ()                          
      (ALOAD_0)                                      ; 0: aload_0    
      (GETFIELD "java.math.BigInteger" "signum" nil) ; 1: getfield "signum" "java.math.BigInteger" 
      (IFGE 7)                                       ; 4: ifge 11  ;;to TAG_0
      (ICONST_M1)                                    ; 7: iconst_m1 
      (GOTO 4)                                       ; 8: goto 12 ;;to TAG_1
      (ICONST_0)                                     ; 11: iconst_0 ;;at TAG_0
      (IRETURN)))

(defun poised-to-invoke-signInt (s)
  (and (equal (next-inst s) 
              '(invokestatic "java.math.BigInteger" "signInt" 1))
       (equal (lookup-method "signInt" "java.math.BigInteger" (class-table s))
              *j.m.BigInteger-signInt*)))

(defun program1 (class method)
  (cond
   ((equal class "java.math.BigInteger")
    (cond
     ((equal method "signInt") *j.m.BigInteger-signInt*)
     (t nil)))
    (t nil)))

(defun programp (frame class method)
  (let ((const (program1 class method)))
    (and (equal (cur-class frame) class)
         (equal (program frame) const))))

(defthm next-inst-from-programp
  (implies (and (syntaxp (quotep pc))
                (programp frame class method))
           (equal (INDEX-INTO-PROGRAM pc
                                      (PROGRAM frame))
                  (index-into-program pc
                                      (program1 class method)))))

(in-theory (disable programp index-into-program))

(defun poised-to-invoke-signInt2 (s)
  (and (equal (next-inst s) 
              '(invokestatic "java.math.BigInteger" "signInt" 0))
       (equal (lookup-method "signInt" "java.math.BigInteger" (class-table s))
              *j.m.BigInteger-signInt*)))

(defthmd init-BigInteger-1
  (implies (poised-to-invoke-signInt2 s)
           (equal (next-inst (step s))                 
                  '(ALOAD_0))))

(defthmd init-BigInteger-2
  (implies (poised-to-invoke-signInt2 s)
           (equal (next-inst (step (step s)))                 
                  '(GETFIELD "java.math.BigInteger" "signum" nil))))

(defthmd init-BigInteger-2-sn
  (implies (poised-to-invoke-signInt2 s)
           (equal (next-inst (step-n 2 s))                
                  '(GETFIELD "java.math.BigInteger" "signum" nil))))

(defthmd init-BigInteger-3-sn
  (implies (poised-to-invoke-signInt2 s)
           (equal (next-inst (step-n 3 s))                
                  '(IFGE 7))))

(defthmd init-BigInteger-3-sn-2
  (implies (poised-to-invoke-signInt s)
           (equal (next-inst (step-n 3 s))                
                  '(IFGE 7))))

(defun poised-to-invoke-signInt-with-ref (s i)
  (let* ((ref (top (stack (top-frame s))))
         (heap (heap s)))
  (and (equal (next-inst s) 
              '(invokestatic "java.math.BigInteger" 
                             "signInt" 
                             1))
       (equal (lookup-method "signInt" 
                             "java.math.BigInteger" 
                             (class-table s))
              *j.m.BigInteger-signInt*)
       (refp ref)
       (equal i
              (field-value "java.math.BigInteger" "signum" (deref ref heap))))))

(defthmd init-BigInteger-4-lt0
  (implies (and (< i 0)
                (poised-to-invoke-signInt-with-ref s i))
           (equal (next-inst (step-n 4 s))                
                  '(ICONST_M1))))

(defthmd init-BigInteger-5-lt0
  (implies (and (< i 0)
                (poised-to-invoke-signInt-with-ref s i))
           (equal (next-inst (step-n 5 s))                
                  '(GOTO 4))))

(defthmd init-BigInteger-6-lt0
  (implies (and (< i 0)
                (poised-to-invoke-signInt-with-ref s i))
           (equal (next-inst (step-n 6 s))                
                  '(IRETURN))))

(defthmd signInt-returns-correctly
  (implies (and (integerp i)
                (< i 0)
                (poised-to-invoke-signInt-with-ref s i))
           (equal (step-n 7 s)
                  (modify s
                          :pc (+ 3 (pc (top-frame s)))
                          :stack (push -1
                                       (pop (stack (top-frame s))))))))

(defthmd signInt-returns-correctly-2
  (implies (and (integerp i)
                (<= 0 i)
                (poised-to-invoke-signInt-with-ref s i))
           (equal (step-n 6 s)
                  (modify s
                          :pc (+ 3 (pc (top-frame s)))
                          :stack (push 0
                                       (pop (stack (top-frame s))))))))

(defthm init-BigInteger-1
  (implies (and (equal (next-inst jstate)
                       ;'(invokestatic "java.math.BigInteger" "add_ia.ia" 0))
                       '(invokestatic "java.math.BigInteger" "<init>" 3))
                (equal (class-table jstate) 
                       (class-table (init-state))))
           (equal (next-inst (step jstate))
                  '(ALOAD_0))))

(defthm init-BigInteger-1
  (let* ((stack (stack (top-frame jstate)))
         (x2 (top stack))
         (x1 (top (pop stack)))
         (x0 (top (pop (pop stack)))))
  (implies (and (pre-<init> jstate x0 x1 x2)
                (equal (bound? "java.math.BigInteger" (class-table jstate))
                       *java.math.BigInteger*)
                (equal (bound? "java.lang.Number" (class-table jstate))
                       *java.lang.Number*))
           (equal (next-inst (step jstate))
                  '(ALOAD_0)))))