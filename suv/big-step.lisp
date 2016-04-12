#|$ACL2s-Preamble$;
(include-book "../mc/mc")
(include-book "../mc/utilities")
(include-book "misc/defpun" :dir :system)

#|

 The MC is a small-step semantic model. Once a property is 
 verified for a Java method, it would be convenient to 
 apply the lemma as a rule in a big-step style. In this 
 file, a series of functions and theorems are introduced to
 configure ACL2 to mimic big-step semantics on the MC.

 The techniques are similar to those used by J Moore and 
 Pete Manolios on previous JVM models. A tail-recursive 
 step function is written as a partial function (defpun) 
 that continues until halting. Moore uses the technique
 on M5 while reasoning with step-wise inductive invariants
 and his programs explicitly halt. Manolios uses it on 
 models without looping, so examples eventually halt, or
 on examples with explicit halts. 
 
 This version does not require explicit halting because it 
 limits the big-step-opener so that the model is only 
 stepped if the next instruction is known.
 
|#

(acl2::begin-book t :ttags :all);$ACL2s-Preamble$|#


(in-package "MC")

(defmacro defpun (g args &rest tail)
  `(acl2::defpun ,g ,args ,@tail))

(defun haltedp (s)
  (equal (step s) s))

(defpun big-step (s)
  (if (haltedp s)
    s
    (big-step (step s))))

; ---------------------------------------------------------
; States are equivalent if they halt at the same position.

(defmacro -> (s1 s2)
  `(equal (big-step ,s1)
          (big-step ,s2)))

(defun inst-known-p (s)
  (let* ((inst (next-inst s))
         (op (op-code inst)))
    (or (equal op 'AALOAD)
        (equal op 'AASTORE)
        (equal op 'ACONST_NULL)
        (equal op 'ALOAD)
        (equal op 'ALOAD_0)
        (equal op 'ALOAD_1)
        (equal op 'ALOAD_2)
        (equal op 'ALOAD_3)
        (equal op 'ANEWARRAY)
        (equal op 'ARETURN)
        (equal op 'ARRAYLENGTH)
        (equal op 'ASTORE)
        (equal op 'ASTORE_0)
        (equal op 'ASTORE_1)
        (equal op 'ASTORE_2)
        (equal op 'ASTORE_3)
        (equal op 'BALOAD)
        (equal op 'BASTORE)
        (equal op 'BIPUSH)
        (equal op 'CHECKCAST)
        (equal op 'CALOAD)
        (equal op 'CASTORE)
        (equal op 'DUP)
        (equal op 'DUP_X1)
        (equal op 'DUP_X2)
        (equal op 'DUP2)
        (equal op 'DUP2_X1)
        (equal op 'DUP2_X2)
        (equal op 'GETFIELD)
        (equal op 'GETSTATIC)
        (equal op 'GOTO)
        (equal op 'GOTO_W)
        (equal op 'I2B)
        (equal op 'I2C)
        (equal op 'I2L)
        (equal op 'I2S)
        (equal op 'IADD)
        (equal op 'IALOAD)
        (equal op 'IAND)
        (equal op 'IASTORE)
        (equal op 'ICONST_M1)
        (equal op 'ICONST_0)
        (equal op 'ICONST_1)
        (equal op 'ICONST_2)
        (equal op 'ICONST_3)
        (equal op 'ICONST_4)
        (equal op 'ICONST_5)
        (equal op 'IDIV)
        (equal op 'IF_ACMPEQ)
        (equal op 'IF_ACMPNE)
        (equal op 'IF_ICMPEQ)
        (equal op 'IF_ICMPGE)
        (equal op 'IF_ICMPGT)
        (equal op 'IF_ICMPLE)
        (equal op 'IF_ICMPLT)
        (equal op 'IF_ICMPNE)
        (equal op 'IFEQ)
        (equal op 'IFGE)
        (equal op 'IFGT)
        (equal op 'IFLE)
        (equal op 'IFLT)
        (equal op 'IFNE)
        (equal op 'IFNONNULL)
        (equal op 'IFNULL)
        (equal op 'IINC)
        (equal op 'ILOAD)
        (equal op 'ILOAD_0)
        (equal op 'ILOAD_1)
        (equal op 'ILOAD_2)
        (equal op 'ILOAD_3)
        (equal op 'IMUL)
        (equal op 'INEG)
        (equal op 'INSTANCEOF)
        (equal op 'INVOKEINTERFACE)
        (equal op 'INVOKESPECIAL)
        (equal op 'INVOKESTATIC)
        (equal op 'INVOKEVIRTUAL)
        (equal op 'IOR)
        (equal op 'IREM)
        (equal op 'IRETURN)
        (equal op 'ISHL)
        (equal op 'ISHR)
        (equal op 'ISTORE)
        (equal op 'ISTORE_0)
        (equal op 'ISTORE_1)
        (equal op 'ISTORE_2)
        (equal op 'ISTORE_3)
        (equal op 'ISUB)
        (equal op 'IUSHR)
        (equal op 'IXOR)
        (equal op 'JSR)
        (equal op 'JSR_W)
        (equal op 'L2I)
        (equal op 'LADD)
        (equal op 'LALOAD)
        (equal op 'LAND)
        (equal op 'LASTORE)
        (equal op 'LCMP)
        (equal op 'LCONST_0)
        (equal op 'LCONST_1)
        (equal op 'LDC)
        (equal op 'LDC_W)
        (equal op 'LDC2_W)
        (equal op 'LDIV)
        (equal op 'LLOAD)
        (equal op 'LLOAD_0)
        (equal op 'LLOAD_1)
        (equal op 'LLOAD_2)
        (equal op 'LLOAD_3)
        (equal op 'LMUL)
        (equal op 'LNEG)
        (equal op 'LOR)
        (equal op 'LREM)
        (equal op 'LRETURN)
        (equal op 'LSHL)
        (equal op 'LSHR)
        (equal op 'LSTORE)
        (equal op 'LSTORE_0)
        (equal op 'LSTORE_1)
        (equal op 'LSTORE_2)
        (equal op 'LSTORE_3)
        (equal op 'LSUB)
        (equal op 'LUSHR)
        (equal op 'LXOR)
        (equal op 'MULTIANEWARRAY)
        (equal op 'NEW)
        (equal op 'NEWARRAY)
        (equal op 'NOP)
        (equal op 'POP)
        (equal op 'POP2)
        (equal op 'PUTFIELD)
        (equal op 'PUTSTATIC)
        (equal op 'RET)
        (equal op 'RETURN)
        (equal op 'SALOAD)
        (equal op 'SASTORE)
        (equal op 'SIPUSH)
        (equal op 'SWAP))))    
    
(defthmd big-step-opener-infinite
  (equal (big-step s) (big-step (step s))))

(defthm big-step-opener
  (implies (inst-known-p s)
           (equal (big-step s) (big-step (step s))))
  :hints (("Goal" :use big-step-opener-infinite)))

(defthmd big-step-halts-all
  (implies (and (haltedp (big-step s2))
                (-> s1 s2))
           (haltedp (big-step s1))))

(defthmd halting-is-s
  (implies (haltedp s)
           (equal (big-step s) s)))

(defun poised-inst (inst s)
  (equal inst (op-code (next-inst s))))

(defthm do-inst-opener
  (implies
   (syntaxp (quotep inst))
   (equal
    (do-inst inst s)
    (CASE (OP-CODE INST)
      (AALOAD (EXECUTE-AALOAD INST S))
      (AASTORE (EXECUTE-AASTORE INST S))
      (ACONST_NULL (EXECUTE-ACONST_NULL INST S))
      (ALOAD (EXECUTE-ALOAD INST S))
      (ALOAD_0 (EXECUTE-ALOAD_X INST S 0))
      (ALOAD_1 (EXECUTE-ALOAD_X INST S 1))
      (ALOAD_2 (EXECUTE-ALOAD_X INST S 2))
      (ALOAD_3 (EXECUTE-ALOAD_X INST S 3))
      (ANEWARRAY (EXECUTE-ANEWARRAY INST S))
      (ARETURN (EXECUTE-ARETURN INST S))
      (ARRAYLENGTH (EXECUTE-ARRAYLENGTH INST S))
      (ASTORE (EXECUTE-ASTORE INST S))
      (ASTORE_0 (EXECUTE-ASTORE_X INST S 0))
      (ASTORE_1 (EXECUTE-ASTORE_X INST S 1))
      (ASTORE_2 (EXECUTE-ASTORE_X INST S 2))
      (ASTORE_3 (EXECUTE-ASTORE_X INST S 3))
      (BALOAD (EXECUTE-BALOAD INST S))
      (BASTORE (EXECUTE-BASTORE INST S))
      (BIPUSH (EXECUTE-BIPUSH INST S))
      (CALOAD (EXECUTE-CALOAD INST S))
      (CASTORE (EXECUTE-CASTORE INST S))
      (CHECKCAST (execute-CHECKCAST INST S))
      (DUP (EXECUTE-DUP INST S))
      (DUP_X1 (EXECUTE-DUP_X1 INST S))
      (DUP_X2 (EXECUTE-DUP_X2 INST S))
      (DUP2 (EXECUTE-DUP2 INST S))
      (DUP2_X1 (EXECUTE-DUP2_X1 INST S))
      (DUP2_X2 (EXECUTE-DUP2_X2 INST S))
      (GETFIELD (EXECUTE-GETFIELD INST S))
      (GETSTATIC (EXECUTE-GETSTATIC INST S))
      (GOTO (EXECUTE-GOTO INST S))
      (GOTO_W (EXECUTE-GOTO_W INST S))
      (I2B (EXECUTE-I2B INST S))
      (I2C (EXECUTE-I2C INST S))
      (I2L (EXECUTE-I2L INST S))
      (I2S (EXECUTE-I2S INST S))
      (IADD (EXECUTE-IADD INST S))
      (IALOAD (EXECUTE-IALOAD INST S))
      (IAND (EXECUTE-IAND INST S))
      (IASTORE (EXECUTE-IASTORE INST S))
      (ICONST_M1 (EXECUTE-ICONST_X INST S -1))
      (ICONST_0 (EXECUTE-ICONST_X INST S 0))
      (ICONST_1 (EXECUTE-ICONST_X INST S 1))
      (ICONST_2 (EXECUTE-ICONST_X INST S 2))
      (ICONST_3 (EXECUTE-ICONST_X INST S 3))
      (ICONST_4 (EXECUTE-ICONST_X INST S 4))
      (ICONST_5 (EXECUTE-ICONST_X INST S 5))
      (IDIV (EXECUTE-IDIV INST S))
      (IF_ACMPEQ (EXECUTE-IF_ACMPEQ INST S))
      (IF_ACMPNE (EXECUTE-IF_ACMPNE INST S))
      (IF_ICMPEQ (EXECUTE-IF_ICMPEQ INST S))
      (IF_ICMPGE (EXECUTE-IF_ICMPGE INST S))
      (IF_ICMPGT (EXECUTE-IF_ICMPGT INST S))
      (IF_ICMPLE (EXECUTE-IF_ICMPLE INST S))
      (IF_ICMPLT (EXECUTE-IF_ICMPLT INST S))
      (IF_ICMPNE (EXECUTE-IF_ICMPNE INST S))
      (IFEQ (EXECUTE-IFEQ INST S))
      (IFGE (EXECUTE-IFGE INST S))
      (IFGT (EXECUTE-IFGT INST S))
      (IFLE (EXECUTE-IFLE INST S))
      (IFLT (EXECUTE-IFLT INST S))
      (IFNE (EXECUTE-IFNE INST S))
      (IFNONNULL (EXECUTE-IFNONNULL INST S))
      (IFNULL (EXECUTE-IFNULL INST S))
      (IINC (EXECUTE-IINC INST S))
      (ILOAD (EXECUTE-ILOAD INST S))
      (ILOAD_0 (EXECUTE-ILOAD_X INST S 0))
      (ILOAD_1 (EXECUTE-ILOAD_X INST S 1))
      (ILOAD_2 (EXECUTE-ILOAD_X INST S 2))
      (ILOAD_3 (EXECUTE-ILOAD_X INST S 3))
      (IMUL (EXECUTE-IMUL INST S))
      (INEG (EXECUTE-INEG INST S))
      (INSTANCEOF (EXECUTE-INSTANCEOF INST S))
      (INVOKEINTERFACE (EXECUTE-INVOKEINTERFACE INST S))      
      (INVOKESPECIAL (EXECUTE-INVOKESPECIAL INST S))
      (INVOKESTATIC (EXECUTE-INVOKESTATIC INST S))
      (INVOKEVIRTUAL (EXECUTE-INVOKEVIRTUAL INST S))
      (IOR (EXECUTE-IOR INST S))
      (IREM (EXECUTE-IREM INST S))
      (IRETURN (EXECUTE-IRETURN INST S))
      (ISHL (EXECUTE-ISHL INST S))
      (ISHR (EXECUTE-ISHR INST S))
      (ISTORE (EXECUTE-ISTORE INST S))
      (ISTORE_0 (EXECUTE-ISTORE_X INST S 0))
      (ISTORE_1 (EXECUTE-ISTORE_X INST S 1))
      (ISTORE_2 (EXECUTE-ISTORE_X INST S 2))
      (ISTORE_3 (EXECUTE-ISTORE_X INST S 3))
      (ISUB (EXECUTE-ISUB INST S))
      (IUSHR (EXECUTE-IUSHR INST S))
      (IXOR (EXECUTE-IXOR INST S))
      (JSR (EXECUTE-JSR INST S))
      (JSR_W (EXECUTE-JSR_W INST S))
      (L2I (EXECUTE-L2I INST S))
      (LADD (EXECUTE-LADD INST S))
      (LALOAD (EXECUTE-LALOAD INST S))
      (LAND (EXECUTE-LAND INST S))
      (LASTORE (EXECUTE-LASTORE INST S))
      (LCMP (EXECUTE-LCMP INST S))
      (LCONST_0 (EXECUTE-LCONST_X INST S 0))
      (LCONST_1 (EXECUTE-LCONST_X INST S 1))
      (LDC (EXECUTE-LDC INST S))
      (LDC_W (EXECUTE-LDC INST S))
      (LDC2_W (EXECUTE-LDC2_W INST S))
      (LDIV (EXECUTE-LDIV INST S))
      (LLOAD (EXECUTE-LLOAD INST S))
      (LLOAD_0 (EXECUTE-LLOAD_X INST S 0))
      (LLOAD_1 (EXECUTE-LLOAD_X INST S 1))
      (LLOAD_2 (EXECUTE-LLOAD_X INST S 2))
      (LLOAD_3 (EXECUTE-LLOAD_X INST S 3))
      (LMUL (EXECUTE-LMUL INST S))
      (LNEG (EXECUTE-LNEG INST S))
      (LOR (EXECUTE-LOR INST S))
      (LREM (EXECUTE-LREM INST S))
      (LRETURN (EXECUTE-LRETURN INST S))
      (LSHL (EXECUTE-LSHL INST S))
      (LSHR (EXECUTE-LSHR INST S))
      (LSTORE (EXECUTE-LSTORE INST S))
      (LSTORE_0 (EXECUTE-LSTORE_X INST S 0))
      (LSTORE_1 (EXECUTE-LSTORE_X INST S 1))
      (LSTORE_2 (EXECUTE-LSTORE_X INST S 2))
      (LSTORE_3 (EXECUTE-LSTORE_X INST S 3))
      (LSUB (EXECUTE-LSUB INST S))
      (LUSHR (EXECUTE-LUSHR INST S))
      (LXOR (EXECUTE-LXOR INST S))
      (MULTIANEWARRAY (EXECUTE-MULTIANEWARRAY INST S))
      (NEW (EXECUTE-NEW INST S))
      (NEWARRAY (EXECUTE-NEWARRAY INST S))
      (NOP (EXECUTE-NOP INST S))
      (POP (EXECUTE-POP INST S))
      (POP2 (EXECUTE-POP2 INST S))
      (PUTFIELD (EXECUTE-PUTFIELD INST S))
      (PUTSTATIC (EXECUTE-PUTSTATIC INST S))
      (RET (EXECUTE-RET INST S))
      (RETURN (EXECUTE-RETURN INST S))
      (SALOAD (EXECUTE-SALOAD INST S))
      (SASTORE (EXECUTE-SASTORE INST S))
      (SIPUSH (EXECUTE-SIPUSH INST S))
      (SWAP (EXECUTE-SWAP INST S))
      (HALT S)
      (OTHERWISE S))))
  :hints (("Goal"
           :in-theory (disable
      EXECUTE-AALOAD
      EXECUTE-AASTORE
      EXECUTE-ACONST_NULL
      EXECUTE-ALOAD
      EXECUTE-ALOAD_X
      EXECUTE-ALOAD_X
      EXECUTE-ALOAD_X
      EXECUTE-ALOAD_X
      EXECUTE-ANEWARRAY
      EXECUTE-ARETURN
      EXECUTE-ARRAYLENGTH
      EXECUTE-ASTORE
      EXECUTE-ASTORE_X
      EXECUTE-ASTORE_X
      EXECUTE-ASTORE_X
      EXECUTE-ASTORE_X
      EXECUTE-BALOAD
      EXECUTE-BASTORE
      EXECUTE-BIPUSH
      EXECUTE-CALOAD
      EXECUTE-CASTORE
      EXECUTE-DUP
      EXECUTE-DUP_X1
      EXECUTE-DUP_X2
      EXECUTE-DUP2
      EXECUTE-DUP2_X1
      EXECUTE-DUP2_X2
      EXECUTE-GETFIELD
      EXECUTE-GETSTATIC
      EXECUTE-GOTO
      EXECUTE-GOTO_W
      EXECUTE-I2B
      EXECUTE-I2C
      EXECUTE-I2L
      EXECUTE-I2S
      EXECUTE-IADD
      EXECUTE-IALOAD
      EXECUTE-IAND
      EXECUTE-IASTORE
      EXECUTE-ICONST_X
      EXECUTE-ICONST_X
      EXECUTE-ICONST_X
      EXECUTE-ICONST_X
      EXECUTE-ICONST_X
      EXECUTE-ICONST_X
      EXECUTE-ICONST_X
      EXECUTE-IDIV
      EXECUTE-IF_ACMPEQ
      EXECUTE-IF_ACMPNE
      EXECUTE-IF_ICMPEQ
      EXECUTE-IF_ICMPGE
      EXECUTE-IF_ICMPGT
      EXECUTE-IF_ICMPLE
      EXECUTE-IF_ICMPLT
      EXECUTE-IF_ICMPNE
      EXECUTE-IFEQ
      EXECUTE-IFGE
      EXECUTE-IFGT
      EXECUTE-IFLE
      EXECUTE-IFLT
      EXECUTE-IFNE
      EXECUTE-IFNONNULL
      EXECUTE-IFNULL
      EXECUTE-IINC
      EXECUTE-ILOAD
      EXECUTE-ILOAD_X
      EXECUTE-ILOAD_X
      EXECUTE-ILOAD_X
      EXECUTE-ILOAD_X
      EXECUTE-IMUL
      EXECUTE-INEG
      EXECUTE-INSTANCEOF
      EXECUTE-INVOKESPECIAL
      EXECUTE-INVOKESTATIC
      EXECUTE-INVOKEVIRTUAL
      EXECUTE-IOR
      EXECUTE-IREM
      EXECUTE-IRETURN
      EXECUTE-ISHL
      EXECUTE-ISHR
      EXECUTE-ISTORE
      EXECUTE-ISTORE_X
      EXECUTE-ISTORE_X
      EXECUTE-ISTORE_X
      EXECUTE-ISTORE_X
      EXECUTE-ISUB
      EXECUTE-IUSHR
      EXECUTE-IXOR
      EXECUTE-JSR
      EXECUTE-JSR_W
      EXECUTE-L2I
      EXECUTE-LADD
      EXECUTE-LALOAD
      EXECUTE-LAND
      EXECUTE-LASTORE
      EXECUTE-LCMP
      EXECUTE-LCONST_X
      EXECUTE-LCONST_X
      EXECUTE-LDC
      EXECUTE-LDC
      EXECUTE-LDC2_W
      EXECUTE-LDIV
      EXECUTE-LLOAD
      EXECUTE-LLOAD_X
      EXECUTE-LLOAD_X
      EXECUTE-LLOAD_X
      EXECUTE-LLOAD_X
      EXECUTE-LMUL
      EXECUTE-LNEG
      EXECUTE-LOR
      EXECUTE-LREM
      EXECUTE-LRETURN
      EXECUTE-LSHL
      EXECUTE-LSHR
      EXECUTE-LSTORE
      EXECUTE-LSTORE_X
      EXECUTE-LSTORE_X
      EXECUTE-LSTORE_X
      EXECUTE-LSTORE_X
      EXECUTE-LSUB
      EXECUTE-LUSHR
      EXECUTE-LXOR
      EXECUTE-MULTIANEWARRAY
      EXECUTE-NEW
      EXECUTE-NEWARRAY
      EXECUTE-NOP
      EXECUTE-POP
      EXECUTE-POP2
      EXECUTE-PUTFIELD
      EXECUTE-PUTSTATIC
      EXECUTE-RET
      EXECUTE-RETURN
      EXECUTE-SALOAD
      EXECUTE-SASTORE
      EXECUTE-SIPUSH
      EXECUTE-SWAP))))

(in-theory (disable do-inst))

; ---------------------------------------------------------
; AALOAD

(defthmd step-AALOAD
  (implies 
   (poised-inst 'AALOAD s)
   (equal (step s) 
          (execute-AALOAD (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-AALOAD
  (implies 
   (poised-inst 'AALOAD s)          
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack 
               (push (element-at 
                      (top (stack (top-frame s))) 
                      (deref (top (pop (stack (top-frame s)))) 
                             (heap s)))
                     (pop (pop (stack (top-frame s))))))))
    :hints (("Goal" :in-theory (enable step-AALOAD))))

; ---------------------------------------------------------
; AASTORE

(defthmd step-AASTORE
  (implies 
   (poised-inst 'AASTORE s)
   (equal (step s) 
          (execute-AASTORE (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-AASTORE
  (implies 
   (poised-inst 'AASTORE s)
   (let* ((arrayref (top (pop (pop (stack (top-frame s)))))))
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (pop (pop (pop (stack (top-frame s)))))
               :heap (bind (cadr arrayref)
                           (set-element-at (top (stack (top-frame s)))
                                           (top (pop (stack (top-frame s))))
                                           (deref arrayref 
                                                  (heap s))
                                           (class-table s))
                           (heap s))))))
  :hints (("Goal" :in-theory (enable step-AASTORE))))

; ---------------------------------------------------------
; ACONST_NULL
        
(defthmd step-ACONST_NULL
  (implies 
   (poised-inst 'ACONST_NULL s)
   (equal (step s) 
          (execute-ACONST_NULL (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-ACONST_NULL 
  (implies 
   (poised-inst 'ACONST_NULL s)          
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push (nullref)
                            (stack (top-frame s))))))
  :hints (("Goal" :in-theory (enable step-ACONST_NULL))))  

; ---------------------------------------------------------
; ALOAD

(defthmd step-ALOAD
  (implies 
   (poised-inst 'ALOAD s)
   (equal (step s) 
          (execute-ALOAD (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-ALOAD
  (implies 
   (poised-inst 'ALOAD s)
   (-> s
       (modify s
               :pc (+ 2 (pc (top-frame s)))
               :stack 
               (push (nth (arg1 (next-inst s)) 
                          (locals (top-frame s)))
                     (stack (top-frame s))))))  
  :hints (("Goal" :in-theory (enable step-ALOAD))))  

; ---------------------------------------------------------
; ALOAD_0

(defthmd step-ALOAD_0
  (implies 
   (poised-inst 'ALOAD_0 s)
   (equal (step s) 
          (execute-ALOAD_X (next-inst s) s 0)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-ALOAD_0
  (implies 
   (poised-inst 'ALOAD_0 s)          
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack 
               (push (nth 0 
                          (locals (top-frame s)))
                     (stack (top-frame s))))))
  :hints (("Goal" :in-theory (enable step-ALOAD_0))))
      
; ---------------------------------------------------------
; ALOAD_1        

(defthmd step-ALOAD_1
  (implies 
   (poised-inst 'ALOAD_1 s)
   (equal (step s) 
          (execute-ALOAD_X (next-inst s) s 1)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-ALOAD_1
  (implies 
   (poised-inst 'ALOAD_1 s)          
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack 
               (push (nth 1 
                          (locals (top-frame s)))
                     (stack (top-frame s))))))
  :hints (("Goal" :in-theory (enable step-ALOAD_1))))  

; ---------------------------------------------------------
; ALOAD_2

(defthmd step-ALOAD_2
  (implies 
   (poised-inst 'ALOAD_2 s)
   (equal (step s) 
          (execute-ALOAD_X (next-inst s) s 2)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-ALOAD_2
  (implies 
   (poised-inst 'ALOAD_2 s)          
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack 
               (push (nth 2
                          (locals (top-frame s)))
                     (stack (top-frame s))))))
  :hints (("Goal" :in-theory (enable step-ALOAD_2))))

; ---------------------------------------------------------
; ALOAD_3

(defthmd step-ALOAD_3
  (implies 
   (poised-inst 'ALOAD_3 s)
   (equal (step s) 
          (execute-ALOAD_X (next-inst s) s 3)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-ALOAD_3
  (implies 
   (poised-inst 'ALOAD_3 s)          
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack 
               (push (nth 3
                          (locals (top-frame s)))
                     (stack (top-frame s))))))
  :hints (("Goal" :in-theory (enable step-ALOAD_3))))  

; ---------------------------------------------------------
; ANEWARRAY
        
(defthmd step-ANEWARRAY
  (implies 
   (poised-inst 'ANEWARRAY s)
   (equal (step s) 
          (execute-ANEWARRAY (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-ANEWARRAY
  (implies 
   (poised-inst 'ANEWARRAY s)
   (-> s
       (modify s
               :pc (+ 3 (pc (top-frame s)))
               :stack (push (list 'REF (len (heap s)))
                            (pop (stack (top-frame s))))
               :heap (bind (len (heap s))
                           (makearray 'T_REF
                                      (top (stack (top-frame s)))
                                      (init-array 'T_REF 
                                                  (top (stack (top-frame s))))
                                      (class-table s))
                           (heap s)))))
  :hints (("Goal" :in-theory (enable step-ANEWARRAY))))

; ---------------------------------------------------------
; ARETURN

(defthmd ->-execute-ARETURN
  (implies (poised-inst 'ARETURN s)             
           (let* ((val (top (stack (top-frame s))))
                  (s1 (modify s
                              :call-stack (pop (call-stack s)))))
             (-> s
                 (modify  s1                                         
                          :stack (push val                                         
                                       (stack (top-frame s1)))))))
  :hints (("Goal" :in-theory (enable do-inst step))))

; ---------------------------------------------------------
; ARRAYLENGTH

(defthmd step-ARRAYLENGTH
  (implies 
   (poised-inst 'ARRAYLENGTH s)
   (equal (step s) 
          (execute-ARRAYLENGTH (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-ARRAYLENGTH
  (implies 
   (poised-inst 'ARRAYLENGTH s)          
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push (array-bound (deref (top (stack (top-frame s))) 
                                                (heap s)))
                            (pop (stack (top-frame s)))))))
  :hints (("Goal" :in-theory (enable step-ARRAYLENGTH))))  

; ---------------------------------------------------------
; ASTORE

(defthmd step-ASTORE
  (implies 
   (poised-inst 'ASTORE s)
   (equal (step s) 
          (execute-ASTORE (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-ASTORE
  (implies 
   (poised-inst 'ASTORE s) 
   (-> s
       (modify s
               :pc (+ 2 (pc (top-frame s)))
               :locals (update-nth (arg1 (next-inst s))
                                   (top (stack (top-frame s)))
                                   (locals (top-frame s)))
               :stack (pop (stack (top-frame s))))))
  :hints (("Goal" :in-theory (enable step-ASTORE)))) 

; ---------------------------------------------------------
; ASTORE_0

(defthmd step-ASTORE_0
  (implies 
   (poised-inst 'ASTORE_0 s)
   (equal (step s) 
          (execute-ASTORE_X (next-inst s) s 0)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-ASTORE_0
  (implies 
   (poised-inst 'ASTORE_0 s) 
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :locals (update-nth 0
                                   (top (stack (top-frame s)))
                                   (locals (top-frame s)))
               :stack (pop (stack (top-frame s))))))
    :hints (("Goal" :in-theory (enable step-ASTORE_0)))) 

; ---------------------------------------------------------
; ASTORE_1

(defthmd step-ASTORE_1
  (implies 
   (poised-inst 'ASTORE_1 s)
   (equal (step s) 
          (execute-ASTORE_X (next-inst s) s 1)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-ASTORE_1
  (implies 
   (poised-inst 'ASTORE_1 s) 
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :locals (update-nth 1
                                   (top (stack (top-frame s)))
                                   (locals (top-frame s)))
               :stack (pop (stack (top-frame s))))))
  :hints (("Goal" :in-theory (enable step-ASTORE_1))))

; ---------------------------------------------------------
; ASTORE_2

(defthmd step-ASTORE_2
  (implies 
   (poised-inst 'ASTORE_2 s)
   (equal (step s) 
          (execute-ASTORE_X (next-inst s) s 2)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-ASTORE_2
  (implies 
   (poised-inst 'ASTORE_2 s) 
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :locals (update-nth 2
                                   (top (stack (top-frame s)))
                                   (locals (top-frame s)))
               :stack (pop (stack (top-frame s))))))
  :hints (("Goal" :in-theory (enable step-ASTORE_2))))

; ---------------------------------------------------------
; ASTORE_3

(defthmd step-ASTORE_3
  (implies 
   (poised-inst 'ASTORE_3 s)
   (equal (step s) 
          (execute-ASTORE_X (next-inst s) s 3)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-ASTORE_3
  (implies 
   (poised-inst 'ASTORE_3 s) 
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :locals (update-nth 3
                                   (top (stack (top-frame s)))
                                   (locals (top-frame s)))
               :stack (pop (stack (top-frame s))))))
  :hints (("Goal" :in-theory (enable step-ASTORE_3))))

; ---------------------------------------------------------
; BALOAD

(defthmd step-BALOAD
  (implies 
   (poised-inst 'BALOAD s)
   (equal (step s) 
          (execute-BALOAD (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-BALOAD
  (implies 
   (poised-inst 'BALOAD s)
   (let* ((index (top (stack (top-frame s))))
          (array (deref (top (pop (stack (top-frame s)))) 
                        (heap s))))
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push (if (equal (array-type array)
                                       'T_BOOLEAN)
                              (ubyte-fix (element-at index array))
                              (byte-fix (element-at index array)))
                            (pop (pop (stack (top-frame s)))))))))
  :hints (("Goal" :in-theory (enable step-BALOAD))))

; ---------------------------------------------------------
; BASTORE

(defthmd step-BASTORE
  (implies 
   (poised-inst 'BASTORE s)
   (equal (step s) 
          (execute-BASTORE (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-BASTORE
  (implies 
   (poised-inst 'BASTORE s)
   (let* ((arrayref (top (pop (pop (stack (top-frame s)))))))
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (pop (pop (pop (stack (top-frame s)))))
               :heap (bind (cadr arrayref)
                           (set-element-at
                            (if (equal (array-type (deref arrayref (heap s)))
                                       'T_BYTE)
                              (byte-fix (top (stack (top-frame s))))
                              (u-fix (top (stack (top-frame s))) 1))
                            (top (pop (stack (top-frame s))))
                            (deref arrayref (heap s))
                            (class-table s))
                           (heap s))))))
  :hints (("Goal" :in-theory (enable step-BASTORE))))

; ---------------------------------------------------------
; BIPUSH

(defthmd step-BIPUSH
  (implies 
   (poised-inst 'BIPUSH s)
   (equal (step s) 
          (execute-BIPUSH (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-BIPUSH
  (implies 
   (poised-inst 'BIPUSH s)
   (-> s
       (modify s
               :pc (+ 2 (pc (top-frame s)))
               :stack (push (byte-fix (arg1 (next-inst s)))
                            (stack (top-frame s))))))
  :hints (("Goal" :in-theory (enable step-BIPUSH))))

; ---------------------------------------------------------
; CHECKCAST

(defthmd step-CHECKCAST
  (implies 
   (poised-inst 'CHECKCAST s)
   (equal (step s) 
          (execute-CHECKCAST (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-CHECKCAST
  (implies 
   (poised-inst 'CHECKCAST s)
   (-> s
       (modify s
               :pc (+ 3 (pc (top-frame s))))))
  :hints (("Goal" :in-theory (enable step-CHECKCAST))))

; ---------------------------------------------------------
; CALOAD

(defthmd step-CALOAD
  (implies 
   (poised-inst 'CALOAD s)
   (equal (step s) 
          (execute-CALOAD (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-CALOAD
  (implies 
   (poised-inst 'CALOAD s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push (char-fix (element-at (top (stack (top-frame s))) 
                                                  (deref (top (pop (stack (top-frame s)))) 
                                                         (heap s))))
                            (pop (pop (stack (top-frame s))))))))
  :hints (("Goal" :in-theory (enable step-CALOAD))))

; ---------------------------------------------------------
; CASTORE

(defthmd step-CASTORE
  (implies 
   (poised-inst 'CASTORE s)
   (equal (step s) 
          (execute-CASTORE (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-CASTORE
  (implies 
   (poised-inst 'CASTORE s)
   (let* ((arrayref (top (pop (pop (stack (top-frame s)))))))
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (pop (pop (pop (stack (top-frame s)))))
               :heap (bind (cadr arrayref)
                           (set-element-at (char-fix (top (stack (top-frame s))))
                                           (top (pop (stack (top-frame s))))
                                           (deref arrayref (heap s))
                                           (class-table s))
                           (heap s))))))
  :hints (("Goal" :in-theory (enable step-CASTORE))))

; ---------------------------------------------------------
; DUP

(defthmd step-DUP
  (implies 
   (poised-inst 'DUP s)
   (equal (step s) 
          (execute-DUP (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-DUP
  (implies 
   (poised-inst 'DUP s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push (top (stack (top-frame s)))
                            (stack (top-frame s))))))
:hints (("Goal" :in-theory (enable step-DUP)))) 

; ---------------------------------------------------------
; DUP_X1

(defthmd step-DUP_X1
  (implies 
   (poised-inst 'DUP_X1 s)
   (equal (step s) 
          (execute-DUP_X1 (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-DUP_X1
  (implies 
   (poised-inst 'DUP_X1 s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push (top (stack (top-frame s))) 
                            (push (top (pop (stack (top-frame s)))) 
                                  (push (top (stack (top-frame s))) 
                                        (pop (pop (stack (top-frame s))))))))))
:hints (("Goal" :in-theory (enable step-DUP_X1)))) 

; ---------------------------------------------------------
; DUP_X2

(defthmd step-DUP_X2
  (implies 
   (poised-inst 'DUP_X2 s)
   (equal (step s) 
          (execute-DUP_X2 (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-DUP_X2
  (implies 
   (poised-inst 'DUP_X2 s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push (top (stack (top-frame s)))
                            (push (top (pop (stack (top-frame s))))
                                  (push (top (popn 2 (stack (top-frame s))))
                                        (push (top (stack (top-frame s))) 
                                              (popn 3 (stack (top-frame s))))))))))
:hints (("Goal" :in-theory (enable step-DUP_X2)))) 

; ---------------------------------------------------------
; DUP2
        
(defthmd step-DUP2
  (implies 
   (poised-inst 'DUP2 s)
   (equal (step s) 
          (execute-DUP2 (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

; ---------------------------------------------------------
; DUP2_X1

(defthmd step-DUP2_X1
  (implies 
   (poised-inst 'DUP2_X1 s)
   (equal (step s) 
          (execute-DUP2_X1 (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-DUP2_X1
  (implies 
   (poised-inst 'DUP2_X1 s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push (top (stack (top-frame s)))
                            (push (top (pop (stack (top-frame s))))
                                  (push (top (popn 2 (stack (top-frame s))))
                                        (push (top (stack (top-frame s)))
                                              (push (top (pop (stack (top-frame s)))) 
                                                    (popn 3 (stack (top-frame s)))))))))))
  :hints (("Goal" :in-theory (enable step-DUP2_X1))))

; ---------------------------------------------------------
; DUP2_X2

(defthmd step-DUP2_X2
  (implies 
   (poised-inst 'DUP2_X2 s)
   (equal (step s) 
          (execute-DUP2_X2 (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-DUP2_X2
  (implies 
   (poised-inst 'DUP2_X2 s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push (top (stack (top-frame s)))
                            (push (top (pop (stack (top-frame s))))
                                  (push (top (popn 2 (stack (top-frame s))))
                                        (push (top (popn 3 (stack (top-frame s))))
                                              (push (top (stack (top-frame s)))
                                                    (push (top (pop (stack (top-frame s)))) 
                                                          (popn 4 (stack (top-frame s))))))))))))
:hints (("Goal" :in-theory (enable step-DUP2_X2))))

; ---------------------------------------------------------
; GETFIELD

(defthmd step-GETFIELD
  (implies 
   (poised-inst 'GETFIELD s)
   (equal (step s) 
          (execute-GETFIELD (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-GETFIELD
  (implies 
   (poised-inst 'GETFIELD s)
   (let* ((instance (deref (top (stack (top-frame s))) 
                           (heap s)))
          (field-value (field-value (arg1 (next-inst s)) 
                                    (arg2 (next-inst s)) 
                                    instance)))           
     (-> s
         (modify  s
                  :pc (+ 3 (pc (top-frame s)))
                  :stack (if (arg3 (next-inst s))
                           (push 0 (push field-value
                                         (pop (stack (top-frame s)))))
                           (push field-value
                                 (pop (stack (top-frame s)))))))))
:hints (("Goal" :in-theory (enable step-GETFIELD))))  

; ---------------------------------------------------------
; GETSTATIC

(defthmd step-GETSTATIC
  (implies 
   (poised-inst 'GETSTATIC s)
   (equal (step s) 
          (execute-GETSTATIC (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-GETSTATIC
  (implies 
   (poised-inst 'GETSTATIC s)
   (let* ((field-value (static-field-value (arg1 (next-inst s)) 
                                           (arg2 (next-inst s)) 
                                           (heap s)
                                           (class-table s))))           
     (-> s
         (modify s
                 :pc (+ 3 (pc (top-frame s)))
                 :stack (if (arg3 (next-inst s))
                          (push 0 (push field-value (stack (top-frame s))))
                          (push field-value (stack (top-frame s))))))))
:hints (("Goal" :in-theory (enable step-GETSTATIC))))

; ---------------------------------------------------------
; GOTO

(defthmd step-GOTO
  (implies 
   (poised-inst 'GOTO s)
   (equal (step s) 
          (execute-GOTO (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-GOTO
  (implies 
   (poised-inst 'GOTO s)
   (-> s
       (modify s
               :pc (+ (arg1 (next-inst s)) 
                      (pc (top-frame s))))))
:hints (("Goal" :in-theory (enable step-GOTO))))

; ---------------------------------------------------------
; GOTO_W
        
(defthmd step-GOTO_W
  (implies 
   (poised-inst 'GOTO_W s)
   (equal (step s) 
          (execute-GOTO_W (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-GOTO_W
  (implies 
   (poised-inst 'GOTO_W s)
   (-> s
       (modify s
               :pc (+ (arg1 (next-inst s)) 
                      (pc (top-frame s))))))
:hints (("Goal" :in-theory (enable step-GOTO_W))))

; ---------------------------------------------------------
; I2B

(defthmd step-I2B
  (implies 
   (poised-inst 'I2B s)
   (equal (step s) 
          (execute-I2B (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-I2B
  (implies 
   (poised-inst 'I2B s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push (byte-fix (top (stack (top-frame s))))
                            (pop (stack (top-frame s)))))))
:hints (("Goal" :in-theory (enable step-I2B))))

; ---------------------------------------------------------
; I2C
        
(defthmd step-I2C
  (implies 
   (poised-inst 'I2C s)
   (equal (step s) 
          (execute-I2C (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-I2C
  (implies 
   (poised-inst 'I2C s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push (char-fix (top (stack (top-frame s))))
                            (pop (stack (top-frame s)))))))
:hints (("Goal" :in-theory (enable step-I2C))))

; ---------------------------------------------------------
; I2L
        
(defthmd step-I2L
  (implies 
   (poised-inst 'I2L s)
   (equal (step s) 
          (execute-I2L (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-I2L
  (implies 
   (poised-inst 'I2L s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push 0
                            (push (long-fix (top (stack (top-frame s))))
                                  (pop (stack (top-frame s))))))))
:hints (("Goal" :in-theory (enable step-I2L))))

; ---------------------------------------------------------
; I2S

(defthmd step-I2S
  (implies 
   (poised-inst 'I2S s)
   (equal (step s) 
          (execute-I2S (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-I2S
  (implies 
   (poised-inst 'I2S s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push (short-fix (top (stack (top-frame s))))
                            (pop (stack (top-frame s)))))))
:hints (("Goal" :in-theory (enable step-I2S))))

; ---------------------------------------------------------
; IADD

(defthmd step-IADD
  (implies 
   (poised-inst 'IADD s)
   (equal (step s) 
          (execute-IADD (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-IADD
  (implies 
   (poised-inst 'IADD s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push (int-fix
                             (+ (top (pop (stack (top-frame s))))
                                (top (stack (top-frame s)))))
                            (pop (pop (stack (top-frame s))))))))
:hints (("Goal" :in-theory (enable step-IADD))))

; ---------------------------------------------------------
; IALOAD

(defthmd step-IALOAD
  (implies 
   (poised-inst 'IALOAD s)
   (equal (step s) 
          (execute-IALOAD (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-IALOAD
  (implies 
   (poised-inst 'IALOAD s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
                :stack (push (element-at (top (stack (top-frame s))) 
                                         (deref (top (pop (stack (top-frame s))))
                                                (heap s)))
                             (pop (pop (stack (top-frame s))))))))
:hints (("Goal" :in-theory (enable step-IALOAD))))

; ---------------------------------------------------------
; IAND

(defthmd step-IAND
  (implies 
   (poised-inst 'IAND s)
   (equal (step s) 
          (execute-IAND (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-IAND
  (implies 
   (poised-inst 'IAND s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push (logand (top (pop (stack (top-frame s))))
                                    (top (stack (top-frame s))))
                            (pop (pop (stack (top-frame s))))))))
:hints (("Goal" :in-theory (enable step-IAND))))

; ---------------------------------------------------------
; IASTORE

(defthmd step-IASTORE
  (implies 
   (poised-inst 'IASTORE s)
   (equal (step s) 
          (execute-IASTORE (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-IASTORE
  (implies 
   (poised-inst 'IASTORE s)
   (let* ((arrayref (top (pop (pop (stack (top-frame s)))))))
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (pop (pop (pop (stack (top-frame s)))))
               :heap (bind (cadr arrayref)
                           (set-element-at (top (stack (top-frame s)))
                                           (top (pop (stack (top-frame s))))
                                           (deref arrayref (heap s))
                                           (class-table s))
                           (heap s))))))
   :hints (("Goal" :in-theory (enable step-IASTORE))))

; ---------------------------------------------------------
; ICONST_M1

(defthmd step-ICONST_M1
  (implies 
   (poised-inst 'ICONST_M1 s)
   (equal (step s) 
          (execute-ICONST_X (next-inst s) s -1)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-ICONST_M1
  (implies 
   (poised-inst 'ICONST_M1 s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push -1 (stack (top-frame s))))))
:hints (("Goal" :in-theory (enable step-ICONST_M1))))

; ---------------------------------------------------------
; ICONST_0

(defthmd step-ICONST_0
  (implies 
   (poised-inst 'ICONST_0 s)
   (equal (step s) 
          (execute-ICONST_X (next-inst s) s 0)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-ICONST_0
  (implies 
   (poised-inst 'ICONST_0 s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push 0 (stack (top-frame s))))))
:hints (("Goal" :in-theory (enable step-ICONST_0))))

; ---------------------------------------------------------
; ICONST_1
        
(defthmd step-ICONST_1
  (implies 
   (poised-inst 'ICONST_1 s)
   (equal (step s) 
          (execute-ICONST_X (next-inst s) s 1)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-ICONST_1
  (implies 
   (poised-inst 'ICONST_1 s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push 1 (stack (top-frame s))))))
:hints (("Goal" :in-theory (enable step-ICONST_1))))

; ---------------------------------------------------------
; ICONST_2

(defthmd step-ICONST_2
  (implies 
   (poised-inst 'ICONST_2 s)
   (equal (step s) 
          (execute-ICONST_X (next-inst s) s 2)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-ICONST_2
  (implies 
   (poised-inst 'ICONST_2 s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push 2 (stack (top-frame s))))))
:hints (("Goal" :in-theory (enable step-ICONST_2))))

; ---------------------------------------------------------
; ICONST_3
        
(defthmd step-ICONST_3
  (implies 
   (poised-inst 'ICONST_3 s)
   (equal (step s) 
          (execute-ICONST_X (next-inst s) s 3)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-ICONST_3
  (implies 
   (poised-inst 'ICONST_3 s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push 3 (stack (top-frame s))))))
:hints (("Goal" :in-theory (enable step-ICONST_3))))

; ---------------------------------------------------------
; ICONST_4
        
(defthmd step-ICONST_4
  (implies 
   (poised-inst 'ICONST_4 s)
   (equal (step s) 
          (execute-ICONST_X (next-inst s) s 4)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-ICONST_4
  (implies 
   (poised-inst 'ICONST_4 s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push 4 (stack (top-frame s))))))
:hints (("Goal" :in-theory (enable step-ICONST_4))))

; ---------------------------------------------------------
; ICONST_5

(defthmd step-ICONST_5
  (implies 
   (poised-inst 'ICONST_5 s)
   (equal (step s) 
          (execute-ICONST_X (next-inst s) s 5)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-ICONST_5
  (implies 
   (poised-inst 'ICONST_5 s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push 5 (stack (top-frame s))))))
:hints (("Goal" :in-theory (enable step-ICONST_5))))

; ---------------------------------------------------------
; IDIV

(defthmd step-IDIV
  (implies 
   (poised-inst 'IDIV s)
   (equal (step s) 
          (execute-IDIV (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-IDIV
  (implies 
   (poised-inst 'IDIV s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push (int-fix
                             (truncate (top (pop (stack (top-frame s))))
                                       (top (stack (top-frame s)))))
                            (pop (pop (stack (top-frame s))))))))
:hints (("Goal" :in-theory (enable step-IDIV))))

; ---------------------------------------------------------
; IF_ACMPEQ
        
(defthmd step-IF_ACMPEQ
  (implies 
   (poised-inst 'IF_ACMPEQ s)
   (equal (step s) 
          (execute-IF_ACMPEQ (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-IF_ACMPEQ
  (implies 
   (poised-inst 'IF_ACMPEQ s)
   (-> s
       (modify s
               :pc (if (equal (top (pop (stack (top-frame s))))
                              (top (stack (top-frame s))))
                     (+ (arg1 (next-inst s)) 
                        (pc (top-frame s)))
                     (+ 3 (pc (top-frame s))))
               :stack (pop (pop (stack (top-frame s)))))))
  :hints (("Goal" :in-theory (enable step-IF_ACMPEQ))))  

; ---------------------------------------------------------
; IF_ACMPNE

(defthmd step-IF_ACMPNE
  (implies 
   (poised-inst 'IF_ACMPNE s)
   (equal (step s) 
          (execute-IF_ACMPNE (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-IF_ACMPNE
  (implies 
   (poised-inst 'IF_ACMPNE s)
   (-> s
       (modify s
          :pc (if (equal (top (pop (stack (top-frame s))))
                         (top (stack (top-frame s))))
                  (+ 3 (pc (top-frame s)))
                  (+ (arg1 (next-inst s)) 
                     (pc (top-frame s))))
          :stack (pop (pop (stack (top-frame s)))))))
  :hints (("Goal" :in-theory (enable step-IF_ACMPNE))))  

; ---------------------------------------------------------
; IF_ICMPEQ

(defthmd step-IF_ICMPEQ
  (implies 
   (poised-inst 'IF_ICMPEQ s)
   (equal (step s) 
          (execute-IF_ICMPEQ (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-IF_ICMPEQ
  (implies 
   (poised-inst 'IF_ICMPEQ s)
   (-> s
       (modify s
               :pc (if (equal (top (pop (stack (top-frame s))))
                              (top (stack (top-frame s))))
                     (+ (arg1 (next-inst s))
                        (pc (top-frame s)))
                     (+ 3 (pc (top-frame s))))
          :stack (pop (pop (stack (top-frame s)))))))
  :hints (("Goal" :in-theory (enable step-IF_ICMPEQ))))  

; ---------------------------------------------------------
; IF_ICMPGE

(defthmd step-IF_ICMPGE
  (implies 
   (poised-inst 'IF_ICMPGE s)
   (equal (step s) 
          (execute-IF_ICMPGE (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-IF_ICMPGE
  (implies 
   (poised-inst 'IF_ICMPGE s)
   (-> s
       (modify s
          :pc (if (>= (top (pop (stack (top-frame s))))
                      (top (stack (top-frame s))))
                (+ (arg1 (next-inst s)) 
                   (pc (top-frame s)))
                (+ 3 (pc (top-frame s))))
          :stack (pop (pop (stack (top-frame s)))))))
  :hints (("Goal" :in-theory (enable step-IF_ICMPGE))))  

; ---------------------------------------------------------
; IF_ICMPGT

(defthmd step-IF_ICMPGT
  (implies 
   (poised-inst 'IF_ICMPGT s)
   (equal (step s) 
          (execute-IF_ICMPGT (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-IF_ICMPGT
  (implies 
   (poised-inst 'IF_ICMPGT s)
   (-> s
       (modify s
          :pc (if (> (top (pop (stack (top-frame s))))
                      (top (stack (top-frame s))))
                (+ (arg1 (next-inst s)) 
                   (pc (top-frame s)))
                (+ 3 (pc (top-frame s))))
          :stack (pop (pop (stack (top-frame s)))))))
  :hints (("Goal" :in-theory (enable step-IF_ICMPGT))))  

; ---------------------------------------------------------
; IF_ICMPLE

(defthmd step-IF_ICMPLE
  (implies 
   (poised-inst 'IF_ICMPLE s)
   (equal (step s) 
          (execute-IF_ICMPLE (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-IF_ICMPLE
  (implies 
   (poised-inst 'IF_ICMPLE s)
   (-> s
       (modify s
          :pc (if (<= (top (pop (stack (top-frame s))))
                      (top (stack (top-frame s))))
                (+ (arg1 (next-inst s)) 
                   (pc (top-frame s)))
                (+ 3 (pc (top-frame s))))
          :stack (pop (pop (stack (top-frame s)))))))
  :hints (("Goal" :in-theory (enable step-IF_ICMPLE))))  

; ---------------------------------------------------------
; IF_ICMPLT

(defthmd step-IF_ICMPLT
  (implies 
   (poised-inst 'IF_ICMPLT s)
   (equal (step s) 
          (execute-IF_ICMPLT (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-IF_ICMPLT
  (implies 
   (poised-inst 'IF_ICMPLT s)  
   (-> s
       (modify s
          :pc (if (< (top (pop (stack (top-frame s))))
                     (top (stack (top-frame s))))
                (+ (arg1 (next-inst s)) 
                   (pc (top-frame s)))
                (+ 3 (pc (top-frame s))))
          :stack (pop (pop (stack (top-frame s)))))))
  :hints (("Goal" :in-theory (enable step-IF_ICMPLT))))  

; ---------------------------------------------------------
; IF_ICMPNE

(defthmd step-IF_ICMPNE
  (implies 
   (poised-inst 'IF_ICMPNE s)
   (equal (step s) 
          (execute-IF_ICMPNE (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-IF_ICMPNE
  (implies 
   (poised-inst 'IF_ICMPNE s)
   (-> s
       (modify s
          :pc (if (equal (top (pop (stack (top-frame s))))
                         (top (stack (top-frame s))))
                (+ 3 (pc (top-frame s)))
                (+ (arg1 (next-inst s)) 
                   (pc (top-frame s))))
          :stack (pop (pop (stack (top-frame s)))))))
  :hints (("Goal" :in-theory (enable step-IF_ICMPNE))))  

; ---------------------------------------------------------
; IFEQ

(defthmd step-IFEQ
  (implies 
   (poised-inst 'IFEQ s)
   (equal (step s) 
          (execute-IFEQ (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-IFEQ
  (implies 
   (poised-inst 'IFEQ s)
   (-> s
       (modify s
               :pc (if (equal (top (stack (top-frame s))) 0)
                     (+ (arg1 (next-inst s)) 
                        (pc (top-frame s)))
                     (+ 3 (pc (top-frame s))))
               :stack (pop (stack (top-frame s))))))
  :hints (("Goal" :in-theory (enable step-IFEQ))))  

; ---------------------------------------------------------
; IFGE

(defthmd step-IFGE
  (implies 
   (poised-inst 'IFGE s)
   (equal (step s) 
          (execute-IFGE (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-IFGE
  (implies 
   (poised-inst 'IFGE s)
   (-> s
       (modify s
               :pc (if (>= (top (stack (top-frame s))) 0)
                     (+ (arg1 (next-inst s)) 
                        (pc (top-frame s)))
                     (+ 3 (pc (top-frame s))))
               :stack (pop (stack (top-frame s))))))
  :hints (("Goal" :in-theory (enable step-IFGE))))  

; ---------------------------------------------------------
; IFGT

(defthmd step-IFGT
  (implies 
   (poised-inst 'IFGT s)
   (equal (step s) 
          (execute-IFGT (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-IFGT
  (implies 
   (poised-inst 'IFGT s)
   (-> s
       (modify s
               :pc (if (> (top (stack (top-frame s))) 0)
                     (+ (arg1 (next-inst s)) 
                        (pc (top-frame s)))
                     (+ 3 (pc (top-frame s))))
               :stack (pop (stack (top-frame s))))))
  :hints (("Goal" :in-theory (enable step-IFGT))))  

; ---------------------------------------------------------
; IFLE

(defthmd step-IFLE
  (implies 
   (poised-inst 'IFLE s)
   (equal (step s) 
          (execute-IFLE (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-IFLE
  (implies 
   (poised-inst 'IFLE s)
   (-> s
       (modify s
               :pc (if (<= (top (stack (top-frame s))) 0)
                     (+ (arg1 (next-inst s)) 
                        (pc (top-frame s)))
                     (+ 3 (pc (top-frame s))))
               :stack (pop (stack (top-frame s))))))
  :hints (("Goal" :in-theory (enable step-IFLE))))  

; ---------------------------------------------------------
; IFLT

(defthmd step-IFLT
  (implies 
   (poised-inst 'IFLT s)
   (equal (step s) 
          (execute-IFLT (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-IFLT
  (implies 
   (poised-inst 'IFLT s)
   (-> s
       (modify s
               :pc (if (< (top (stack (top-frame s))) 0)
                     (+ (arg1 (next-inst s)) 
                        (pc (top-frame s)))
                     (+ 3 (pc (top-frame s))))
               :stack (pop (stack (top-frame s))))))
  :hints (("Goal" :in-theory (enable step-IFLT))))  

; ---------------------------------------------------------
; IFNE

(defthmd step-IFNE
  (implies 
   (poised-inst 'IFNE s)
   (equal (step s) 
          (execute-IFNE (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-IFNE
  (implies 
   (poised-inst 'IFNE s)
   (-> s
       (modify s
               :pc (if (equal (top (stack (top-frame s))) 0)
                     (+ 3 (pc (top-frame s)))
                     (+ (arg1 (next-inst s)) 
                        (pc (top-frame s))))
               :stack (pop (stack (top-frame s))))))
  :hints (("Goal" :in-theory (enable step-IFNE))))  

; ---------------------------------------------------------
; IFNONNULL

(defthmd step-IFNONNULL
  (implies 
   (poised-inst 'IFNONNULL s)
   (equal (step s) 
          (execute-IFNONNULL (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-IFNONNULL
  (implies 
   (poised-inst 'IFNONNULL s)
   (-> s
       (modify s
               :pc (if (equal (top (stack (top-frame s))) 
                              (nullref))
                     (+ 3 (pc (top-frame s)))
                     (+ (arg1 (next-inst s)) 
                        (pc (top-frame s))))
               :stack (pop (stack (top-frame s))))))
  :hints (("Goal" :in-theory (enable step-IFNONNULL))))  

; ---------------------------------------------------------
; IFNULL

(defthmd step-IFNULL
  (implies 
   (poised-inst 'IFNULL s)
   (equal (step s) 
          (execute-IFNULL (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-IFNULL
  (implies 
   (poised-inst 'IFNULL s)
   (-> s
       (modify s
               :pc (if (equal (top (stack (top-frame s))) 
                              (nullref))
                     (+ (arg1 (next-inst s)) 
                        (pc (top-frame s)))                     
                     (+ 3 (pc (top-frame s))))
               :stack (pop (stack (top-frame s))))))
  :hints (("Goal" :in-theory (enable step-IFNULL))))  

; ---------------------------------------------------------
; IINC

(defthmd step-IINC
  (implies 
   (poised-inst 'IINC s)
   (equal (step s) 
          (execute-IINC (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-IINC
  (implies 
   (poised-inst 'IINC s)
   (-> s
       (modify s
               :pc (+ 3 (pc (top-frame s)))
               :locals 
               (update-nth (arg1 (next-inst s))
                           (int-fix
                            (+ (arg2 (next-inst s))
                               (nth (arg1 (next-inst s)) 
                                    (locals (top-frame s)))))
                           (locals (top-frame s))))))
  :hints (("Goal" :in-theory (enable step-IINC))))  

; ---------------------------------------------------------
; ILOAD

(defthmd step-ILOAD
  (implies 
   (poised-inst 'ILOAD s)
   (equal (step s) 
          (execute-ILOAD (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-ILOAD
  (implies 
   (poised-inst 'ILOAD s)
   (-> s
       (modify s
               :pc (+ 2 (pc (top-frame s)))
               :stack (push (nth (arg1 (next-inst s))
                                 (locals (top-frame s)))
                            (stack (top-frame s))))))
  :hints (("Goal" :in-theory (enable step-ILOAD))))  
  
; ---------------------------------------------------------
; ILOAD_0

(defthmd step-ILOAD_0
  (implies 
   (poised-inst 'ILOAD_0 s)
   (equal (step s) 
          (execute-ILOAD_X (next-inst s) s 0)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-ILOAD_0
  (implies 
   (poised-inst 'ILOAD_0 s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push (nth 0
                                 (locals (top-frame s)))
                            (stack (top-frame s))))))
  :hints (("Goal" :in-theory (enable step-ILOAD_0)))) 

; ---------------------------------------------------------
; ILOAD_1

(defthmd step-ILOAD_1
  (implies 
   (poised-inst 'ILOAD_1 s)
   (equal (step s) 
          (execute-ILOAD_X (next-inst s) s 1)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-ILOAD_1
  (implies 
   (poised-inst 'ILOAD_1 s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push (nth 1
                                 (locals (top-frame s)))
                            (stack (top-frame s))))))
  :hints (("Goal" :in-theory (enable step-ILOAD_1)))) 

; ---------------------------------------------------------
; ILOAD_2

(defthmd step-ILOAD_2
  (implies 
   (poised-inst 'ILOAD_2 s)
   (equal (step s) 
          (execute-ILOAD_X (next-inst s) s 2)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-ILOAD_2
  (implies 
   (poised-inst 'ILOAD_2 s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push (nth 2
                                 (locals (top-frame s)))
                            (stack (top-frame s))))))
  :hints (("Goal" :in-theory (enable step-ILOAD_2)))) 

; ---------------------------------------------------------
; ILOAD_3

(defthmd step-ILOAD_3
  (implies 
   (poised-inst 'ILOAD_3 s)
   (equal (step s) 
          (execute-ILOAD_X (next-inst s) s 3)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-ILOAD_3
  (implies 
   (poised-inst 'ILOAD_3 s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push (nth 3
                                 (locals (top-frame s)))
                            (stack (top-frame s))))))
  :hints (("Goal" :in-theory (enable step-ILOAD_3)))) 

; ---------------------------------------------------------
; IMUL

(defthmd step-IMUL
  (implies 
   (poised-inst 'IMUL s)
   (equal (step s) 
          (execute-IMUL (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-IMUL
  (implies 
   (poised-inst 'IMUL s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push (int-fix
                             (* (top (pop (stack (top-frame s))))
                                (top (stack (top-frame s)))))
                            (pop (pop (stack (top-frame s))))))))
  :hints (("Goal" :in-theory (enable step-IMUL)))) 

; ---------------------------------------------------------
; INEG

(defthmd step-INEG
  (implies 
   (poised-inst 'INEG s)
   (equal (step s) 
          (execute-INEG (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-INEG
  (implies 
   (poised-inst 'INEG s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push (if (equal (top (stack (top-frame s)))
                                       *most-negative-integer*)
                              *most-negative-integer*
                              (- (top (stack (top-frame s))))) 
                            (pop (stack (top-frame s)))))))
   :hints (("Goal" :in-theory (enable step-INEG))))

; ---------------------------------------------------------
; INSTANCEOF

(defthmd step-INSTANCEOF
  (implies 
   (poised-inst 'INSTANCEOF s)
   (equal (step s) 
          (execute-INSTANCEOF (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-INSTANCEOF
  (implies 
   (poised-inst 'INSTANCEOF s)
   (-> s       
       (modify s
               :pc (+ 3 (pc (top-frame s)))
               :stack (push (instance-of (arg1 (next-inst s))
                                         (top (stack (top-frame s)))
                                         (heap s)
                                         (class-table s))
                            (pop (stack (top-frame s)))))))
  :hints (("Goal" :in-theory (enable step-INSTANCEOF))))

; ---------------------------------------------------------
; INVOKEINTERFACE

(defthmd step-INVOKEINTERFACE
  (implies 
   (poised-inst 'INVOKEINTERFACE s)
   (equal (step s) 
          (execute-INVOKEINTERFACE (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-INVOKEINTERFACE
  (implies 
   (poised-inst 'INVOKEINTERFACE s)
   (let* ((nformals (arg3 (next-inst s)))
          (obj-ref (top (popn nformals (stack (top-frame s)))))
          (obj-class-name (class-name-of-ref obj-ref (heap s)))
          (closest-method
           (lookup-method (arg2 (next-inst s))
                          obj-class-name
                          (class-table s)))                  
          (s1 (modify s
                      :pc (+ 5 (pc (top-frame s)))
                      :stack (popn (+ nformals 1)
                                   (stack (top-frame s))))))           
     (-> s
         (modify s1
                 :call-stack
                 (push (make-frame 0
                                   (reverse
                                    (bind-formals (+ nformals 1)
                                                  (stack (top-frame s))))
                                   nil
                                   (method-program closest-method)                                
                                   (arg1 (next-inst s)))
                       (call-stack s1))))))
  :hints (("Goal" :in-theory (enable step-INVOKEINTERFACE))))
 
; ---------------------------------------------------------
; INVOKESPECIAL

(defthmd step-INVOKESPECIAL
  (implies 
   (poised-inst 'INVOKESPECIAL s)
   (equal (step s) 
          (execute-INVOKESPECIAL (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-INVOKESPECIAL
  (implies 
   (poised-inst 'INVOKESPECIAL s)
   (let* ((nformals (arg3 (next-inst s)))
          (obj-class-name (arg1 (next-inst s)))
          (closest-method
           (lookup-method (arg2 (next-inst s))
                          obj-class-name
                          (class-table s)))                  
          (s1 (modify s
                      :pc (+ 3 (pc (top-frame s)))
                      :stack (popn (+ nformals 1)
                                   (stack (top-frame s))))))           
     (-> s
         (if (method-isNative? closest-method)
           s
           (modify s1
                 :call-stack
                 (push (make-frame 0
                                   (reverse
                                    (bind-formals (+ nformals 1)
                                                  (stack (top-frame s))))
                                   nil
                                   (method-program closest-method)                                
                                   (arg1 (next-inst s)))
                       (call-stack s1)))))))
  :hints (("Goal" :in-theory (enable step-INVOKESPECIAL))))

; ---------------------------------------------------------
; INVOKESTATIC

(defthmd step-INVOKESTATIC
  (implies 
   (poised-inst 'INVOKESTATIC s)
   (equal (step s) 
          (execute-INVOKESTATIC (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-INVOKESTATIC
  (implies 
   (poised-inst 'INVOKESTATIC s)
   (let* ((nformals (arg3 (next-inst s)))
          (closest-method
           (lookup-method (arg2 (next-inst s))
                          (arg1 (next-inst s))
                          (class-table s)))                  
          (s1 (modify s
                      :pc (+ 3 (pc (top-frame s)))
                      :stack (popn nformals
                                   (stack (top-frame s))))))           
     (-> s
         (if (method-isNative? closest-method)
           (execute-native (arg2 (next-inst s))
                           (modify s
                                   :pc (+ 3 (pc (top-frame s)))))
           (modify s1
                 :call-stack
                 (push (make-frame 0
                                   (reverse
                                    (bind-formals nformals
                                                  (stack (top-frame s))))
                                   nil
                                   (method-program closest-method)                                
                                   (arg1 (next-inst s)))
                       (call-stack s1)))))))
  :hints (("Goal" :in-theory (enable step-INVOKESTATIC))))

; ---------------------------------------------------------
; INVOKEVIRTUAL

(defthmd step-INVOKEVIRTUAL
  (implies 
   (poised-inst 'INVOKEVIRTUAL s)
   (equal (step s) 
          (execute-INVOKEVIRTUAL (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-INVOKEVIRTUAL
  (implies 
   (poised-inst 'INVOKEVIRTUAL s)
   (let* ((nformals (arg3 (next-inst s)))
          (obj-class-name (class-name-of-ref (top (popn nformals (stack (top-frame s))))
                                             (heap s)))
          (closest-method
           (lookup-method (arg2 (next-inst s))
                          obj-class-name
                          (class-table s)))                  
          (s1 (modify s
                      :pc (+ 3 (pc (top-frame s)))
                      :stack (popn (+ nformals 1)
                                   (stack (top-frame s))))))           
     (-> s
         (if (method-isNative? closest-method)
           (execute-native (arg2 (next-inst s))
                           (modify s
                                   :pc (+ 3 (pc (top-frame s)))))
           (modify s1
                   :call-stack
                   (push (make-frame 0
                                     (reverse
                                      (bind-formals (+ nformals 1)
                                                    (stack (top-frame s))))
                                     nil
                                     (method-program closest-method)                                
                                     (arg1 (next-inst s)))
                         (call-stack s1)))))))
  :hints (("Goal" :in-theory (enable step-INVOKEVIRTUAL))))

; ---------------------------------------------------------
; IOR

(defthmd step-IOR
  (implies 
   (poised-inst 'IOR s)
   (equal (step s) 
          (execute-IOR (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-IOR
  (implies 
   (poised-inst 'IOR s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push (logior (top (pop (stack (top-frame s))))
                                    (top (stack (top-frame s))))
                            (pop (pop (stack (top-frame s))))))))
  :hints (("Goal" :in-theory (enable step-IOR))))

; ---------------------------------------------------------
; IREM
        
(defthmd step-IREM
  (implies 
   (poised-inst 'IREM s)
   (equal (step s) 
          (execute-IREM (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-IREM
  (implies 
   (poised-inst 'IREM s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push (- (top (pop (stack (top-frame s)))) 
                               (* (truncate (top (pop (stack (top-frame s)))) 
                                            (top (stack (top-frame s)))) 
                                  (top (stack (top-frame s))))) 
                            (pop (pop (stack (top-frame s))))))))
  :hints (("Goal" :in-theory (enable step-IREM))))

; ---------------------------------------------------------
; IRETURN
        
(defthmd step-IRETURN
  (implies 
   (poised-inst 'IRETURN s)
   (equal (step s) 
          (execute-IRETURN (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-IRETURN
  (implies 
   (poised-inst 'IRETURN s)
   (let* ((s1 (modify s
                      :call-stack (pop (call-stack s)))))
   (-> s
       (modify s1               
               :stack (push (top (stack (top-frame s))) 
                            (stack (top-frame s1)))))))
  :hints (("Goal" :in-theory (enable step-IRETURN))))

; ---------------------------------------------------------
; ISHL

(defthmd step-ISHL
  (implies 
   (poised-inst 'ISHL s)
   (equal (step s) 
          (execute-ISHL (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-ISHL
  (implies 
   (poised-inst 'ISHL s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push (int-fix (shl (top (pop (stack (top-frame s)))) 
                                          (5-bit-fix (top (stack (top-frame s))))))
                            (pop (pop (stack (top-frame s))))))))
  :hints (("Goal" :in-theory (enable step-ISHL))))

; ---------------------------------------------------------
; ISHR
        
(defthmd step-ISHR
  (implies 
   (poised-inst 'ISHR s)
   (equal (step s) 
          (execute-ISHR (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-ISHR
  (implies 
   (poised-inst 'ISHR s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push (int-fix (shr (top (pop (stack (top-frame s)))) 
                                          (5-bit-fix (top (stack (top-frame s))))))
                            (pop (pop (stack (top-frame s))))))))
  :hints (("Goal" :in-theory (enable step-ISHR))))

; ---------------------------------------------------------
; ISTORE

(defthmd step-ISTORE
  (implies 
   (poised-inst 'ISTORE s)
   (equal (step s) 
          (execute-ISTORE (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-ISTORE
  (implies 
   (poised-inst 'ISTORE s)
   (-> s
       (modify s
               :pc (+ 2 (pc (top-frame s)))
               :locals (update-nth (arg1 (next-inst s))
                                   (top (stack (top-frame s)))
                                   (locals (top-frame s)))
               :stack (pop (stack (top-frame s))))))
  :hints (("Goal" :in-theory (enable step-ISTORE))))

; ---------------------------------------------------------
; ISTORE_0

(defthmd step-ISTORE_0
  (implies 
   (poised-inst 'ISTORE_0 s)
   (equal (step s) 
          (execute-ISTORE_X (next-inst s) s 0)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-ISTORE_0
  (implies 
   (poised-inst 'ISTORE_0 s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :locals (update-nth 0
                                   (top (stack (top-frame s)))
                                   (locals (top-frame s)))
               :stack (pop (stack (top-frame s))))))
  :hints (("Goal" :in-theory (enable step-ISTORE_0))))

; ---------------------------------------------------------
; ISTORE_1

(defthmd step-ISTORE_1
  (implies 
   (poised-inst 'ISTORE_1 s)
   (equal (step s) 
          (execute-ISTORE_X (next-inst s) s 1)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-ISTORE_1
  (implies 
   (poised-inst 'ISTORE_1 s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :locals (update-nth 1
                                   (top (stack (top-frame s)))
                                   (locals (top-frame s)))
               :stack (pop (stack (top-frame s))))))
  :hints (("Goal" :in-theory (enable step-ISTORE_1))))

; ---------------------------------------------------------
; ISTORE_2

(defthmd step-ISTORE_2
  (implies 
   (poised-inst 'ISTORE_2 s)
   (equal (step s) 
          (execute-ISTORE_X (next-inst s) s 2)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-ISTORE_2
  (implies 
   (poised-inst 'ISTORE_2 s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :locals (update-nth 2
                                   (top (stack (top-frame s)))
                                   (locals (top-frame s)))
               :stack (pop (stack (top-frame s))))))
  :hints (("Goal" :in-theory (enable step-ISTORE_2))))

; ---------------------------------------------------------
; ISTORE_3
        
(defthmd step-ISTORE_3
  (implies 
   (poised-inst 'ISTORE_3 s)
   (equal (step s) 
          (execute-ISTORE_X (next-inst s) s 3)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-ISTORE_3
  (implies 
   (poised-inst 'ISTORE_3 s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :locals (update-nth 3
                                   (top (stack (top-frame s)))
                                   (locals (top-frame s)))
               :stack (pop (stack (top-frame s))))))
  :hints (("Goal" :in-theory (enable step-ISTORE_3))))

; ---------------------------------------------------------
; ISUB
        
(defthmd step-ISUB
  (implies 
   (poised-inst 'ISUB s)
   (equal (step s) 
          (execute-ISUB (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-ISUB
  (implies 
   (poised-inst 'ISUB s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push (int-fix (- (top (pop (stack (top-frame s))))
                                        (top (stack (top-frame s)))))
                            (pop (pop (stack (top-frame s))))))))
  :hints (("Goal" :in-theory (enable step-ISUB))))

; ---------------------------------------------------------
; IUSHR
        
(defthmd step-IUSHR
  (implies 
   (poised-inst 'IUSHR s)
   (equal (step s) 
          (execute-IUSHR (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-IUSHR
  (implies 
   (poised-inst 'IUSHR s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push (int-fix (iushr (top (pop (stack (top-frame s)))) 
                                            (5-bit-fix (top (stack (top-frame s))))))
                            (pop (pop (stack (top-frame s))))))))
  :hints (("Goal" :in-theory (enable step-IUSHR))))

; ---------------------------------------------------------
; IXOR
        
(defthmd step-IXOR
  (implies 
   (poised-inst 'IXOR s)
   (equal (step s) 
          (execute-IXOR (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-IXOR
  (implies 
   (poised-inst 'IXOR s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push (logxor (top (pop (stack (top-frame s))))
                                    (top (stack (top-frame s))))
                            (pop (pop (stack (top-frame s))))))))
  :hints (("Goal" :in-theory (enable step-IXOR))))

; ---------------------------------------------------------
; JSR
        
; ---------------------------------------------------------
; JSR_W
        
; ---------------------------------------------------------
; L2I
        
(defthmd step-L2I
  (implies 
   (poised-inst 'L2I s)
   (equal (step s) 
          (execute-L2I (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-L2I
  (implies 
   (poised-inst 'L2I s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push (int-fix (top (pop (stack (top-frame s)))))
                            (pop (pop (stack (top-frame s))))))))
  :hints (("Goal" :in-theory (enable step-L2I))))

; ---------------------------------------------------------
; LADD

(defthmd step-LADD
  (implies 
   (poised-inst 'LADD s)
   (equal (step s) 
          (execute-LADD (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-LADD
  (implies 
   (poised-inst 'LADD s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push 0
                            (push (long-fix (+ (top (pop (stack (top-frame s)))) 
                                               (top (popn 3 (stack (top-frame s))))))
                                  (popn 4 (stack (top-frame s))))))))
  :hints (("Goal" :in-theory (enable step-LADD))))

; ---------------------------------------------------------
; LALOAD
        
(defthmd step-LALOAD
  (implies 
   (poised-inst 'LALOAD s)
   (equal (step s) 
          (execute-LALOAD (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-LALOAD
  (implies 
   (poised-inst 'LALOAD s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push 0
                            (push (element-at (top (stack (top-frame s))) 
                                              (deref (top (pop (stack (top-frame s)))) 
                                                     (heap s)))
                                  (pop (pop (stack (top-frame s)))))))))
  :hints (("Goal" :in-theory (enable step-LALOAD))))

; ---------------------------------------------------------
; LAND

(defthmd step-LAND
  (implies 
   (poised-inst 'LAND s)
   (equal (step s) 
          (execute-LAND (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-LAND
  (implies 
   (poised-inst 'LAND s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push 0
                            (push (logand (top (pop (stack (top-frame s)))) 
                                          (top (popn 3 (stack (top-frame s)))))
                                  (popn 4 (stack (top-frame s))))))))
  :hints (("Goal" :in-theory (enable step-LAND))))

; ---------------------------------------------------------
; LASTORE

(defthmd step-LASTORE
  (implies 
   (poised-inst 'LASTORE s)
   (equal (step s) 
          (execute-LASTORE (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-LASTORE
  (implies 
   (poised-inst 'LASTORE s)
   (let* ((arrayref (top (popn 3 (stack (top-frame s))))))
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (popn 4 (stack (top-frame s)))
               :heap (bind (cadr arrayref)
                           (set-element-at (top (pop (stack (top-frame s))))
                                           (top (pop (pop (stack (top-frame s)))))
                                           (deref arrayref (heap s))
                                           (class-table s))
                           (heap s))))))
   :hints (("Goal" :in-theory (enable step-LASTORE))))

; ---------------------------------------------------------
; LCMP

(defthmd step-LCMP
  (implies 
   (poised-inst 'LCMP s)
   (equal (step s) 
          (execute-LCMP (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-LCMP
  (implies 
   (poised-inst 'LCMP s)
   (let* ((val2 (top (pop (stack (top-frame s)))))
          (val1 (top (popn 3 (stack (top-frame s)))))
          (result (cond ((> val1 val2) 1)
                        ((< val1 val2) -1)
                        (t 0))))
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push result
                            (popn 4 (stack (top-frame s))))))))
  :hints (("Goal" :in-theory (enable step-LCMP))))

; ---------------------------------------------------------
; LCONST_0

(defthmd step-LCONST_0
  (implies 
   (poised-inst 'LCONST_0 s)
   (equal (step s) 
          (execute-LCONST_X (next-inst s) s 0)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-LCONST_0
  (implies 
   (poised-inst 'LCONST_0 s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push 0
                            (push 0 (stack (top-frame s)))))))
  :hints (("Goal" :in-theory (enable step-LCONST_0))))

; ---------------------------------------------------------
; LCONST_1

(defthmd step-LCONST_1
  (implies 
   (poised-inst 'LCONST_1 s)
   (equal (step s) 
          (execute-LCONST_X (next-inst s) s 1)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-LCONST_1
  (implies 
   (poised-inst 'LCONST_1 s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push 0
                            (push 1 (stack (top-frame s)))))))
  :hints (("Goal" :in-theory (enable step-LCONST_1))))

; ---------------------------------------------------------
; LDC

(defthmd step-LDC
  (implies 
   (poised-inst 'LDC s)
   (equal (step s) 
          (execute-LDC (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-LDC
  (implies 
   (poised-inst 'LDC s)
   (-> s
       (modify s
               :pc (+ 2 (pc (top-frame s)))
               :stack (push (cadr (nth (arg1 (next-inst s)) 
                                       (retrieve-cp (cur-class (top-frame s)) 
                                                    (class-table s)))) 
                            (stack (top-frame s))))))
  :hints (("Goal" :in-theory (enable step-LDC))))

; ---------------------------------------------------------
; LDC_W       

; ---------------------------------------------------------
; LDC2_W
        
(defthmd step-LDC2_W
  (implies 
   (poised-inst 'LDC2_W s)
   (equal (step s) 
          (execute-LDC2_W (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-LDC2_W
  (implies 
   (poised-inst 'LDC2_W s)
   (-> s
       (modify s
               :pc (+ 3 (pc (top-frame s)))
               :stack (push 0
                            (push (cadr (nth (arg1 (next-inst s)) 
                                             (retrieve-cp (cur-class (top-frame s)) 
                                                          (class-table s)))) 
                                  (stack (top-frame s)))))))
  :hints (("Goal" :in-theory (enable step-LDC2_W))))

; ---------------------------------------------------------
; LDIV

(defthmd step-LDIV
  (implies 
   (poised-inst 'LDIV s)
   (equal (step s) 
          (execute-LDIV (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-LDIV
  (implies 
   (poised-inst 'LDIV s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push 0
                            (push
                             (long-fix
                              (truncate (top (popn 3 (stack (top-frame s))))
                                        (top (pop (stack (top-frame s))))))
                             (popn 4 (stack (top-frame s))))))))
  :hints (("Goal" :in-theory (enable step-LDIV))))

; ---------------------------------------------------------
; LLOAD

(defthmd step-LLOAD
  (implies 
   (poised-inst 'LLOAD s)
   (equal (step s) 
          (execute-LLOAD (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-LLOAD
  (implies 
   (poised-inst 'LLOAD s)
   (-> s
       (modify s
               :pc (+ 2 (pc (top-frame s)))
               :stack (push 0
                            (push (nth (arg1 (next-inst s))
                                       (locals (top-frame s)))
                                  (stack (top-frame s)))))))
  :hints (("Goal" :in-theory (enable step-LLOAD))))

; ---------------------------------------------------------
; LLOAD_0

(defthmd step-LLOAD_0
  (implies 
   (poised-inst 'LLOAD_0 s)
   (equal (step s) 
          (execute-LLOAD_X (next-inst s) s 0)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-LLOAD_0
  (implies 
   (poised-inst 'LLOAD_0 s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push 0
                            (push (nth 0 
                                       (locals (top-frame s)))
                                  (stack (top-frame s)))))))
  :hints (("Goal" :in-theory (enable step-LLOAD_0))))

; ---------------------------------------------------------
; LLOAD_1
        
(defthmd step-LLOAD_1
  (implies 
   (poised-inst 'LLOAD_1 s)
   (equal (step s) 
          (execute-LLOAD_X (next-inst s) s 1)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-LLOAD_1
  (implies 
   (poised-inst 'LLOAD_1 s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push 0
                            (push (nth 1 
                                       (locals (top-frame s)))
                                  (stack (top-frame s)))))))
  :hints (("Goal" :in-theory (enable step-LLOAD_1))))

; ---------------------------------------------------------
; LLOAD_2
        
(defthmd step-LLOAD_2
  (implies 
   (poised-inst 'LLOAD_2 s)
   (equal (step s) 
          (execute-LLOAD_X (next-inst s) s 2)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-LLOAD_2
  (implies 
   (poised-inst 'LLOAD_2 s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push 0
                            (push (nth 2 
                                       (locals (top-frame s)))
                                  (stack (top-frame s)))))))
  :hints (("Goal" :in-theory (enable step-LLOAD_2))))

; ---------------------------------------------------------
; LLOAD_3
        
(defthmd step-LLOAD_3
  (implies 
   (poised-inst 'LLOAD_3 s)
   (equal (step s) 
          (execute-LLOAD_X (next-inst s) s 3)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-LLOAD_3
  (implies 
   (poised-inst 'LLOAD_3 s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push 0
                            (push (nth 3 
                                       (locals (top-frame s)))
                                  (stack (top-frame s)))))))
  :hints (("Goal" :in-theory (enable step-LLOAD_3))))

; ---------------------------------------------------------
; LMUL
        
(defthmd step-LMUL
  (implies 
   (poised-inst 'LMUL s)
   (equal (step s) 
          (execute-LMUL (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-LMUL
  (implies 
   (poised-inst 'LMUL s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push 0
                            (push (ulong-fix
                                   (* (top (pop (stack (top-frame s))))
                                      (top (popn 3 (stack (top-frame s))))))
                                  (popn 4 (stack (top-frame s))))))))
  :hints (("Goal" :in-theory (enable step-LMUL))))

; ---------------------------------------------------------
; LNEG

(defthmd step-LNEG
  (implies 
   (poised-inst 'LNEG s)
   (equal (step s) 
          (execute-LNEG (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-LNEG
  (implies 
   (poised-inst 'LNEG s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push 0
                            (push (if (equal (top (pop (stack (top-frame s))))
                                             *most-negative-long*)
                                    *most-negative-long*
                                    (- (top (pop (stack (top-frame s)))))) 
                                  (popn 2 (stack (top-frame s))))))))
  :hints (("Goal" :in-theory (enable step-LNEG))))

; ---------------------------------------------------------
; LOR
        
(defthmd step-LOR
  (implies 
   (poised-inst 'LOR s)
   (equal (step s) 
          (execute-LOR (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-LOR
  (implies 
   (poised-inst 'LOR s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push 0
                            (push (logior (top (pop (stack (top-frame s))))
                                          (top (popn 3 (stack (top-frame s)))))
                                  (popn 4 (stack (top-frame s))))))))
  :hints (("Goal" :in-theory (enable step-LOR))))

; ---------------------------------------------------------
; LREM

(defthmd step-LREM
  (implies 
   (poised-inst 'LREM s)
   (equal (step s) 
          (execute-LREM (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-LREM
  (implies 
   (poised-inst 'LREM s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push 0
                           (push (- (top (pop (stack (top-frame s)))) 
                                    (* (truncate (top (pop (stack (top-frame s)))) 
                                                 (top (popn 3 (stack (top-frame s))))) 
                                       (top (popn 3 (stack (top-frame s)))))) 
                                 (popn 4 (stack (top-frame s))))))))
  :hints (("Goal" :in-theory (enable step-LREM))))

; ---------------------------------------------------------
; LRETURN
        
(defthmd step-LRETURN
  (implies 
   (poised-inst 'LRETURN s)
   (equal (step s) 
          (execute-LRETURN (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-LRETURN
  (implies 
   (poised-inst 'LRETURN s)
   (let* ((s1 (modify s
                      :call-stack (pop (call-stack s)))))
   (-> s
       (modify s1               
               :stack (push 0 
                            (push (top (pop (stack (top-frame s)))) 
                                  (stack (top-frame s1))))))))
  :hints (("Goal" :in-theory (enable step-LRETURN))))

; ---------------------------------------------------------
; LSHL

(defthmd step-LSHL
  (implies 
   (poised-inst 'LSHL s)
   (equal (step s) 
          (execute-LSHL (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-LSHL
  (implies 
   (poised-inst 'LSHL s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push 0 
                           (push (long-fix (shl (top (popn 2 (stack (top-frame s)))) 
                                                (6-bit-fix (top (stack (top-frame s))))))
                                 (popn 3 (stack (top-frame s))))))))
  :hints (("Goal" :in-theory (enable step-LSHL))))

; ---------------------------------------------------------
; LSHR

(defthmd step-LSHR
  (implies 
   (poised-inst 'LSHR s)
   (equal (step s) 
          (execute-LSHR (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-LSHR
  (implies 
   (poised-inst 'LSHR s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push 0 
                           (push (long-fix (shr (top (popn 2 (stack (top-frame s)))) 
                                                (6-bit-fix (top (stack (top-frame s))))))
                                 (popn 3 (stack (top-frame s))))))))
  :hints (("Goal" :in-theory (enable step-LSHR))))
        
; ---------------------------------------------------------
; LSTORE

(defthmd step-LSTORE
  (implies 
   (poised-inst 'LSTORE s)
   (equal (step s) 
          (execute-LSTORE (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-LSTORE
  (implies 
   (poised-inst 'LSTORE s)
   (-> s
       (modify s
               :pc (+ 2 (pc (top-frame s)))
               :locals (update-nth (arg1 (next-inst s))
                                   (top (pop (stack (top-frame s))))
                                   (locals (top-frame s)))
               :stack (popn 2 (stack (top-frame s))))))
  :hints (("Goal" :in-theory (enable step-LSTORE))))

; ---------------------------------------------------------
; LSTORE_0

(defthmd step-LSTORE_0
  (implies 
   (poised-inst 'LSTORE_0 s)
   (equal (step s) 
          (execute-LSTORE_X (next-inst s) s 0)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-LSTORE_0
  (implies 
   (poised-inst 'LSTORE_0 s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :locals (update-nth 0
                                   (top (pop (stack (top-frame s))))
                                   (locals (top-frame s)))
               :stack (popn 2 (stack (top-frame s))))))
  :hints (("Goal" :in-theory (enable step-LSTORE_0))))

; ---------------------------------------------------------
; LSTORE_1

(defthmd step-LSTORE_1
  (implies 
   (poised-inst 'LSTORE_1 s)
   (equal (step s) 
          (execute-LSTORE_X (next-inst s) s 1)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-LSTORE_1
  (implies 
   (poised-inst 'LSTORE_1 s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :locals (update-nth 1
                                   (top (pop (stack (top-frame s))))
                                   (locals (top-frame s)))
               :stack (popn 2 (stack (top-frame s))))))
  :hints (("Goal" :in-theory (enable step-LSTORE_1))))

; ---------------------------------------------------------
; LSTORE_2

(defthmd step-LSTORE_2
  (implies 
   (poised-inst 'LSTORE_2 s)
   (equal (step s) 
          (execute-LSTORE_X (next-inst s) s 2)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-LSTORE_2
  (implies 
   (poised-inst 'LSTORE_2 s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :locals (update-nth 2
                                   (top (pop (stack (top-frame s))))
                                   (locals (top-frame s)))
               :stack (popn 2 (stack (top-frame s))))))
  :hints (("Goal" :in-theory (enable step-LSTORE_2))))

; ---------------------------------------------------------
; LSTORE_3

(defthmd step-LSTORE_3
  (implies 
   (poised-inst 'LSTORE_3 s)
   (equal (step s) 
          (execute-LSTORE_X (next-inst s) s 3)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-LSTORE_3
  (implies 
   (poised-inst 'LSTORE_3 s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :locals (update-nth 3
                                   (top (pop (stack (top-frame s))))
                                   (locals (top-frame s)))
               :stack (popn 2 (stack (top-frame s))))))
  :hints (("Goal" :in-theory (enable step-LSTORE_3))))

; ---------------------------------------------------------
; LSUB

(defthmd step-LSUB
  (implies 
   (poised-inst 'LSUB s)
   (equal (step s) 
          (execute-LSUB (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-LSUB
  (implies 
   (poised-inst 'LSUB s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push 0
                            (push 
                             (ulong-fix (- (top (popn 3 (stack (top-frame s))))
                                           (top (pop (stack (top-frame s))))))
                             (popn 4 (stack (top-frame s))))))))
  :hints (("Goal" :in-theory (enable step-LSUB))))

; ---------------------------------------------------------
; LUSHR
        
(defthmd step-LUSHR
  (implies 
   (poised-inst 'LUSHR s)
   (equal (step s) 
          (execute-LUSHR (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-LUSHR
  (implies 
   (poised-inst 'LUSHR s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push 0
                            (push (long-fix (lushr (top (popn 2 (stack (top-frame s)))) 
                                                   (6-bit-fix (top (stack (top-frame s))))))
                                  (popn 3 (stack (top-frame s))))))))
  :hints (("Goal" :in-theory (enable step-LUSHR))))

; ---------------------------------------------------------
; LXOR

(defthmd step-LXOR
  (implies 
   (poised-inst 'LXOR s)
   (equal (step s) 
          (execute-LXOR (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-LXOR
  (implies 
   (poised-inst 'LXOR s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push 0
                            (push (logxor (top (pop (stack (top-frame s))))
                                          (top (popn 3 (stack (top-frame s)))))
                                  (popn 4 (stack (top-frame s))))))))
  :hints (("Goal" :in-theory (enable step-LXOR))))

; ---------------------------------------------------------
; MULTIANEWARRAY

; ---------------------------------------------------------
; NEW
        
(defthmd step-NEW
  (implies 
   (poised-inst 'NEW s)
   (equal (step s) 
          (execute-NEW (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-NEW
  (implies 
   (poised-inst 'NEW s)
   (-> s
       (modify s
               :pc (+ 3 (pc (top-frame s)))
               :stack (push (list 'REF (len (heap s)))
                            (stack (top-frame s)))
               :heap (bind (len (heap s)) 
                           (build-an-instance
                            (cons (arg1 (next-inst s))
                                  (class-superclasses (arg1 (next-inst s))
                                                      (class-table s)))
                            (class-table s)) 
                           (heap s)))))
  :hints (("Goal" :in-theory (enable step-NEW))))

; ---------------------------------------------------------
; NEWARRAY
        
(defthmd step-NEWARRAY
  (implies 
   (poised-inst 'NEWARRAY s)
   (equal (step s) 
          (execute-NEWARRAY (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-NEWARRAY
  (implies 
   (poised-inst 'NEWARRAY s)
   (-> s
       (modify s
               :pc (+ 2 (pc (top-frame s)))
                :stack (push (list 'REF (len (heap s)))
                             (pop (stack (top-frame s))))
                :heap (bind (len (heap s))
                            (makearray (arg1 (next-inst s))
                                       (top (stack (top-frame s)))
                                       (init-array (arg1 (next-inst s))
                                                   (top (stack (top-frame s))))
                                       (class-table s))
                            (heap s)))))
  :hints (("Goal" :in-theory (enable step-NEWARRAY))))

; ---------------------------------------------------------
; NOP
               
(defthmd step-NOP
  (implies 
   (poised-inst 'NOP s)
   (equal (step s) 
          (execute-NOP (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-NOP
  (implies 
   (poised-inst 'NOP s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s))))))
  :hints (("Goal" :in-theory (enable step-NOP))))

; ---------------------------------------------------------
; POP

(defthmd step-POP
  (implies 
   (poised-inst 'POP s)
   (equal (step s) 
          (execute-POP (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-POP
  (implies 
   (poised-inst 'POP s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (pop (stack (top-frame s))))))
  :hints (("Goal" :in-theory (enable step-POP))))

; ---------------------------------------------------------
; POP2
        
(defthmd step-POP2
  (implies 
   (poised-inst 'POP2 s)
   (equal (step s) 
          (execute-POP2 (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-POP2
  (implies 
   (poised-inst 'POP2 s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (popn 2 (stack (top-frame s))))))
  :hints (("Goal" :in-theory (enable step-POP2))))

; ---------------------------------------------------------
; PUTFIELD

(defthmd step-PUTFIELD
  (implies 
   (poised-inst 'PUTFIELD s)
   (equal (step s) 
          (execute-PUTFIELD (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-PUTFIELD
  (implies 
   (poised-inst 'PUTFIELD s)
   (let* ((long-flag (arg3 (next-inst s))))
   (-> s
       (modify s
               :pc (+ 3 (pc (top-frame s)))               
               :stack (if long-flag
                        (popn 3 (stack (top-frame s)))
                        (pop (pop (stack (top-frame s)))))
               :heap (bind (cadr (if long-flag
                                   (top (popn 2 (stack (top-frame s))))
                                   (top (pop (stack (top-frame s))))))
                           (set-instance-field (arg1 (next-inst s))
                                               (arg2 (next-inst s))
                                               (if long-flag
                                                 (top (pop (stack (top-frame s))))
                                                 (top (stack (top-frame s))))
                                               (if long-flag
                                                 (deref (top (popn 2 (stack (top-frame s)))) 
                                                        (heap s))
                                                 (deref (top (pop (stack (top-frame s)))) 
                                                        (heap s))))
                           (heap s))))))
  :hints (("Goal" :in-theory (enable step-PUTFIELD))))

; ---------------------------------------------------------
; PUTSTATIC

(defthmd step-PUTSTATIC
  (implies 
   (poised-inst 'PUTSTATIC s)
   (equal (step s) 
          (execute-PUTSTATIC (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-PUTSTATIC
  (implies 
   (poised-inst 'PUTSTATIC s)
   (let* ((long-flag (arg3 (next-inst s)))
          (class-ref (class-decl-heapref
                      (bound? (arg1 (next-inst s)) 
                              (class-table s)))))
   (-> s
       (modify s
               :pc (+ 3 (pc (top-frame s)))               
               :stack (if long-flag
                        (popn 2 (stack (top-frame s)))
                        (pop (stack (top-frame s))))
               :heap (bind (cadr class-ref)
                           (set-instance-field "java.lang.Class"
                                               (arg2 (next-inst s))
                                               (if long-flag
                                                 (top (pop (stack (top-frame s))))
                                                 (top (stack (top-frame s))))
                                               (deref class-ref (heap s)))
                           (heap s))))))
  :hints (("Goal" :in-theory (enable step-PUTSTATIC))))

; ---------------------------------------------------------
; RET
        
; ---------------------------------------------------------
; RETURN

(defthmd step-RETURN
  (implies 
   (poised-inst 'RETURN s)
   (equal (step s) 
          (execute-RETURN (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-RETURN
  (implies 
   (poised-inst 'RETURN s)
   (-> s
       (modify s
               :call-stack (pop (call-stack s)))))
  :hints (("Goal" :in-theory (enable step-RETURN))))

; ---------------------------------------------------------
; SALOAD
        
(defthmd step-SALOAD
  (implies 
   (poised-inst 'SALOAD s)
   (equal (step s) 
          (execute-SALOAD (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-SALOAD
  (implies 
   (poised-inst 'SALOAD s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push (element-at (top (stack (top-frame s))) 
                                        (deref (top (pop (stack (top-frame s)))) 
                                               (heap s)))
                            (pop (pop (stack (top-frame s))))))))
  :hints (("Goal" :in-theory (enable step-SALOAD))))

; ---------------------------------------------------------
; SASTORE

(defthmd step-SASTORE
  (implies 
   (poised-inst 'SASTORE s)
   (equal (step s) 
          (execute-SASTORE (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-SASTORE
  (implies 
   (poised-inst 'SASTORE s)
   (let* ((arrayref (top (pop (pop (stack (top-frame s)))))))
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))               
               :stack (pop (pop (pop (stack (top-frame s)))))
               :heap (bind (cadr arrayref)
                           (set-element-at (short-fix (top (stack (top-frame s))))
                                           (top (pop (stack (top-frame s))))
                                           (deref arrayref (heap s))
                                           (class-table s))
                           (heap s))))))
  :hints (("Goal" :in-theory (enable step-SASTORE))))

; ---------------------------------------------------------
; SIPUSH
        
(defthmd step-SIPUSH
  (implies 
   (poised-inst 'SIPUSH s)
   (equal (step s) 
          (execute-SIPUSH (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-SIPUSH
  (implies 
   (poised-inst 'SIPUSH s)
   (-> s
       (modify s
               :pc (+ 3 (pc (top-frame s)))
               :stack (push (short-fix (arg1 (next-inst s)))
                            (stack (top-frame s))))))
  :hints (("Goal" :in-theory (enable step-SIPUSH))))

; ---------------------------------------------------------
; SWAP

(defthmd step-SWAP
  (implies 
   (poised-inst 'SWAP s)
   (equal (step s) 
          (execute-SWAP (next-inst s) s)))
  :hints (("Goal" :in-theory (enable do-inst step))))

(defthmd ->-execute-SWAP
  (implies 
   (poised-inst 'SWAP s)
   (-> s
       (modify s
               :pc (+ 1 (pc (top-frame s)))
               :stack (push (top (pop (stack (top-frame s)))) 
                            (push (top (stack (top-frame s))) 
                                  (pop (pop (stack (top-frame s)))))))))
  :hints (("Goal" :in-theory (enable step-SWAP))))

(in-theory (disable
      EXECUTE-AALOAD
      EXECUTE-AASTORE
      EXECUTE-ACONST_NULL
      EXECUTE-ALOAD
      EXECUTE-ALOAD_X
      EXECUTE-ALOAD_X
      EXECUTE-ALOAD_X
      EXECUTE-ALOAD_X
      EXECUTE-ANEWARRAY
      EXECUTE-ARETURN
      EXECUTE-ARRAYLENGTH
      EXECUTE-ASTORE
      EXECUTE-ASTORE_X
      EXECUTE-ASTORE_X
      EXECUTE-ASTORE_X
      EXECUTE-ASTORE_X
      EXECUTE-BALOAD
      EXECUTE-BASTORE
      EXECUTE-BIPUSH
      EXECUTE-CALOAD
      EXECUTE-CASTORE
      EXECUTE-DUP
      EXECUTE-DUP_X1
      EXECUTE-DUP_X2
      EXECUTE-DUP2
      EXECUTE-DUP2_X1
      EXECUTE-DUP2_X2
      EXECUTE-GETFIELD
      EXECUTE-GETSTATIC
      EXECUTE-GOTO
      EXECUTE-GOTO_W
      EXECUTE-I2B
      EXECUTE-I2C
      EXECUTE-I2L
      EXECUTE-I2S
      EXECUTE-IADD
      EXECUTE-IALOAD
      EXECUTE-IAND
      EXECUTE-IASTORE
      EXECUTE-ICONST_X
      EXECUTE-ICONST_X
      EXECUTE-ICONST_X
      EXECUTE-ICONST_X
      EXECUTE-ICONST_X
      EXECUTE-ICONST_X
      EXECUTE-ICONST_X
      EXECUTE-IDIV
      EXECUTE-IF_ACMPEQ
      EXECUTE-IF_ACMPNE
      EXECUTE-IF_ICMPEQ
      EXECUTE-IF_ICMPGE
      EXECUTE-IF_ICMPGT
      EXECUTE-IF_ICMPLE
      EXECUTE-IF_ICMPLT
      EXECUTE-IF_ICMPNE
      EXECUTE-IFEQ
      EXECUTE-IFGE
      EXECUTE-IFGT
      EXECUTE-IFLE
      EXECUTE-IFLT
      EXECUTE-IFNE
      EXECUTE-IFNONNULL
      EXECUTE-IFNULL
      EXECUTE-IINC
      EXECUTE-ILOAD
      EXECUTE-ILOAD_X
      EXECUTE-ILOAD_X
      EXECUTE-ILOAD_X
      EXECUTE-ILOAD_X
      EXECUTE-IMUL
      EXECUTE-INEG
      EXECUTE-INSTANCEOF
      EXECUTE-INVOKESPECIAL
      EXECUTE-INVOKESTATIC
      EXECUTE-INVOKEVIRTUAL
      EXECUTE-IOR
      EXECUTE-IREM
      EXECUTE-IRETURN
      EXECUTE-ISHL
      EXECUTE-ISHR
      EXECUTE-ISTORE
      EXECUTE-ISTORE_X
      EXECUTE-ISTORE_X
      EXECUTE-ISTORE_X
      EXECUTE-ISTORE_X
      EXECUTE-ISUB
      EXECUTE-IUSHR
      EXECUTE-IXOR
      EXECUTE-JSR
      EXECUTE-JSR_W
      EXECUTE-L2I
      EXECUTE-LADD
      EXECUTE-LALOAD
      EXECUTE-LAND
      EXECUTE-LASTORE
      EXECUTE-LCMP
      EXECUTE-LCONST_X
      EXECUTE-LCONST_X
      EXECUTE-LDC
      EXECUTE-LDC
      EXECUTE-LDC2_W
      EXECUTE-LDIV
      EXECUTE-LLOAD
      EXECUTE-LLOAD_X
      EXECUTE-LLOAD_X
      EXECUTE-LLOAD_X
      EXECUTE-LLOAD_X
      EXECUTE-LMUL
      EXECUTE-LNEG
      EXECUTE-LOR
      EXECUTE-LREM
      EXECUTE-LRETURN
      EXECUTE-LSHL
      EXECUTE-LSHR
      EXECUTE-LSTORE
      EXECUTE-LSTORE_X
      EXECUTE-LSTORE_X
      EXECUTE-LSTORE_X
      EXECUTE-LSTORE_X
      EXECUTE-LSUB
      EXECUTE-LUSHR
      EXECUTE-LXOR
      EXECUTE-MULTIANEWARRAY
      EXECUTE-NEW
      EXECUTE-NEWARRAY
      EXECUTE-NOP
      EXECUTE-POP
      EXECUTE-POP2
      EXECUTE-PUTFIELD
      EXECUTE-PUTSTATIC
      EXECUTE-RET
      EXECUTE-RETURN
      EXECUTE-SALOAD
      EXECUTE-SASTORE
      EXECUTE-SIPUSH
      EXECUTE-SWAP))#|ACL2s-ToDo-Line|#

