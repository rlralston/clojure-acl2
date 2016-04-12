#|$ACL2s-Preamble$;
(include-book "mc-structures")
(include-book "mc-native")

(acl2::begin-book t :ttags :all);$ACL2s-Preamble$|#


(in-package "MC")

; -----------------------------------------------------------------------------
; Instruction length table -- PCs are now in bytes, not # of instructions

(defun inst-length (inst)
  (case (op-code inst)
    (AALOAD             1)
    (AASTORE            1)
    (ACONST_NULL        1)
    (ALOAD              2)
    (ALOAD_0            1)
    (ALOAD_1            1)
    (ALOAD_2            1)
    (ALOAD_3            1)
    (ANEWARRAY          3)
    (ARETURN            1)
    (ARRAYLENGTH        1)
    (ASTORE             2)
    (ASTORE_0           1)
    (ASTORE_1           1)
    (ASTORE_2           1)
    (ASTORE_3           1)
    (BALOAD             1)
    (BASTORE            1)
    (BIPUSH             2)
    (CHECKCAST          3)
    (CALOAD             1)
    (CASTORE            1)
    (DUP                1)
    (DUP_X1             1)
    (DUP_X2             1)
    (DUP2               1)
    (DUP2_X1            1)
    (DUP2_X2            1)
    (GETFIELD           3)
    (GETSTATIC          3)
    (GOTO               3)
    (GOTO_W             5)
    (I2B                1)
    (I2C                1)
    (I2L                1)
    (I2S                1)
    (IADD               1)
    (IALOAD             1)
    (IAND               1)
    (IASTORE            1)
    (ICONST_M1          1)
    (ICONST_0           1)
    (ICONST_1           1)
    (ICONST_2           1)
    (ICONST_3           1)
    (ICONST_4           1)
    (ICONST_5           1)
    (IDIV               1)
    (IF_ACMPEQ          3)
    (IF_ACMPNE          3)
    (IF_ICMPEQ          3)
    (IF_ICMPGE          3)
    (IF_ICMPGT          3)
    (IF_ICMPLE          3)
    (IF_ICMPLT          3)
    (IF_ICMPNE          3)
    (IFEQ               3)
    (IFGE               3)
    (IFGT               3)
    (IFLE               3)
    (IFLT               3)
    (IFNE               3)
    (IFNONNULL          3)
    (IFNULL             3)
    (IINC               3)
    (ILOAD              2)
    (ILOAD_0            1)
    (ILOAD_1            1)
    (ILOAD_2            1)
    (ILOAD_3            1)
    (IMUL               1)
    (INEG               1)
    (INSTANCEOF         3)
    (INVOKEINTERFACE    5)
    (INVOKESPECIAL      3)
    (INVOKESTATIC       3)
    (INVOKEVIRTUAL      3)
    (IOR                1)
    (IREM               1)
    (IRETURN            1)
    (ISHL               1)
    (ISHR               1)
    (ISTORE             2)
    (ISTORE_0           1)
    (ISTORE_1           1)
    (ISTORE_2           1)
    (ISTORE_3           1)
    (ISUB               1)
    (IUSHR              1)
    (IXOR               1)
    (JSR                3)
    (JSR_W              5)
    (L2I                1)
    (LADD               1)
    (LALOAD             1)
    (LAND               1)
    (LASTORE            1)
    (LCMP               1)
    (LCONST_0           1)
    (LCONST_1           1)
    (LDC                2)
    (LDC_W              3)
    (LDC2_W             3)
    (LDIV               1)
    (LLOAD              2)
    (LLOAD_0            1)
    (LLOAD_1            1)
    (LLOAD_2            1)
    (LLOAD_3            1)
    (LMUL               1)
    (LNEG               1)
    (LOR                1)
    (LREM               1)
    (LRETURN            1)
    (LSHL               1)
    (LSHR               1)
    (LSTORE             2)
    (LSTORE_0           1)
    (LSTORE_1           1)
    (LSTORE_2           1)
    (LSTORE_3           1)
    (LSUB               1)
    (LUSHR              1)
    (LXOR               1)
    (MONITORENTER       1)
    (MONITOREXIT        1)
    (MULTIANEWARRAY     4)
    (NEW                3)
    (NEWARRAY           2)
    (NOP                1)
    (POP                1)
    (POP2               1)
    (PUTFIELD           3)
    (PUTSTATIC          3)
    (RET                2)
    (RETURN             1)
    (SALOAD             1)
    (SASTORE            1)
    (SIPUSH             3)
    (SWAP               1)
    (t 1)))

; =============================================================================
; JVM INSTRUCTIONS BEGIN HERE
; =============================================================================

; -----------------------------------------------------------------------------
; (AALOAD) Instruction

(defun execute-AALOAD (inst s)
  (let* ((index (top (stack (top-frame s))))
         (arrayref (top (pop (stack (top-frame s)))))
         (array (deref arrayref (heap s))))
        (modify  s
                :pc (+ (inst-length inst) (pc (top-frame s)))
                :stack (push (element-at index array)
                             (pop (pop (stack (top-frame s))))))))

; -----------------------------------------------------------------------------
; (AASTORE) Instruction

(defun execute-AASTORE (inst s)
  (let* ((value (top (stack (top-frame s))))
         (index (top (pop (stack (top-frame s)))))
         (arrayref (top (pop (pop (stack (top-frame s)))))))
        (modify  s
                :pc (+ (inst-length inst) (pc (top-frame s)))
                :stack (pop (pop (pop (stack (top-frame s)))))
                :heap (bind (cadr arrayref)
                            (set-element-at value
                                            index
                                            (deref arrayref (heap s))
                                            (class-table s))
                            (heap s)))))

; -----------------------------------------------------------------------------
; (ACONST_NULL) Instruction

(defun execute-ACONST_NULL (inst s)
  (modify  s
          :pc (+ (inst-length inst) (pc (top-frame s)))
          :stack (push '(REF -1)
                       (stack (top-frame s)))))#|ACL2s-ToDo-Line|#


; -----------------------------------------------------------------------------
; (ALOAD idx) Instruction - load a reference from the locals

(defun execute-ALOAD (inst s)
  (modify 
   s
   :pc (+ (inst-length inst) (pc (top-frame s)))
   :stack (push (nth (arg1 inst)
                     (locals (top-frame s)))
                (stack (top-frame s)))))

; -----------------------------------------------------------------------------
; (ALOAD_X) Instruction - load a reference from the locals
;                         covers ALOAD_{0, 1, 2, 3}

(defun execute-ALOAD_X (inst s n)
  (modify  s
          :pc (+ (inst-length inst) (pc (top-frame s)))
          :stack (push (nth n (locals (top-frame s)))
                       (stack (top-frame s)))))

; -----------------------------------------------------------------------------
; (ANEWARRAY) Instruction

(defun execute-ANEWARRAY (inst s)
  (let* ((type 'T_REF)
         (count (top (stack (top-frame s))))
         (addr (len (heap s)))
         (obj (makearray type
                         count
                         (init-array type count)
                         (class-table s))))
        (modify  s
                :pc (+ (inst-length inst) (pc (top-frame s)))
                :stack (push (list 'REF addr)
                             (pop (stack (top-frame s))))
                :heap (bind addr
                            obj
                            (heap s)))))

; -----------------------------------------------------------------------------
; (ARETURN) Instruction - return a reference to the preceeding frame

(defun execute-ARETURN (inst s)
  (declare (ignore inst))
  (let* ((val (top (stack (top-frame s))))
         (s1 (modify s
                     :call-stack (pop (call-stack s)))))
    (modify s1
            :stack (push val (stack (top-frame s1))))))

; -----------------------------------------------------------------------------
; (ARRAYLENGTH) Instruction

(defun execute-ARRAYLENGTH (inst s)
  (let* ((arrayref (top (stack (top-frame s))))
         (array (deref arrayref (heap s))))
        (modify  s
                :pc (+ (inst-length inst) (pc (top-frame s)))
                :stack (push (array-bound array)
                             (pop (stack (top-frame s)))))))

; -----------------------------------------------------------------------------
; (ASTORE idx) Instruction - store a reference into the locals

(defun execute-ASTORE (inst s)
  (modify  s
          :pc (+ (inst-length inst) (pc (top-frame s)))
          :locals (update-nth (arg1 inst)
                               (top (stack (top-frame s)))
                               (locals (top-frame s)))
          :stack (pop (stack (top-frame s)))))

; -----------------------------------------------------------------------------
; (ASTORE_X) Instruction - store a reference into the locals
;                          covers ASTORE_{0, 1, 2, 3}

(defun execute-ASTORE_X (inst s n)
  (modify  s
          :pc (+ (inst-length inst) (pc (top-frame s)))
          :locals (update-nth n
                               (top (stack (top-frame s)))
                               (locals (top-frame s)))
          :stack (pop (stack (top-frame s)))))

; -----------------------------------------------------------------------------
; (BALOAD) Instruction

(defun execute-BALOAD (inst s)
  (let* ((index (top (stack (top-frame s))))
         (arrayref (top (pop (stack (top-frame s)))))
         (array (deref arrayref (heap s)))
         (element (if (equal (array-type array)
                             'T_BOOLEAN)
                      (ubyte-fix (element-at index array))
                      (byte-fix (element-at index array)))))
        (modify  s
                :pc (+ (inst-length inst) (pc (top-frame s)))
                :stack (push element
                             (pop (pop (stack (top-frame s))))))))

; -----------------------------------------------------------------------------
; (BASTORE) Instruction

(defun execute-BASTORE (inst s)
  (let* ((value (top (stack (top-frame s))))
         (index (top (pop (stack (top-frame s)))))
         (arrayref (top (pop (pop (stack (top-frame s))))))
         (element (if (equal (array-type (deref arrayref (heap s)))
                             'T_BYTE)
                      (byte-fix value)
                      (u-fix value 1))))
        (modify  s
                :pc (+ (inst-length inst) (pc (top-frame s)))
                :stack (pop (pop (pop (stack (top-frame s)))))
                :heap (bind (cadr arrayref)
                            (set-element-at element
                                            index
                                            (deref arrayref (heap s))
                                            (class-table s))
                            (heap s)))))

; -----------------------------------------------------------------------------
; (BIPUSH const) Instruction

(defun execute-BIPUSH (inst s)
  (modify  s
          :pc (+ (inst-length inst) (pc (top-frame s)))
          :stack (push (byte-fix (arg1 inst))
                       (stack (top-frame s)))))

; -----------------------------------------------------------------------------
; (CHECKCAST) Instruction: It is currently just a shell that doesn't do anything

(defun execute-CHECKCAST (inst s)
  (modify s
          :pc (+ (inst-length inst) (pc (top-frame s)))))
  
; -----------------------------------------------------------------------------
; (CALOAD) Instruction

(defun execute-CALOAD (inst s)
  (let* ((index (top (stack (top-frame s))))
         (arrayref (top (pop (stack (top-frame s)))))
         (array (deref arrayref (heap s))))
        (modify  s
                :pc (+ (inst-length inst) (pc (top-frame s)))
                :stack (push (char-fix (element-at index array))
                             (pop (pop (stack (top-frame s))))))))

; -----------------------------------------------------------------------------
; (CASTORE) Instruction

(defun execute-CASTORE (inst s)
  (let* ((value (top (stack (top-frame s))))
         (index (top (pop (stack (top-frame s)))))
         (arrayref (top (pop (pop (stack (top-frame s)))))))
        (modify  s
                :pc (+ (inst-length inst) (pc (top-frame s)))
                :stack (pop (pop (pop (stack (top-frame s)))))
                :heap (bind (cadr arrayref)
                            (set-element-at (char-fix value)
                                            index
                                            (deref arrayref (heap s))
                                            (class-table s))
                            (heap s)))))

; -----------------------------------------------------------------------------
; (DUP) Instruction

(defun execute-DUP (inst s)
  (modify  s
          :pc (+ (inst-length inst) (pc (top-frame s)))
          :stack (push (top (stack (top-frame s)))
                       (stack (top-frame s)))))

; -----------------------------------------------------------------------------
; (DUP_X1) Instruction

(defun execute-DUP_X1 (inst s)
  (let* ((val1 (top (stack (top-frame s))))
         (val2 (top (pop (stack (top-frame s)))))
         (stack_prime (pop (pop (stack (top-frame s))))))
      (modify  s
              :pc (+ (inst-length inst) (pc (top-frame s)))
              :stack (push val1 (push val2 (push val1 stack_prime))))))

; -----------------------------------------------------------------------------
; (DUP_X2) Instruction

(defun execute-DUP_X2 (inst s)
  (let* ((val1 (top (stack (top-frame s))))
         (val2 (top (pop (stack (top-frame s)))))
         (val3 (top (popn 2 (stack (top-frame s)))))
         (stack_prime (popn 3 (stack (top-frame s)))))
      (modify  s
              :pc (+ (inst-length inst) (pc (top-frame s)))
              :stack (push val1
                           (push val2
                                 (push val3
                                       (push val1 stack_prime)))))))

; -----------------------------------------------------------------------------
; (DUP2) Instruction

(defun execute-DUP2 (inst s)
  (let* ((val1 (top (stack (top-frame s))))
         (val2 (top (pop (stack (top-frame s)))))
         (stack_prime (pop (pop (stack (top-frame s))))))
      (modify  s
              :pc (+ (inst-length inst) (pc (top-frame s)))
              :stack (push val1
                           (push val2
                                 (push val1
                                       (push val2 stack_prime)))))))

; -----------------------------------------------------------------------------
; (DUP2_X1) Instruction

(defun execute-DUP2_X1 (inst s)
  (let* ((val1 (top (stack (top-frame s))))
         (val2 (top (pop (stack (top-frame s)))))
         (val3 (top (popn 2 (stack (top-frame s)))))
         (stack_prime (popn 3 (stack (top-frame s)))))
      (modify  s
              :pc (+ (inst-length inst) (pc (top-frame s)))
              :stack (push val1
                           (push val2
                                 (push val3
                                       (push val1
                                             (push val2 stack_prime))))))))

; -----------------------------------------------------------------------------
; (DUP2_X2) Instruction

(defun execute-DUP2_X2 (inst s)
  (let* ((val1 (top (stack (top-frame s))))
         (val2 (top (pop (stack (top-frame s)))))
         (val3 (top (popn 2 (stack (top-frame s)))))
         (val4 (top (popn 3 (stack (top-frame s)))))
         (stack_prime (popn 4 (stack (top-frame s)))))
      (modify  s
              :pc (+ (inst-length inst) (pc (top-frame s)))
              :stack (push val1
                       (push val2
                         (push val3
                           (push val4
                             (push val1
                               (push val2 stack_prime)))))))))

; -----------------------------------------------------------------------------
; (GETFIELD "class" "field" ?long-flag?) Instruction

(defun execute-GETFIELD (inst s)
  (let* ((class-name (arg1 inst))
         (field-name (arg2 inst))
         (long-flag  (arg3 inst))
         (instance (deref (top (stack (top-frame s))) (heap s)))
         (field-value (field-value class-name field-name instance)))
    (modify  s
            :pc (+ (inst-length inst) (pc (top-frame s)))
            :stack (if long-flag
                       (push 0 (push field-value
                                     (pop (stack (top-frame s)))))
                       (push field-value
                             (pop (stack (top-frame s))))))))

; -----------------------------------------------------------------------------
; (GETSTATIC "class" "field" ?long-flag?) Instruction

(defun static-field-value (class-name field-name heap class-table)
  (let* ((class-ref (class-decl-heapref
                     (bound? class-name class-table)))
         (instance (deref class-ref heap)))
    (field-value "java.lang.Class" field-name instance)))

(defun execute-GETSTATIC (inst s)
  (let* ((class-name (arg1 inst))
         (field-name (arg2 inst))
         (long-flag (arg3 inst))
         ;(class-ref (class-decl-heapref
         ;            (bound? class-name (class-table s))))
         ;(instance (deref class-ref (heap s)))
         ;(field-value (field-value "java.lang.Class" field-name instance)))
         (field-value (static-field-value class-name 
                                          field-name 
                                          (heap s)
                                          (class-table s))))
        (modify  s
                :pc (+ (inst-length inst) (pc (top-frame s)))
                :stack (if long-flag
                           (push 0 (push field-value (stack (top-frame s))))
                           (push field-value (stack (top-frame s)))))))

; -----------------------------------------------------------------------------
; (GOTO pc) Instruction

(defun execute-GOTO (inst s)
  (modify  s
          :pc (+ (arg1 inst) (pc (top-frame s)))))

; -----------------------------------------------------------------------------
; (GOTO_W pc) Instruction

(defun execute-GOTO_W (inst s)
  (modify  s
          :pc (+ (arg1 inst) (pc (top-frame s)))))

; -----------------------------------------------------------------------------
; (I2B) Instruction - int to byte narrowing conversion

(defun execute-I2B (inst s)
  (modify  s
          :pc (+ (inst-length inst) (pc (top-frame s)))
          :stack (push (byte-fix (top (stack (top-frame s))))
                       (pop (stack (top-frame s))))))

; -----------------------------------------------------------------------------
; (I2C) Instruction - int to char narrowing conversion

(defun execute-I2C (inst s)
  (modify  s
          :pc (+ (inst-length inst) (pc (top-frame s)))
          :stack (push (char-fix (top (stack (top-frame s))))
                       (pop (stack (top-frame s))))))

; -----------------------------------------------------------------------------
; (I2L) Instruction - int to long conversion

(defun execute-I2L (inst s)
  (modify  s
          :pc (+ (inst-length inst) (pc (top-frame s)))
          :stack (push 0
                       (push (long-fix (top (stack (top-frame s))))
                             (pop (stack (top-frame s)))))))

; -----------------------------------------------------------------------------
; (I2S) Instruction - int to short narrowing conversion

(defun execute-I2S (inst s)
  (modify  s
          :pc (+ (inst-length inst) (pc (top-frame s)))
          :stack (push (short-fix (top (stack (top-frame s))))
                       (pop (stack (top-frame s))))))

; -----------------------------------------------------------------------------
; (IADD) Instruction

(defun execute-IADD (inst s)
  (modify  s
          :pc (+ (inst-length inst) (pc (top-frame s)))
          :stack (push (int-fix
                          (+ (top (pop (stack (top-frame s))))
                             (top (stack (top-frame s)))))
                       (pop (pop (stack (top-frame s)))))))

; -----------------------------------------------------------------------------
; (IALOAD) Instruction

(defun execute-IALOAD (inst s)
  (let* ((index (top (stack (top-frame s))))
         (arrayref (top (pop (stack (top-frame s)))))
         (array (deref arrayref (heap s))))
        (modify  s
                :pc (+ (inst-length inst) (pc (top-frame s)))
                :stack (push (element-at index array)
                             (pop (pop (stack (top-frame s))))))))

; -----------------------------------------------------------------------------
; (IAND) Instruction

(defun execute-IAND (inst s)
  (modify  s
          :pc (+ (inst-length inst) (pc (top-frame s)))
          :stack (push (logand (top (pop (stack (top-frame s))))
                               (top (stack (top-frame s))))
                       (pop (pop (stack (top-frame s)))))))

; -----------------------------------------------------------------------------
; (IASTORE) Instruction

(defun execute-IASTORE (inst s)
  (let* ((value (top (stack (top-frame s))))
         (index (top (pop (stack (top-frame s)))))
         (arrayref (top (pop (pop (stack (top-frame s)))))))
        (modify  s
                :pc (+ (inst-length inst) (pc (top-frame s)))
                :stack (pop (pop (pop (stack (top-frame s)))))
                :heap (bind (cadr arrayref)
                            (set-element-at value
                                            index
                                            (deref arrayref (heap s))
                                            (class-table s))
                            (heap s)))))

; -----------------------------------------------------------------------------
; (ICONST_X) Instruction - push a certain constant onto the stack
;                          covers ICONST_{M1, 0, 1, 2, 3, 4, 5}

(defun execute-ICONST_X (inst s n)
  (modify  s
          :pc (+ (inst-length inst) (pc (top-frame s)))
          :stack (push n (stack (top-frame s)))))

; -----------------------------------------------------------------------------
; (IDIV) Instruction

(defun execute-IDIV (inst s)
  (modify  s
          :pc (+ (inst-length inst) (pc (top-frame s)))
          :stack (push (int-fix
                            (truncate (top (pop (stack (top-frame s))))
                                      (top (stack (top-frame s)))))
                       (pop (pop (stack (top-frame s)))))))

; -----------------------------------------------------------------------------
; (IF_ACMPEQ pc) Instruction

(defun execute-IF_ACMPEQ (inst s)
  (modify  s
          :pc (if (equal (top (pop (stack (top-frame s))))
                         (top (stack (top-frame s))))
                  (+ (arg1 inst) (pc (top-frame s)))
                (+ (inst-length inst) (pc (top-frame s))))
          :stack (pop (pop (stack (top-frame s))))))

; -----------------------------------------------------------------------------
; (IF_ACMPNE pc) Instruction

(defun execute-IF_ACMPNE (inst s)
  (modify  s
          :pc (if (equal (top (pop (stack (top-frame s))))
                         (top (stack (top-frame s))))
                  (+ (inst-length inst) (pc (top-frame s)))
                  (+ (arg1 inst) (pc (top-frame s))))
          :stack (pop (pop (stack (top-frame s))))))

; -----------------------------------------------------------------------------
; (IF_ICMPEQ pc) Instruction

(defun execute-IF_ICMPEQ (inst s)
  (modify  s
          :pc (if (equal (top (pop (stack (top-frame s))))
                         (top (stack (top-frame s))))
                  (+ (arg1 inst) (pc (top-frame s)))
                  (+ (inst-length inst) (pc (top-frame s))))
          :stack (pop (pop (stack (top-frame s))))))

; -----------------------------------------------------------------------------
; (IF_ICMPGE pc) Instruction

(defun execute-IF_ICMPGE (inst s)
  (modify  s
          :pc (if (>= (top (pop (stack (top-frame s))))
                      (top (stack (top-frame s))))
                  (+ (arg1 inst) (pc (top-frame s)))
                  (+ (inst-length inst) (pc (top-frame s))))
          :stack (pop (pop (stack (top-frame s))))))

; -----------------------------------------------------------------------------
; (IF_ICMPGT pc) Instruction

(defun execute-IF_ICMPGT (inst s)
  (modify  s
          :pc (if (> (top (pop (stack (top-frame s))))
                     (top (stack (top-frame s))))
                  (+ (arg1 inst) (pc (top-frame s)))
                  (+ (inst-length inst) (pc (top-frame s))))
          :stack (pop (pop (stack (top-frame s))))))

; -----------------------------------------------------------------------------
; (IF_ICMPLT pc) Instruction

(defun execute-IF_ICMPLT (inst s)
  (modify  s
          :pc (if (< (top (pop (stack (top-frame s))))
                     (top (stack (top-frame s))))
                  (+ (arg1 inst) (pc (top-frame s)))
                  (+ (inst-length inst) (pc (top-frame s))))
          :stack (pop (pop (stack (top-frame s))))))

; -----------------------------------------------------------------------------
; (IF_ICMPLE pc) Instruction

(defun execute-IF_ICMPLE (inst s)
  (modify  s
          :pc (if (<= (top (pop (stack (top-frame s))))
                      (top (stack (top-frame s))))
                  (+ (arg1 inst) (pc (top-frame s)))
                  (+ (inst-length inst) (pc (top-frame s))))
          :stack (pop (pop (stack (top-frame s))))))

; -----------------------------------------------------------------------------
; (IF_ICMPNE pc) Instruction

(defun execute-IF_ICMPNE (inst s)
  (modify  s
          :pc (if (equal (top (pop (stack (top-frame s))))
                         (top (stack (top-frame s))))
                  (+ (inst-length inst) (pc (top-frame s)))
                  (+ (arg1 inst) (pc (top-frame s))))
          :stack (pop (pop (stack (top-frame s))))))

; -----------------------------------------------------------------------------
; (IFEQ pc) Instruction

(defun execute-IFEQ (inst s)
  (modify  s
          :pc (if (equal (top (stack (top-frame s))) 0)
                  (+ (arg1 inst) (pc (top-frame s)))
                (+ (inst-length inst) (pc (top-frame s))))
          :stack (pop (stack (top-frame s)))))

; -----------------------------------------------------------------------------
; (IFGE pc) Instruction

(defun execute-IFGE (inst s)
  (modify  s
          :pc (if (>= (top (stack (top-frame s))) 0)
                  (+ (arg1 inst) (pc (top-frame s)))
                (+ (inst-length inst) (pc (top-frame s))))
          :stack (pop (stack (top-frame s)))))

; -----------------------------------------------------------------------------
; (IFGT pc) Instruction

(defun execute-IFGT (inst s)
  (modify  s
          :pc (if (> (top (stack (top-frame s))) 0)
                  (+ (arg1 inst) (pc (top-frame s)))
                (+ (inst-length inst) (pc (top-frame s))))
          :stack (pop (stack (top-frame s)))))

; -----------------------------------------------------------------------------
; (IFLE pc) Instruction

(defun execute-IFLE (inst s)
  (modify  s
          :pc (if (<= (top (stack (top-frame s))) 0)
                  (+ (arg1 inst) (pc (top-frame s)))
                (+ (inst-length inst) (pc (top-frame s))))
          :stack (pop (stack (top-frame s)))))

; -----------------------------------------------------------------------------
; (IFLT pc) Instruction

(defun execute-IFLT (inst s)
  (modify  s
          :pc (if (< (top (stack (top-frame s))) 0)
                  (+ (arg1 inst) (pc (top-frame s)))
                (+ (inst-length inst) (pc (top-frame s))))
          :stack (pop (stack (top-frame s)))))

; -----------------------------------------------------------------------------
; (IFNE pc) Instruction

(defun execute-IFNE (inst s)
  (modify  s
          :pc (if (equal (top (stack (top-frame s))) 0)
                  (+ (inst-length inst) (pc (top-frame s)))
                (+ (arg1 inst) (pc (top-frame s))))
          :stack (pop (stack (top-frame s)))))

; -----------------------------------------------------------------------------
; (IFNONNULL pc) Instruction

(defun execute-IFNONNULL (inst s)
  (modify  s
          :pc (if (equal (top (stack (top-frame s))) '(REF -1))
                  (+ (inst-length inst) (pc (top-frame s)))
                  (+ (arg1 inst) (pc (top-frame s))))
          :stack (pop (stack (top-frame s)))))

; -----------------------------------------------------------------------------
; (IFNULL pc) Instruction

(defun execute-IFNULL (inst s)
  (modify  s
          :pc (if (equal (top (stack (top-frame s))) '(REF -1))
                  (+ (arg1 inst) (pc (top-frame s)))
                  (+ (inst-length inst) (pc (top-frame s))))
          :stack (pop (stack (top-frame s)))))

; -----------------------------------------------------------------------------
; (IINC idx const) Instruction - Increment local variable by a constant

(defun execute-IINC (inst s)
  (modify  s
          :pc (+ (inst-length inst) (pc (top-frame s)))
          :locals (update-nth (arg1 inst)
                               (int-fix
                                  (+ (arg2 inst)
                                     (nth (arg1 inst) 
                                          (locals (top-frame s)))))
                               (locals (top-frame s)))))

; -----------------------------------------------------------------------------
; (ILOAD idx) Instruction - Push a local onto the stack

(defun execute-ILOAD (inst s)
  (modify  s
          :pc (+ (inst-length inst) (pc (top-frame s)))
          :stack (push (nth (arg1 inst)
                            (locals (top-frame s)))
                       (stack (top-frame s)))))

; -----------------------------------------------------------------------------
; (ILOAD_X) Instruction - Push a local onto the stack
;                         covers ILOAD_{0, 1, 2, 3}

(defun execute-ILOAD_X (inst s n)
  (modify  s
          :pc (+ (inst-length inst) (pc (top-frame s)))
          :stack (push (nth n (locals (top-frame s)))
                       (stack (top-frame s)))))

; -----------------------------------------------------------------------------
; (IMUL) Instruction

(defun execute-IMUL (inst s)
  (modify  s
          :pc (+ (inst-length inst) (pc (top-frame s)))
          :stack (push (int-fix
                        (* (top (pop (stack (top-frame s))))
                           (top (stack (top-frame s)))))
                       (pop (pop (stack (top-frame s)))))))

; -----------------------------------------------------------------------------
; (INEG) Instruction
;        Because of the way the JVM represents 2's complement ints,
;         the negation of the most negative int is itself

(defun execute-INEG (inst s)
  (let* ((result (if (equal (top (stack (top-frame s)))
                            *most-negative-integer*)
                     *most-negative-integer*
                     (- (top (stack (top-frame s)))))))
  (modify  s
          :pc (+ (inst-length inst) (pc (top-frame s)))
          :stack (push result (pop (stack (top-frame s)))))))

; -----------------------------------------------------------------------------
; (INSTANCEOF) Instruction

(defun class-types (class-name class-table)
  (let* 
    ((obj-supers 
      (cons class-name 
            (class-superclasses class-name
                                class-table))))
    (append obj-supers
            (class-interfaces class-name
                              class-table))))

(defun instance-of (type ref heap class-table)
  (let* 
    ((obj (deref ref heap))
     (obj-class (caar obj))        
     (obj-types (class-types obj-class 
                             class-table)))
    (if (nullrefp ref)
      0
      (if (member-equal type obj-types)
        1
        0))))

(defun execute-INSTANCEOF (inst s)
  (let* ((ref (top (stack (top-frame s)))))
    (modify  
     s
     :pc (+ (inst-length inst) (pc (top-frame s)))
     :stack (push (instance-of (arg1 inst)
                               ref 
                               (heap s)
                               (class-table s))
                  (pop (stack (top-frame s)))))))

; -----------------------------------------------------------------------------
; (IOR) Instruction

(defun execute-IOR (inst s)
  (modify  s
          :pc (+ (inst-length inst) (pc (top-frame s)))
          :stack (push (logior (top (pop (stack (top-frame s))))
                               (top (stack (top-frame s))))
                       (pop (pop (stack (top-frame s)))))))

; -----------------------------------------------------------------------------
; (IREM) Instruction

(defun execute-IREM (inst s)
  (let* ((val1 (top (pop (stack (top-frame s)))))
         (val2 (top (stack (top-frame s))))
         (result (- val1 (* (truncate val1 val2) val2))))
      (modify  s
              :pc (+ (inst-length inst) (pc (top-frame s)))
              :stack (push result 
                           (pop (pop (stack (top-frame s))))))))

; -----------------------------------------------------------------------------
; (IRETURN) Instruction - return an int

(defun execute-IRETURN (inst s)
  (declare (ignore inst))
  (let* ((val (top (stack (top-frame s))))
         (s1 (modify  s
                     :call-stack (pop (call-stack s)))))
    (modify  s1
            :stack (push val (stack (top-frame s1))))))

; -----------------------------------------------------------------------------
; (ISHL) Instruction

(defun execute-ISHL (inst s)
  (let* ((val1 (top (pop (stack (top-frame s)))))
         (val2 (top (stack (top-frame s))))
         (shiftval (5-bit-fix val2))
         (result (shl val1 shiftval)))
      (modify  s
              :pc (+ (inst-length inst) (pc (top-frame s)))
              :stack (push (int-fix result)
                           (pop (pop (stack (top-frame s))))))))

; -----------------------------------------------------------------------------
; (ISHR) Instruction

(defun execute-ISHR (inst s)
  (let* ((val1 (top (pop (stack (top-frame s)))))
         (val2 (top (stack (top-frame s))))
         (shiftval (5-bit-fix val2))
         (result (shr val1 shiftval)))
      (modify  s
              :pc (+ (inst-length inst) (pc (top-frame s)))
              :stack (push (int-fix result)
                           (pop (pop (stack (top-frame s))))))))

; -----------------------------------------------------------------------------
; (ISTORE idx) Instruction - store an int into the locals

(defun execute-ISTORE (inst s)
  (modify  s
          :pc (+ (inst-length inst) (pc (top-frame s)))
          :locals (update-nth (arg1 inst)
                               (top (stack (top-frame s)))
                               (locals (top-frame s)))
          :stack (pop (stack (top-frame s)))))

; -----------------------------------------------------------------------------
; (ISTORE_X) Instruction - store an int into the locals
;                          covers ISTORE_{0, 1, 2, 3}

(defun execute-ISTORE_X (inst s n)
  (modify  s
          :pc (+ (inst-length inst) (pc (top-frame s)))
          :locals (update-nth n
                               (top (stack (top-frame s)))
                               (locals (top-frame s)))
          :stack (pop (stack (top-frame s)))))

; -----------------------------------------------------------------------------
; (ISUB) Instruction

(defun execute-ISUB (inst s)
  (modify  s
          :pc (+ (inst-length inst) (pc (top-frame s)))
          :stack (push (int-fix (- (top (pop (stack (top-frame s))))
                                   (top (stack (top-frame s)))))
                       (pop (pop (stack (top-frame s)))))))

; -----------------------------------------------------------------------------
; (IUSHR) Instruction

(defun iushr (val1 shft)
  (if (< val1 0)
      (+ (shr val1 shft) (shl 1 (- 32 shft)))
    (shr val1 shft)))

(defun execute-IUSHR (inst s)
  (let* ((val1 (top (pop (stack (top-frame s)))))
         (val2 (top (stack (top-frame s))))
         (shiftval (5-bit-fix val2))
         (result (iushr val1 shiftval)))
      (modify  s
              :pc (+ (inst-length inst) (pc (top-frame s)))
              :stack (push (int-fix result)
                           (pop (pop (stack (top-frame s))))))))

; -----------------------------------------------------------------------------
; (IXOR) Instruction

(defun execute-IXOR (inst s)
  (modify  s
          :pc (+ (inst-length inst) (pc (top-frame s)))
          :stack (push (logxor (top (pop (stack (top-frame s))))
                               (top (stack (top-frame s))))
                       (pop (pop (stack (top-frame s)))))))

; -----------------------------------------------------------------------------
; (JSR) Instruction

(defun execute-JSR (inst s)
  (modify  s
          :pc (+ (arg1 inst) (pc (top-frame s)))
          :stack (push (list 'RETURNADDRESS 
                             (+ (inst-length inst)
                                (pc (top-frame s))))
                       (stack (top-frame s)))))

; -----------------------------------------------------------------------------
; (JSR_W) Instruction

(defun execute-JSR_W (inst s)
  (modify  s
          :pc (+ (arg1 inst) (pc (top-frame s)))
          :stack (push (list 'RETURNADDRESS 
                             (+ (inst-length inst)
                                (pc (top-frame s))))
                       (stack (top-frame s)))))

; -----------------------------------------------------------------------------
; (INVOKEINTERFACE "class" "name" n) Instruction

(defun bind-formals (n stack)
  (if (zp n)
      nil
    (cons (top stack)
          (bind-formals (- n 1) (pop stack)))))

(defun lookup-method-in-superclasses (name classes class-table)
  (cond ((endp classes) nil)
        (t (let* ((class-name (car classes))
                  (class-decl (bound? class-name class-table))
                  (method (bound? name (class-decl-methods class-decl))))
             (if method
                 method
                (lookup-method-in-superclasses name (cdr classes)
                                               class-table))))))

(defun lookup-method (name class-name class-table)
  (lookup-method-in-superclasses name
                                 (cons class-name
                                       (class-decl-superclasses
                                        (bound? class-name class-table)))
                                 class-table))

(defun execute-INVOKEINTERFACE (inst s)
  (let* 
    ((method-name (arg2 inst))
     (nformals (arg3 inst))
     (obj-ref 
      (top (popn nformals (stack (top-frame s)))))
     (obj-class-name 
      (class-name-of-ref obj-ref (heap s)))
     (closest-method
      (lookup-method method-name
                     obj-class-name
                     (class-table s)))
     (prog (method-program closest-method))
     (s1 (modify s
                 :pc 
                 (+ (inst-length inst) 
                    (pc (top-frame s)))
                 :stack 
                 (popn (+ nformals 1)
                       (stack (top-frame s))))))
    (modify 
     s1
     :call-stack
     (push 
      (make-frame 
       0
       (reverse
        (bind-formals (+ nformals 1)
                      (stack (top-frame s))))
       nil
       prog                                
       (arg1 inst))
      (call-stack s1)))))

; -----------------------------------------------------------------------------
; (INVOKESPECIAL "class" "name" n) Instruction

(defun execute-INVOKESPECIAL (inst s)
  (let* ((method-name (arg2 inst))
         (nformals (arg3 inst))
         (obj-class-name (arg1 inst))
         (closest-method
          (lookup-method method-name
                         obj-class-name
                         (class-table s)))
         (prog (method-program closest-method))
         (s1 (modify s
                     :pc (+ (inst-length inst) (pc (top-frame s)))
                     :stack (popn (+ nformals 1)
                                  (stack (top-frame s))))))
    (cond
     ((method-isNative? closest-method) s)    
     (t
      (modify s1
              :call-stack
              (push (make-frame 0
                                (reverse
                                 (bind-formals (+ nformals 1)
                                               (stack (top-frame s))))
                                nil
                                prog                                
                                (arg1 inst))
                    (call-stack s1)))))))

; -----------------------------------------------------------------------------
; (INVOKESTATIC "class" "name" n) Instruction

(defun execute-INVOKESTATIC (inst s)
  (let* ((method-name (arg2 inst))
         (nformals (arg3 inst))
         (closest-method
          (lookup-method method-name
                         (arg1 inst)
                         (class-table s)))
         (prog (method-program closest-method))
         (s1 (modify  s
                     :pc (+ (inst-length inst) (pc (top-frame s)))
                     :stack (popn nformals (stack (top-frame s))))))
    (cond
     ((method-isNative? closest-method) 
      (execute-native
       method-name
       (modify s
               :pc (+ (inst-length inst) (pc (top-frame s))))))
     (t
      (modify  s1
               :call-stack (push (make-frame 0
                                             (reverse
                                              (bind-formals nformals
                                                            (stack (top-frame s))))
                                             nil
                                             prog                               
                                             (arg1 inst))
                                 (call-stack s1)))))))

; -----------------------------------------------------------------------------
; (INVOKEVIRTUAL "class" "name" n) Instruction

(defun execute-INVOKEVIRTUAL (inst s)
  (let* ((method-name (arg2 inst))
         (nformals (arg3 inst))
         (obj-ref (top (popn nformals (stack (top-frame s)))))
         (obj-class-name (class-name-of-ref obj-ref (heap s)))
         (closest-method
          (lookup-method method-name
                         obj-class-name
                         (class-table s)))
         (prog (method-program closest-method))
         (s1 (modify  s
                     :pc (+ (inst-length inst) (pc (top-frame s)))
                     :stack (popn (+ nformals 1)
                                  (stack (top-frame s))))))
    (cond
     ((method-isNative? closest-method) 
      (execute-native
       method-name
       (modify s
               :pc (+ (inst-length inst) (pc (top-frame s))))))
     (t
      (modify s1
              :call-stack
              (push (make-frame 0
                                (reverse
                                 (bind-formals (+ nformals 1)
                                               (stack (top-frame s))))
                                nil
                                prog                                
                                (arg1 inst))
                    (call-stack s1)))))))

; -----------------------------------------------------------------------------
; (L2I) Instruction - long to int narrowing conversion

(defun execute-L2I (inst s)
  (modify  s
          :pc (+ (inst-length inst) (pc (top-frame s)))
          :stack (push (int-fix (top (pop (stack (top-frame s)))))
                       (pop (pop (stack (top-frame s)))))))

; -----------------------------------------------------------------------------
; (LADD) Instruction - Add to longs from the top of the stack
;   Stack: value1:long, value2:long -> result:long

(defun execute-LADD (inst s)
  (let* ((val1 (top (pop (stack (top-frame s)))))
         (val2 (top (popn 3 (stack (top-frame s))))))
        (modify  s
                :pc (+ (inst-length inst) (pc (top-frame s)))
                :stack (push 0
                             (push (long-fix (+ val1 val2))
                                   (popn 4 (stack (top-frame s))))))))

; -----------------------------------------------------------------------------
; (LALOAD) Instruction

(defun execute-LALOAD (inst s)
  (let* ((index (top (stack (top-frame s))))
         (arrayref (top (pop (stack (top-frame s)))))
         (array (deref arrayref (heap s))))
        (modify  s
                :pc (+ (inst-length inst) (pc (top-frame s)))
                :stack (push 0
                             (push (element-at index array)
                                   (pop (pop (stack (top-frame s)))))))))

; -----------------------------------------------------------------------------
; (LAND) Instruction

(defun execute-LAND (inst s)
  (let* ((val1 (top (pop (stack (top-frame s)))))
         (val2 (top (popn 3 (stack (top-frame s))))))
        (modify  s
                :pc (+ (inst-length inst) (pc (top-frame s)))
                :stack (push 0
                             (push (logand val1 val2)
                                   (popn 4 (stack (top-frame s))))))))

; -----------------------------------------------------------------------------
; (LASTORE) Instruction

(defun execute-LASTORE (inst s)
  (let* ((value (top (pop (stack (top-frame s)))))
         (index (top (pop (pop (stack (top-frame s))))))
         (arrayref (top (popn 3 (stack (top-frame s))))))
        (modify  s
                :pc (+ (inst-length inst) (pc (top-frame s)))
                :stack (popn 4 (stack (top-frame s)))
                :heap (bind (cadr arrayref)
                            (set-element-at value
                                            index
                                            (deref arrayref (heap s))
                                            (class-table s))
                            (heap s)))))

; -----------------------------------------------------------------------------
; (LCMP) Instruction - compare two longs
;                      val1 > val2 --> 1
;                      val1 = val2 --> 0
;                      val1 < val2 --> -1

(defun execute-LCMP (inst s)
  (let* ((val2 (top (pop (stack (top-frame s)))))
         (val1 (top (popn 3 (stack (top-frame s)))))
         (result (cond ((> val1 val2) 1)
                       ((< val1 val2) -1)
                       (t 0))))
        (modify  s
                :pc (+ (inst-length inst) (pc (top-frame s)))
                :stack (push result
                             (popn 4 (stack (top-frame s)))))))

; -----------------------------------------------------------------------------
; (LCONST_X) Instruction - push a certain long constant onto the stack
;                          covers LCONST_{0, 1}

(defun execute-LCONST_X (inst s n)
  (modify  s
          :pc (+ (inst-length inst) (pc (top-frame s)))
          :stack (push 0
                       (push n (stack (top-frame s))))))

; -----------------------------------------------------------------------------
; (LDC) Instruction

(defun execute-LDC (inst s)
  (let* ((class (cur-class (top-frame s)))
         (cp (retrieve-cp class (class-table s)))
         (entry (nth (arg1 inst) cp))
         (value (cadr entry)))
        (modify  s
                :pc (+ (inst-length inst) (pc (top-frame s)))
                :stack (push value (stack (top-frame s))))))

; -----------------------------------------------------------------------------
; (LDC2_W) Instruction

(defun execute-LDC2_W (inst s)
  (let* ((class (cur-class (top-frame s)))
         (cp (retrieve-cp class (class-table s)))
         (entry (nth (arg1 inst) cp))
         (value (cadr entry)))
         ;(value (caddr entry)))
        (modify  s
                :pc (+ (inst-length inst) (pc (top-frame s)))
                :stack (push 0
                             (push value (stack (top-frame s)))))))

; -----------------------------------------------------------------------------
; (LDIV) Instruction

(defun execute-LDIV (inst s)
  (modify  s
          :pc (+ (inst-length inst) (pc (top-frame s)))
          :stack (push 0
                       (push
                         (long-fix
                            (truncate (top (popn 3 (stack (top-frame s))))
                                      (top (pop (stack (top-frame s))))))
                       (popn 4 (stack (top-frame s)))))))

; -----------------------------------------------------------------------------
; (LLOAD idx) Instruction - Push a long local onto the stack

(defun execute-LLOAD (inst s)
  (modify  s
          :pc (+ (inst-length inst) (pc (top-frame s)))
          :stack (push 0
                       (push (nth (arg1 inst)
                                  (locals (top-frame s)))
                             (stack (top-frame s))))))

; -----------------------------------------------------------------------------
; (LLOAD_X) Instruction - Push a long local onto the stack
;                         covers LLOAD_{0, 1, 2, 3}

(defun execute-LLOAD_X (inst s n)
  (modify  s
          :pc (+ (inst-length inst) (pc (top-frame s)))
          :stack (push 0
                       (push (nth n (locals (top-frame s)))
                             (stack (top-frame s))))))

; -----------------------------------------------------------------------------
; (LMUL) Instruction

(defun execute-LMUL (inst s)
  (modify  s
          :pc (+ (inst-length inst) (pc (top-frame s)))
          :stack (push 0
                       (push (ulong-fix
                              (* (top (pop (stack (top-frame s))))
                                 (top (popn 3 (stack (top-frame s))))))
                             (popn 4 (stack (top-frame s)))))))

; -----------------------------------------------------------------------------
; (LNEG) Instruction
;        Because of the way the JVM represents 2's complement ints,
;         the negation of the most negative int is itself

(defun execute-LNEG (inst s)
  (let* ((result (if (equal (top (pop (stack (top-frame s))))
                            *most-negative-long*)
                     *most-negative-long*
                     (- (top (pop (stack (top-frame s))))))))
  (modify  s
          :pc (+ (inst-length inst) (pc (top-frame s)))
          :stack (push 0
                       (push result (popn 2 (stack (top-frame s))))))))

; -----------------------------------------------------------------------------
; (LOR) Instruction

(defun execute-LOR (inst s)
  (modify  s
          :pc (+ (inst-length inst) (pc (top-frame s)))
          :stack (push 0
                       (push (logior (top (pop (stack (top-frame s))))
                                     (top (popn 3 (stack (top-frame s)))))
                             (popn 4 (stack (top-frame s)))))))

; -----------------------------------------------------------------------------
; (LREM) Instruction

(defun execute-LREM (inst s)
  (let* ((val1 (top (pop (stack (top-frame s)))))
         (val2 (top (popn 3 (stack (top-frame s)))))
         (result (- val1 (* (truncate val1 val2) val2))))
      (modify  s
              :pc (+ (inst-length inst) (pc (top-frame s)))
              :stack (push 0
                           (push result 
                                 (popn 4 (stack (top-frame s))))))))

; -----------------------------------------------------------------------------
; (LRETURN) Instruction - return a long

(defun execute-LRETURN (inst s)
  (declare (ignore inst))
  (let* ((val (top (pop (stack (top-frame s)))))      
         (s1 (modify  s
                     :call-stack (pop (call-stack s)))))
    (modify s1
            :stack (push 0 (push val (stack (top-frame s1)))))))

; -----------------------------------------------------------------------------
; (LSHL) Instruction

(defun execute-LSHL (inst s)
  (let* ((val1 (top (popn 2 (stack (top-frame s)))))
         (val2 (top (stack (top-frame s))))
         (shiftval (6-bit-fix val2))
         (result (shl val1 shiftval)))
      (modify  s
              :pc (+ (inst-length inst) (pc (top-frame s)))
              :stack (push 0
                           (push (long-fix result)
                                 (popn 3 (stack (top-frame s))))))))

; -----------------------------------------------------------------------------
; LSHR Instruction: long shift right 
;   Stack: value1:long, value2:int -> result:long

(defun execute-LSHR (inst s)
  (let* ((val1 (top (popn 2 (stack (top-frame s)))))
         (val2 (top (stack (top-frame s))))
         (shiftval (6-bit-fix val2))
         (result (shr val1 shiftval)))
      (modify  s
              :pc (+ (inst-length inst) (pc (top-frame s)))
              :stack (push 0
                           (push (long-fix result)
                                 (popn 3 (stack (top-frame s))))))))

; -----------------------------------------------------------------------------
; (LSTORE idx) Instruction - store a long into the locals

(defun execute-LSTORE (inst s)
  (modify  s
          :pc (+ (inst-length inst) (pc (top-frame s)))
          :locals (update-nth (arg1 inst)
                               (top (pop (stack (top-frame s))))
                               (locals (top-frame s)))
          :stack (popn 2 (stack (top-frame s)))))

; -----------------------------------------------------------------------------
; (LSTORE_X) Instruction - store a long into the locals
;                          covers LSTORE_{0, 1, 2, 3}

(defun execute-LSTORE_X (inst s n)
  (modify  s
          :pc (+ (inst-length inst) (pc (top-frame s)))
          :locals (update-nth n
                               (top (pop (stack (top-frame s))))
                               (locals (top-frame s)))
          :stack (popn 2 (stack (top-frame s)))))

; -----------------------------------------------------------------------------
; (LSUB) Instruction

(defun execute-LSUB (inst s)
  (modify  s
          :pc (+ (inst-length inst) (pc (top-frame s)))
          :stack (push 0
                       (push 
                        (ulong-fix (- (top (popn 3 (stack (top-frame s))))
                                      (top (pop (stack (top-frame s))))))
                             (popn 4 (stack (top-frame s)))))))

; -----------------------------------------------------------------------------
; (LUSHR) Instruction

(defun lushr (val1 shft)
  (if (< val1 0)
      (+ (shr val1 shft) (shl 1 (- 64 shft)))
    (shr val1 shft)))

(defun execute-LUSHR (inst s)
  (let* ((val1 (top (popn 2 (stack (top-frame s)))))
         (val2 (top (stack (top-frame s))))
         (shiftval (6-bit-fix val2))
         (result (lushr val1 shiftval)))
      (modify  s
              :pc (+ (inst-length inst) (pc (top-frame s)))
              :stack (push 0
                           (push (long-fix result)
                                 (popn 3 (stack (top-frame s))))))))

; -----------------------------------------------------------------------------
; (LXOR) Instruction

(defun execute-LXOR (inst s)
  (modify  s
          :pc (+ (inst-length inst) (pc (top-frame s)))
          :stack (push 0
                       (push (logxor (top (pop (stack (top-frame s))))
                                     (top (popn 3 (stack (top-frame s)))))
                             (popn 4 (stack (top-frame s)))))))

; -----------------------------------------------------------------------------
; (MULTIANEWARRAY) Instruction

(defun execute-MULTIANEWARRAY (inst s)
  (let* ((dimentions (arg1 inst))
         (counts (reverse (take dimentions (stack (top-frame s))))))
        (mv-let (addr new-heap)
                (makemultiarray counts s)
            (modify  s
                    :pc (+ (inst-length inst) (pc (top-frame s)))
                    :stack (push (list 'REF addr)
                                 (nthcdr dimentions (stack (top-frame s))))
                    :heap new-heap))))

; -----------------------------------------------------------------------------
; (NEW "class") Instruction

(defun execute-NEW (inst s)
  (let* ((class-name (arg1 inst))
         (class-table (class-table s))
         (new-object (build-an-instance
                      (cons class-name
                            (class-superclasses class-name
                                                class-table))
                            ;(class-decl-superclasses
                            ; (bound? class-name class-table)))
                      class-table))
         (new-address (len (heap s)))
         (s1 (modify  s
                     :pc (+ (inst-length inst) (pc (top-frame s)))
                     :stack (push (list 'REF new-address)
                                  (stack (top-frame s)))
                     :heap (bind new-address new-object (heap s)))))
    s1))

; -----------------------------------------------------------------------------
; (NEWARRAY) Instruction

(defun execute-NEWARRAY (inst s)
  (let* ((type (arg1 inst))
         (count (top (stack (top-frame s))))
         (addr (len (heap s)))
         (obj (makearray type
                         count
                         (init-array type count)
                         (class-table s))))
        (modify  s
                :pc (+ (inst-length inst) (pc (top-frame s)))
                :stack (push (list 'REF addr)
                             (pop (stack (top-frame s))))
                :heap (bind addr
                            obj
                            (heap s)))))

; -----------------------------------------------------------------------------
; (NOP) Instruction

(defun execute-NOP (inst s)
  (modify  s
          :pc (+ (inst-length inst) (pc (top-frame s)))))

; -----------------------------------------------------------------------------
; (POP) Instruction

(defun execute-POP (inst s)
  (modify  s
          :pc (+ (inst-length inst) (pc (top-frame s)))
          :stack (pop (stack (top-frame s)))))

; -----------------------------------------------------------------------------
; (POP2) Instruction

(defun execute-POP2 (inst s)
  (modify  s
          :pc (+ (inst-length inst) (pc (top-frame s)))
          :stack (popn 2 (stack (top-frame s)))))

; -----------------------------------------------------------------------------
; (PUTFIELD "class" "field" ?long-flag?) Instruction

(defun execute-PUTFIELD (inst s)
  (let* ((class-name (arg1 inst))
         (field-name (arg2 inst))
         (long-flag  (arg3 inst))
         (value (if long-flag
                    (top (pop (stack (top-frame s))))
                    (top (stack (top-frame s)))))
         (instance (if long-flag
                       (deref (top (popn 2 (stack (top-frame s)))) (heap s))
                       (deref (top (pop (stack (top-frame s)))) (heap s))))
         (address (cadr (if long-flag
                            (top (popn 2 (stack (top-frame s))))
                            (top (pop (stack (top-frame s))))))))
        (modify  s
                :pc (+ (inst-length inst) (pc (top-frame s)))
                :stack (if long-flag
                           (popn 3 (stack (top-frame s)))
                           (pop (pop (stack (top-frame s)))))
                :heap (bind address
                            (set-instance-field class-name
                                                field-name
                                                value
                                                instance)
                            (heap s)))))

; -----------------------------------------------------------------------------
; (PUTSTATIC "class" "field" ?long-flag?) Instruction

(defun execute-PUTSTATIC (inst s)
  (let* ((class-name (arg1 inst))
         (field-name (arg2 inst))
         (long-flag (arg3 inst))
         (class-ref (class-decl-heapref
                     (bound? class-name (class-table s))))
         (value (if long-flag
                    (top (pop (stack (top-frame s))))
                    (top (stack (top-frame s)))))
         (instance (deref class-ref (heap s))))
        (modify  s
                :pc (+ (inst-length inst) (pc (top-frame s)))
                :stack (if long-flag
                           (popn 2 (stack (top-frame s)))
                           (pop (stack (top-frame s))))
                :heap (bind (cadr class-ref)
                            (set-instance-field "java.lang.Class"
                                                field-name
                                                value
                                                instance)
                            (heap s)))))

; -----------------------------------------------------------------------------
; (RET) Instruction

(defun execute-RET (inst s)
  (let* ((ret-addr (nth (arg1 inst) (locals (top-frame s))))
         (addr (cadr ret-addr)))
        (modify  s :pc addr)))

; -----------------------------------------------------------------------------
; (RETURN) Instruction - Void Return

(defun execute-RETURN (inst s)
  (declare (ignore inst))
  (modify  s
           :call-stack (pop (call-stack s))))

; -----------------------------------------------------------------------------
; (SALOAD) Instruction

(defun execute-SALOAD (inst s)
  (let* ((index (top (stack (top-frame s))))
         (arrayref (top (pop (stack (top-frame s)))))
         (array (deref arrayref (heap s))))
        (modify  s
                :pc (+ (inst-length inst) (pc (top-frame s)))
                :stack (push (element-at index array)
                             (pop (pop (stack (top-frame s))))))))

; -----------------------------------------------------------------------------
; (SASTORE) Instruction

(defun execute-SASTORE (inst s)
  (let* ((value (top (stack (top-frame s))))
         (index (top (pop (stack (top-frame s)))))
         (arrayref (top (pop (pop (stack (top-frame s)))))))
        (modify  s
                :pc (+ (inst-length inst) (pc (top-frame s)))
                :stack (pop (pop (pop (stack (top-frame s)))))
                :heap (bind (cadr arrayref)
                            (set-element-at (short-fix value)
                                            index
                                            (deref arrayref (heap s))
                                            (class-table s))
                            (heap s)))))

; -----------------------------------------------------------------------------
; (SIPUSH const) Instruction

(defun execute-SIPUSH (inst s)
  (modify  s
          :pc (+ (inst-length inst) (pc (top-frame s)))
          :stack (push (short-fix (arg1 inst))
                       (stack (top-frame s)))))

; -----------------------------------------------------------------------------
; (SWAP) Instruction

(defun execute-SWAP (inst s)
  (let* ((val1 (top (stack (top-frame s))))
         (val2 (top (pop (stack (top-frame s))))))
      (modify  s
              :pc (+ (inst-length inst) (pc (top-frame s)))
              :stack (push val2 
                           (push val1 
                              (pop (pop (stack (top-frame s)))))))))

; -----------------------------------------------------------------------------
; Putting it all together

(defun index-into-program (byte-offset program)
  (declare (xargs :measure (len program)))
  (if (endp program)
      nil
      (if (zp byte-offset)
          (car program)
          (index-into-program (- byte-offset
                                 (inst-length (car program)))
                              (cdr program)))))

(defun next-inst (s)
  (index-into-program (pc (top-frame s))
                      (program (top-frame s))))

(defun do-inst (inst s)
  (case (op-code inst)
    (AALOAD          (execute-AALOAD inst s))
    (AASTORE         (execute-AASTORE inst s))
    (ACONST_NULL     (execute-ACONST_NULL inst s))
    (ALOAD           (execute-ALOAD inst s))
    (ALOAD_0         (execute-ALOAD_X inst s 0))
    (ALOAD_1         (execute-ALOAD_X inst s 1))
    (ALOAD_2         (execute-ALOAD_X inst s 2))
    (ALOAD_3         (execute-ALOAD_X inst s 3))
    (ANEWARRAY       (execute-ANEWARRAY inst s))
    (ARETURN         (execute-ARETURN inst s))
    (ARRAYLENGTH     (execute-ARRAYLENGTH inst s))
    (ASTORE          (execute-ASTORE inst s))
    (ASTORE_0        (execute-ASTORE_X inst s 0))
    (ASTORE_1        (execute-ASTORE_X inst s 1))
    (ASTORE_2        (execute-ASTORE_X inst s 2))
    (ASTORE_3        (execute-ASTORE_X inst s 3))
    (BALOAD          (execute-BALOAD inst s))
    (BASTORE         (execute-BASTORE inst s))
    (BIPUSH          (execute-BIPUSH inst s))
    (CHECKCAST       (execute-CHECKCAST inst s))
    (CALOAD          (execute-CALOAD inst s))
    (CASTORE         (execute-CASTORE inst s))
    (DUP             (execute-DUP inst s))
    (DUP_X1          (execute-DUP_X1 inst s))
    (DUP_X2          (execute-DUP_X2 inst s))
    (DUP2            (execute-DUP2 inst s))
    (DUP2_X1         (execute-DUP2_X1 inst s))
    (DUP2_X2         (execute-DUP2_X2 inst s))
    (GETFIELD        (execute-GETFIELD inst s))
    (GETSTATIC       (execute-GETSTATIC inst s))
    (GOTO            (execute-GOTO inst s))
    (GOTO_W          (execute-GOTO_W inst s))
    (I2B             (execute-I2B inst s))
    (I2C             (execute-I2C inst s))
    (I2L             (execute-I2L inst s))
    (I2S             (execute-I2S inst s))
    (IADD            (execute-IADD inst s))
    (IALOAD          (execute-IALOAD inst s))
    (IAND            (execute-IAND inst s))
    (IASTORE         (execute-IASTORE inst s))
    (ICONST_M1       (execute-ICONST_X inst s -1))
    (ICONST_0        (execute-ICONST_X inst s 0))
    (ICONST_1        (execute-ICONST_X inst s 1))
    (ICONST_2        (execute-ICONST_X inst s 2))
    (ICONST_3        (execute-ICONST_X inst s 3))
    (ICONST_4        (execute-ICONST_X inst s 4))
    (ICONST_5        (execute-ICONST_X inst s 5))
    (IDIV            (execute-IDIV inst s))
    (IF_ACMPEQ       (execute-IF_ACMPEQ inst s))
    (IF_ACMPNE       (execute-IF_ACMPNE inst s))
    (IF_ICMPEQ       (execute-IF_ICMPEQ inst s))
    (IF_ICMPGE       (execute-IF_ICMPGE inst s))
    (IF_ICMPGT       (execute-IF_ICMPGT inst s))
    (IF_ICMPLE       (execute-IF_ICMPLE inst s))
    (IF_ICMPLT       (execute-IF_ICMPLT inst s))
    (IF_ICMPNE       (execute-IF_ICMPNE inst s))
    (IFEQ            (execute-IFEQ inst s))
    (IFGE            (execute-IFGE inst s))
    (IFGT            (execute-IFGT inst s))
    (IFLE            (execute-IFLE inst s))
    (IFLT            (execute-IFLT inst s))
    (IFNE            (execute-IFNE inst s))
    (IFNONNULL       (execute-IFNONNULL inst s))
    (IFNULL          (execute-IFNULL inst s))
    (IINC            (execute-IINC inst s))
    (ILOAD           (execute-ILOAD inst s))
    (ILOAD_0         (execute-ILOAD_X inst s 0))
    (ILOAD_1         (execute-ILOAD_X inst s 1))
    (ILOAD_2         (execute-ILOAD_X inst s 2))
    (ILOAD_3         (execute-ILOAD_X inst s 3))
    (IMUL            (execute-IMUL inst s))
    (INEG            (execute-INEG inst s))
    (INSTANCEOF      (execute-INSTANCEOF inst s))
    (INVOKEINTERFACE (execute-INVOKEINTERFACE inst s))
    (INVOKESPECIAL   (execute-INVOKESPECIAL inst s))
    (INVOKESTATIC    (execute-INVOKESTATIC inst s))
    (INVOKEVIRTUAL   (execute-INVOKEVIRTUAL inst s))
    (IOR             (execute-IOR inst s))
    (IREM            (execute-IREM inst s))
    (IRETURN         (execute-IRETURN inst s))
    (ISHL            (execute-ISHL inst s))
    (ISHR            (execute-ISHR inst s))
    (ISTORE          (execute-ISTORE inst s))
    (ISTORE_0        (execute-ISTORE_X inst s 0))
    (ISTORE_1        (execute-ISTORE_X inst s 1))
    (ISTORE_2        (execute-ISTORE_X inst s 2))
    (ISTORE_3        (execute-ISTORE_X inst s 3))
    (ISUB            (execute-ISUB inst s))
    (IUSHR           (execute-IUSHR inst s))
    (IXOR            (execute-IXOR inst s))
    (JSR             (execute-JSR inst s))
    (JSR_W           (execute-JSR_W inst s))
    (L2I             (execute-L2I inst s))
    (LADD            (execute-LADD inst s))
    (LALOAD          (execute-LALOAD inst s))
    (LAND            (execute-LAND inst s))
    (LASTORE         (execute-LASTORE inst s))
    (LCMP            (execute-LCMP inst s))
    (LCONST_0        (execute-LCONST_X inst s 0))
    (LCONST_1        (execute-LCONST_X inst s 1))
    (LDC             (execute-LDC inst s))
    (LDC_W           (execute-LDC inst s))
    (LDC2_W          (execute-LDC2_W inst s))
    (LDIV            (execute-LDIV inst s))
    (LLOAD           (execute-LLOAD inst s))
    (LLOAD_0         (execute-LLOAD_X inst s 0))
    (LLOAD_1         (execute-LLOAD_X inst s 1))
    (LLOAD_2         (execute-LLOAD_X inst s 2))
    (LLOAD_3         (execute-LLOAD_X inst s 3))
    (LMUL            (execute-LMUL inst s))
    (LNEG            (execute-LNEG inst s))
    (LOR             (execute-LOR inst s))
    (LREM            (execute-LREM inst s))
    (LRETURN         (execute-LRETURN inst s))
    (LSHL            (execute-LSHL inst s))
    (LSHR            (execute-LSHR inst s))
    (LSTORE          (execute-LSTORE inst s))
    (LSTORE_0        (execute-LSTORE_X inst s 0))
    (LSTORE_1        (execute-LSTORE_X inst s 1))
    (LSTORE_2        (execute-LSTORE_X inst s 2))
    (LSTORE_3        (execute-LSTORE_X inst s 3))
    (LSUB            (execute-LSUB inst s))
    (LUSHR           (execute-LUSHR inst s))
    (LXOR            (execute-LXOR inst s))
    (MULTIANEWARRAY  (execute-MULTIANEWARRAY inst s))
    (NEW             (execute-NEW inst s))
    (NEWARRAY        (execute-NEWARRAY inst s))
    (NOP             (execute-NOP inst s))
    (POP             (execute-POP  inst s))
    (POP2            (execute-POP2  inst s))
    (PUTFIELD        (execute-PUTFIELD inst s))
    (PUTSTATIC       (execute-PUTSTATIC inst s))
    (RET             (execute-RET inst s))
    (RETURN          (execute-RETURN inst s))
    (SALOAD          (execute-SALOAD inst s))
    (SASTORE         (execute-SASTORE inst s))
    (SIPUSH          (execute-SIPUSH inst s))
    (SWAP            (execute-SWAP inst s))    
    (HALT            s)
    (otherwise s)))