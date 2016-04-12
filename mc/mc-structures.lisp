#|$ACL2s-Preamble$;
(defpkg "LABEL" '(nil t))
(defpkg "JVM" '(nil t))
(DEFPKG "MC"
  (set-difference-equal
   (union-eq '(JVM::SCHEDULED
               JVM::UNSCHEDULED
               JVM::REF
               JVM::LOCKED
               JVM::S_LOCKED
               JVM::UNLOCKED
               JVM::AALOAD
               JVM::AASTORE
               JVM::ACONST_NULL
               JVM::ALOAD
               JVM::ALOAD_0
               JVM::ALOAD_1
               JVM::ALOAD_2
               JVM::ALOAD_3
               JVM::ANEWARRAY
               JVM::ARETURN
               JVM::ARRAYLENGTH
               JVM::ASTORE
               JVM::ASTORE_0
               JVM::ASTORE_1
               JVM::ASTORE_2
               JVM::ASTORE_3
               JVM::BALOAD
               JVM::BASTORE
               JVM::BIPUSH
               JVM::CALOAD
               JVM::CASTORE
               JVM::CHECKCAST
               JVM::DUP
               JVM::DUP_X1
               JVM::DUP_X2
               JVM::DUP2
               JVM::DUP2_X1
               JVM::DUP2_X2
               JVM::GETFIELD
               JVM::GETSTATIC
               JVM::GOTO
               JVM::GOTO_W
               JVM::I2B
               JVM::I2C
               JVM::I2L
               JVM::I2S
               JVM::IADD
               JVM::IALOAD
               JVM::IAND
               JVM::IASTORE
               JVM::ICONST_M1
               JVM::ICONST_0
               JVM::ICONST_1
               JVM::ICONST_2
               JVM::ICONST_3
               JVM::ICONST_4
               JVM::ICONST_5
               JVM::IDIV
               JVM::IF_ACMPEQ
               JVM::IF_ACMPNE
               JVM::IF_ICMPEQ
               JVM::IF_ICMPGE
               JVM::IF_ICMPGT
               JVM::IF_ICMPLE
               JVM::IF_ICMPLT
               JVM::IF_ICMPNE
               JVM::IFEQ
               JVM::IFGE
               JVM::IFGT
               JVM::IFLE
               JVM::IFLT
               JVM::IFNE
               JVM::IFNONNULL
               JVM::IFNULL
               JVM::IINC
               JVM::ILOAD
               JVM::ILOAD_0
               JVM::ILOAD_1
               JVM::ILOAD_2
               JVM::ILOAD_3
               JVM::IMUL
               JVM::INEG
               JVM::INSTANCEOF
               JVM::INVOKEINTERFACE
               JVM::INVOKESPECIAL
               JVM::INVOKESTATIC               
               JVM::INVOKEVIRTUAL
               JVM::IOR
               JVM::IREM
               JVM::IRETURN
               JVM::ISHL
               JVM::ISHR
               JVM::ISTORE
               JVM::ISTORE_0
               JVM::ISTORE_1
               JVM::ISTORE_2
               JVM::ISTORE_3
               JVM::ISUB
               JVM::IUSHR
               JVM::IXOR
               JVM::JSR
               JVM::JSR_W
               JVM::L2I
               JVM::LADD
               JVM::LALOAD
               JVM::LAND
               JVM::LASTORE
               JVM::LCMP
               JVM::LCONST_0
               JVM::LCONST_1
               JVM::LDC
               JVM::LDC_W
               JVM::LDC2_W
               JVM::LDIV
               JVM::LLOAD
               JVM::LLOAD_0
               JVM::LLOAD_1
               JVM::LLOAD_2
               JVM::LLOAD_3
               JVM::LMUL
               JVM::LNEG
               JVM::LOR
               JVM::LREM
               JVM::LRETURN
               JVM::LSHL
               JVM::LSHR
               JVM::LSTORE
               JVM::LSTORE_0
               JVM::LSTORE_1
               JVM::LSTORE_2
               JVM::LSTORE_3
               JVM::LSUB
               JVM::LUSHR
               JVM::LXOR
               JVM::MONITORENTER
               JVM::MONITOREXIT
               JVM::MULTIANEWARRAY
               JVM::NEW
               JVM::NEWARRAY
               JVM::NOP
               JVM::POP
               JVM::POP2
               JVM::PUTFIELD
               JVM::PUTSTATIC
               JVM::RET
               JVM::RETURN
               JVM::SALOAD
               JVM::SASTORE
               JVM::SIPUSH
               JVM::SWAP
               ASSOC-EQUAL LEN NTH ZP SYNTAXP
               QUOTEP FIX NFIX E0-ORDINALP E0-ORD-<)
             (union-eq *acl2-exports*
                       *common-lisp-symbols-from-main-lisp-package*))
   '(PC PROGRAM PUSH POP RETURN REVERSE STEP ++)))

(acl2::begin-book t :ttags :all);$ACL2s-Preamble$|#


(in-package "MC")

; -----------------------------------------------------------------------------
; Utilities

(defun push (obj stack) (cons obj stack))
(defun top (stack) (car stack))
(defun pop (stack) (cdr stack))

; Longs are stored on the stack as 2 elements: a zero and the value
; These utility functions manage that representation (which is *probably*
; necessary for instructions that move certain distances on the stack 
; such as bipush)
(defun pushlong (obj stack) (cons 0 (cons obj stack)))
(defun toplong (stack) (cadr stack))
(defun poplong (stack) (cddr stack))

(defun popn (n stack)
  (if (zp n)
      stack
      (popn (- n 1) (pop stack))))

(defun bound? (x alist) (assoc-equal x alist))

(defun bind (x y alist)
  (cond ((endp alist) (list (cons x y)))
        ((equal x (car (car alist)))
         (cons (cons x y) (cdr alist)))
        (t (cons (car alist) (bind x y (cdr alist))))))

(defun binding (x alist) (cdr (assoc-equal x alist)))

(defun op-code (inst) (car inst))
(defun arg1 (inst) (car (cdr inst)))
(defun arg2 (inst) (car (cdr (cdr inst))))
(defun arg3 (inst) (car (cdr (cdr (cdr inst)))))

(defun nullref () 
  '(REF -1))

(defun nullrefp (ref)
  (equal ref '(ref -1)))

; Imported from ACL2

(defun reverse (x)
  (if (consp x)
      (append (reverse (cdr x)) (list (car x)))
    nil))

; The following are constants and functions related to fixed integer sizes

(defconst *largest-integer-value* (- (expt 2 31) 1))
(defconst *largest-long-value* (- (expt 2 63) 1))
(defconst *most-negative-integer* (- (expt 2 31)))
(defconst *most-negative-long* (- (expt 2 63)))

; Coerce x to an unsigned integer which will fit in n bits.
(defun u-fix (x n)
  (mod (ifix x) (expt 2 n)))

; Coerce x to a signed integer which will fit in n bits.
(defun s-fix (x n)
  (let ((temp (mod (ifix x) (expt 2 n))))
    (if (< temp (expt 2 (1- n)))
        temp
      (- temp (expt 2 n)))))

(defun byte-fix (x)
  (s-fix x 8))

(defun ubyte-fix (x)
  (u-fix x 8))

(defun short-fix (x)
  (s-fix x 16))

(defun int-fix (x)
  (s-fix x 32))

(defun uint-fix (x)
  (u-fix x 32))

(defun long-fix (x)
  (s-fix x 64))

(defun ulong-fix (x)
  (u-fix x 64))

(defun char-fix (x)
  (u-fix x 16))

(defun 6-bit-fix (x)
  (u-fix x 6))

(defun 5-bit-fix (x)
  (u-fix x 5))

(defun expt2 (n)
  (expt 2 n))

(defun shl (x n)
  (* x (expt2 n)))

(defun shr (x n)
  (floor (* x (expt2 (- n))) 1))

; -----------------------------------------------------------------------------
; States

(defun make-state (call-stack heap class-table)
  (list call-stack heap class-table))
(defun call-stack  (s) (nth 0 s))
(defun heap        (s) (nth 1 s))
(defun class-table (s) (nth 2 s))

; -----------------------------------------------------------------------------
; Class Declarations and the Class Table

; The class table of a state is an alist.  Each entry in a class table is
; a "class declaration" and is of the form

;   (class-name super-class-names interfaces fields sfields cp methods ref)

(defun make-class-decl (name superclasses interfaces fields sfields cp methods href)
  (list name superclasses interfaces fields sfields cp methods href))

(defun class-decl-name (dcl)
  (nth 0 dcl))
(defun class-decl-superclasses (dcl)
  (nth 1 dcl))
(defun class-decl-interfaces (dcl)
  (nth 2 dcl))
(defun class-decl-fields (dcl)
  (nth 3 dcl))
(defun class-decl-sfields (dcl)
  (nth 4 dcl))
(defun class-decl-cp (dcl)
  (nth 5 dcl))
(defun class-decl-methods (dcl)
  (nth 6 dcl))
(defun class-decl-heapref (dcl)
  (nth 7 dcl))

(defun base-class-def ()
   (list (make-class-decl "java.lang.Object"
                          nil
                          '()
                          '()
                          '()
                          '()
                          (list
                           '("<init>" () (RETURN))
                           '("getClass" () ()))
                          '(REF -1))
         (make-class-decl "java.lang.System"
                          '("java.lang.Object")
                          '()
                          '()
                          '()
                          '()
                          (list                           
                           '("arraycopy" () ()))
                          '(REF -1))         
         (make-class-decl "ARRAY"
                          '("java.lang.Object")
                          '()                          
                          '(("<array>" . *ARRAY*))
                          '()
                          '()
                          '()
                          '(REF -1))
         (make-class-decl "java.lang.String"
                          '("java.lang.Object")
                          '()
                          '("strcontents")
                          '()
                          '()
                          '(("<init>" ()
                                      (aload_0)
                                      (invokespecial "java.lang.Object" "<init>" 0)
                                      (return)))
                          '(REF -1))
         (make-class-decl "java.lang.Class"
                          '("java.lang.Object")
                          '()
                          '()
                          '()
                          '()
                          '(("<init>" ()                                     
                                     (aload_0)
                                     (invokespecial "java.lang.Object" "<init>" 0)
                                     (return)))
                          '(REF -1))
         (make-class-decl "java.util.Arrays"
                          nil
                          '()                          
                          '()
                          '()
                          '()
                          (list
                           '("copyOfRange" () ()))
                          '(REF -1))))

(defun make-class-def (list-of-class-decls)
   (append (base-class-def) list-of-class-decls))

(defun class-superclasses (class-name class-table)
  (class-decl-superclasses (bound? class-name class-table)))

(defun class-interfaces (class-name class-table)
  (class-decl-interfaces (bound? class-name class-table)))

; -----------------------------------------------------------------------------
; A Constant Pool
;
; There is one constant pool per class

; A constant pool is a list of entries.  Each entry is either:
;
;  '(INT n)
;       Where n is a 32-bit number, in the range specified by the JVM spec
;
;  '(STRING (REF -1) "Hello, World!")
;       The 3rd element (a string) is resolved to a heap reference the
;       first time it is used.  Once it is resolved, its reference is placed
;       as the second element (displacing the null ref currently there).

(defun cp-make-int-entry (n)
  (list 'INT n))

(defun cp-make-string-entry (str)
  (list 'STRING '(REF -1) str))

(defun cp-string-resolved? (entry)
  (not (equal (cadr (caddr entry)) -1)))

(defun retrieve-cp (class-name class-table)
  (class-decl-cp (bound? class-name class-table)))

(defun update-ct-string-ref (class idx newval ct)
  (let* ((class-entry (bound? class ct))
         (oldstrval (caddr (nth idx (retrieve-cp class ct))))
         (newstrentry (list 'STRING newval oldstrval))
         (new-cp (update-nth idx
                              newstrentry
                              (class-decl-cp class-entry)))
         (new-class-entry
          (make-class-decl (class-decl-name class-entry)
                           (class-decl-superclasses class-entry)
                           (class-decl-interfaces class-entry)
                           (class-decl-fields class-entry)
                           (class-decl-sfields class-entry)
                           new-cp
                           (class-decl-methods class-entry)
                           (class-decl-heapref class-entry))))
        (bind class (cdr new-class-entry) ct)))

(defun update-ct-class-ref (class idx newval ct)
  (let* ((class-entry (bound? class ct))
         (class-name (caddr (nth idx (retrieve-cp class ct))))
         (newentry (list 'CLASS newval class-name))
         (new-cp (update-nth idx newentry (class-decl-cp class-entry)))
         (new-class-entry
          (make-class-decl (class-decl-name class-entry)
                           (class-decl-superclasses class-entry)
                           (class-decl-interfaces class-entry)
                           (class-decl-fields class-entry)
                           (class-decl-sfields class-entry)
                           new-cp
                           (class-decl-methods class-entry)
                           (class-decl-heapref class-entry))))
        (bind class (cdr new-class-entry) ct)))

; -----------------------------------------------------------------------------
; Frames

(defun make-frame (pc locals stack program cur-class)
  (list pc locals stack program cur-class))

(defun top-frame (s) (top (call-stack s)))

(defun pc        (frame) (nth 0 frame))
(defun locals    (frame) (nth 1 frame))
(defun stack     (frame) (nth 2 frame))
(defun program   (frame) (nth 3 frame))
(defun cur-class (frame) (nth 4 frame))

; -----------------------------------------------------------------------------
; Method Declarations

; The methods component of a class declaration is a list of method definitions.
; A method definition is a list of the form

; (name formals sync-status . program)

; We never build these declarations but just enter list constants for them,

; Note the similarity to our old notion of a program definition.  We
; will use strings to name methods now.

; Method definitions will be constructed by expressions such as:
; (Note:  all of the symbols below are understood to be in the pkg "JVM".)

; ("move" (dx dy) 
;   (load this)
;   (load this)
;   (getfield "Point" "x")
;   (load dx)
;   (add)
;   (putfield "Point" "x")    ; this.x = this.x + dx;
;   (load :this)
;   (load :this)
;   (getfield "Point" "y")
;   (load dy)
;   (add)
;   (putfield "Point" "y")    ; this.y = this.y + dy;
;   (push 1)
;   (xreturn)))               ; return 1;

; Provided this method is defined in the class "Point" it can be invoked by

;   (invokevirtual "Point" "move" 2)

; This assumes that the stack, at the time of invocation, contains an
; reference to an object of type "Point" and two numbers, dx and dy.

; If a method declaration has an empty list for the program (ie- there are
; no bytecodes associated with the method), then the method is considered
; native.  Native methods are normally written in something like C or
; assembly language.  The JVM would normally ensure that the correct number
; and type of arguments are passed to the native method, and would then hand
; over control to C.  In our model, we simply "hardwire" invokevirtual to
; to handle these methods.
;  * Note that a method in Java will never have 0 bytecodes, since even if
;    it has no body, it will consist of at least the (xreturn) bytecode.

; The accessors for methods are:

(defun method-name (m)
  (nth 0 m))
(defun method-formals (m)
  (nth 1 m))
(defun method-program (m)
  (cddr m))
(defun method-isNative? (m)
  (equal '(NIL)
         (method-program m)))

; The Standard Modify

(defun suppliedp (key args)
  (cond ((endp args) nil)
        ((equal key (car args)) t)
        (t (suppliedp key (cdr args)))))

(defun actual (key args)
  (cond ((endp args) nil)
        ((equal key (car args)) (cadr args))
        (t (actual key (cdr args)))))
        
(defmacro modify (s &rest args)
  (list 'make-state
        (cond
         ((or (suppliedp :call-stack args)
              (suppliedp :pc args)
              (suppliedp :locals args)
              (suppliedp :stack args)
              (suppliedp :program args)
              (suppliedp :cur-class args))
          (cond
            ((suppliedp :call-stack args)
             (actual :call-stack args))
            (t
             (list 'push
              (list 'make-frame
                (if (suppliedp :pc args)
                    (actual :pc args)
                    (list 'pc (list 'top-frame s)))
                (if (suppliedp :locals args)
                    (actual :locals args)
                    (list 'locals (list 'top-frame s)))
                (if (suppliedp :stack args)
                    (actual :stack args)
                    (list 'stack (list 'top-frame s)))
                (if (suppliedp :program args)
                    (actual :program args)
                    (list 'program (list 'top-frame s)))
                (if (suppliedp :cur-class args)
                    (actual :cur-class args)
                    (list 'cur-class
                          (list 'top-frame s))))
                          (list 'pop (list 'call-stack s))))))
         (t (list 'call-stack s)))
        (if (suppliedp :heap args)
            (actual :heap args)
          (list 'heap s))
        (if (suppliedp :class-table args)
            (actual :class-table args)
          (list 'class-table s))))

; -----------------------------------------------------------------------------
; Helper functions related to building instances of objects

(defun deref (ref heap)
  (binding (cadr ref) heap))

(defun field-value (class-name field-name instance)
  (binding field-name
           (binding class-name instance)))

(defun build-class-field-bindings (field-names)
  (if (endp field-names)
      nil
    (cons (cons (car field-names) 0)
          (build-class-field-bindings (cdr field-names)))))

(defun build-class-object-field-bindings ()
  '(("monitor" . 0) ("monitor-count" . 0) ("wait-set" . nil)))

(defun build-immediate-instance-data (class-name class-table)
  (cons class-name
      (build-class-field-bindings
       (class-decl-fields
        (bound? class-name class-table)))))

(defun build-an-instance (class-names class-table)
  (if (endp class-names)
      nil
    (cons (build-immediate-instance-data (car class-names) class-table)
          (build-an-instance (cdr class-names) class-table))))

(defun build-class-data (sfields)
  (cons "java.lang.Class"
        (build-class-field-bindings
         (cons "<name>" sfields))))

(defun build-a-class-instance (sfields class-table)
    (list (build-class-data sfields)
          (build-immediate-instance-data "java.lang.Object" class-table)))


; -----------------------------------------------------------------------------
; Arrays

(defun value-of (obj)
  (cdr obj))

(defun superclasses-of (class ct)
  (class-decl-superclasses (bound? class ct)))

(defun array-content (array)
  (value-of (field-value "ARRAY" "<array>" array)))

(defun array-type (array)
  (nth 0 (array-content array)))

(defun array-bound (array)
  (nth 1 (array-content array)))

(defun array-data (array)
  (nth 2 (array-content array)))

(defun element-at (index array)
  (nth index (array-data array)))

(defun makearray (type bound data class-table)
  (cons (list "ARRAY"
              (cons "<array>" (cons '*array* (list type bound data))))
        (build-an-instance
         (superclasses-of "ARRAY" class-table)
         class-table)))

(defun set-element-at (value index array class-table)
  (makearray (array-type array)
             (array-bound array)
             (update-nth index value (array-data array))
             class-table))

(defun primitive-type (type)
  (cond ((equal type 'T_BYTE) t)
        ((equal type 'T_SHORT) t)
        ((equal type 'T_INT) t)
        ((equal type 'T_LONG) t)
        ((equal type 'T_FLOAT) t)
        ((equal type 'T_DOUBLE) t)
        ((equal type 'T_CHAR) t)
        ((equal type 'T_BOOLEAN) t)
        (t nil)))

(defun atype-to-identifier (atype-num)
  (cond ((equal atype-num 4) 'T_BOOLEAN)
        ((equal atype-num 5) 'T_CHAR)
        ((equal atype-num 6) 'T_FLOAT)
        ((equal atype-num 7) 'T_DOUBLE)
        ((equal atype-num 8) 'T_BYTE)
        ((equal atype-num 9) 'T_SHORT)
        ((equal atype-num 10) 'T_INT)
        ((equal atype-num 11) 'T_LONG)
        (t nil)))

(defun identifier-to-atype (ident)
  (cond ((equal ident 'T_BOOLEAN) 4)
        ((equal ident 'T_CHAR) 5)
        ((equal ident 'T_FLOAT) 6)
        ((equal ident 'T_DOUBLE) 7)
        ((equal ident 'T_BYTE) 8)
        ((equal ident 'T_SHORT) 9)
        ((equal ident 'T_INT) 10)
        ((equal ident 'T_LONG) 11)
        (t nil)))

(defun default-value1 (type)
  (if (primitive-type type)
      0
      nil))

(defun init-array (type count)
  (if (zp count)
      nil
      (cons (default-value1 type) (init-array type (- count 1)))))

; The following measure is due to J
(defun natural-sum (lst)
  (cond ((endp lst) 0)
        (t (+ (nfix (car lst)) (natural-sum (cdr lst))))))

(include-book "ordinals/lexicographic-ordering-without-arithmetic" :dir :system)

(mutual-recursion

  ; makemultiarray2 :: num, counts, s, ac --> [refs]
  (defun makemultiarray2 (car-counts cdr-counts s ac)
    (declare (xargs :measure (acl2::llist
                              (len (cons car-counts cdr-counts))
                              (natural-sum (cons car-counts cdr-counts)))
                    :well-founded-relation acl2::l<))
    (if (zp car-counts)
        (mv (heap s) ac)
        (mv-let (new-addr new-heap) 
                (makemultiarray cdr-counts s)
                (makemultiarray2 (- car-counts 1)
                                 cdr-counts
                                 (make-state (call-stack s)
                                             new-heap
                                             (class-table s))
                                 (cons (list 'REF new-addr) ac)))))
                             
  ; makemultiarray :: [counts], s --> addr, new-heap
  (defun makemultiarray (counts s)
    (declare (xargs :measure (acl2::llist (+ 1 (len counts))
                                          (natural-sum counts))
                    :well-founded-relation acl2::l<))
    (if (<= (len counts) 1)
        
        ; "Base case"  Handles initializing the final dimension
        (mv (len (heap s))
            (bind (len (heap s))
                  (makearray 'T_REF
                             (car counts)
                             (init-array 'T_REF (car counts))
                             (class-table s))
                  (heap s)))

        ; "Recursive Case"
        (mv-let (heap-prime lst-of-refs)
                (makemultiarray2 (car counts)
                                 (cdr counts)
                                 s
                                 nil)
                (let* ((obj (makearray 'T_REF
                                       (car counts)
                                       lst-of-refs
                                       (class-table s)))
                       (new-addr (len heap-prime))
                       (new-heap (bind new-addr obj heap-prime)))
                      (mv new-addr new-heap)))))
)

(defun class-name-of-ref (ref heap)
  (car (car (deref ref heap))))

(defun set-instance-field (class-name field-name value instance)
  (bind class-name
        (bind field-name value
              (binding class-name instance))
        instance))#|ACL2s-ToDo-Line|#
