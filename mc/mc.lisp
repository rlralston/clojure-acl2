#|$ACL2s-Preamble$;
(include-book "mc-structures")
(include-book "mc-native")
(include-book "mc-bytecode")

(acl2::begin-book t :ttags :all);$ACL2s-Preamble$|#


(in-package "MC")

(defun step (s)
  (do-inst (next-inst s) s))

(defun step-n (n s)
  (if (zp n)
    s
    (step-n (1- n) (step s))))

; Depth stack
(defun sdepth (stk)
  (declare (xargs :hints (("Goal" :in-theory (enable pop)))))
  (if (endp stk)
      0
    (+ 1 (sdepth (pop stk)))))

; Steps until the return to the invoking frame
;   d0:  The depth of the invoking frame
;    n:  Maximum number of steps (necessary to guarantee termination)
;    s:  The state
(defun run-to-return (d0 n s)
  (cond ((zp n) s)
        ((<= d0 (sdepth (call-stack s)))
         (run-to-return d0 (- n 1) (step s)))
        (t s)))

; Begin the simulator
;

(defun ack2 (num n lst)
  (if (zp n)
      lst
      (ack2 num (- n 1) (cons num lst))))

(defun ack0 (n)
  (ack2 0 n nil))

(acl2::set-state-ok t)

;; defun sim-loop (s acl2::state) 

; A small assembler to resolve labels into relative byte addresses
;
; Labels are symbols in the "LABEL" package.  Examples include:
;      LABEL::JUMP  LABEL::FOR  LABEL::START1
;
; To denote the jump-to point, insert a label before the opcode
;
; '((aconst_null)                       '((aconst_null)
;   (goto LABEL::TARGET)                  (goto 5)
;   (iconst_0)             =====>         (iconst_0)
;   (iconst_2)                            (iconst_2)
;   (LABEL::TARGET ADD)                   (add)
;   (ireturn))                            (ireturn))

(defun isLabel? (sym)
  (and (symbolp sym)
       (equal (symbol-package-name sym) "LABEL")))

(defun isLabeledInst? (inst)
  (isLabel? (car inst)))

(defun gen_label_alist (bytecodes cur_pc label_alist)
  (if (endp bytecodes)
      label_alist
      (let* ((bare_inst (if (isLabeledInst? (car bytecodes))
                            (cdr (car bytecodes))
                            (car bytecodes))))
            (gen_label_alist (cdr bytecodes)
                             (+ cur_pc
                                (inst-length bare_inst))
                             (if (isLabeledInst? (car bytecodes))
                                 (bind (car (car bytecodes))
                                       cur_pc
                                       label_alist)
                                 label_alist)))))

(defun resolve_labels (bytecodes cur_pc label_alist)
  (if (endp bytecodes)
      nil
      (let* ((inst (car bytecodes))
             (bare-inst (if (isLabeledInst? inst)
                            (cdr inst)
                            inst))
             (resolved-inst (if (isLabel? (arg1 bare-inst))
                                (list (op-code bare-inst)
                                      (- (binding (arg1 bare-inst)
                                                  label_alist)
                                         cur_pc))
                                bare-inst)))
            (append (list resolved-inst)
                  (resolve_labels (cdr bytecodes)
                                  (+ cur_pc
                                     (inst-length bare-inst))
                                  label_alist)))))

; resolve_basic_block takes a method and resolves all of the labels
;
; note that the JVM restricts jumps to within the method

(defun resolve_basic_block (bytecodes)
  (resolve_labels bytecodes
                  0
                  (gen_label_alist bytecodes 0 nil)))

; The following functions are used to strip a state down to resolve
;  all of the basic blocks and build up the newly resolved state

; resolving thread tables
;
(defun assemble_frame (frame)
  (make-frame (pc frame)
              (locals frame)
              (stack frame)
              (resolve_basic_block (program frame))
              (cur-class frame)))

(defun assemble_call_stack (cs)
  (if (endp cs)
      nil
      (cons (assemble_frame (car cs))
            (assemble_call_stack (cdr cs)))))

(defun assemble_thread (thread)
  (list (assemble_call_stack (car thread))
        (cadr thread)
        (caddr thread)))

;(defun assemble_thread_table (tt)
;  (if (endp tt)
;      nil
;      (cons (cons (caar tt)
;                  (assemble_thread (cdar tt)))
;            (assemble_thread_table (cdr tt)))))

; resolving class tables
;
(defun assemble_method (method)
  (append (list (method-name method)
                (method-formals method))
          (resolve_basic_block (method-program method))))

(defun assemble_methods (methods)
  (if (endp methods)
      nil
      (cons (assemble_method (car methods))
            (assemble_methods (cdr methods)))))

(defun assemble_class (class)
  (make-class-decl (class-decl-name class)
                   (class-decl-superclasses class)
                   (class-decl-interfaces class)
                   (class-decl-fields class)
                   (class-decl-sfields class)
                   (class-decl-cp class)
                   (assemble_methods (class-decl-methods class))
                   (class-decl-heapref class)))

(defun assemble_class_table (ct)
  (if (endp ct)
      nil
      (cons (assemble_class (car ct))
            (assemble_class_table (cdr ct)))))

(defun assemble_state (s)
  (make-state (assemble_call_stack (call-stack s))
              (heap s)
              (assemble_class_table (class-table s))))

; -----------------------------------------------------------------------------
; load_class_library: a utility for populating the heap with Class and
;                     String objects

(defun make-string-obj (class cpentry s idx)
  (let* ((new-object (build-an-instance
                      (cons "java.lang.String"
                            (class-decl-superclasses
                             (bound? "java.lang.String" (class-table s))))
                     (class-table s)))
         (stuffed-obj (set-instance-field "java.lang.String"
                                          "strcontents"
                                          (caddr cpentry)
                                          new-object))
         (new-address (len (heap s))))
        (modify s
                :heap (bind new-address stuffed-obj (heap s))
                :class-table (update-ct-string-ref
                              class
                              idx
                              (list 'REF new-address)
                              (class-table s)))))

(defun resolve-string-constants (class cp s idx)
  (cond ((endp cp) s)
        ((equal (caar cp) 'STRING) 
         (resolve-string-constants class
                                   (cdr cp)
                                   (make-string-obj class (car cp) s idx)
                                   (+ idx 1)))
        (t (resolve-string-constants class (cdr cp) s (+ idx 1)))))

(defun make-class-obj (class cpentry s idx)
  (modify s           
          :class-table (update-ct-class-ref
                        class
                        idx
                        (class-decl-heapref (bound? (caddr cpentry) (class-table s)))
                        (class-table s))))

(defun resolve-class-constants (class cp s idx)
  (cond ((endp cp) s)
        ((equal (caar cp) 'CLASS) 
         (resolve-class-constants class
                                  (cdr cp)
                                  (make-class-obj class (car cp) s idx)
                                  (+ idx 1)))
        (t (resolve-class-constants class (cdr cp) s (+ idx 1)))))

(defun gen_class_obj (class s)
  (let* ((ns (resolve-class-constants class
                                      (retrieve-cp class (class-table s))
                                      s
                                      0))
         (new-state (resolve-string-constants class
                                             (retrieve-cp class (class-table ns))
                                             ns
                                             0))
         (new-heap (heap new-state))
         (new-ct (class-table new-state))
         (new-object (build-a-class-instance
                      (class-decl-sfields (bound? class new-ct))
                      new-ct))
         (stuffed-obj (set-instance-field "java.lang.Class"
                                          "<name>"
                                          class
                                          new-object))
         (new-address (len new-heap))
         (old-class-ent (bound? class new-ct))
         (new-class-ent
          (make-class-decl (class-decl-name old-class-ent)
                           (class-decl-superclasses old-class-ent)
                           (class-decl-interfaces old-class-ent)
                           (class-decl-fields old-class-ent)
                           (class-decl-sfields old-class-ent)
                           (class-decl-cp old-class-ent)
                           (class-decl-methods old-class-ent)
                           (list 'REF new-address)))
         (new-class-table (bind class
                                (cdr new-class-ent)
                                new-ct)))
        (make-state (call-stack s)
                    (bind new-address stuffed-obj new-heap)
                    new-class-table)))

(defun ld_class_lib (classes s)
  (if (endp classes)
      s
      (ld_class_lib (cdr classes) (gen_class_obj (car classes) s))))

(defun load_class_library (s)
  (ld_class_lib (strip-cars (class-table s)) s))

; -----------------------------------------------------------------------------
; m5_load: both load and resolve a given state

(defun m5_load (s)
  (load_class_library (assemble_state s)))

; -----------------------------------------------------------------------------
; Testing methods to help walk through code manually

(defun get-frame-method (s)
  (if (> (len (call-stack s)) 1)
    (let* ((call-frame (top (pop (call-stack s))))
           (call-inst (index-into-program (- (pc call-frame) 3)
                                          (program call-frame))))
      (list (cadr call-inst) (caddr call-inst)))
    nil))

(defun state-pp (s)
  (declare (acl2::xargs :mode :program))
  (let* ((method (get-frame-method s)))
    (progn$
     (acl2::cw "~x0 ~x1~%" (car method) (cadr method))
     (acl2::cw "Break: ~x0: ~x1~%" (pc (top-frame s)) (next-inst s))
     (acl2::cw "Stack: ~x0~%" (stack (top-frame s))))))#|ACL2s-ToDo-Line|#
