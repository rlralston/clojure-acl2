#|$ACL2s-Preamble$;
(acl2::begin-book t :ttags :all);$ACL2s-Preamble$|#


(in-package "ACL2")

; -----------------------------------------------------------------------------
; Translates m6 classes created from java2acl tool provided by Hanbing Liu into 
; class declarations that may be used in the mc model

(defun classname-s (sclass-rep)
  (nth 1 sclass-rep))

(defun super-s (sclass-rep)
  (nth 2 sclass-rep))

(defun constantpool-s (sclass-rep)
  (cdr (nth 3 sclass-rep)))

(defun fields-s (sclass-rep)
  (cdr (nth 4 sclass-rep)))

(defun methods-s (sclass-rep)
  (cdr (nth 5 sclass-rep)))

(defun interfaces-s (sclass-rep)
  (cdr (nth 6 sclass-rep)))

(defun accessflags-s (sclass-rep)
  (cdr (nth 7 sclass-rep)))

(defun make-static-class-desc (classname super constantpool
                                         fields methods
                                         interfaces
                                         accessflags)
  (list 'class classname                   ;; just names 
        super                              ;; name of the super
        (cons 'constant_pool constantpool)  ;; runtime representation
        (cons 'fields fields)               ;; fields 
        (cons 'methods methods)             ;; list of methods 
        (cons 'interfaces interfaces)       ;; interfaces 
        (cons 'accessflags accessflags)))   ;; access flags

(defun make-class-def (raw-class) 
  (make-static-class-desc 
     (classname-s raw-class)
     (super-s     raw-class)
     (constantpool-s raw-class)
     (fields-s       raw-class)
     (methods-s      raw-class)
     (interfaces-s   raw-class)
     (accessflags-s  raw-class)))

(defun method-name (method)
  (cadr method))

(defun method-params (method)
  (cdr (caddr method)))

(defun method-code (method)
  (cdr (nth 4 (nth 5 method))))

(defun opcode (inst)
  (caadr inst))

(defun count-longs (paramlist) 
  (if (consp paramlist)
    (if (equal (car paramlist) 'long)
      (1+ (count-longs (cdr paramlist)))
      (count-longs (cdr paramlist)))
    (if (equal (car paramlist) 'long)
      1
      0)))

(defun translate-methodcp (methodcp) 
  (list
   (nth 2 methodcp) ; The class name
   (nth 1 methodcp) ; The method name
   (+ (len (nth 3 methodcp)) (count-longs (nth 3 methodcp))))) 

(defun translate-fieldcp (fieldcp) 
  (list
   (nth 2 fieldcp) ; The class name
   (nth 1 fieldcp) ; The method name
   (if (equal 'long (nth 3 fieldcp))
     t
     nil)))

(defun translate-method-code-inst (inst)
  (declare (acl2::xargs :mode :program))
  (case (opcode inst)
    (ANEWARRAY       (list 'ANEWARRAY (cdr (cadadr inst))))
    (CHECKCAST       (list 'CHECKCAST (cadr (cadadr inst))))
    (GETFIELD        (cons 'GETFIELD (translate-fieldcp (cadadr inst))))
    (GETSTATIC       (cons 'GETSTATIC (translate-fieldcp (cadadr inst))))
    (GOTO            (list 'GOTO (- (cadadr inst) (car inst))))
    (GOTO_W          (list 'GOTO_W (- (cadadr inst) (car inst))))
    (IF_ACMPEQ       (list 'IF_ACMPEQ (- (cadadr inst) (car inst))))
    (IF_ACMPNE       (list 'IF_ACMPNE (- (cadadr inst) (car inst))))
    (IF_ICMPEQ       (list 'IF_ICMPEQ (- (cadadr inst) (car inst))))
    (IF_ICMPGE       (list 'IF_ICMPGE (- (cadadr inst) (car inst))))
    (IF_ICMPGT       (list 'IF_ICMPGT (- (cadadr inst) (car inst))))
    (IF_ICMPLE       (list 'IF_ICMPLE (- (cadadr inst) (car inst))))
    (IF_ICMPLT       (list 'IF_ICMPLT (- (cadadr inst) (car inst))))
    (IF_ICMPNE       (list 'IF_ICMPNE (- (cadadr inst) (car inst))))
    (IFEQ            (list 'IFEQ (- (cadadr inst) (car inst))))
    (IFGE            (list 'IFGE (- (cadadr inst) (car inst))))
    (IFGT            (list 'IFGT (- (cadadr inst) (car inst))))
    (IFLE            (list 'IFLE (- (cadadr inst) (car inst))))
    (IFLT            (list 'IFLT (- (cadadr inst) (car inst))))
    (IFNE            (list 'IFNE (- (cadadr inst) (car inst))))
    (IFNONNULL       (list 'IFNONNULL (- (cadadr inst) (car inst))))
    (IFNULL          (list 'IFNULL (- (cadadr inst) (car inst))))
    (INSTANCEOF      (list 'INSTANCEOF (cadr (cadadr inst))))
    (INVOKEINTERFACE (cons 'INVOKEINTERFACE (translate-methodcp (cadadr inst))))
    (INVOKESPECIAL   (cons 'INVOKESPECIAL (translate-methodcp (cadadr inst))))
    (INVOKESTATIC    (cons 'INVOKESTATIC (translate-methodcp (cadadr inst))))
    (INVOKEVIRTUAL   (cons 'INVOKEVIRTUAL (translate-methodcp (cadadr inst))))    
    ;(MULTIANEWARRAY  (execute-MULTIANEWARRAY inst s))
    (NEW             (list 'NEW (cadr (cadadr inst))))
    (NEWARRAY        (list 'NEWARRAY))
    (PUTFIELD        (cons 'PUTFIELD (translate-fieldcp (cadadr inst))))
    (PUTSTATIC       (cons 'PUTSTATIC (translate-fieldcp (cadadr inst))))
    (otherwise (cadr inst))))
  
(defun print-method-code (method)
  (declare (acl2::xargs :mode :program))
  (if (equal (caar method) 'endofcode)
    nil
    (prog2$   
     (acl2::cw "~ ~ ~ ~ ~ ~ ~x0~%" (translate-method-code-inst (car method)))
     (print-method-code (cdr method)))))
  
(defun print-method (method)
  (declare (acl2::xargs :mode :program))
  (progn$
   (acl2::cw "~%~ ~ ~ ~ '(~x0 ~x1~%" (method-name method) (method-params method))
   (print-method-code (method-code method))
   (acl2::cw "~ ~ ~ ~ ~ )~%")))

(defun print-methods (methods)
  (declare (acl2::xargs :mode :program))
  (if (consp methods)
    (prog2$
     (print-method (car methods))
     (print-methods (cdr methods)))
    nil))
    
(defun field-staticp (field) 
  (consp (member-eq '*STATIC* (nth 3 field))))

(defun field-name (field)
  (cadr field))

(defun get-static-fields (fields static-fields)
  (if (endp fields)
    static-fields
    (if (field-staticp (car fields))
      (get-static-fields (cdr fields) (append static-fields (list (car fields))))
      (get-static-fields (cdr fields) static-fields))))

(defun get-member-fields (fields member-fields)
  (if (endp fields)
    member-fields
    (if (field-staticp (car fields))      
      (get-member-fields (cdr fields) member-fields)
      (get-member-fields (cdr fields) (append member-fields (list (car fields)))))))

(defun get-field-names (fields field-names)
  (if (endp fields)
    field-names
    (get-field-names (cdr fields) 
                     (append field-names (list (nth 1 (car fields)))))))

(defun print-fields (fields)
  (declare (acl2::xargs :mode :program))
  (if (consp fields)
    (prog2$
     (acl2::cw "~ ~ ~ ~ ~x0~%" (car fields))
     (print-fields (cdr fields)))
    nil))

(defun print-cp (cp)
  (declare (acl2::xargs :mode :program))
  (if (consp cp)
    (prog2$
     (acl2::cw "~ ~ ~ ~ ~x0~%" (car cp))
     (print-cp (cdr cp)))
    nil))

(defun print-class (raw-class)
  (declare (acl2::xargs :mode :program))
  (progn$   
   (acl2::cw "~ ~ ~x0~%" (classname-s raw-class))
   (acl2::cw "~ ~ '(~x0)~%" (super-s raw-class))
   (acl2::cw "~ ~ '(~%")
   (print-fields (get-field-names (get-member-fields (fields-s raw-class) '()) '()))
   (acl2::cw "~ ~ )~%~ ~ '(~%")
   (print-fields (get-field-names (get-static-fields (fields-s raw-class) '()) '()))
   (acl2::cw "~ ~ )~%~ ~ '(~%")
   (print-cp (constantpool-s raw-class))   
   (acl2::cw "~ ~ )~%")
   (print-methods (methods-s raw-class))))#|ACL2s-ToDo-Line|#


; (method-code (nth 10 (methods-s *java.math.BigInteger*)))
; (print-method-code (method-code (nth 1 (methods-s *java.math.BigInteger*))))
; (print-methods (methods-s *java.math.BigInteger*))
; (get-member-fields (fields-s *java.math.BigInteger*) '())

; (print-class *java.math.BigInteger*)