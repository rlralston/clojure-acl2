; ObjectInputStream$ValidationList-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:32 CDT 2014.
;

(defconst *java.io.ObjectInputStream$ValidationList*
 (make-class-def
      '(class "java.io.ObjectInputStream$ValidationList"
            "java.lang.Object"
            (constant_pool
                        (STRING  "null callback"))
            (fields
                        (field "list" (class "java.io.ObjectInputStream$ValidationList$Callback") (accessflags  *class*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "register"
                              (parameters (class "java.io.ObjectInputValidation") int)
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 7) (max_locals . 6) (code_length . 98)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (ifnonnull 14)) ;;to TAG_0
                                      (4 (new (class "java.io.InvalidObjectException"))) 
                                      (7 (dup)) 
                                      (8 (ldc 0)) ;;STRING:: "null callback"
                                      (10 (invokespecial (methodCP "<init>" "java.io.InvalidObjectException" ((class "java.lang.String")) void))) 
                                      (13 (athrow)) 
                                      (14 (aconst_null)) ;;at TAG_0
                                      (15 (astore_3)) 
                                      (16 (aload_0)) 
                                      (17 (getfield (fieldCP "list" "java.io.ObjectInputStream$ValidationList" (class "java.io.ObjectInputStream$ValidationList$Callback")))) 
                                      (20 (astore 4)) 
                                      (22 (aload 4)) ;;at TAG_2
                                      (24 (ifnull 49)) ;;to TAG_1
                                      (27 (iload_2)) 
                                      (28 (aload 4)) 
                                      (30 (getfield (fieldCP "priority" "java.io.ObjectInputStream$ValidationList$Callback" int))) 
                                      (33 (if_icmpge 49)) ;;to TAG_1
                                      (36 (aload 4)) 
                                      (38 (astore_3)) 
                                      (39 (aload 4)) 
                                      (41 (getfield (fieldCP "next" "java.io.ObjectInputStream$ValidationList$Callback" (class "java.io.ObjectInputStream$ValidationList$Callback")))) 
                                      (44 (astore 4)) 
                                      (46 (goto 22))  ;;to TAG_2
                                      (49 (invokestatic (methodCP "getContext" "java.security.AccessController" () (class "java.security.AccessControlContext")))) ;;at TAG_1
                                      (52 (astore 5)) 
                                      (54 (aload_3)) 
                                      (55 (ifnull 78)) ;;to TAG_3
                                      (58 (aload_3)) 
                                      (59 (new (class "java.io.ObjectInputStream$ValidationList$Callback"))) 
                                      (62 (dup)) 
                                      (63 (aload_1)) 
                                      (64 (iload_2)) 
                                      (65 (aload 4)) 
                                      (67 (aload 5)) 
                                      (69 (invokespecial (methodCP "<init>" "java.io.ObjectInputStream$ValidationList$Callback" ((class "java.io.ObjectInputValidation") int (class "java.io.ObjectInputStream$ValidationList$Callback") (class "java.security.AccessControlContext")) void))) 
                                      (72 (putfield (fieldCP "next" "java.io.ObjectInputStream$ValidationList$Callback" (class "java.io.ObjectInputStream$ValidationList$Callback")))) 
                                      (75 (goto 97)) ;;to TAG_4
                                      (78 (aload_0)) ;;at TAG_3
                                      (79 (new (class "java.io.ObjectInputStream$ValidationList$Callback"))) 
                                      (82 (dup)) 
                                      (83 (aload_1)) 
                                      (84 (iload_2)) 
                                      (85 (aload_0)) 
                                      (86 (getfield (fieldCP "list" "java.io.ObjectInputStream$ValidationList" (class "java.io.ObjectInputStream$ValidationList$Callback")))) 
                                      (89 (aload 5)) 
                                      (91 (invokespecial (methodCP "<init>" "java.io.ObjectInputStream$ValidationList$Callback" ((class "java.io.ObjectInputValidation") int (class "java.io.ObjectInputStream$ValidationList$Callback") (class "java.security.AccessControlContext")) void))) 
                                      (94 (putfield (fieldCP "list" "java.io.ObjectInputStream$ValidationList" (class "java.io.ObjectInputStream$ValidationList$Callback")))) 
                                      (97 (return)) ;;at TAG_4
                                      (endofcode 98))
                                   (Exceptions )
                                   (StackMap )))
                        (method "doCallbacks"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 58)
                                   (parsedcode
                                      (0 (aload_0)) ;;at TAG_1
                                      (1 (getfield (fieldCP "list" "java.io.ObjectInputStream$ValidationList" (class "java.io.ObjectInputStream$ValidationList$Callback")))) 
                                      (4 (ifnull 40)) ;;to TAG_0
                                      (7 (new (class "java.io.ObjectInputStream$ValidationList$1"))) 
                                      (10 (dup)) 
                                      (11 (aload_0)) 
                                      (12 (invokespecial (methodCP "<init>" "java.io.ObjectInputStream$ValidationList$1" ((class "java.io.ObjectInputStream$ValidationList")) void))) 
                                      (15 (aload_0)) 
                                      (16 (getfield (fieldCP "list" "java.io.ObjectInputStream$ValidationList" (class "java.io.ObjectInputStream$ValidationList$Callback")))) 
                                      (19 (getfield (fieldCP "acc" "java.io.ObjectInputStream$ValidationList$Callback" (class "java.security.AccessControlContext")))) 
                                      (22 (invokestatic (methodCP "doPrivileged" "java.security.AccessController" ((class "java.security.PrivilegedExceptionAction") (class "java.security.AccessControlContext")) (class "java.lang.Object")))) 
                                      (25 (pop)) 
                                      (26 (aload_0)) 
                                      (27 (aload_0)) 
                                      (28 (getfield (fieldCP "list" "java.io.ObjectInputStream$ValidationList" (class "java.io.ObjectInputStream$ValidationList$Callback")))) 
                                      (31 (getfield (fieldCP "next" "java.io.ObjectInputStream$ValidationList$Callback" (class "java.io.ObjectInputStream$ValidationList$Callback")))) 
                                      (34 (putfield (fieldCP "list" "java.io.ObjectInputStream$ValidationList" (class "java.io.ObjectInputStream$ValidationList$Callback")))) 
                                      (37 (goto 0)) ;;to TAG_1
                                      (40 (goto 57))  ;;to TAG_2;;at TAG_0
                                      (43 (astore_1)) ;;at TAG_3
                                      (44 (aload_0)) 
                                      (45 (aconst_null)) 
                                      (46 (putfield (fieldCP "list" "java.io.ObjectInputStream$ValidationList" (class "java.io.ObjectInputStream$ValidationList$Callback")))) 
                                      (49 (aload_1)) 
                                      (50 (invokevirtual (methodCP "getException" "java.security.PrivilegedActionException" () (class "java.lang.Exception")))) 
                                      (53 (checkcast (class "java.io.InvalidObjectException"))) 
                                      (56 (athrow)) 
                                      (57 (return)) ;;at TAG_2
                                      (endofcode 58))
                                   (Exceptions 
                                     (handler 0 40  43 (class "java.security.PrivilegedActionException")))
                                   (StackMap )))
                        (method "clear"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aconst_null))
                                      (2 (putfield (fieldCP "list" "java.io.ObjectInputStream$ValidationList" (class "java.io.ObjectInputStream$ValidationList$Callback"))))
                                      (5 (return))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "access$400"
                              (parameters (class "java.io.ObjectInputStream$ValidationList"))
                              (returntype . (class "java.io.ObjectInputStream$ValidationList$Callback"))
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "list" "java.io.ObjectInputStream$ValidationList" (class "java.io.ObjectInputStream$ValidationList$Callback"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *ObjectInputStream$ValidationList-class-table*
  (make-static-class-decls 
   *java.io.ObjectInputStream$ValidationList*))

(defconst *package-name-map* 
  ("java.io.ObjectInputStream$ValidationList" . "java.io"))

