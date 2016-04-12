; Class$EnclosingMethodInfo-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:33 CDT 2014.
;

(defconst *java.lang.Class$EnclosingMethodInfo*
 (make-class-def
      '(class "java.lang.Class$EnclosingMethodInfo"
            "java.lang.Object"
            (constant_pool
                        (STRING  "Malformed enclosing method information")
                        (STRING  "Invalid type in enclosing method information")
                        (STRING  "<init>")
                        (STRING  "<clinit>"))
            (fields
                        (field "enclosingClass" (class "java.lang.Class") (accessflags  *class*  *private* ) -1)
                        (field "name" (class "java.lang.String") (accessflags  *class*  *private* ) -1)
                        (field "descriptor" (class "java.lang.String") (accessflags  *class*  *private* ) -1)
                        (field "$assertionsDisabled" boolean (accessflags  *class*  *final*  *static* ) -1))
            (methods
                        (method "<init>"
                              (parameters (array (class "java.lang.Object")))
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 125)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokespecial (methodCP "<init>" "java.lang.Object" () void))) 
                                      (4 (aload_1)) 
                                      (5 (arraylength)) 
                                      (6 (iconst_3)) 
                                      (7 (if_icmpeq 20)) ;;to TAG_0
                                      (10 (new (class "java.lang.InternalError"))) 
                                      (13 (dup)) 
                                      (14 (ldc 0)) ;;STRING:: "Malformed enclosing method information"
                                      (16 (invokespecial (methodCP "<init>" "java.lang.InternalError" ((class "java.lang.String")) void))) 
                                      (19 (athrow)) 
                                      (20 (aload_0)) ;;at TAG_0
                                      (21 (aload_1)) 
                                      (22 (iconst_0)) 
                                      (23 (aaload)) 
                                      (24 (checkcast (class "java.lang.Class"))) 
                                      (27 (putfield (fieldCP "enclosingClass" "java.lang.Class$EnclosingMethodInfo" (class "java.lang.Class")))) 
                                      (30 (getstatic (fieldCP "$assertionsDisabled" "java.lang.Class$EnclosingMethodInfo" boolean))) 
                                      (33 (ifne 51)) ;;to TAG_1
                                      (36 (aload_0)) 
                                      (37 (getfield (fieldCP "enclosingClass" "java.lang.Class$EnclosingMethodInfo" (class "java.lang.Class")))) 
                                      (40 (ifnonnull 51)) ;;to TAG_1
                                      (43 (new (class "java.lang.AssertionError"))) 
                                      (46 (dup)) 
                                      (47 (invokespecial (methodCP "<init>" "java.lang.AssertionError" () void))) 
                                      (50 (athrow)) 
                                      (51 (aload_0)) ;;at TAG_1
                                      (52 (aload_1)) 
                                      (53 (iconst_1)) 
                                      (54 (aaload)) 
                                      (55 (checkcast (class "java.lang.String"))) 
                                      (58 (putfield (fieldCP "name" "java.lang.Class$EnclosingMethodInfo" (class "java.lang.String")))) 
                                      (61 (aload_0)) 
                                      (62 (aload_1)) 
                                      (63 (iconst_2)) 
                                      (64 (aaload)) 
                                      (65 (checkcast (class "java.lang.String"))) 
                                      (68 (putfield (fieldCP "descriptor" "java.lang.Class$EnclosingMethodInfo" (class "java.lang.String")))) 
                                      (71 (getstatic (fieldCP "$assertionsDisabled" "java.lang.Class$EnclosingMethodInfo" boolean))) 
                                      (74 (ifne 110))  ;;to TAG_2
                                      (77 (aload_0)) 
                                      (78 (getfield (fieldCP "name" "java.lang.Class$EnclosingMethodInfo" (class "java.lang.String")))) 
                                      (81 (ifnull 91)) ;;to TAG_3
                                      (84 (aload_0)) 
                                      (85 (getfield (fieldCP "descriptor" "java.lang.Class$EnclosingMethodInfo" (class "java.lang.String")))) 
                                      (88 (ifnonnull 110))  ;;to TAG_2
                                      (91 (aload_0)) ;;at TAG_3
                                      (92 (getfield (fieldCP "name" "java.lang.Class$EnclosingMethodInfo" (class "java.lang.String")))) 
                                      (95 (aload_0)) 
                                      (96 (getfield (fieldCP "descriptor" "java.lang.Class$EnclosingMethodInfo" (class "java.lang.String")))) 
                                      (99 (if_acmpeq 110))  ;;to TAG_2
                                      (102 (new (class "java.lang.AssertionError"))) 
                                      (105 (dup)) 
                                      (106 (invokespecial (methodCP "<init>" "java.lang.AssertionError" () void))) 
                                      (109 (athrow)) 
                                      (110 (goto 124)) ;;to TAG_4;;at TAG_2
                                      (113 (astore_2)) ;;at TAG_5
                                      (114 (new (class "java.lang.InternalError"))) 
                                      (117 (dup)) 
                                      (118 (ldc 1)) ;;STRING:: "Invalid type in enclosing method information"
                                      (120 (invokespecial (methodCP "<init>" "java.lang.InternalError" ((class "java.lang.String")) void))) 
                                      (123 (athrow)) 
                                      (124 (return)) ;;at TAG_4
                                      (endofcode 125))
                                   (Exceptions 
                                     (handler 20 110  113 (class "java.lang.ClassCastException")))
                                   (StackMap )))
                        (method "isPartial"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 27)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "enclosingClass" "java.lang.Class$EnclosingMethodInfo" (class "java.lang.Class")))) 
                                      (4 (ifnull 21)) ;;to TAG_0
                                      (7 (aload_0)) 
                                      (8 (getfield (fieldCP "name" "java.lang.Class$EnclosingMethodInfo" (class "java.lang.String")))) 
                                      (11 (ifnull 21)) ;;to TAG_0
                                      (14 (aload_0)) 
                                      (15 (getfield (fieldCP "descriptor" "java.lang.Class$EnclosingMethodInfo" (class "java.lang.String")))) 
                                      (18 (ifnonnull 25)) ;;to TAG_1
                                      (21 (iconst_1)) ;;at TAG_0
                                      (22 (goto 26))  ;;to TAG_2
                                      (25 (iconst_0)) ;;at TAG_1
                                      (26 (ireturn)) ;;at TAG_2
                                      (endofcode 27))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isConstructor"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 25)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "isPartial" "java.lang.Class$EnclosingMethodInfo" () boolean))) 
                                      (4 (ifne 23))  ;;to TAG_0
                                      (7 (ldc 2)) ;;STRING:: "<init>"
                                      (9 (aload_0)) 
                                      (10 (getfield (fieldCP "name" "java.lang.Class$EnclosingMethodInfo" (class "java.lang.String")))) 
                                      (13 (invokevirtual (methodCP "equals" "java.lang.String" ((class "java.lang.Object")) boolean))) 
                                      (16 (ifeq 23))  ;;to TAG_0
                                      (19 (iconst_1)) 
                                      (20 (goto 24)) ;;to TAG_1
                                      (23 (iconst_0)) ;;at TAG_0
                                      (24 (ireturn)) ;;at TAG_1
                                      (endofcode 25))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isMethod"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 32)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "isPartial" "java.lang.Class$EnclosingMethodInfo" () boolean))) 
                                      (4 (ifne 30))  ;;to TAG_0
                                      (7 (aload_0)) 
                                      (8 (invokevirtual (methodCP "isConstructor" "java.lang.Class$EnclosingMethodInfo" () boolean))) 
                                      (11 (ifne 30))  ;;to TAG_0
                                      (14 (ldc 3)) ;;STRING:: "<clinit>"
                                      (16 (aload_0)) 
                                      (17 (getfield (fieldCP "name" "java.lang.Class$EnclosingMethodInfo" (class "java.lang.String")))) 
                                      (20 (invokevirtual (methodCP "equals" "java.lang.String" ((class "java.lang.Object")) boolean))) 
                                      (23 (ifne 30))  ;;to TAG_0
                                      (26 (iconst_1)) 
                                      (27 (goto 31)) ;;to TAG_1
                                      (30 (iconst_0)) ;;at TAG_0
                                      (31 (ireturn)) ;;at TAG_1
                                      (endofcode 32))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getEnclosingClass"
                              (parameters )
                              (returntype . (class "java.lang.Class"))
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "enclosingClass" "java.lang.Class$EnclosingMethodInfo" (class "java.lang.Class"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getName"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "name" "java.lang.Class$EnclosingMethodInfo" (class "java.lang.String"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getDescriptor"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "descriptor" "java.lang.Class$EnclosingMethodInfo" (class "java.lang.String"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (array (class "java.lang.Object")) (class "java.lang.Class$1"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokespecial
					(methodCP "<init>" "java.lang.Class$EnclosingMethodInfo" ((array (class "java.lang.Object"))) void)))
                                      (5 (return))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 0) (code_length . 18)
                                   (parsedcode
                                      (0 (ldc_w )) 
                                      (3 (invokevirtual (methodCP "desiredAssertionStatus" "java.lang.Class" () boolean))) 
                                      (6 (ifne 13))  ;;to TAG_0
                                      (9 (iconst_1)) 
                                      (10 (goto 14)) ;;to TAG_1
                                      (13 (iconst_0)) ;;at TAG_0
                                      (14 (putstatic (fieldCP "$assertionsDisabled" "java.lang.Class$EnclosingMethodInfo" boolean))) ;;at TAG_1
                                      (17 (return)) 
                                      (endofcode 18))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *Class$EnclosingMethodInfo-class-table*
  (make-static-class-decls 
   *java.lang.Class$EnclosingMethodInfo*))

(defconst *package-name-map* 
  ("java.lang.Class$EnclosingMethodInfo" . "java.lang"))

