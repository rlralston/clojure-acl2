; Insets-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:29 CDT 2014.
;

(defconst *java.awt.Insets*
 (make-class-def
      '(class "java.awt.Insets"
            "java.lang.Object"
            (constant_pool
                        (LONG -2272572637695466749)
                        (STRING  "[top=")
                        (STRING  ",left=")
                        (STRING  ",bottom=")
                        (STRING  ",right=")
                        (STRING  "]"))
            (fields
                        (field "top" int (accessflags  *class*  *public* ) -1)
                        (field "left" int (accessflags  *class*  *public* ) -1)
                        (field "bottom" int (accessflags  *class*  *public* ) -1)
                        (field "right" int (accessflags  *class*  *public* ) -1)
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0))
            (methods
                        (method "<init>"
                              (parameters int int int int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 5) (code_length . 26)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (iload_1))
                                      (6 (putfield (fieldCP "top" "java.awt.Insets" int)))
                                      (9 (aload_0))
                                      (10 (iload_2))
                                      (11 (putfield (fieldCP "left" "java.awt.Insets" int)))
                                      (14 (aload_0))
                                      (15 (iload_3))
                                      (16 (putfield (fieldCP "bottom" "java.awt.Insets" int)))
                                      (19 (aload_0))
                                      (20 (iload 4))
                                      (22 (putfield (fieldCP "right" "java.awt.Insets" int)))
                                      (25 (return))
                                      (endofcode 26))
                                   (Exceptions )
                                   (StackMap )))
                        (method "set"
                              (parameters int int int int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 5) (code_length . 22)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (iload_1))
                                      (2 (putfield (fieldCP "top" "java.awt.Insets" int)))
                                      (5 (aload_0))
                                      (6 (iload_2))
                                      (7 (putfield (fieldCP "left" "java.awt.Insets" int)))
                                      (10 (aload_0))
                                      (11 (iload_3))
                                      (12 (putfield (fieldCP "bottom" "java.awt.Insets" int)))
                                      (15 (aload_0))
                                      (16 (iload 4))
                                      (18 (putfield (fieldCP "right" "java.awt.Insets" int)))
                                      (21 (return))
                                      (endofcode 22))
                                   (Exceptions )
                                   (StackMap )))
                        (method "equals"
                              (parameters (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 64)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (instanceof (class "java.awt.Insets"))) 
                                      (4 (ifeq 62)) ;;to TAG_0
                                      (7 (aload_1)) 
                                      (8 (checkcast (class "java.awt.Insets"))) 
                                      (11 (astore_2)) 
                                      (12 (aload_0)) 
                                      (13 (getfield (fieldCP "top" "java.awt.Insets" int))) 
                                      (16 (aload_2)) 
                                      (17 (getfield (fieldCP "top" "java.awt.Insets" int))) 
                                      (20 (if_icmpne 60)) ;;to TAG_1
                                      (23 (aload_0)) 
                                      (24 (getfield (fieldCP "left" "java.awt.Insets" int))) 
                                      (27 (aload_2)) 
                                      (28 (getfield (fieldCP "left" "java.awt.Insets" int))) 
                                      (31 (if_icmpne 60)) ;;to TAG_1
                                      (34 (aload_0)) 
                                      (35 (getfield (fieldCP "bottom" "java.awt.Insets" int))) 
                                      (38 (aload_2)) 
                                      (39 (getfield (fieldCP "bottom" "java.awt.Insets" int))) 
                                      (42 (if_icmpne 60)) ;;to TAG_1
                                      (45 (aload_0)) 
                                      (46 (getfield (fieldCP "right" "java.awt.Insets" int))) 
                                      (49 (aload_2)) 
                                      (50 (getfield (fieldCP "right" "java.awt.Insets" int))) 
                                      (53 (if_icmpne 60)) ;;to TAG_1
                                      (56 (iconst_1)) 
                                      (57 (goto 61))  ;;to TAG_2
                                      (60 (iconst_0)) ;;at TAG_1
                                      (61 (ireturn)) ;;at TAG_2
                                      (62 (iconst_0)) ;;at TAG_0
                                      (63 (ireturn)) 
                                      (endofcode 64))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hashCode"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 6) (code_length . 66)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "left" "java.awt.Insets" int)))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "bottom" "java.awt.Insets" int)))
                                      (8 (iadd))
                                      (9 (istore_1))
                                      (10 (aload_0))
                                      (11 (getfield (fieldCP "right" "java.awt.Insets" int)))
                                      (14 (aload_0))
                                      (15 (getfield (fieldCP "top" "java.awt.Insets" int)))
                                      (18 (iadd))
                                      (19 (istore_2))
                                      (20 (iload_1))
                                      (21 (iload_1))
                                      (22 (iconst_1))
                                      (23 (iadd))
                                      (24 (imul))
                                      (25 (iconst_2))
                                      (26 (idiv))
                                      (27 (aload_0))
                                      (28 (getfield (fieldCP "left" "java.awt.Insets" int)))
                                      (31 (iadd))
                                      (32 (istore_3))
                                      (33 (iload_2))
                                      (34 (iload_2))
                                      (35 (iconst_1))
                                      (36 (iadd))
                                      (37 (imul))
                                      (38 (iconst_2))
                                      (39 (idiv))
                                      (40 (aload_0))
                                      (41 (getfield (fieldCP "top" "java.awt.Insets" int)))
                                      (44 (iadd))
                                      (45 (istore 4))
                                      (47 (iload_3))
                                      (48 (iload 4))
                                      (50 (iadd))
                                      (51 (istore 5))
                                      (53 (iload 5))
                                      (55 (iload 5))
                                      (57 (iconst_1))
                                      (58 (iadd))
                                      (59 (imul))
                                      (60 (iconst_2))
                                      (61 (idiv))
                                      (62 (iload 4))
                                      (64 (iadd))
                                      (65 (ireturn))
                                      (endofcode 66))
                                   (Exceptions )
                                   (StackMap )))
                        (method "toString"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 74)
                                   (parsedcode
                                      (0 (new (class "java.lang.StringBuilder")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.lang.StringBuilder" () void)))
                                      (7 (aload_0))
                                      (8 (invokevirtual
					(methodCP "getClass" "java.lang.Object" () (class "java.lang.Class"))))
                                      (11 (invokevirtual
					(methodCP "getName" "java.lang.Class" () (class "java.lang.String"))))
                                      (14 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (17 (ldc 1))        ;;STRING:: "[top="
                                      (19 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (22 (aload_0))
                                      (23 (getfield (fieldCP "top" "java.awt.Insets" int)))
                                      (26 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" (int) (class "java.lang.StringBuilder"))))
                                      (29 (ldc 2))        ;;STRING:: ",left="
                                      (31 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (34 (aload_0))
                                      (35 (getfield (fieldCP "left" "java.awt.Insets" int)))
                                      (38 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" (int) (class "java.lang.StringBuilder"))))
                                      (41 (ldc 3))        ;;STRING:: ",bottom="
                                      (43 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (46 (aload_0))
                                      (47 (getfield (fieldCP "bottom" "java.awt.Insets" int)))
                                      (50 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" (int) (class "java.lang.StringBuilder"))))
                                      (53 (ldc 4))        ;;STRING:: ",right="
                                      (55 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (58 (aload_0))
                                      (59 (getfield (fieldCP "right" "java.awt.Insets" int)))
                                      (62 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" (int) (class "java.lang.StringBuilder"))))
                                      (65 (ldc 5))        ;;STRING:: "]"
                                      (67 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (70 (invokevirtual
					(methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String"))))
                                      (73 (areturn))
                                      (endofcode 74))
                                   (Exceptions )
                                   (StackMap )))
                        (method "clone"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 14)
                                   (parsedcode
                                      (0 (aload_0)) ;;at TAG_0
                                      (1 (invokespecial (methodCP "clone" "java.lang.Object" () (class "java.lang.Object")))) 
                                      (4 (areturn)) ;;at TAG_1
                                      (5 (astore_1)) ;;at TAG_2
                                      (6 (new (class "java.lang.InternalError"))) 
                                      (9 (dup)) 
                                      (10 (invokespecial (methodCP "<init>" "java.lang.InternalError" () void))) 
                                      (13 (athrow)) 
                                      (endofcode 14))
                                   (Exceptions 
                                     (handler 0 4  5 (class "java.lang.CloneNotSupportedException")))
                                   (StackMap )))
                        (method "initIDs"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *native*  *private*  *static* )
                              (code))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 0) (code_length . 13)
                                   (parsedcode
                                      (0 (invokestatic (methodCP "loadLibraries" "java.awt.Toolkit" () void))) 
                                      (3 (invokestatic (methodCP "isHeadless" "java.awt.GraphicsEnvironment" () boolean))) 
                                      (6 (ifne 12))  ;;to TAG_0
                                      (9 (invokestatic (methodCP "initIDs" "java.awt.Insets" () void))) 
                                      (12 (return)) ;;at TAG_0
                                      (endofcode 13))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.lang.Cloneable" "java.io.Serializable")
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *Insets-class-table*
  (make-static-class-decls 
   *java.awt.Insets*))

(defconst *package-name-map* 
  ("java.awt.Insets" . "java.awt"))

