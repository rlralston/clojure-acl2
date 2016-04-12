; Stack-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:48 CDT 2014.
;

(defconst *java.util.Stack*
 (make-class-def
      '(class "java.util.Stack"
            "java.util.Vector"
            (constant_pool
                        (LONG 1224463164541339165))
            (fields
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0))
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.util.Vector" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "push"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 7)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokevirtual
					(methodCP "addElement" "java.util.Stack" ((class "java.lang.Object")) void)))
                                      (5 (aload_1))
                                      (6 (areturn))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap )))
                        (method "pop"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public*  *super*  *synchronized* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 19)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokevirtual
					(methodCP "size" "java.util.Stack" () int)))
                                      (4 (istore_2))
                                      (5 (aload_0))
                                      (6 (invokevirtual
					(methodCP "peek" "java.util.Stack" () (class "java.lang.Object"))))
                                      (9 (astore_1))
                                      (10 (aload_0))
                                      (11 (iload_2))
                                      (12 (iconst_1))
                                      (13 (isub))
                                      (14 (invokevirtual
					(methodCP "removeElementAt" "java.util.Stack" (int) void)))
                                      (17 (aload_1))
                                      (18 (areturn))
                                      (endofcode 19))
                                   (Exceptions )
                                   (StackMap )))
                        (method "peek"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public*  *super*  *synchronized* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 25)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "size" "java.util.Stack" () int))) 
                                      (4 (istore_1)) 
                                      (5 (iload_1)) 
                                      (6 (ifne 17))  ;;to TAG_0
                                      (9 (new (class "java.util.EmptyStackException"))) 
                                      (12 (dup)) 
                                      (13 (invokespecial (methodCP "<init>" "java.util.EmptyStackException" () void))) 
                                      (16 (athrow)) 
                                      (17 (aload_0)) ;;at TAG_0
                                      (18 (iload_1)) 
                                      (19 (iconst_1)) 
                                      (20 (isub)) 
                                      (21 (invokevirtual (methodCP "elementAt" "java.util.Stack" (int) (class "java.lang.Object")))) 
                                      (24 (areturn)) 
                                      (endofcode 25))
                                   (Exceptions )
                                   (StackMap )))
                        (method "empty"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 13)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "size" "java.util.Stack" () int))) 
                                      (4 (ifne 11))  ;;to TAG_0
                                      (7 (iconst_1)) 
                                      (8 (goto 12)) ;;to TAG_1
                                      (11 (iconst_0)) ;;at TAG_0
                                      (12 (ireturn)) ;;at TAG_1
                                      (endofcode 13))
                                   (Exceptions )
                                   (StackMap )))
                        (method "search"
                              (parameters (class "java.lang.Object"))
                              (returntype . int)
                              (accessflags  *class*  *public*  *super*  *synchronized* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 19)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (aload_1)) 
                                      (2 (invokevirtual (methodCP "lastIndexOf" "java.util.Stack" ((class "java.lang.Object")) int))) 
                                      (5 (istore_2)) 
                                      (6 (iload_2)) 
                                      (7 (iflt 17))  ;;to TAG_0
                                      (10 (aload_0)) 
                                      (11 (invokevirtual (methodCP "size" "java.util.Stack" () int))) 
                                      (14 (iload_2)) 
                                      (15 (isub)) 
                                      (16 (ireturn)) 
                                      (17 (iconst_m1)) ;;at TAG_0
                                      (18 (ireturn)) 
                                      (endofcode 19))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")))))


(defconst *Stack-class-table*
  (make-static-class-decls 
   *java.util.Stack*))

(defconst *package-name-map* 
  ("java.util.Stack" . "java.util"))

