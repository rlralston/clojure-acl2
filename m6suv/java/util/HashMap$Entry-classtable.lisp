; HashMap$Entry-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:45 CDT 2014.
;

(defconst *java.util.HashMap$Entry*
 (make-class-def
      '(class "java.util.HashMap$Entry"
            "java.lang.Object"
            (constant_pool
                        (STRING  "="))
            (fields
                        (field "key" (class "java.lang.Object") (accessflags  *class*  *final* ) -1)
                        (field "value" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "next" (class "java.util.HashMap$Entry") (accessflags  *class* ) -1)
                        (field "hash" int (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters int (class "java.lang.Object") (class "java.lang.Object") (class "java.util.HashMap$Entry"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 5) (code_length . 26)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (aload_3))
                                      (6 (putfield (fieldCP "value" "java.util.HashMap$Entry" (class "java.lang.Object"))))
                                      (9 (aload_0))
                                      (10 (aload 4))
                                      (12 (putfield (fieldCP "next" "java.util.HashMap$Entry" (class "java.util.HashMap$Entry"))))
                                      (15 (aload_0))
                                      (16 (aload_2))
                                      (17 (putfield (fieldCP "key" "java.util.HashMap$Entry" (class "java.lang.Object"))))
                                      (20 (aload_0))
                                      (21 (iload_1))
                                      (22 (putfield (fieldCP "hash" "java.util.HashMap$Entry" int)))
                                      (25 (return))
                                      (endofcode 26))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getKey"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "key" "java.util.HashMap$Entry" (class "java.lang.Object"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getValue"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "value" "java.util.HashMap$Entry" (class "java.lang.Object"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "setValue"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 12)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "value" "java.util.HashMap$Entry" (class "java.lang.Object"))))
                                      (4 (astore_2))
                                      (5 (aload_0))
                                      (6 (aload_1))
                                      (7 (putfield (fieldCP "value" "java.util.HashMap$Entry" (class "java.lang.Object"))))
                                      (10 (aload_2))
                                      (11 (areturn))
                                      (endofcode 12))
                                   (Exceptions )
                                   (StackMap )))
                        (method "equals"
                              (parameters (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 7) (code_length . 86)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (instanceof (class "java.util.Map$Entry"))) 
                                      (4 (ifne 9)) ;;to TAG_0
                                      (7 (iconst_0)) 
                                      (8 (ireturn)) 
                                      (9 (aload_1)) ;;at TAG_0
                                      (10 (checkcast (class "java.util.Map$Entry"))) 
                                      (13 (astore_2)) 
                                      (14 (aload_0)) 
                                      (15 (invokevirtual (methodCP "getKey" "java.util.HashMap$Entry" () (class "java.lang.Object")))) 
                                      (18 (astore_3)) 
                                      (19 (aload_2)) 
                                      (20 (invokeinterface (methodCP "getKey" "java.util.Map$Entry" () (class "java.lang.Object")) 1)) 
                                      (25 (astore 4)) 
                                      (27 (aload_3)) 
                                      (28 (aload 4)) 
                                      (30 (if_acmpeq 46)) ;;to TAG_1
                                      (33 (aload_3)) 
                                      (34 (ifnull 84))  ;;to TAG_2
                                      (37 (aload_3)) 
                                      (38 (aload 4)) 
                                      (40 (invokevirtual (methodCP "equals" "java.lang.Object" ((class "java.lang.Object")) boolean))) 
                                      (43 (ifeq 84))  ;;to TAG_2
                                      (46 (aload_0)) ;;at TAG_1
                                      (47 (invokevirtual (methodCP "getValue" "java.util.HashMap$Entry" () (class "java.lang.Object")))) 
                                      (50 (astore 5)) 
                                      (52 (aload_2)) 
                                      (53 (invokeinterface (methodCP "getValue" "java.util.Map$Entry" () (class "java.lang.Object")) 1)) 
                                      (58 (astore 6)) 
                                      (60 (aload 5)) 
                                      (62 (aload 6)) 
                                      (64 (if_acmpeq 82)) ;;to TAG_3
                                      (67 (aload 5)) 
                                      (69 (ifnull 84))  ;;to TAG_2
                                      (72 (aload 5)) 
                                      (74 (aload 6)) 
                                      (76 (invokevirtual (methodCP "equals" "java.lang.Object" ((class "java.lang.Object")) boolean))) 
                                      (79 (ifeq 84))  ;;to TAG_2
                                      (82 (iconst_1)) ;;at TAG_3
                                      (83 (ireturn)) 
                                      (84 (iconst_0)) ;;at TAG_2
                                      (85 (ireturn)) 
                                      (endofcode 86))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hashCode"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 38)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "key" "java.util.HashMap$Entry" (class "java.lang.Object")))) 
                                      (4 (ifnonnull 11)) ;;to TAG_0
                                      (7 (iconst_0)) 
                                      (8 (goto 18)) ;;to TAG_1
                                      (11 (aload_0)) ;;at TAG_0
                                      (12 (getfield (fieldCP "key" "java.util.HashMap$Entry" (class "java.lang.Object")))) 
                                      (15 (invokevirtual (methodCP "hashCode" "java.lang.Object" () int))) 
                                      (18 (aload_0)) ;;at TAG_1
                                      (19 (getfield (fieldCP "value" "java.util.HashMap$Entry" (class "java.lang.Object")))) 
                                      (22 (ifnonnull 29))  ;;to TAG_2
                                      (25 (iconst_0)) 
                                      (26 (goto 36)) ;;to TAG_3
                                      (29 (aload_0)) ;;at TAG_2
                                      (30 (getfield (fieldCP "value" "java.util.HashMap$Entry" (class "java.lang.Object")))) 
                                      (33 (invokevirtual (methodCP "hashCode" "java.lang.Object" () int))) 
                                      (36 (ixor)) ;;at TAG_3
                                      (37 (ireturn)) 
                                      (endofcode 38))
                                   (Exceptions )
                                   (StackMap )))
                        (method "toString"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 30)
                                   (parsedcode
                                      (0 (new (class "java.lang.StringBuilder")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.lang.StringBuilder" () void)))
                                      (7 (aload_0))
                                      (8 (invokevirtual
					(methodCP "getKey" "java.util.HashMap$Entry" () (class "java.lang.Object"))))
                                      (11 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.Object")) (class "java.lang.StringBuilder"))))
                                      (14 (ldc 0))        ;;STRING:: "="
                                      (16 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (19 (aload_0))
                                      (20 (invokevirtual
					(methodCP "getValue" "java.util.HashMap$Entry" () (class "java.lang.Object"))))
                                      (23 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.Object")) (class "java.lang.StringBuilder"))))
                                      (26 (invokevirtual
					(methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String"))))
                                      (29 (areturn))
                                      (endofcode 30))
                                   (Exceptions )
                                   (StackMap )))
                        (method "recordAccess"
                              (parameters (class "java.util.HashMap"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 0) (max_locals . 2) (code_length . 1)
                                   (parsedcode
                                      (0 (return))
                                      (endofcode 1))
                                   (Exceptions )
                                   (StackMap )))
                        (method "recordRemoval"
                              (parameters (class "java.util.HashMap"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 0) (max_locals . 2) (code_length . 1)
                                   (parsedcode
                                      (0 (return))
                                      (endofcode 1))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.util.Map$Entry")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *HashMap$Entry-class-table*
  (make-static-class-decls 
   *java.util.HashMap$Entry*))

(defconst *package-name-map* 
  ("java.util.HashMap$Entry" . "java.util"))

