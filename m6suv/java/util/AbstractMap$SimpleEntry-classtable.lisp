; AbstractMap$SimpleEntry-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:42 CDT 2014.
;

(defconst *java.util.AbstractMap$SimpleEntry*
 (make-class-def
      '(class "java.util.AbstractMap$SimpleEntry"
            "java.lang.Object"
            (constant_pool
                        (LONG -8499721149061103585)
                        (STRING  "="))
            (fields
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0)
                        (field "key" (class "java.lang.Object") (accessflags  *class*  *final*  *private* ) -1)
                        (field "value" (class "java.lang.Object") (accessflags  *class*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "key" "java.util.AbstractMap$SimpleEntry" (class "java.lang.Object"))))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "value" "java.util.AbstractMap$SimpleEntry" (class "java.lang.Object"))))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.util.Map$Entry"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 25)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (invokeinterface
					(methodCP "getKey" "java.util.Map$Entry" () (class "java.lang.Object")) 1))
                                      (11 (putfield (fieldCP "key" "java.util.AbstractMap$SimpleEntry" (class "java.lang.Object"))))
                                      (14 (aload_0))
                                      (15 (aload_1))
                                      (16 (invokeinterface
					(methodCP "getValue" "java.util.Map$Entry" () (class "java.lang.Object")) 1))
                                      (21 (putfield (fieldCP "value" "java.util.AbstractMap$SimpleEntry" (class "java.lang.Object"))))
                                      (24 (return))
                                      (endofcode 25))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getKey"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "key" "java.util.AbstractMap$SimpleEntry" (class "java.lang.Object"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getValue"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "value" "java.util.AbstractMap$SimpleEntry" (class "java.lang.Object"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "setValue"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 12)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "value" "java.util.AbstractMap$SimpleEntry" (class "java.lang.Object"))))
                                      (4 (astore_2))
                                      (5 (aload_0))
                                      (6 (aload_1))
                                      (7 (putfield (fieldCP "value" "java.util.AbstractMap$SimpleEntry" (class "java.lang.Object"))))
                                      (10 (aload_2))
                                      (11 (areturn))
                                      (endofcode 12))
                                   (Exceptions )
                                   (StackMap )))
                        (method "equals"
                              (parameters (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 52)
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
                                      (15 (getfield (fieldCP "key" "java.util.AbstractMap$SimpleEntry" (class "java.lang.Object")))) 
                                      (18 (aload_2)) 
                                      (19 (invokeinterface (methodCP "getKey" "java.util.Map$Entry" () (class "java.lang.Object")) 1)) 
                                      (24 (invokestatic (methodCP "access$000" "java.util.AbstractMap" ((class "java.lang.Object") (class "java.lang.Object")) boolean))) 
                                      (27 (ifeq 50)) ;;to TAG_1
                                      (30 (aload_0)) 
                                      (31 (getfield (fieldCP "value" "java.util.AbstractMap$SimpleEntry" (class "java.lang.Object")))) 
                                      (34 (aload_2)) 
                                      (35 (invokeinterface (methodCP "getValue" "java.util.Map$Entry" () (class "java.lang.Object")) 1)) 
                                      (40 (invokestatic (methodCP "access$000" "java.util.AbstractMap" ((class "java.lang.Object") (class "java.lang.Object")) boolean))) 
                                      (43 (ifeq 50)) ;;to TAG_1
                                      (46 (iconst_1)) 
                                      (47 (goto 51))  ;;to TAG_2
                                      (50 (iconst_0)) ;;at TAG_1
                                      (51 (ireturn)) ;;at TAG_2
                                      (endofcode 52))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hashCode"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 38)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "key" "java.util.AbstractMap$SimpleEntry" (class "java.lang.Object")))) 
                                      (4 (ifnonnull 11)) ;;to TAG_0
                                      (7 (iconst_0)) 
                                      (8 (goto 18)) ;;to TAG_1
                                      (11 (aload_0)) ;;at TAG_0
                                      (12 (getfield (fieldCP "key" "java.util.AbstractMap$SimpleEntry" (class "java.lang.Object")))) 
                                      (15 (invokevirtual (methodCP "hashCode" "java.lang.Object" () int))) 
                                      (18 (aload_0)) ;;at TAG_1
                                      (19 (getfield (fieldCP "value" "java.util.AbstractMap$SimpleEntry" (class "java.lang.Object")))) 
                                      (22 (ifnonnull 29))  ;;to TAG_2
                                      (25 (iconst_0)) 
                                      (26 (goto 36)) ;;to TAG_3
                                      (29 (aload_0)) ;;at TAG_2
                                      (30 (getfield (fieldCP "value" "java.util.AbstractMap$SimpleEntry" (class "java.lang.Object")))) 
                                      (33 (invokevirtual (methodCP "hashCode" "java.lang.Object" () int))) 
                                      (36 (ixor)) ;;at TAG_3
                                      (37 (ireturn)) 
                                      (endofcode 38))
                                   (Exceptions )
                                   (StackMap )))
                        (method "toString"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 30)
                                   (parsedcode
                                      (0 (new (class "java.lang.StringBuilder")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.lang.StringBuilder" () void)))
                                      (7 (aload_0))
                                      (8 (getfield (fieldCP "key" "java.util.AbstractMap$SimpleEntry" (class "java.lang.Object"))))
                                      (11 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.Object")) (class "java.lang.StringBuilder"))))
                                      (14 (ldc 1))        ;;STRING:: "="
                                      (16 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (19 (aload_0))
                                      (20 (getfield (fieldCP "value" "java.util.AbstractMap$SimpleEntry" (class "java.lang.Object"))))
                                      (23 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.Object")) (class "java.lang.StringBuilder"))))
                                      (26 (invokevirtual
					(methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String"))))
                                      (29 (areturn))
                                      (endofcode 30))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.util.Map$Entry" "java.io.Serializable")
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *AbstractMap$SimpleEntry-class-table*
  (make-static-class-decls 
   *java.util.AbstractMap$SimpleEntry*))

(defconst *package-name-map* 
  ("java.util.AbstractMap$SimpleEntry" . "java.util"))

