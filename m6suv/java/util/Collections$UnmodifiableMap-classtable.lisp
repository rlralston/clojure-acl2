; Collections$UnmodifiableMap-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:43 CDT 2014.
;

(defconst *java.util.Collections$UnmodifiableMap*
 (make-class-def
      '(class "java.util.Collections$UnmodifiableMap"
            "java.lang.Object"
            (constant_pool
                        (LONG -1034234728574286014))
            (fields
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0)
                        (field "m" (class "java.util.Map") (accessflags  *class*  *final*  *private* ) -1)
                        (field "keySet" (class "java.util.Set") (accessflags  *class*  *private*  *transient* ) -1)
                        (field "entrySet" (class "java.util.Set") (accessflags  *class*  *private*  *transient* ) -1)
                        (field "values" (class "java.util.Collection") (accessflags  *class*  *private*  *transient* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.Map"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 37)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokespecial (methodCP "<init>" "java.lang.Object" () void))) 
                                      (4 (aload_0)) 
                                      (5 (aconst_null)) 
                                      (6 (putfield (fieldCP "keySet" "java.util.Collections$UnmodifiableMap" (class "java.util.Set")))) 
                                      (9 (aload_0)) 
                                      (10 (aconst_null)) 
                                      (11 (putfield (fieldCP "entrySet" "java.util.Collections$UnmodifiableMap" (class "java.util.Set")))) 
                                      (14 (aload_0)) 
                                      (15 (aconst_null)) 
                                      (16 (putfield (fieldCP "values" "java.util.Collections$UnmodifiableMap" (class "java.util.Collection")))) 
                                      (19 (aload_1)) 
                                      (20 (ifnonnull 31))  ;;to TAG_0
                                      (23 (new (class "java.lang.NullPointerException"))) 
                                      (26 (dup)) 
                                      (27 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" () void))) 
                                      (30 (athrow)) 
                                      (31 (aload_0)) ;;at TAG_0
                                      (32 (aload_1)) 
                                      (33 (putfield (fieldCP "m" "java.util.Collections$UnmodifiableMap" (class "java.util.Map")))) 
                                      (36 (return)) 
                                      (endofcode 37))
                                   (Exceptions )
                                   (StackMap )))
                        (method "size"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "m" "java.util.Collections$UnmodifiableMap" (class "java.util.Map"))))
                                      (4 (invokeinterface
					(methodCP "size" "java.util.Map" () int) 1))
                                      (9 (ireturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isEmpty"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "m" "java.util.Collections$UnmodifiableMap" (class "java.util.Map"))))
                                      (4 (invokeinterface
					(methodCP "isEmpty" "java.util.Map" () boolean) 1))
                                      (9 (ireturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "containsKey"
                              (parameters (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "m" "java.util.Collections$UnmodifiableMap" (class "java.util.Map"))))
                                      (4 (aload_1))
                                      (5 (invokeinterface
					(methodCP "containsKey" "java.util.Map" ((class "java.lang.Object")) boolean) 2))
                                      (10 (ireturn))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "containsValue"
                              (parameters (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "m" "java.util.Collections$UnmodifiableMap" (class "java.util.Map"))))
                                      (4 (aload_1))
                                      (5 (invokeinterface
					(methodCP "containsValue" "java.util.Map" ((class "java.lang.Object")) boolean) 2))
                                      (10 (ireturn))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "get"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "m" "java.util.Collections$UnmodifiableMap" (class "java.util.Map"))))
                                      (4 (aload_1))
                                      (5 (invokeinterface
					(methodCP "get" "java.util.Map" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (10 (areturn))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "put"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 8)
                                   (parsedcode
                                      (0 (new (class "java.lang.UnsupportedOperationException")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.lang.UnsupportedOperationException" () void)))
                                      (7 (athrow))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "remove"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 8)
                                   (parsedcode
                                      (0 (new (class "java.lang.UnsupportedOperationException")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.lang.UnsupportedOperationException" () void)))
                                      (7 (athrow))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "putAll"
                              (parameters (class "java.util.Map"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 8)
                                   (parsedcode
                                      (0 (new (class "java.lang.UnsupportedOperationException")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.lang.UnsupportedOperationException" () void)))
                                      (7 (athrow))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "clear"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (new (class "java.lang.UnsupportedOperationException")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.lang.UnsupportedOperationException" () void)))
                                      (7 (athrow))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "keySet"
                              (parameters )
                              (returntype . (class "java.util.Set"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 28)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "keySet" "java.util.Collections$UnmodifiableMap" (class "java.util.Set")))) 
                                      (4 (ifnonnull 23))  ;;to TAG_0
                                      (7 (aload_0)) 
                                      (8 (aload_0)) 
                                      (9 (getfield (fieldCP "m" "java.util.Collections$UnmodifiableMap" (class "java.util.Map")))) 
                                      (12 (invokeinterface (methodCP "keySet" "java.util.Map" () (class "java.util.Set")) 1)) 
                                      (17 (invokestatic (methodCP "unmodifiableSet" "java.util.Collections" ((class "java.util.Set")) (class "java.util.Set")))) 
                                      (20 (putfield (fieldCP "keySet" "java.util.Collections$UnmodifiableMap" (class "java.util.Set")))) 
                                      (23 (aload_0)) ;;at TAG_0
                                      (24 (getfield (fieldCP "keySet" "java.util.Collections$UnmodifiableMap" (class "java.util.Set")))) 
                                      (27 (areturn)) 
                                      (endofcode 28))
                                   (Exceptions )
                                   (StackMap )))
                        (method "entrySet"
                              (parameters )
                              (returntype . (class "java.util.Set"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 1) (code_length . 32)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "entrySet" "java.util.Collections$UnmodifiableMap" (class "java.util.Set")))) 
                                      (4 (ifnonnull 27))  ;;to TAG_0
                                      (7 (aload_0)) 
                                      (8 (new (class "java.util.Collections$UnmodifiableMap$UnmodifiableEntrySet"))) 
                                      (11 (dup)) 
                                      (12 (aload_0)) 
                                      (13 (getfield (fieldCP "m" "java.util.Collections$UnmodifiableMap" (class "java.util.Map")))) 
                                      (16 (invokeinterface (methodCP "entrySet" "java.util.Map" () (class "java.util.Set")) 1)) 
                                      (21 (invokespecial (methodCP "<init>" "java.util.Collections$UnmodifiableMap$UnmodifiableEntrySet" ((class "java.util.Set")) void))) 
                                      (24 (putfield (fieldCP "entrySet" "java.util.Collections$UnmodifiableMap" (class "java.util.Set")))) 
                                      (27 (aload_0)) ;;at TAG_0
                                      (28 (getfield (fieldCP "entrySet" "java.util.Collections$UnmodifiableMap" (class "java.util.Set")))) 
                                      (31 (areturn)) 
                                      (endofcode 32))
                                   (Exceptions )
                                   (StackMap )))
                        (method "values"
                              (parameters )
                              (returntype . (class "java.util.Collection"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 28)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "values" "java.util.Collections$UnmodifiableMap" (class "java.util.Collection")))) 
                                      (4 (ifnonnull 23))  ;;to TAG_0
                                      (7 (aload_0)) 
                                      (8 (aload_0)) 
                                      (9 (getfield (fieldCP "m" "java.util.Collections$UnmodifiableMap" (class "java.util.Map")))) 
                                      (12 (invokeinterface (methodCP "values" "java.util.Map" () (class "java.util.Collection")) 1)) 
                                      (17 (invokestatic (methodCP "unmodifiableCollection" "java.util.Collections" ((class "java.util.Collection")) (class "java.util.Collection")))) 
                                      (20 (putfield (fieldCP "values" "java.util.Collections$UnmodifiableMap" (class "java.util.Collection")))) 
                                      (23 (aload_0)) ;;at TAG_0
                                      (24 (getfield (fieldCP "values" "java.util.Collections$UnmodifiableMap" (class "java.util.Collection")))) 
                                      (27 (areturn)) 
                                      (endofcode 28))
                                   (Exceptions )
                                   (StackMap )))
                        (method "equals"
                              (parameters (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 24)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (aload_0)) 
                                      (2 (if_acmpeq 18)) ;;to TAG_0
                                      (5 (aload_0)) 
                                      (6 (getfield (fieldCP "m" "java.util.Collections$UnmodifiableMap" (class "java.util.Map")))) 
                                      (9 (aload_1)) 
                                      (10 (invokeinterface (methodCP "equals" "java.util.Map" ((class "java.lang.Object")) boolean) 2)) 
                                      (15 (ifeq 22)) ;;to TAG_1
                                      (18 (iconst_1)) ;;at TAG_0
                                      (19 (goto 23))  ;;to TAG_2
                                      (22 (iconst_0)) ;;at TAG_1
                                      (23 (ireturn)) ;;at TAG_2
                                      (endofcode 24))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hashCode"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "m" "java.util.Collections$UnmodifiableMap" (class "java.util.Map"))))
                                      (4 (invokeinterface
					(methodCP "hashCode" "java.util.Map" () int) 1))
                                      (9 (ireturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "toString"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "m" "java.util.Collections$UnmodifiableMap" (class "java.util.Map"))))
                                      (4 (invokevirtual
					(methodCP "toString" "java.lang.Object" () (class "java.lang.String"))))
                                      (7 (areturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.util.Map" "java.io.Serializable")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *Collections$UnmodifiableMap-class-table*
  (make-static-class-decls 
   *java.util.Collections$UnmodifiableMap*))

(defconst *package-name-map* 
  ("java.util.Collections$UnmodifiableMap" . "java.util"))

