; Collections$UnmodifiableMap$UnmodifiableEntrySet$UnmodifiableEntry-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:43 CDT 2014.
;

(defconst *java.util.Collections$UnmodifiableMap$UnmodifiableEntrySet$UnmodifiableEntry*
 (make-class-def
      '(class "java.util.Collections$UnmodifiableMap$UnmodifiableEntrySet$UnmodifiableEntry"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "e" (class "java.util.Map$Entry") (accessflags  *class*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.Map$Entry"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "e" "java.util.Collections$UnmodifiableMap$UnmodifiableEntrySet$UnmodifiableEntry" (class "java.util.Map$Entry"))))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getKey"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "e" "java.util.Collections$UnmodifiableMap$UnmodifiableEntrySet$UnmodifiableEntry" (class "java.util.Map$Entry"))))
                                      (4 (invokeinterface
					(methodCP "getKey" "java.util.Map$Entry" () (class "java.lang.Object")) 1))
                                      (9 (areturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getValue"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "e" "java.util.Collections$UnmodifiableMap$UnmodifiableEntrySet$UnmodifiableEntry" (class "java.util.Map$Entry"))))
                                      (4 (invokeinterface
					(methodCP "getValue" "java.util.Map$Entry" () (class "java.lang.Object")) 1))
                                      (9 (areturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "setValue"
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
                        (method "hashCode"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "e" "java.util.Collections$UnmodifiableMap$UnmodifiableEntrySet$UnmodifiableEntry" (class "java.util.Map$Entry"))))
                                      (4 (invokeinterface
					(methodCP "hashCode" "java.util.Map$Entry" () int) 1))
                                      (9 (ireturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "equals"
                              (parameters (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 62)
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
                                      (15 (getfield (fieldCP "e" "java.util.Collections$UnmodifiableMap$UnmodifiableEntrySet$UnmodifiableEntry" (class "java.util.Map$Entry")))) 
                                      (18 (invokeinterface (methodCP "getKey" "java.util.Map$Entry" () (class "java.lang.Object")) 1)) 
                                      (23 (aload_2)) 
                                      (24 (invokeinterface (methodCP "getKey" "java.util.Map$Entry" () (class "java.lang.Object")) 1)) 
                                      (29 (invokestatic (methodCP "eq" "java.util.Collections" ((class "java.lang.Object") (class "java.lang.Object")) boolean))) 
                                      (32 (ifeq 60)) ;;to TAG_1
                                      (35 (aload_0)) 
                                      (36 (getfield (fieldCP "e" "java.util.Collections$UnmodifiableMap$UnmodifiableEntrySet$UnmodifiableEntry" (class "java.util.Map$Entry")))) 
                                      (39 (invokeinterface (methodCP "getValue" "java.util.Map$Entry" () (class "java.lang.Object")) 1)) 
                                      (44 (aload_2)) 
                                      (45 (invokeinterface (methodCP "getValue" "java.util.Map$Entry" () (class "java.lang.Object")) 1)) 
                                      (50 (invokestatic (methodCP "eq" "java.util.Collections" ((class "java.lang.Object") (class "java.lang.Object")) boolean))) 
                                      (53 (ifeq 60)) ;;to TAG_1
                                      (56 (iconst_1)) 
                                      (57 (goto 61))  ;;to TAG_2
                                      (60 (iconst_0)) ;;at TAG_1
                                      (61 (ireturn)) ;;at TAG_2
                                      (endofcode 62))
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
                                      (1 (getfield (fieldCP "e" "java.util.Collections$UnmodifiableMap$UnmodifiableEntrySet$UnmodifiableEntry" (class "java.util.Map$Entry"))))
                                      (4 (invokevirtual
					(methodCP "toString" "java.lang.Object" () (class "java.lang.String"))))
                                      (7 (areturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.util.Map$Entry")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *Collections$UnmodifiableMap$UnmodifiableEntrySet$UnmodifiableEntry-class-table*
  (make-static-class-decls 
   *java.util.Collections$UnmodifiableMap$UnmodifiableEntrySet$UnmodifiableEntry*))

(defconst *package-name-map* 
  ("java.util.Collections$UnmodifiableMap$UnmodifiableEntrySet$UnmodifiableEntry" . "java.util"))

