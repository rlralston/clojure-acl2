; EnumMap$KeySet-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:45 CDT 2014.
;

(defconst *java.util.EnumMap$KeySet*
 (make-class-def
      '(class "java.util.EnumMap$KeySet"
            "java.util.AbstractSet"
            (constant_pool)
            (fields
                        (field "this$0" (class "java.util.EnumMap") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.EnumMap"))
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.util.EnumMap$KeySet" (class "java.util.EnumMap"))))
                                      (5 (aload_0))
                                      (6 (invokespecial
					(methodCP "<init>" "java.util.AbstractSet" () void)))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "iterator"
                              (parameters )
                              (returntype . (class "java.util.Iterator"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 1) (code_length . 13)
                                   (parsedcode
                                      (0 (new (class "java.util.EnumMap$KeyIterator")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "this$0" "java.util.EnumMap$KeySet" (class "java.util.EnumMap"))))
                                      (8 (aconst_null))
                                      (9 (invokespecial
					(methodCP "<init>" "java.util.EnumMap$KeyIterator" ((class "java.util.EnumMap") (class "java.util.EnumMap$1")) void)))
                                      (12 (areturn))
                                      (endofcode 13))
                                   (Exceptions )
                                   (StackMap )))
                        (method "size"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "this$0" "java.util.EnumMap$KeySet" (class "java.util.EnumMap"))))
                                      (4 (invokestatic
					(methodCP "access$200" "java.util.EnumMap" ((class "java.util.EnumMap")) int)))
                                      (7 (ireturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "contains"
                              (parameters (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "this$0" "java.util.EnumMap$KeySet" (class "java.util.EnumMap"))))
                                      (4 (aload_1))
                                      (5 (invokevirtual
					(methodCP "containsKey" "java.util.EnumMap" ((class "java.lang.Object")) boolean)))
                                      (8 (ireturn))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "remove"
                              (parameters (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 34)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "this$0" "java.util.EnumMap$KeySet" (class "java.util.EnumMap")))) 
                                      (4 (invokestatic (methodCP "access$200" "java.util.EnumMap" ((class "java.util.EnumMap")) int))) 
                                      (7 (istore_2)) 
                                      (8 (aload_0)) 
                                      (9 (getfield (fieldCP "this$0" "java.util.EnumMap$KeySet" (class "java.util.EnumMap")))) 
                                      (12 (aload_1)) 
                                      (13 (invokevirtual (methodCP "remove" "java.util.EnumMap" ((class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (16 (pop)) 
                                      (17 (aload_0)) 
                                      (18 (getfield (fieldCP "this$0" "java.util.EnumMap$KeySet" (class "java.util.EnumMap")))) 
                                      (21 (invokestatic (methodCP "access$200" "java.util.EnumMap" ((class "java.util.EnumMap")) int))) 
                                      (24 (iload_2)) 
                                      (25 (if_icmpeq 32))  ;;to TAG_0
                                      (28 (iconst_1)) 
                                      (29 (goto 33)) ;;to TAG_1
                                      (32 (iconst_0)) ;;at TAG_0
                                      (33 (ireturn)) ;;at TAG_1
                                      (endofcode 34))
                                   (Exceptions )
                                   (StackMap )))
                        (method "clear"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "this$0" "java.util.EnumMap$KeySet" (class "java.util.EnumMap"))))
                                      (4 (invokevirtual
					(methodCP "clear" "java.util.EnumMap" () void)))
                                      (7 (return))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.util.EnumMap") (class "java.util.EnumMap$1"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokespecial
					(methodCP "<init>" "java.util.EnumMap$KeySet" ((class "java.util.EnumMap")) void)))
                                      (5 (return))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *EnumMap$KeySet-class-table*
  (make-static-class-decls 
   *java.util.EnumMap$KeySet*))

(defconst *package-name-map* 
  ("java.util.EnumMap$KeySet" . "java.util"))

