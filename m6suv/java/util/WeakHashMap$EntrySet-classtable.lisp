; WeakHashMap$EntrySet-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:48 CDT 2014.
;

(defconst *java.util.WeakHashMap$EntrySet*
 (make-class-def
      '(class "java.util.WeakHashMap$EntrySet"
            "java.util.AbstractSet"
            (constant_pool)
            (fields
                        (field "this$0" (class "java.util.WeakHashMap") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.WeakHashMap"))
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.util.WeakHashMap$EntrySet" (class "java.util.WeakHashMap"))))
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
                                      (0 (new (class "java.util.WeakHashMap$EntryIterator")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "this$0" "java.util.WeakHashMap$EntrySet" (class "java.util.WeakHashMap"))))
                                      (8 (aconst_null))
                                      (9 (invokespecial
					(methodCP "<init>" "java.util.WeakHashMap$EntryIterator" ((class "java.util.WeakHashMap") (class "java.util.WeakHashMap$1")) void)))
                                      (12 (areturn))
                                      (endofcode 13))
                                   (Exceptions )
                                   (StackMap )))
                        (method "contains"
                              (parameters (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 46)
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
                                      (15 (getfield (fieldCP "this$0" "java.util.WeakHashMap$EntrySet" (class "java.util.WeakHashMap")))) 
                                      (18 (aload_2)) 
                                      (19 (invokeinterface (methodCP "getKey" "java.util.Map$Entry" () (class "java.lang.Object")) 1)) 
                                      (24 (invokevirtual (methodCP "getEntry" "java.util.WeakHashMap" ((class "java.lang.Object")) (class "java.util.WeakHashMap$Entry")))) 
                                      (27 (astore_3)) 
                                      (28 (aload_3)) 
                                      (29 (ifnull 44)) ;;to TAG_1
                                      (32 (aload_3)) 
                                      (33 (aload_2)) 
                                      (34 (invokevirtual (methodCP "equals" "java.util.WeakHashMap$Entry" ((class "java.lang.Object")) boolean))) 
                                      (37 (ifeq 44)) ;;to TAG_1
                                      (40 (iconst_1)) 
                                      (41 (goto 45))  ;;to TAG_2
                                      (44 (iconst_0)) ;;at TAG_1
                                      (45 (ireturn)) ;;at TAG_2
                                      (endofcode 46))
                                   (Exceptions )
                                   (StackMap )))
                        (method "remove"
                              (parameters (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "this$0" "java.util.WeakHashMap$EntrySet" (class "java.util.WeakHashMap"))))
                                      (4 (aload_1))
                                      (5 (invokevirtual
					(methodCP "removeMapping" "java.util.WeakHashMap" ((class "java.lang.Object")) boolean)))
                                      (8 (ireturn))
                                      (endofcode 9))
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
                                      (1 (getfield (fieldCP "this$0" "java.util.WeakHashMap$EntrySet" (class "java.util.WeakHashMap"))))
                                      (4 (invokevirtual
					(methodCP "size" "java.util.WeakHashMap" () int)))
                                      (7 (ireturn))
                                      (endofcode 8))
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
                                      (1 (getfield (fieldCP "this$0" "java.util.WeakHashMap$EntrySet" (class "java.util.WeakHashMap"))))
                                      (4 (invokevirtual
					(methodCP "clear" "java.util.WeakHashMap" () void)))
                                      (7 (return))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "deepCopy"
                              (parameters )
                              (returntype . (class "java.util.List"))
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 4) (max_locals . 4) (code_length . 56)
                                   (parsedcode
                                      (0 (new (class "java.util.ArrayList"))) 
                                      (3 (dup)) 
                                      (4 (aload_0)) 
                                      (5 (invokevirtual (methodCP "size" "java.util.WeakHashMap$EntrySet" () int))) 
                                      (8 (invokespecial (methodCP "<init>" "java.util.ArrayList" (int) void))) 
                                      (11 (astore_1)) 
                                      (12 (aload_0)) 
                                      (13 (invokevirtual (methodCP "iterator" "java.util.WeakHashMap$EntrySet" () (class "java.util.Iterator")))) 
                                      (16 (astore_2)) 
                                      (17 (aload_2)) ;;at TAG_1
                                      (18 (invokeinterface (methodCP "hasNext" "java.util.Iterator" () boolean) 1)) 
                                      (23 (ifeq 54))  ;;to TAG_0
                                      (26 (aload_2)) 
                                      (27 (invokeinterface (methodCP "next" "java.util.Iterator" () (class "java.lang.Object")) 1)) 
                                      (32 (checkcast (class "java.util.Map$Entry"))) 
                                      (35 (astore_3)) 
                                      (36 (aload_1)) 
                                      (37 (new (class "java.util.AbstractMap$SimpleEntry"))) 
                                      (40 (dup)) 
                                      (41 (aload_3)) 
                                      (42 (invokespecial (methodCP "<init>" "java.util.AbstractMap$SimpleEntry" ((class "java.util.Map$Entry")) void))) 
                                      (45 (invokeinterface (methodCP "add" "java.util.List" ((class "java.lang.Object")) boolean) 2)) 
                                      (50 (pop)) 
                                      (51 (goto 17)) ;;to TAG_1
                                      (54 (aload_1)) ;;at TAG_0
                                      (55 (areturn)) 
                                      (endofcode 56))
                                   (Exceptions )
                                   (StackMap )))
                        (method "toArray"
                              (parameters )
                              (returntype . (array (class "java.lang.Object")))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "deepCopy" "java.util.WeakHashMap$EntrySet" () (class "java.util.List"))))
                                      (4 (invokeinterface
					(methodCP "toArray" "java.util.List" () (array (class "java.lang.Object"))) 1))
                                      (9 (areturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "toArray"
                              (parameters (array (class "java.lang.Object")))
                              (returntype . (array (class "java.lang.Object")))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "deepCopy" "java.util.WeakHashMap$EntrySet" () (class "java.util.List"))))
                                      (4 (aload_1))
                                      (5 (invokeinterface
					(methodCP "toArray" "java.util.List" ((array (class "java.lang.Object"))) (array (class "java.lang.Object"))) 2))
                                      (10 (areturn))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.util.WeakHashMap") (class "java.util.WeakHashMap$1"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokespecial
					(methodCP "<init>" "java.util.WeakHashMap$EntrySet" ((class "java.util.WeakHashMap")) void)))
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


(defconst *WeakHashMap$EntrySet-class-table*
  (make-static-class-decls 
   *java.util.WeakHashMap$EntrySet*))

(defconst *package-name-map* 
  ("java.util.WeakHashMap$EntrySet" . "java.util"))

