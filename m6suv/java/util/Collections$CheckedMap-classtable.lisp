; Collections$CheckedMap-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:42 CDT 2014.
;

(defconst *java.util.Collections$CheckedMap*
 (make-class-def
      '(class "java.util.Collections$CheckedMap"
            "java.lang.Object"
            (constant_pool
                        (LONG 5742860141034234728)
                        (STRING  "Attempt to insert ")
                        (STRING  " key into map with key type ")
                        (STRING  " value into map with value type "))
            (fields
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0)
                        (field "m" (class "java.util.Map") (accessflags  *class*  *final*  *private* ) -1)
                        (field "keyType" (class "java.lang.Class") (accessflags  *class*  *final* ) -1)
                        (field "valueType" (class "java.lang.Class") (accessflags  *class*  *final* ) -1)
                        (field "entrySet" (class "java.util.Set") (accessflags  *class*  *private*  *transient* ) -1))
            (methods
                        (method "typeCheck"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 57)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (ifnull 28))  ;;to TAG_0
                                      (4 (aload_0)) 
                                      (5 (getfield (fieldCP "keyType" "java.util.Collections$CheckedMap" (class "java.lang.Class")))) 
                                      (8 (aload_1)) 
                                      (9 (invokevirtual (methodCP "isInstance" "java.lang.Class" ((class "java.lang.Object")) boolean))) 
                                      (12 (ifne 28))  ;;to TAG_0
                                      (15 (new (class "java.lang.ClassCastException"))) 
                                      (18 (dup)) 
                                      (19 (aload_0)) 
                                      (20 (aload_1)) 
                                      (21 (invokespecial (methodCP "badKeyMsg" "java.util.Collections$CheckedMap" ((class "java.lang.Object")) (class "java.lang.String")))) 
                                      (24 (invokespecial (methodCP "<init>" "java.lang.ClassCastException" ((class "java.lang.String")) void))) 
                                      (27 (athrow)) 
                                      (28 (aload_2)) ;;at TAG_0
                                      (29 (ifnull 56)) ;;to TAG_1
                                      (32 (aload_0)) 
                                      (33 (getfield (fieldCP "valueType" "java.util.Collections$CheckedMap" (class "java.lang.Class")))) 
                                      (36 (aload_2)) 
                                      (37 (invokevirtual (methodCP "isInstance" "java.lang.Class" ((class "java.lang.Object")) boolean))) 
                                      (40 (ifne 56)) ;;to TAG_1
                                      (43 (new (class "java.lang.ClassCastException"))) 
                                      (46 (dup)) 
                                      (47 (aload_0)) 
                                      (48 (aload_2)) 
                                      (49 (invokespecial (methodCP "badValueMsg" "java.util.Collections$CheckedMap" ((class "java.lang.Object")) (class "java.lang.String")))) 
                                      (52 (invokespecial (methodCP "<init>" "java.lang.ClassCastException" ((class "java.lang.String")) void))) 
                                      (55 (athrow)) 
                                      (56 (return)) ;;at TAG_1
                                      (endofcode 57))
                                   (Exceptions )
                                   (StackMap )))
                        (method "badKeyMsg"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 35)
                                   (parsedcode
                                      (0 (new (class "java.lang.StringBuilder")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.lang.StringBuilder" () void)))
                                      (7 (ldc 1))         ;;STRING:: "Attempt to insert "
                                      (9 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (12 (aload_1))
                                      (13 (invokevirtual
					(methodCP "getClass" "java.lang.Object" () (class "java.lang.Class"))))
                                      (16 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.Object")) (class "java.lang.StringBuilder"))))
                                      (19 (ldc 2))        ;;STRING:: " key into map with key type "
                                      (21 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (24 (aload_0))
                                      (25 (getfield (fieldCP "keyType" "java.util.Collections$CheckedMap" (class "java.lang.Class"))))
                                      (28 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.Object")) (class "java.lang.StringBuilder"))))
                                      (31 (invokevirtual
					(methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String"))))
                                      (34 (areturn))
                                      (endofcode 35))
                                   (Exceptions )
                                   (StackMap )))
                        (method "badValueMsg"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 35)
                                   (parsedcode
                                      (0 (new (class "java.lang.StringBuilder")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.lang.StringBuilder" () void)))
                                      (7 (ldc 1))         ;;STRING:: "Attempt to insert "
                                      (9 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (12 (aload_1))
                                      (13 (invokevirtual
					(methodCP "getClass" "java.lang.Object" () (class "java.lang.Class"))))
                                      (16 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.Object")) (class "java.lang.StringBuilder"))))
                                      (19 (ldc 3))        ;;STRING:: " value into map with value type "
                                      (21 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (24 (aload_0))
                                      (25 (getfield (fieldCP "valueType" "java.util.Collections$CheckedMap" (class "java.lang.Class"))))
                                      (28 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.Object")) (class "java.lang.StringBuilder"))))
                                      (31 (invokevirtual
					(methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String"))))
                                      (34 (areturn))
                                      (endofcode 35))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.util.Map") (class "java.lang.Class") (class "java.lang.Class"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 45)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokespecial (methodCP "<init>" "java.lang.Object" () void))) 
                                      (4 (aload_0)) 
                                      (5 (aconst_null)) 
                                      (6 (putfield (fieldCP "entrySet" "java.util.Collections$CheckedMap" (class "java.util.Set")))) 
                                      (9 (aload_1)) 
                                      (10 (ifnull 21))  ;;to TAG_0
                                      (13 (aload_2)) 
                                      (14 (ifnull 21))  ;;to TAG_0
                                      (17 (aload_3)) 
                                      (18 (ifnonnull 29)) ;;to TAG_1
                                      (21 (new (class "java.lang.NullPointerException"))) ;;at TAG_0
                                      (24 (dup)) 
                                      (25 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" () void))) 
                                      (28 (athrow)) 
                                      (29 (aload_0)) ;;at TAG_1
                                      (30 (aload_1)) 
                                      (31 (putfield (fieldCP "m" "java.util.Collections$CheckedMap" (class "java.util.Map")))) 
                                      (34 (aload_0)) 
                                      (35 (aload_2)) 
                                      (36 (putfield (fieldCP "keyType" "java.util.Collections$CheckedMap" (class "java.lang.Class")))) 
                                      (39 (aload_0)) 
                                      (40 (aload_3)) 
                                      (41 (putfield (fieldCP "valueType" "java.util.Collections$CheckedMap" (class "java.lang.Class")))) 
                                      (44 (return)) 
                                      (endofcode 45))
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
                                      (1 (getfield (fieldCP "m" "java.util.Collections$CheckedMap" (class "java.util.Map"))))
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
                                      (1 (getfield (fieldCP "m" "java.util.Collections$CheckedMap" (class "java.util.Map"))))
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
                                      (1 (getfield (fieldCP "m" "java.util.Collections$CheckedMap" (class "java.util.Map"))))
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
                                      (1 (getfield (fieldCP "m" "java.util.Collections$CheckedMap" (class "java.util.Map"))))
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
                                      (1 (getfield (fieldCP "m" "java.util.Collections$CheckedMap" (class "java.util.Map"))))
                                      (4 (aload_1))
                                      (5 (invokeinterface
					(methodCP "get" "java.util.Map" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (10 (areturn))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "remove"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "m" "java.util.Collections$CheckedMap" (class "java.util.Map"))))
                                      (4 (aload_1))
                                      (5 (invokeinterface
					(methodCP "remove" "java.util.Map" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (10 (areturn))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "clear"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "m" "java.util.Collections$CheckedMap" (class "java.util.Map"))))
                                      (4 (invokeinterface
					(methodCP "clear" "java.util.Map" () void) 1))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "keySet"
                              (parameters )
                              (returntype . (class "java.util.Set"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "m" "java.util.Collections$CheckedMap" (class "java.util.Map"))))
                                      (4 (invokeinterface
					(methodCP "keySet" "java.util.Map" () (class "java.util.Set")) 1))
                                      (9 (areturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "values"
                              (parameters )
                              (returntype . (class "java.util.Collection"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "m" "java.util.Collections$CheckedMap" (class "java.util.Map"))))
                                      (4 (invokeinterface
					(methodCP "values" "java.util.Map" () (class "java.util.Collection")) 1))
                                      (9 (areturn))
                                      (endofcode 10))
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
                                      (6 (getfield (fieldCP "m" "java.util.Collections$CheckedMap" (class "java.util.Map")))) 
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
                                      (1 (getfield (fieldCP "m" "java.util.Collections$CheckedMap" (class "java.util.Map"))))
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
                                      (1 (getfield (fieldCP "m" "java.util.Collections$CheckedMap" (class "java.util.Map"))))
                                      (4 (invokevirtual
					(methodCP "toString" "java.lang.Object" () (class "java.lang.String"))))
                                      (7 (areturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "put"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 18)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (aload_2))
                                      (3 (invokespecial
					(methodCP "typeCheck" "java.util.Collections$CheckedMap" ((class "java.lang.Object") (class "java.lang.Object")) void)))
                                      (6 (aload_0))
                                      (7 (getfield (fieldCP "m" "java.util.Collections$CheckedMap" (class "java.util.Map"))))
                                      (10 (aload_1))
                                      (11 (aload_2))
                                      (12 (invokeinterface
					(methodCP "put" "java.util.Map" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (17 (areturn))
                                      (endofcode 18))
                                   (Exceptions )
                                   (StackMap )))
                        (method "putAll"
                              (parameters (class "java.util.Map"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 11) (code_length . 162)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (invokeinterface (methodCP "entrySet" "java.util.Map" () (class "java.util.Set")) 1)) 
                                      (6 (invokeinterface (methodCP "toArray" "java.util.Set" () (array (class "java.lang.Object"))) 1)) 
                                      (11 (astore_2)) 
                                      (12 (new (class "java.util.ArrayList"))) 
                                      (15 (dup)) 
                                      (16 (aload_2)) 
                                      (17 (arraylength)) 
                                      (18 (invokespecial (methodCP "<init>" "java.util.ArrayList" (int) void))) 
                                      (21 (astore_3)) 
                                      (22 (aload_2)) 
                                      (23 (astore 4)) 
                                      (25 (aload 4)) 
                                      (27 (arraylength)) 
                                      (28 (istore 5)) 
                                      (30 (iconst_0)) 
                                      (31 (istore 6)) 
                                      (33 (iload 6)) ;;at TAG_1
                                      (35 (iload 5)) 
                                      (37 (if_icmpge 104)) ;;to TAG_0
                                      (40 (aload 4)) 
                                      (42 (iload 6)) 
                                      (44 (aaload)) 
                                      (45 (astore 7)) 
                                      (47 (aload 7)) 
                                      (49 (checkcast (class "java.util.Map$Entry"))) 
                                      (52 (astore 8)) 
                                      (54 (aload 8)) 
                                      (56 (invokeinterface (methodCP "getKey" "java.util.Map$Entry" () (class "java.lang.Object")) 1)) 
                                      (61 (astore 9)) 
                                      (63 (aload 8)) 
                                      (65 (invokeinterface (methodCP "getValue" "java.util.Map$Entry" () (class "java.lang.Object")) 1)) 
                                      (70 (astore 10)) 
                                      (72 (aload_0)) 
                                      (73 (aload 9)) 
                                      (75 (aload 10)) 
                                      (77 (invokespecial (methodCP "typeCheck" "java.util.Collections$CheckedMap" ((class "java.lang.Object") (class "java.lang.Object")) void))) 
                                      (80 (aload_3)) 
                                      (81 (new (class "java.util.AbstractMap$SimpleImmutableEntry"))) 
                                      (84 (dup)) 
                                      (85 (aload 9)) 
                                      (87 (aload 10)) 
                                      (89 (invokespecial (methodCP "<init>" "java.util.AbstractMap$SimpleImmutableEntry" ((class "java.lang.Object") (class "java.lang.Object")) void))) 
                                      (92 (invokeinterface (methodCP "add" "java.util.List" ((class "java.lang.Object")) boolean) 2)) 
                                      (97 (pop)) 
                                      (98 (iinc 6 1)) 
                                      (101 (goto 33)) ;;to TAG_1
                                      (104 (aload_3)) ;;at TAG_0
                                      (105 (invokeinterface (methodCP "iterator" "java.util.List" () (class "java.util.Iterator")) 1)) 
                                      (110 (astore 4)) 
                                      (112 (aload 4)) ;;at TAG_3
                                      (114 (invokeinterface (methodCP "hasNext" "java.util.Iterator" () boolean) 1)) 
                                      (119 (ifeq 161))  ;;to TAG_2
                                      (122 (aload 4)) 
                                      (124 (invokeinterface (methodCP "next" "java.util.Iterator" () (class "java.lang.Object")) 1)) 
                                      (129 (checkcast (class "java.util.Map$Entry"))) 
                                      (132 (astore 5)) 
                                      (134 (aload_0)) 
                                      (135 (getfield (fieldCP "m" "java.util.Collections$CheckedMap" (class "java.util.Map")))) 
                                      (138 (aload 5)) 
                                      (140 (invokeinterface (methodCP "getKey" "java.util.Map$Entry" () (class "java.lang.Object")) 1)) 
                                      (145 (aload 5)) 
                                      (147 (invokeinterface (methodCP "getValue" "java.util.Map$Entry" () (class "java.lang.Object")) 1)) 
                                      (152 (invokeinterface (methodCP "put" "java.util.Map" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (157 (pop)) 
                                      (158 (goto 112)) ;;to TAG_3
                                      (161 (return)) ;;at TAG_2
                                      (endofcode 162))
                                   (Exceptions )
                                   (StackMap )))
                        (method "entrySet"
                              (parameters )
                              (returntype . (class "java.util.Set"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 1) (code_length . 36)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "entrySet" "java.util.Collections$CheckedMap" (class "java.util.Set")))) 
                                      (4 (ifnonnull 31))  ;;to TAG_0
                                      (7 (aload_0)) 
                                      (8 (new (class "java.util.Collections$CheckedMap$CheckedEntrySet"))) 
                                      (11 (dup)) 
                                      (12 (aload_0)) 
                                      (13 (getfield (fieldCP "m" "java.util.Collections$CheckedMap" (class "java.util.Map")))) 
                                      (16 (invokeinterface (methodCP "entrySet" "java.util.Map" () (class "java.util.Set")) 1)) 
                                      (21 (aload_0)) 
                                      (22 (getfield (fieldCP "valueType" "java.util.Collections$CheckedMap" (class "java.lang.Class")))) 
                                      (25 (invokespecial (methodCP "<init>" "java.util.Collections$CheckedMap$CheckedEntrySet" ((class "java.util.Set") (class "java.lang.Class")) void))) 
                                      (28 (putfield (fieldCP "entrySet" "java.util.Collections$CheckedMap" (class "java.util.Set")))) 
                                      (31 (aload_0)) ;;at TAG_0
                                      (32 (getfield (fieldCP "entrySet" "java.util.Collections$CheckedMap" (class "java.util.Set")))) 
                                      (35 (areturn)) 
                                      (endofcode 36))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.util.Map" "java.io.Serializable")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *Collections$CheckedMap-class-table*
  (make-static-class-decls 
   *java.util.Collections$CheckedMap*))

(defconst *package-name-map* 
  ("java.util.Collections$CheckedMap" . "java.util"))

