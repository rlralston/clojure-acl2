; Collections$UnmodifiableMap$UnmodifiableEntrySet-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:43 CDT 2014.
;

(defconst *java.util.Collections$UnmodifiableMap$UnmodifiableEntrySet*
 (make-class-def
      '(class "java.util.Collections$UnmodifiableMap$UnmodifiableEntrySet"
            "java.util.Collections$UnmodifiableSet"
            (constant_pool
                        (LONG 7854390611657943733))
            (fields
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.Set"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokespecial
					(methodCP "<init>" "java.util.Collections$UnmodifiableSet" ((class "java.util.Set")) void)))
                                      (5 (return))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "iterator"
                              (parameters )
                              (returntype . (class "java.util.Iterator"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 9)
                                   (parsedcode
                                      (0 (new (class "java.util.Collections$UnmodifiableMap$UnmodifiableEntrySet$1")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (invokespecial
					(methodCP "<init>" "java.util.Collections$UnmodifiableMap$UnmodifiableEntrySet$1" ((class "java.util.Collections$UnmodifiableMap$UnmodifiableEntrySet")) void)))
                                      (8 (areturn))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "toArray"
                              (parameters )
                              (returntype . (array (class "java.lang.Object")))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 3) (code_length . 42)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "c" "java.util.Collections$UnmodifiableMap$UnmodifiableEntrySet" (class "java.util.Collection")))) 
                                      (4 (invokeinterface (methodCP "toArray" "java.util.Collection" () (array (class "java.lang.Object"))) 1)) 
                                      (9 (astore_1)) 
                                      (10 (iconst_0)) 
                                      (11 (istore_2)) 
                                      (12 (iload_2)) ;;at TAG_1
                                      (13 (aload_1)) 
                                      (14 (arraylength)) 
                                      (15 (if_icmpge 40))  ;;to TAG_0
                                      (18 (aload_1)) 
                                      (19 (iload_2)) 
                                      (20 (new (class "java.util.Collections$UnmodifiableMap$UnmodifiableEntrySet$UnmodifiableEntry"))) 
                                      (23 (dup)) 
                                      (24 (aload_1)) 
                                      (25 (iload_2)) 
                                      (26 (aaload)) 
                                      (27 (checkcast (class "java.util.Map$Entry"))) 
                                      (30 (invokespecial (methodCP "<init>" "java.util.Collections$UnmodifiableMap$UnmodifiableEntrySet$UnmodifiableEntry" ((class "java.util.Map$Entry")) void))) 
                                      (33 (aastore)) 
                                      (34 (iinc 2 1)) 
                                      (37 (goto 12)) ;;to TAG_1
                                      (40 (aload_1)) ;;at TAG_0
                                      (41 (areturn)) 
                                      (endofcode 42))
                                   (Exceptions )
                                   (StackMap )))
                        (method "toArray"
                              (parameters (array (class "java.lang.Object")))
                              (returntype . (array (class "java.lang.Object")))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 4) (code_length . 89)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "c" "java.util.Collections$UnmodifiableMap$UnmodifiableEntrySet" (class "java.util.Collection")))) 
                                      (4 (aload_1)) 
                                      (5 (arraylength)) 
                                      (6 (ifne 13)) ;;to TAG_0
                                      (9 (aload_1)) 
                                      (10 (goto 18)) ;;to TAG_1
                                      (13 (aload_1)) ;;at TAG_0
                                      (14 (iconst_0)) 
                                      (15 (invokestatic (methodCP "copyOf" "java.util.Arrays" ((array (class "java.lang.Object")) int) (array (class "java.lang.Object"))))) 
                                      (18 (invokeinterface (methodCP "toArray" "java.util.Collection" ((array (class "java.lang.Object"))) (array (class "java.lang.Object"))) 2)) ;;at TAG_1
                                      (23 (astore_2)) 
                                      (24 (iconst_0)) 
                                      (25 (istore_3)) 
                                      (26 (iload_3)) ;;at TAG_3
                                      (27 (aload_2)) 
                                      (28 (arraylength)) 
                                      (29 (if_icmpge 54))  ;;to TAG_2
                                      (32 (aload_2)) 
                                      (33 (iload_3)) 
                                      (34 (new (class "java.util.Collections$UnmodifiableMap$UnmodifiableEntrySet$UnmodifiableEntry"))) 
                                      (37 (dup)) 
                                      (38 (aload_2)) 
                                      (39 (iload_3)) 
                                      (40 (aaload)) 
                                      (41 (checkcast (class "java.util.Map$Entry"))) 
                                      (44 (invokespecial (methodCP "<init>" "java.util.Collections$UnmodifiableMap$UnmodifiableEntrySet$UnmodifiableEntry" ((class "java.util.Map$Entry")) void))) 
                                      (47 (aastore)) 
                                      (48 (iinc 3 1)) 
                                      (51 (goto 26)) ;;to TAG_3
                                      (54 (aload_2)) ;;at TAG_2
                                      (55 (arraylength)) 
                                      (56 (aload_1)) 
                                      (57 (arraylength)) 
                                      (58 (if_icmple 66)) ;;to TAG_4
                                      (61 (aload_2)) 
                                      (62 (checkcast (array (class "java.lang.Object")))) 
                                      (65 (areturn)) 
                                      (66 (aload_2)) ;;at TAG_4
                                      (67 (iconst_0)) 
                                      (68 (aload_1)) 
                                      (69 (iconst_0)) 
                                      (70 (aload_2)) 
                                      (71 (arraylength)) 
                                      (72 (invokestatic (methodCP "arraycopy" "java.lang.System" ((class "java.lang.Object") int (class "java.lang.Object") int int) void))) 
                                      (75 (aload_1)) 
                                      (76 (arraylength)) 
                                      (77 (aload_2)) 
                                      (78 (arraylength)) 
                                      (79 (if_icmple 87)) ;;to TAG_5
                                      (82 (aload_1)) 
                                      (83 (aload_2)) 
                                      (84 (arraylength)) 
                                      (85 (aconst_null)) 
                                      (86 (aastore)) 
                                      (87 (aload_1)) ;;at TAG_5
                                      (88 (areturn)) 
                                      (endofcode 89))
                                   (Exceptions )
                                   (StackMap )))
                        (method "contains"
                              (parameters (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 30)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (instanceof (class "java.util.Map$Entry"))) 
                                      (4 (ifne 9))  ;;to TAG_0
                                      (7 (iconst_0)) 
                                      (8 (ireturn)) 
                                      (9 (aload_0)) ;;at TAG_0
                                      (10 (getfield (fieldCP "c" "java.util.Collections$UnmodifiableMap$UnmodifiableEntrySet" (class "java.util.Collection")))) 
                                      (13 (new (class "java.util.Collections$UnmodifiableMap$UnmodifiableEntrySet$UnmodifiableEntry"))) 
                                      (16 (dup)) 
                                      (17 (aload_1)) 
                                      (18 (checkcast (class "java.util.Map$Entry"))) 
                                      (21 (invokespecial (methodCP "<init>" "java.util.Collections$UnmodifiableMap$UnmodifiableEntrySet$UnmodifiableEntry" ((class "java.util.Map$Entry")) void))) 
                                      (24 (invokeinterface (methodCP "contains" "java.util.Collection" ((class "java.lang.Object")) boolean) 2)) 
                                      (29 (ireturn)) 
                                      (endofcode 30))
                                   (Exceptions )
                                   (StackMap )))
                        (method "containsAll"
                              (parameters (class "java.util.Collection"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 38)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (invokeinterface (methodCP "iterator" "java.util.Collection" () (class "java.util.Iterator")) 1)) 
                                      (6 (astore_2)) 
                                      (7 (aload_2)) ;;at TAG_2
                                      (8 (invokeinterface (methodCP "hasNext" "java.util.Iterator" () boolean) 1)) 
                                      (13 (ifeq 36)) ;;to TAG_0
                                      (16 (aload_2)) 
                                      (17 (invokeinterface (methodCP "next" "java.util.Iterator" () (class "java.lang.Object")) 1)) 
                                      (22 (astore_3)) 
                                      (23 (aload_0)) 
                                      (24 (aload_3)) 
                                      (25 (invokevirtual (methodCP "contains" "java.util.Collections$UnmodifiableMap$UnmodifiableEntrySet" ((class "java.lang.Object")) boolean))) 
                                      (28 (ifne 33)) ;;to TAG_1
                                      (31 (iconst_0)) 
                                      (32 (ireturn)) 
                                      (33 (goto 7))  ;;to TAG_2;;at TAG_1
                                      (36 (iconst_1)) ;;at TAG_0
                                      (37 (ireturn)) 
                                      (endofcode 38))
                                   (Exceptions )
                                   (StackMap )))
                        (method "equals"
                              (parameters (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 47)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (aload_0)) 
                                      (2 (if_acmpne 7)) ;;to TAG_0
                                      (5 (iconst_1)) 
                                      (6 (ireturn)) 
                                      (7 (aload_1)) ;;at TAG_0
                                      (8 (instanceof (class "java.util.Set"))) 
                                      (11 (ifne 16)) ;;to TAG_1
                                      (14 (iconst_0)) 
                                      (15 (ireturn)) 
                                      (16 (aload_1)) ;;at TAG_1
                                      (17 (checkcast (class "java.util.Set"))) 
                                      (20 (astore_2)) 
                                      (21 (aload_2)) 
                                      (22 (invokeinterface (methodCP "size" "java.util.Set" () int) 1)) 
                                      (27 (aload_0)) 
                                      (28 (getfield (fieldCP "c" "java.util.Collections$UnmodifiableMap$UnmodifiableEntrySet" (class "java.util.Collection")))) 
                                      (31 (invokeinterface (methodCP "size" "java.util.Collection" () int) 1)) 
                                      (36 (if_icmpeq 41))  ;;to TAG_2
                                      (39 (iconst_0)) 
                                      (40 (ireturn)) 
                                      (41 (aload_0)) ;;at TAG_2
                                      (42 (aload_2)) 
                                      (43 (invokevirtual (methodCP "containsAll" "java.util.Collections$UnmodifiableMap$UnmodifiableEntrySet" ((class "java.util.Collection")) boolean))) 
                                      (46 (ireturn)) 
                                      (endofcode 47))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *Collections$UnmodifiableMap$UnmodifiableEntrySet-class-table*
  (make-static-class-decls 
   *java.util.Collections$UnmodifiableMap$UnmodifiableEntrySet*))

(defconst *package-name-map* 
  ("java.util.Collections$UnmodifiableMap$UnmodifiableEntrySet" . "java.util"))
