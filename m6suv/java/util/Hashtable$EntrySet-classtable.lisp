; Hashtable$EntrySet-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:45 CDT 2014.
;

(defconst *java.util.Hashtable$EntrySet*
 (make-class-def
      '(class "java.util.Hashtable$EntrySet"
            "java.util.AbstractSet"
            (constant_pool
                        (INT 2147483647))
            (fields
                        (field "this$0" (class "java.util.Hashtable") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.Hashtable"))
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.util.Hashtable$EntrySet" (class "java.util.Hashtable"))))
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
                                   (max_stack . 2) (max_locals . 1) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "this$0" "java.util.Hashtable$EntrySet" (class "java.util.Hashtable"))))
                                      (4 (iconst_2))
                                      (5 (invokestatic
					(methodCP "access$100" "java.util.Hashtable" ((class "java.util.Hashtable") int) (class "java.util.Iterator"))))
                                      (8 (areturn))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "add"
                              (parameters (class "java.util.Map$Entry"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokespecial
					(methodCP "add" "java.util.AbstractSet" ((class "java.lang.Object")) boolean)))
                                      (5 (ireturn))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "contains"
                              (parameters (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 8) (code_length . 92)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (instanceof (class "java.util.Map$Entry"))) 
                                      (4 (ifne 9)) ;;to TAG_0
                                      (7 (iconst_0)) 
                                      (8 (ireturn)) 
                                      (9 (aload_1)) ;;at TAG_0
                                      (10 (checkcast (class "java.util.Map$Entry"))) 
                                      (13 (astore_2)) 
                                      (14 (aload_2)) 
                                      (15 (invokeinterface (methodCP "getKey" "java.util.Map$Entry" () (class "java.lang.Object")) 1)) 
                                      (20 (astore_3)) 
                                      (21 (aload_0)) 
                                      (22 (getfield (fieldCP "this$0" "java.util.Hashtable$EntrySet" (class "java.util.Hashtable")))) 
                                      (25 (invokestatic (methodCP "access$400" "java.util.Hashtable" ((class "java.util.Hashtable")) (array (class "java.util.Hashtable$Entry"))))) 
                                      (28 (astore 4)) 
                                      (30 (aload_3)) 
                                      (31 (invokevirtual (methodCP "hashCode" "java.lang.Object" () int))) 
                                      (34 (istore 5)) 
                                      (36 (iload 5)) 
                                      (38 (ldc 0)) ;;INT:: "2147483647"
                                      (40 (iand)) 
                                      (41 (aload 4)) 
                                      (43 (arraylength)) 
                                      (44 (irem)) 
                                      (45 (istore 6)) 
                                      (47 (aload 4)) 
                                      (49 (iload 6)) 
                                      (51 (aaload)) 
                                      (52 (astore 7)) 
                                      (54 (aload 7)) ;;at TAG_3
                                      (56 (ifnull 90)) ;;to TAG_1
                                      (59 (aload 7)) 
                                      (61 (getfield (fieldCP "hash" "java.util.Hashtable$Entry" int))) 
                                      (64 (iload 5)) 
                                      (66 (if_icmpne 80))  ;;to TAG_2
                                      (69 (aload 7)) 
                                      (71 (aload_2)) 
                                      (72 (invokevirtual (methodCP "equals" "java.util.Hashtable$Entry" ((class "java.lang.Object")) boolean))) 
                                      (75 (ifeq 80))  ;;to TAG_2
                                      (78 (iconst_1)) 
                                      (79 (ireturn)) 
                                      (80 (aload 7)) ;;at TAG_2
                                      (82 (getfield (fieldCP "next" "java.util.Hashtable$Entry" (class "java.util.Hashtable$Entry")))) 
                                      (85 (astore 7)) 
                                      (87 (goto 54)) ;;to TAG_3
                                      (90 (iconst_0)) ;;at TAG_1
                                      (91 (ireturn)) 
                                      (endofcode 92))
                                   (Exceptions )
                                   (StackMap )))
                        (method "remove"
                              (parameters (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 9) (code_length . 149)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (instanceof (class "java.util.Map$Entry"))) 
                                      (4 (ifne 9)) ;;to TAG_0
                                      (7 (iconst_0)) 
                                      (8 (ireturn)) 
                                      (9 (aload_1)) ;;at TAG_0
                                      (10 (checkcast (class "java.util.Map$Entry"))) 
                                      (13 (astore_2)) 
                                      (14 (aload_2)) 
                                      (15 (invokeinterface (methodCP "getKey" "java.util.Map$Entry" () (class "java.lang.Object")) 1)) 
                                      (20 (astore_3)) 
                                      (21 (aload_0)) 
                                      (22 (getfield (fieldCP "this$0" "java.util.Hashtable$EntrySet" (class "java.util.Hashtable")))) 
                                      (25 (invokestatic (methodCP "access$400" "java.util.Hashtable" ((class "java.util.Hashtable")) (array (class "java.util.Hashtable$Entry"))))) 
                                      (28 (astore 4)) 
                                      (30 (aload_3)) 
                                      (31 (invokevirtual (methodCP "hashCode" "java.lang.Object" () int))) 
                                      (34 (istore 5)) 
                                      (36 (iload 5)) 
                                      (38 (ldc 0)) ;;INT:: "2147483647"
                                      (40 (iand)) 
                                      (41 (aload 4)) 
                                      (43 (arraylength)) 
                                      (44 (irem)) 
                                      (45 (istore 6)) 
                                      (47 (aload 4)) 
                                      (49 (iload 6)) 
                                      (51 (aaload)) 
                                      (52 (astore 7)) 
                                      (54 (aconst_null)) 
                                      (55 (astore 8)) 
                                      (57 (aload 7)) ;;at TAG_5
                                      (59 (ifnull 147)) ;;to TAG_1
                                      (62 (aload 7)) 
                                      (64 (getfield (fieldCP "hash" "java.util.Hashtable$Entry" int))) 
                                      (67 (iload 5)) 
                                      (69 (if_icmpne 133))  ;;to TAG_2
                                      (72 (aload 7)) 
                                      (74 (aload_2)) 
                                      (75 (invokevirtual (methodCP "equals" "java.util.Hashtable$Entry" ((class "java.lang.Object")) boolean))) 
                                      (78 (ifeq 133))  ;;to TAG_2
                                      (81 (aload_0)) 
                                      (82 (getfield (fieldCP "this$0" "java.util.Hashtable$EntrySet" (class "java.util.Hashtable")))) 
                                      (85 (invokestatic (methodCP "access$508" "java.util.Hashtable" ((class "java.util.Hashtable")) int))) 
                                      (88 (pop)) 
                                      (89 (aload 8)) 
                                      (91 (ifnull 107)) ;;to TAG_3
                                      (94 (aload 8)) 
                                      (96 (aload 7)) 
                                      (98 (getfield (fieldCP "next" "java.util.Hashtable$Entry" (class "java.util.Hashtable$Entry")))) 
                                      (101 (putfield (fieldCP "next" "java.util.Hashtable$Entry" (class "java.util.Hashtable$Entry")))) 
                                      (104 (goto 117)) ;;to TAG_4
                                      (107 (aload 4)) ;;at TAG_3
                                      (109 (iload 6)) 
                                      (111 (aload 7)) 
                                      (113 (getfield (fieldCP "next" "java.util.Hashtable$Entry" (class "java.util.Hashtable$Entry")))) 
                                      (116 (aastore)) 
                                      (117 (aload_0)) ;;at TAG_4
                                      (118 (getfield (fieldCP "this$0" "java.util.Hashtable$EntrySet" (class "java.util.Hashtable")))) 
                                      (121 (invokestatic (methodCP "access$210" "java.util.Hashtable" ((class "java.util.Hashtable")) int))) 
                                      (124 (pop)) 
                                      (125 (aload 7)) 
                                      (127 (aconst_null)) 
                                      (128 (putfield (fieldCP "value" "java.util.Hashtable$Entry" (class "java.lang.Object")))) 
                                      (131 (iconst_1)) 
                                      (132 (ireturn)) 
                                      (133 (aload 7)) ;;at TAG_2
                                      (135 (astore 8)) 
                                      (137 (aload 7)) 
                                      (139 (getfield (fieldCP "next" "java.util.Hashtable$Entry" (class "java.util.Hashtable$Entry")))) 
                                      (142 (astore 7)) 
                                      (144 (goto 57)) ;;to TAG_5
                                      (147 (iconst_0)) ;;at TAG_1
                                      (148 (ireturn)) 
                                      (endofcode 149))
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
                                      (1 (getfield (fieldCP "this$0" "java.util.Hashtable$EntrySet" (class "java.util.Hashtable"))))
                                      (4 (invokestatic
					(methodCP "access$200" "java.util.Hashtable" ((class "java.util.Hashtable")) int)))
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
                                      (1 (getfield (fieldCP "this$0" "java.util.Hashtable$EntrySet" (class "java.util.Hashtable"))))
                                      (4 (invokevirtual
					(methodCP "clear" "java.util.Hashtable" () void)))
                                      (7 (return))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "add"
                              (parameters (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *class*  *public*  *volatile* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (checkcast (class "java.util.Map$Entry")))
                                      (5 (invokevirtual
					(methodCP "add" "java.util.Hashtable$EntrySet" ((class "java.util.Map$Entry")) boolean)))
                                      (8 (ireturn))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.util.Hashtable") (class "java.util.Hashtable$1"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokespecial
					(methodCP "<init>" "java.util.Hashtable$EntrySet" ((class "java.util.Hashtable")) void)))
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


(defconst *Hashtable$EntrySet-class-table*
  (make-static-class-decls 
   *java.util.Hashtable$EntrySet*))

(defconst *package-name-map* 
  ("java.util.Hashtable$EntrySet" . "java.util"))

