; Hashtable$Enumerator-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:45 CDT 2014.
;

(defconst *java.util.Hashtable$Enumerator*
 (make-class-def
      '(class "java.util.Hashtable$Enumerator"
            "java.lang.Object"
            (constant_pool
                        (STRING  "Hashtable Enumerator")
                        (INT 2147483647))
            (fields
                        (field "table" (array (class "java.util.Hashtable$Entry")) (accessflags  *class* ) -1)
                        (field "index" int (accessflags  *class* ) -1)
                        (field "entry" (class "java.util.Hashtable$Entry") (accessflags  *class* ) -1)
                        (field "lastReturned" (class "java.util.Hashtable$Entry") (accessflags  *class* ) -1)
                        (field "type" int (accessflags  *class* ) -1)
                        (field "iterator" boolean (accessflags  *class* ) -1)
                        (field "expectedModCount" int (accessflags  *class*  *protected* ) -1)
                        (field "this$0" (class "java.util.Hashtable") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.Hashtable") int boolean)
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 61)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.util.Hashtable$Enumerator" (class "java.util.Hashtable"))))
                                      (5 (aload_0))
                                      (6 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (9 (aload_0))
                                      (10 (aload_0))
                                      (11 (getfield (fieldCP "this$0" "java.util.Hashtable$Enumerator" (class "java.util.Hashtable"))))
                                      (14 (invokestatic
					(methodCP "access$400" "java.util.Hashtable" ((class "java.util.Hashtable")) (array (class "java.util.Hashtable$Entry")))))
                                      (17 (putfield (fieldCP "table" "java.util.Hashtable$Enumerator" (array (class "java.util.Hashtable$Entry")))))
                                      (20 (aload_0))
                                      (21 (aload_0))
                                      (22 (getfield (fieldCP "table" "java.util.Hashtable$Enumerator" (array (class "java.util.Hashtable$Entry")))))
                                      (25 (arraylength))
                                      (26 (putfield (fieldCP "index" "java.util.Hashtable$Enumerator" int)))
                                      (29 (aload_0))
                                      (30 (aconst_null))
                                      (31 (putfield (fieldCP "entry" "java.util.Hashtable$Enumerator" (class "java.util.Hashtable$Entry"))))
                                      (34 (aload_0))
                                      (35 (aconst_null))
                                      (36 (putfield (fieldCP "lastReturned" "java.util.Hashtable$Enumerator" (class "java.util.Hashtable$Entry"))))
                                      (39 (aload_0))
                                      (40 (aload_0))
                                      (41 (getfield (fieldCP "this$0" "java.util.Hashtable$Enumerator" (class "java.util.Hashtable"))))
                                      (44 (invokestatic
					(methodCP "access$500" "java.util.Hashtable" ((class "java.util.Hashtable")) int)))
                                      (47 (putfield (fieldCP "expectedModCount" "java.util.Hashtable$Enumerator" int)))
                                      (50 (aload_0))
                                      (51 (iload_2))
                                      (52 (putfield (fieldCP "type" "java.util.Hashtable$Enumerator" int)))
                                      (55 (aload_0))
                                      (56 (iload_3))
                                      (57 (putfield (fieldCP "iterator" "java.util.Hashtable$Enumerator" boolean)))
                                      (60 (return))
                                      (endofcode 61))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hasMoreElements"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 53)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "entry" "java.util.Hashtable$Enumerator" (class "java.util.Hashtable$Entry")))) 
                                      (4 (astore_1)) 
                                      (5 (aload_0)) 
                                      (6 (getfield (fieldCP "index" "java.util.Hashtable$Enumerator" int))) 
                                      (9 (istore_2)) 
                                      (10 (aload_0)) 
                                      (11 (getfield (fieldCP "table" "java.util.Hashtable$Enumerator" (array (class "java.util.Hashtable$Entry"))))) 
                                      (14 (astore_3)) 
                                      (15 (aload_1)) ;;at TAG_1
                                      (16 (ifnonnull 33)) ;;to TAG_0
                                      (19 (iload_2)) 
                                      (20 (ifle 33)) ;;to TAG_0
                                      (23 (aload_3)) 
                                      (24 (iinc 2 -1)) 
                                      (27 (iload_2)) 
                                      (28 (aaload)) 
                                      (29 (astore_1)) 
                                      (30 (goto 15)) ;;to TAG_1
                                      (33 (aload_0)) ;;at TAG_0
                                      (34 (aload_1)) 
                                      (35 (putfield (fieldCP "entry" "java.util.Hashtable$Enumerator" (class "java.util.Hashtable$Entry")))) 
                                      (38 (aload_0)) 
                                      (39 (iload_2)) 
                                      (40 (putfield (fieldCP "index" "java.util.Hashtable$Enumerator" int))) 
                                      (43 (aload_1)) 
                                      (44 (ifnull 51))  ;;to TAG_2
                                      (47 (iconst_1)) 
                                      (48 (goto 52)) ;;to TAG_3
                                      (51 (iconst_0)) ;;at TAG_2
                                      (52 (ireturn)) ;;at TAG_3
                                      (endofcode 53))
                                   (Exceptions )
                                   (StackMap )))
                        (method "nextElement"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 5) (code_length . 111)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "entry" "java.util.Hashtable$Enumerator" (class "java.util.Hashtable$Entry")))) 
                                      (4 (astore_1)) 
                                      (5 (aload_0)) 
                                      (6 (getfield (fieldCP "index" "java.util.Hashtable$Enumerator" int))) 
                                      (9 (istore_2)) 
                                      (10 (aload_0)) 
                                      (11 (getfield (fieldCP "table" "java.util.Hashtable$Enumerator" (array (class "java.util.Hashtable$Entry"))))) 
                                      (14 (astore_3)) 
                                      (15 (aload_1)) ;;at TAG_1
                                      (16 (ifnonnull 33)) ;;to TAG_0
                                      (19 (iload_2)) 
                                      (20 (ifle 33)) ;;to TAG_0
                                      (23 (aload_3)) 
                                      (24 (iinc 2 -1)) 
                                      (27 (iload_2)) 
                                      (28 (aaload)) 
                                      (29 (astore_1)) 
                                      (30 (goto 15)) ;;to TAG_1
                                      (33 (aload_0)) ;;at TAG_0
                                      (34 (aload_1)) 
                                      (35 (putfield (fieldCP "entry" "java.util.Hashtable$Enumerator" (class "java.util.Hashtable$Entry")))) 
                                      (38 (aload_0)) 
                                      (39 (iload_2)) 
                                      (40 (putfield (fieldCP "index" "java.util.Hashtable$Enumerator" int))) 
                                      (43 (aload_1)) 
                                      (44 (ifnull 101))  ;;to TAG_2
                                      (47 (aload_0)) 
                                      (48 (aload_0)) 
                                      (49 (getfield (fieldCP "entry" "java.util.Hashtable$Enumerator" (class "java.util.Hashtable$Entry")))) 
                                      (52 (dup_x1)) 
                                      (53 (putfield (fieldCP "lastReturned" "java.util.Hashtable$Enumerator" (class "java.util.Hashtable$Entry")))) 
                                      (56 (astore 4)) 
                                      (58 (aload_0)) 
                                      (59 (aload 4)) 
                                      (61 (getfield (fieldCP "next" "java.util.Hashtable$Entry" (class "java.util.Hashtable$Entry")))) 
                                      (64 (putfield (fieldCP "entry" "java.util.Hashtable$Enumerator" (class "java.util.Hashtable$Entry")))) 
                                      (67 (aload_0)) 
                                      (68 (getfield (fieldCP "type" "java.util.Hashtable$Enumerator" int))) 
                                      (71 (ifne 82)) ;;to TAG_3
                                      (74 (aload 4)) 
                                      (76 (getfield (fieldCP "key" "java.util.Hashtable$Entry" (class "java.lang.Object")))) 
                                      (79 (goto 100)) ;;to TAG_4
                                      (82 (aload_0)) ;;at TAG_3
                                      (83 (getfield (fieldCP "type" "java.util.Hashtable$Enumerator" int))) 
                                      (86 (iconst_1)) 
                                      (87 (if_icmpne 98)) ;;to TAG_5
                                      (90 (aload 4)) 
                                      (92 (getfield (fieldCP "value" "java.util.Hashtable$Entry" (class "java.lang.Object")))) 
                                      (95 (goto 100)) ;;to TAG_4
                                      (98 (aload 4)) ;;at TAG_5
                                      (100 (areturn)) ;;at TAG_4
                                      (101 (new (class "java.util.NoSuchElementException"))) ;;at TAG_2
                                      (104 (dup)) 
                                      (105 (ldc 0)) ;;STRING:: "Hashtable Enumerator"
                                      (107 (invokespecial (methodCP "<init>" "java.util.NoSuchElementException" ((class "java.lang.String")) void))) 
                                      (110 (athrow)) 
                                      (endofcode 111))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hasNext"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokevirtual
					(methodCP "hasMoreElements" "java.util.Hashtable$Enumerator" () boolean)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "next"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 27)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "this$0" "java.util.Hashtable$Enumerator" (class "java.util.Hashtable")))) 
                                      (4 (invokestatic (methodCP "access$500" "java.util.Hashtable" ((class "java.util.Hashtable")) int))) 
                                      (7 (aload_0)) 
                                      (8 (getfield (fieldCP "expectedModCount" "java.util.Hashtable$Enumerator" int))) 
                                      (11 (if_icmpeq 22))  ;;to TAG_0
                                      (14 (new (class "java.util.ConcurrentModificationException"))) 
                                      (17 (dup)) 
                                      (18 (invokespecial (methodCP "<init>" "java.util.ConcurrentModificationException" () void))) 
                                      (21 (athrow)) 
                                      (22 (aload_0)) ;;at TAG_0
                                      (23 (invokevirtual (methodCP "nextElement" "java.util.Hashtable$Enumerator" () (class "java.lang.Object")))) 
                                      (26 (areturn)) 
                                      (endofcode 27))
                                   (Exceptions )
                                   (StackMap )))
                        (method "remove"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 7) (code_length . 194)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "iterator" "java.util.Hashtable$Enumerator" boolean))) 
                                      (4 (ifne 15)) ;;to TAG_0
                                      (7 (new (class "java.lang.UnsupportedOperationException"))) 
                                      (10 (dup)) 
                                      (11 (invokespecial (methodCP "<init>" "java.lang.UnsupportedOperationException" () void))) 
                                      (14 (athrow)) 
                                      (15 (aload_0)) ;;at TAG_0
                                      (16 (getfield (fieldCP "lastReturned" "java.util.Hashtable$Enumerator" (class "java.util.Hashtable$Entry")))) 
                                      (19 (ifnonnull 32))  ;;to TAG_1
                                      (22 (new (class "java.lang.IllegalStateException"))) 
                                      (25 (dup)) 
                                      (26 (ldc 0)) ;;STRING:: "Hashtable Enumerator"
                                      (28 (invokespecial (methodCP "<init>" "java.lang.IllegalStateException" ((class "java.lang.String")) void))) 
                                      (31 (athrow)) 
                                      (32 (aload_0)) ;;at TAG_1
                                      (33 (getfield (fieldCP "this$0" "java.util.Hashtable$Enumerator" (class "java.util.Hashtable")))) 
                                      (36 (invokestatic (methodCP "access$500" "java.util.Hashtable" ((class "java.util.Hashtable")) int))) 
                                      (39 (aload_0)) 
                                      (40 (getfield (fieldCP "expectedModCount" "java.util.Hashtable$Enumerator" int))) 
                                      (43 (if_icmpeq 54)) ;;to TAG_2
                                      (46 (new (class "java.util.ConcurrentModificationException"))) 
                                      (49 (dup)) 
                                      (50 (invokespecial (methodCP "<init>" "java.util.ConcurrentModificationException" () void))) 
                                      (53 (athrow)) 
                                      (54 (aload_0)) ;;at TAG_2
                                      (55 (getfield (fieldCP "this$0" "java.util.Hashtable$Enumerator" (class "java.util.Hashtable")))) 
                                      (58 (dup)) 
                                      (59 (astore_1)) 
                                      (60 (monitorenter)) 
                                      (61 (aload_0)) ;;at TAG_8
                                      (62 (getfield (fieldCP "this$0" "java.util.Hashtable$Enumerator" (class "java.util.Hashtable")))) 
                                      (65 (invokestatic (methodCP "access$400" "java.util.Hashtable" ((class "java.util.Hashtable")) (array (class "java.util.Hashtable$Entry"))))) 
                                      (68 (astore_2)) 
                                      (69 (aload_0)) 
                                      (70 (getfield (fieldCP "lastReturned" "java.util.Hashtable$Enumerator" (class "java.util.Hashtable$Entry")))) 
                                      (73 (getfield (fieldCP "hash" "java.util.Hashtable$Entry" int))) 
                                      (76 (ldc 1)) ;;INT:: "2147483647"
                                      (78 (iand)) 
                                      (79 (aload_2)) 
                                      (80 (arraylength)) 
                                      (81 (irem)) 
                                      (82 (istore_3)) 
                                      (83 (aload_2)) 
                                      (84 (iload_3)) 
                                      (85 (aaload)) 
                                      (86 (astore 4)) 
                                      (88 (aconst_null)) 
                                      (89 (astore 5)) 
                                      (91 (aload 4)) ;;at TAG_7
                                      (93 (ifnull 179)) ;;to TAG_3
                                      (96 (aload 4)) 
                                      (98 (aload_0)) 
                                      (99 (getfield (fieldCP "lastReturned" "java.util.Hashtable$Enumerator" (class "java.util.Hashtable$Entry")))) 
                                      (102 (if_acmpne 165)) ;;to TAG_4
                                      (105 (aload_0)) 
                                      (106 (getfield (fieldCP "this$0" "java.util.Hashtable$Enumerator" (class "java.util.Hashtable")))) 
                                      (109 (invokestatic (methodCP "access$508" "java.util.Hashtable" ((class "java.util.Hashtable")) int))) 
                                      (112 (pop)) 
                                      (113 (aload_0)) 
                                      (114 (dup)) 
                                      (115 (getfield (fieldCP "expectedModCount" "java.util.Hashtable$Enumerator" int))) 
                                      (118 (iconst_1)) 
                                      (119 (iadd)) 
                                      (120 (putfield (fieldCP "expectedModCount" "java.util.Hashtable$Enumerator" int))) 
                                      (123 (aload 5)) 
                                      (125 (ifnonnull 139)) ;;to TAG_5
                                      (128 (aload_2)) 
                                      (129 (iload_3)) 
                                      (130 (aload 4)) 
                                      (132 (getfield (fieldCP "next" "java.util.Hashtable$Entry" (class "java.util.Hashtable$Entry")))) 
                                      (135 (aastore)) 
                                      (136 (goto 149)) ;;to TAG_6
                                      (139 (aload 5)) ;;at TAG_5
                                      (141 (aload 4)) 
                                      (143 (getfield (fieldCP "next" "java.util.Hashtable$Entry" (class "java.util.Hashtable$Entry")))) 
                                      (146 (putfield (fieldCP "next" "java.util.Hashtable$Entry" (class "java.util.Hashtable$Entry")))) 
                                      (149 (aload_0)) ;;at TAG_6
                                      (150 (getfield (fieldCP "this$0" "java.util.Hashtable$Enumerator" (class "java.util.Hashtable")))) 
                                      (153 (invokestatic (methodCP "access$210" "java.util.Hashtable" ((class "java.util.Hashtable")) int))) 
                                      (156 (pop)) 
                                      (157 (aload_0)) 
                                      (158 (aconst_null)) 
                                      (159 (putfield (fieldCP "lastReturned" "java.util.Hashtable$Enumerator" (class "java.util.Hashtable$Entry")))) 
                                      (162 (aload_1)) 
                                      (163 (monitorexit)) 
                                      (164 (return)) ;;at TAG_9
                                      (165 (aload 4)) ;;at TAG_4
                                      (167 (astore 5)) 
                                      (169 (aload 4)) 
                                      (171 (getfield (fieldCP "next" "java.util.Hashtable$Entry" (class "java.util.Hashtable$Entry")))) 
                                      (174 (astore 4)) 
                                      (176 (goto 91)) ;;to TAG_7
                                      (179 (new (class "java.util.ConcurrentModificationException"))) ;;at TAG_3
                                      (182 (dup)) 
                                      (183 (invokespecial (methodCP "<init>" "java.util.ConcurrentModificationException" () void))) 
                                      (186 (athrow)) 
                                      (187 (astore 6)) ;;at TAG_10
                                      (189 (aload_1)) 
                                      (190 (monitorexit)) 
                                      (191 (aload 6)) ;;at TAG_11
                                      (193 (athrow)) 
                                      (endofcode 194))
                                   (Exceptions 
                                     (handler 61 164  187 (class "java.lang.Throwable"))
                                     (handler 165 191  187 (class "java.lang.Throwable")))
                                   (StackMap ))))
            (interfaces "java.util.Enumeration" "java.util.Iterator")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *Hashtable$Enumerator-class-table*
  (make-static-class-decls 
   *java.util.Hashtable$Enumerator*))

(defconst *package-name-map* 
  ("java.util.Hashtable$Enumerator" . "java.util"))

