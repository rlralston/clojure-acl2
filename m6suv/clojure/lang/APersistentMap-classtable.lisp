; APersistentMap-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:50 CDT 2014.
;

(defconst *clojure.lang.APersistentMap*
 (make-class-def
      '(class "clojure.lang.APersistentMap"
            "clojure.lang.AFn"
            (constant_pool
                        (STRING  "Vector arg to map conj must be a pair"))
            (fields
                        (field "_hash" int (accessflags  *class* ) -1)
                        (field "_hasheq" int (accessflags  *class* ) -1))
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.AFn" () void)))
                                      (4 (aload_0))
                                      (5 (iconst_m1))
                                      (6 (putfield (fieldCP "_hash" "clojure.lang.APersistentMap" int)))
                                      (9 (aload_0))
                                      (10 (iconst_m1))
                                      (11 (putfield (fieldCP "_hasheq" "clojure.lang.APersistentMap" int)))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "toString"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokestatic
					(methodCP "printString" "clojure.lang.RT" ((class "java.lang.Object")) (class "java.lang.String"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "cons"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "clojure.lang.IPersistentCollection"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 5) (code_length . 135)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (instanceof (class "java.util.Map$Entry"))) 
                                      (4 (ifeq 29)) ;;to TAG_0
                                      (7 (aload_1)) 
                                      (8 (checkcast (class "java.util.Map$Entry"))) 
                                      (11 (astore_2)) 
                                      (12 (aload_0)) 
                                      (13 (aload_2)) 
                                      (14 (invokeinterface (methodCP "getKey" "java.util.Map$Entry" () (class "java.lang.Object")) 1)) 
                                      (19 (aload_2)) 
                                      (20 (invokeinterface (methodCP "getValue" "java.util.Map$Entry" () (class "java.lang.Object")) 1)) 
                                      (25 (invokevirtual (methodCP "assoc" "clojure.lang.APersistentMap" ((class "java.lang.Object") (class "java.lang.Object")) (class "clojure.lang.IPersistentMap")))) 
                                      (28 (areturn)) 
                                      (29 (aload_1)) ;;at TAG_0
                                      (30 (instanceof (class "clojure.lang.IPersistentVector"))) 
                                      (33 (ifeq 80)) ;;to TAG_1
                                      (36 (aload_1)) 
                                      (37 (checkcast (class "clojure.lang.IPersistentVector"))) 
                                      (40 (astore_2)) 
                                      (41 (aload_2)) 
                                      (42 (invokeinterface (methodCP "count" "clojure.lang.IPersistentVector" () int) 1)) 
                                      (47 (iconst_2)) 
                                      (48 (if_icmpeq 61))  ;;to TAG_2
                                      (51 (new (class "java.lang.IllegalArgumentException"))) 
                                      (54 (dup)) 
                                      (55 (ldc 0)) ;;STRING:: "Vector arg to map conj must be a pair"
                                      (57 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (60 (athrow)) 
                                      (61 (aload_0)) ;;at TAG_2
                                      (62 (aload_2)) 
                                      (63 (iconst_0)) 
                                      (64 (invokeinterface (methodCP "nth" "clojure.lang.IPersistentVector" (int) (class "java.lang.Object")) 2)) 
                                      (69 (aload_2)) 
                                      (70 (iconst_1)) 
                                      (71 (invokeinterface (methodCP "nth" "clojure.lang.IPersistentVector" (int) (class "java.lang.Object")) 2)) 
                                      (76 (invokevirtual (methodCP "assoc" "clojure.lang.APersistentMap" ((class "java.lang.Object") (class "java.lang.Object")) (class "clojure.lang.IPersistentMap")))) 
                                      (79 (areturn)) 
                                      (80 (aload_0)) ;;at TAG_1
                                      (81 (astore_2)) 
                                      (82 (aload_1)) 
                                      (83 (invokestatic (methodCP "seq" "clojure.lang.RT" ((class "java.lang.Object")) (class "clojure.lang.ISeq")))) 
                                      (86 (astore_3)) 
                                      (87 (aload_3)) ;;at TAG_4
                                      (88 (ifnull 133)) ;;to TAG_3
                                      (91 (aload_3)) 
                                      (92 (invokeinterface (methodCP "first" "clojure.lang.ISeq" () (class "java.lang.Object")) 1)) 
                                      (97 (checkcast (class "java.util.Map$Entry"))) 
                                      (100 (astore 4)) 
                                      (102 (aload_2)) 
                                      (103 (aload 4)) 
                                      (105 (invokeinterface (methodCP "getKey" "java.util.Map$Entry" () (class "java.lang.Object")) 1)) 
                                      (110 (aload 4)) 
                                      (112 (invokeinterface (methodCP "getValue" "java.util.Map$Entry" () (class "java.lang.Object")) 1)) 
                                      (117 (invokeinterface (methodCP "assoc" "clojure.lang.IPersistentMap" ((class "java.lang.Object") (class "java.lang.Object")) (class "clojure.lang.IPersistentMap")) 3)) 
                                      (122 (astore_2)) 
                                      (123 (aload_3)) 
                                      (124 (invokeinterface (methodCP "next" "clojure.lang.ISeq" () (class "clojure.lang.ISeq")) 1)) 
                                      (129 (astore_3)) 
                                      (130 (goto 87)) ;;to TAG_4
                                      (133 (aload_2)) ;;at TAG_3
                                      (134 (areturn)) 
                                      (endofcode 135))
                                   (Exceptions )
                                   (StackMap )))
                        (method "equals"
                              (parameters (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokestatic
					(methodCP "mapEquals" "clojure.lang.APersistentMap" ((class "clojure.lang.IPersistentMap") (class "java.lang.Object")) boolean)))
                                      (5 (ireturn))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "mapEquals"
                              (parameters (class "clojure.lang.IPersistentMap") (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 6) (code_length . 120)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (aload_1)) 
                                      (2 (if_acmpne 7)) ;;to TAG_0
                                      (5 (iconst_1)) 
                                      (6 (ireturn)) 
                                      (7 (aload_1)) ;;at TAG_0
                                      (8 (instanceof (class "java.util.Map"))) 
                                      (11 (ifne 16)) ;;to TAG_1
                                      (14 (iconst_0)) 
                                      (15 (ireturn)) 
                                      (16 (aload_1)) ;;at TAG_1
                                      (17 (checkcast (class "java.util.Map"))) 
                                      (20 (astore_2)) 
                                      (21 (aload_2)) 
                                      (22 (invokeinterface (methodCP "size" "java.util.Map" () int) 1)) 
                                      (27 (aload_0)) 
                                      (28 (invokeinterface (methodCP "count" "clojure.lang.IPersistentMap" () int) 1)) 
                                      (33 (if_icmpeq 38))  ;;to TAG_2
                                      (36 (iconst_0)) 
                                      (37 (ireturn)) 
                                      (38 (aload_0)) ;;at TAG_2
                                      (39 (invokeinterface (methodCP "seq" "clojure.lang.IPersistentMap" () (class "clojure.lang.ISeq")) 1)) 
                                      (44 (astore_3)) 
                                      (45 (aload_3)) ;;at TAG_6
                                      (46 (ifnull 118)) ;;to TAG_3
                                      (49 (aload_3)) 
                                      (50 (invokeinterface (methodCP "first" "clojure.lang.ISeq" () (class "java.lang.Object")) 1)) 
                                      (55 (checkcast (class "java.util.Map$Entry"))) 
                                      (58 (astore 4)) 
                                      (60 (aload_2)) 
                                      (61 (aload 4)) 
                                      (63 (invokeinterface (methodCP "getKey" "java.util.Map$Entry" () (class "java.lang.Object")) 1)) 
                                      (68 (invokeinterface (methodCP "containsKey" "java.util.Map" ((class "java.lang.Object")) boolean) 2)) 
                                      (73 (istore 5)) 
                                      (75 (iload 5)) 
                                      (77 (ifeq 106)) ;;to TAG_4
                                      (80 (aload 4)) 
                                      (82 (invokeinterface (methodCP "getValue" "java.util.Map$Entry" () (class "java.lang.Object")) 1)) 
                                      (87 (aload_2)) 
                                      (88 (aload 4)) 
                                      (90 (invokeinterface (methodCP "getKey" "java.util.Map$Entry" () (class "java.lang.Object")) 1)) 
                                      (95 (invokeinterface (methodCP "get" "java.util.Map" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (100 (invokestatic (methodCP "equals" "clojure.lang.Util" ((class "java.lang.Object") (class "java.lang.Object")) boolean))) 
                                      (103 (ifne 108)) ;;to TAG_5
                                      (106 (iconst_0)) ;;at TAG_4
                                      (107 (ireturn)) 
                                      (108 (aload_3)) ;;at TAG_5
                                      (109 (invokeinterface (methodCP "next" "clojure.lang.ISeq" () (class "clojure.lang.ISeq")) 1)) 
                                      (114 (astore_3)) 
                                      (115 (goto 45)) ;;to TAG_6
                                      (118 (iconst_1)) ;;at TAG_3
                                      (119 (ireturn)) 
                                      (endofcode 120))
                                   (Exceptions )
                                   (StackMap )))
                        (method "equiv"
                              (parameters (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 6) (code_length . 125)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (instanceof (class "java.util.Map"))) 
                                      (4 (ifne 9)) ;;to TAG_0
                                      (7 (iconst_0)) 
                                      (8 (ireturn)) 
                                      (9 (aload_1)) ;;at TAG_0
                                      (10 (instanceof (class "clojure.lang.IPersistentMap"))) 
                                      (13 (ifeq 25)) ;;to TAG_1
                                      (16 (aload_1)) 
                                      (17 (instanceof (class "clojure.lang.MapEquivalence"))) 
                                      (20 (ifne 25)) ;;to TAG_1
                                      (23 (iconst_0)) 
                                      (24 (ireturn)) 
                                      (25 (aload_1)) ;;at TAG_1
                                      (26 (checkcast (class "java.util.Map"))) 
                                      (29 (astore_2)) 
                                      (30 (aload_2)) 
                                      (31 (invokeinterface (methodCP "size" "java.util.Map" () int) 1)) 
                                      (36 (aload_0)) 
                                      (37 (invokevirtual (methodCP "size" "clojure.lang.APersistentMap" () int))) 
                                      (40 (if_icmpeq 45))  ;;to TAG_2
                                      (43 (iconst_0)) 
                                      (44 (ireturn)) 
                                      (45 (aload_0)) ;;at TAG_2
                                      (46 (invokevirtual (methodCP "seq" "clojure.lang.APersistentMap" () (class "clojure.lang.ISeq")))) 
                                      (49 (astore_3)) 
                                      (50 (aload_3)) ;;at TAG_6
                                      (51 (ifnull 123)) ;;to TAG_3
                                      (54 (aload_3)) 
                                      (55 (invokeinterface (methodCP "first" "clojure.lang.ISeq" () (class "java.lang.Object")) 1)) 
                                      (60 (checkcast (class "java.util.Map$Entry"))) 
                                      (63 (astore 4)) 
                                      (65 (aload_2)) 
                                      (66 (aload 4)) 
                                      (68 (invokeinterface (methodCP "getKey" "java.util.Map$Entry" () (class "java.lang.Object")) 1)) 
                                      (73 (invokeinterface (methodCP "containsKey" "java.util.Map" ((class "java.lang.Object")) boolean) 2)) 
                                      (78 (istore 5)) 
                                      (80 (iload 5)) 
                                      (82 (ifeq 111)) ;;to TAG_4
                                      (85 (aload 4)) 
                                      (87 (invokeinterface (methodCP "getValue" "java.util.Map$Entry" () (class "java.lang.Object")) 1)) 
                                      (92 (aload_2)) 
                                      (93 (aload 4)) 
                                      (95 (invokeinterface (methodCP "getKey" "java.util.Map$Entry" () (class "java.lang.Object")) 1)) 
                                      (100 (invokeinterface (methodCP "get" "java.util.Map" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (105 (invokestatic (methodCP "equiv" "clojure.lang.Util" ((class "java.lang.Object") (class "java.lang.Object")) boolean))) 
                                      (108 (ifne 113)) ;;to TAG_5
                                      (111 (iconst_0)) ;;at TAG_4
                                      (112 (ireturn)) 
                                      (113 (aload_3)) ;;at TAG_5
                                      (114 (invokeinterface (methodCP "next" "clojure.lang.ISeq" () (class "clojure.lang.ISeq")) 1)) 
                                      (119 (astore_3)) 
                                      (120 (goto 50)) ;;to TAG_6
                                      (123 (iconst_1)) ;;at TAG_3
                                      (124 (ireturn)) 
                                      (endofcode 125))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hashCode"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 21)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "_hash" "clojure.lang.APersistentMap" int))) 
                                      (4 (iconst_m1)) 
                                      (5 (if_icmpne 16))  ;;to TAG_0
                                      (8 (aload_0)) 
                                      (9 (aload_0)) 
                                      (10 (invokestatic (methodCP "mapHash" "clojure.lang.APersistentMap" ((class "clojure.lang.IPersistentMap")) int))) 
                                      (13 (putfield (fieldCP "_hash" "clojure.lang.APersistentMap" int))) 
                                      (16 (aload_0)) ;;at TAG_0
                                      (17 (getfield (fieldCP "_hash" "clojure.lang.APersistentMap" int))) 
                                      (20 (ireturn)) 
                                      (endofcode 21))
                                   (Exceptions )
                                   (StackMap )))
                        (method "mapHash"
                              (parameters (class "clojure.lang.IPersistentMap"))
                              (returntype . int)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 4) (code_length . 83)
                                   (parsedcode
                                      (0 (iconst_0)) 
                                      (1 (istore_1)) 
                                      (2 (aload_0)) 
                                      (3 (invokeinterface (methodCP "seq" "clojure.lang.IPersistentMap" () (class "clojure.lang.ISeq")) 1)) 
                                      (8 (astore_2)) 
                                      (9 (aload_2)) ;;at TAG_5
                                      (10 (ifnull 81)) ;;to TAG_0
                                      (13 (aload_2)) 
                                      (14 (invokeinterface (methodCP "first" "clojure.lang.ISeq" () (class "java.lang.Object")) 1)) 
                                      (19 (checkcast (class "java.util.Map$Entry"))) 
                                      (22 (astore_3)) 
                                      (23 (iload_1)) 
                                      (24 (aload_3)) 
                                      (25 (invokeinterface (methodCP "getKey" "java.util.Map$Entry" () (class "java.lang.Object")) 1)) 
                                      (30 (ifnonnull 37)) ;;to TAG_1
                                      (33 (iconst_0)) 
                                      (34 (goto 46))  ;;to TAG_2
                                      (37 (aload_3)) ;;at TAG_1
                                      (38 (invokeinterface (methodCP "getKey" "java.util.Map$Entry" () (class "java.lang.Object")) 1)) 
                                      (43 (invokevirtual (methodCP "hashCode" "java.lang.Object" () int))) 
                                      (46 (aload_3)) ;;at TAG_2
                                      (47 (invokeinterface (methodCP "getValue" "java.util.Map$Entry" () (class "java.lang.Object")) 1)) 
                                      (52 (ifnonnull 59)) ;;to TAG_3
                                      (55 (iconst_0)) 
                                      (56 (goto 68)) ;;to TAG_4
                                      (59 (aload_3)) ;;at TAG_3
                                      (60 (invokeinterface (methodCP "getValue" "java.util.Map$Entry" () (class "java.lang.Object")) 1)) 
                                      (65 (invokevirtual (methodCP "hashCode" "java.lang.Object" () int))) 
                                      (68 (ixor)) ;;at TAG_4
                                      (69 (iadd)) 
                                      (70 (istore_1)) 
                                      (71 (aload_2)) 
                                      (72 (invokeinterface (methodCP "next" "clojure.lang.ISeq" () (class "clojure.lang.ISeq")) 1)) 
                                      (77 (astore_2)) 
                                      (78 (goto 9)) ;;to TAG_5
                                      (81 (iload_1)) ;;at TAG_0
                                      (82 (ireturn)) 
                                      (endofcode 83))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hasheq"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 21)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "_hasheq" "clojure.lang.APersistentMap" int))) 
                                      (4 (iconst_m1)) 
                                      (5 (if_icmpne 16))  ;;to TAG_0
                                      (8 (aload_0)) 
                                      (9 (aload_0)) 
                                      (10 (invokestatic (methodCP "mapHasheq" "clojure.lang.APersistentMap" ((class "clojure.lang.IPersistentMap")) int))) 
                                      (13 (putfield (fieldCP "_hasheq" "clojure.lang.APersistentMap" int))) 
                                      (16 (aload_0)) ;;at TAG_0
                                      (17 (getfield (fieldCP "_hasheq" "clojure.lang.APersistentMap" int))) 
                                      (20 (ireturn)) 
                                      (endofcode 21))
                                   (Exceptions )
                                   (StackMap )))
                        (method "mapHasheq"
                              (parameters (class "clojure.lang.IPersistentMap"))
                              (returntype . int)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 4) (code_length . 57)
                                   (parsedcode
                                      (0 (iconst_0)) 
                                      (1 (istore_1)) 
                                      (2 (aload_0)) 
                                      (3 (invokeinterface (methodCP "seq" "clojure.lang.IPersistentMap" () (class "clojure.lang.ISeq")) 1)) 
                                      (8 (astore_2)) 
                                      (9 (aload_2)) ;;at TAG_1
                                      (10 (ifnull 55))  ;;to TAG_0
                                      (13 (aload_2)) 
                                      (14 (invokeinterface (methodCP "first" "clojure.lang.ISeq" () (class "java.lang.Object")) 1)) 
                                      (19 (checkcast (class "java.util.Map$Entry"))) 
                                      (22 (astore_3)) 
                                      (23 (iload_1)) 
                                      (24 (aload_3)) 
                                      (25 (invokeinterface (methodCP "getKey" "java.util.Map$Entry" () (class "java.lang.Object")) 1)) 
                                      (30 (invokestatic (methodCP "hasheq" "clojure.lang.Util" ((class "java.lang.Object")) int))) 
                                      (33 (aload_3)) 
                                      (34 (invokeinterface (methodCP "getValue" "java.util.Map$Entry" () (class "java.lang.Object")) 1)) 
                                      (39 (invokestatic (methodCP "hasheq" "clojure.lang.Util" ((class "java.lang.Object")) int))) 
                                      (42 (ixor)) 
                                      (43 (iadd)) 
                                      (44 (istore_1)) 
                                      (45 (aload_2)) 
                                      (46 (invokeinterface (methodCP "next" "clojure.lang.ISeq" () (class "clojure.lang.ISeq")) 1)) 
                                      (51 (astore_2)) 
                                      (52 (goto 9)) ;;to TAG_1
                                      (55 (iload_1)) ;;at TAG_0
                                      (56 (ireturn)) 
                                      (endofcode 57))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokevirtual
					(methodCP "valAt" "clojure.lang.APersistentMap" ((class "java.lang.Object")) (class "java.lang.Object"))))
                                      (5 (areturn))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 7)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (aload_2))
                                      (3 (invokevirtual
					(methodCP "valAt" "clojure.lang.APersistentMap" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object"))))
                                      (6 (areturn))
                                      (endofcode 7))
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
                        (method "containsValue"
                              (parameters (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokevirtual
					(methodCP "values" "clojure.lang.APersistentMap" () (class "java.util.Collection"))))
                                      (4 (aload_1))
                                      (5 (invokeinterface
					(methodCP "contains" "java.util.Collection" ((class "java.lang.Object")) boolean) 2))
                                      (10 (ireturn))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "entrySet"
                              (parameters )
                              (returntype . (class "java.util.Set"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 9)
                                   (parsedcode
                                      (0 (new (class "clojure.lang.APersistentMap$1")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (invokespecial
					(methodCP "<init>" "clojure.lang.APersistentMap$1" ((class "clojure.lang.APersistentMap")) void)))
                                      (8 (areturn))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "get"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokevirtual
					(methodCP "valAt" "clojure.lang.APersistentMap" ((class "java.lang.Object")) (class "java.lang.Object"))))
                                      (5 (areturn))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isEmpty"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 13)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "count" "clojure.lang.APersistentMap" () int))) 
                                      (4 (ifne 11))  ;;to TAG_0
                                      (7 (iconst_1)) 
                                      (8 (goto 12)) ;;to TAG_1
                                      (11 (iconst_0)) ;;at TAG_0
                                      (12 (ireturn)) ;;at TAG_1
                                      (endofcode 13))
                                   (Exceptions )
                                   (StackMap )))
                        (method "keySet"
                              (parameters )
                              (returntype . (class "java.util.Set"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 9)
                                   (parsedcode
                                      (0 (new (class "clojure.lang.APersistentMap$2")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (invokespecial
					(methodCP "<init>" "clojure.lang.APersistentMap$2" ((class "clojure.lang.APersistentMap")) void)))
                                      (8 (areturn))
                                      (endofcode 9))
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
                        (method "size"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokevirtual
					(methodCP "count" "clojure.lang.APersistentMap" () int)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "values"
                              (parameters )
                              (returntype . (class "java.util.Collection"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 9)
                                   (parsedcode
                                      (0 (new (class "clojure.lang.APersistentMap$3")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (invokespecial
					(methodCP "<init>" "clojure.lang.APersistentMap$3" ((class "clojure.lang.APersistentMap")) void)))
                                      (8 (areturn))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "clojure.lang.IPersistentMap" "java.util.Map" "java.lang.Iterable" "java.io.Serializable" "clojure.lang.MapEquivalence" "clojure.lang.IHashEq")
            (accessflags  *abstract*  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *APersistentMap-class-table*
  (make-static-class-decls 
   *clojure.lang.APersistentMap*))

(defconst *package-name-map* 
  ("clojure.lang.APersistentMap" . "clojure.lang"))

