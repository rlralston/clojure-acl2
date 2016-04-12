; ObjectInputStream$HandleTable-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:32 CDT 2014.
;

(defconst *java.io.ObjectInputStream$HandleTable*
 (make-class-def
      '(class "java.io.ObjectInputStream$HandleTable"
            "java.lang.Object"
            (constant_pool
                        (INT 1)
                        (INT 2)
                        (INT 3))
            (fields
                        (field "STATUS_OK" byte (accessflags  *class*  *final*  *private*  *static* ) 0)
                        (field "STATUS_UNKNOWN" byte (accessflags  *class*  *final*  *private*  *static* ) 1)
                        (field "STATUS_EXCEPTION" byte (accessflags  *class*  *final*  *private*  *static* ) 2)
                        (field "status" (array byte) (accessflags  *class* ) -1)
                        (field "entries" (array (class "java.lang.Object")) (accessflags  *class* ) -1)
                        (field "deps" (array (class "java.io.ObjectInputStream$HandleTable$HandleList")) (accessflags  *class* ) -1)
                        (field "lowDep" int (accessflags  *class* ) -1)
                        (field "size" int (accessflags  *class* ) -1))
            (methods
                        (method "<init>"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 38)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (iconst_m1))
                                      (6 (putfield (fieldCP "lowDep" "java.io.ObjectInputStream$HandleTable" int)))
                                      (9 (aload_0))
                                      (10 (iconst_0))
                                      (11 (putfield (fieldCP "size" "java.io.ObjectInputStream$HandleTable" int)))
                                      (14 (aload_0))
                                      (15 (iload_1))
                                      (16 (newarray BYTE))
                                      (18 (putfield (fieldCP "status" "java.io.ObjectInputStream$HandleTable" (array byte))))
                                      (21 (aload_0))
                                      (22 (iload_1))
                                      (23 (anewarray (class "java.lang.Object")))
                                      (26 (putfield (fieldCP "entries" "java.io.ObjectInputStream$HandleTable" (array (class "java.lang.Object")))))
                                      (29 (aload_0))
                                      (30 (iload_1))
                                      (31 (anewarray (class "java.io.ObjectInputStream$HandleTable$HandleList")))
                                      (34 (putfield (fieldCP "deps" "java.io.ObjectInputStream$HandleTable" (array (class "java.io.ObjectInputStream$HandleTable$HandleList")))))
                                      (37 (return))
                                      (endofcode 38))
                                   (Exceptions )
                                   (StackMap )))
                        (method "assign"
                              (parameters (class "java.lang.Object"))
                              (returntype . int)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 48)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "size" "java.io.ObjectInputStream$HandleTable" int))) 
                                      (4 (aload_0)) 
                                      (5 (getfield (fieldCP "entries" "java.io.ObjectInputStream$HandleTable" (array (class "java.lang.Object"))))) 
                                      (8 (arraylength)) 
                                      (9 (if_icmplt 16))  ;;to TAG_0
                                      (12 (aload_0)) 
                                      (13 (invokespecial (methodCP "grow" "java.io.ObjectInputStream$HandleTable" () void))) 
                                      (16 (aload_0)) ;;at TAG_0
                                      (17 (getfield (fieldCP "status" "java.io.ObjectInputStream$HandleTable" (array byte)))) 
                                      (20 (aload_0)) 
                                      (21 (getfield (fieldCP "size" "java.io.ObjectInputStream$HandleTable" int))) 
                                      (24 (iconst_2)) 
                                      (25 (bastore)) 
                                      (26 (aload_0)) 
                                      (27 (getfield (fieldCP "entries" "java.io.ObjectInputStream$HandleTable" (array (class "java.lang.Object"))))) 
                                      (30 (aload_0)) 
                                      (31 (getfield (fieldCP "size" "java.io.ObjectInputStream$HandleTable" int))) 
                                      (34 (aload_1)) 
                                      (35 (aastore)) 
                                      (36 (aload_0)) 
                                      (37 (dup)) 
                                      (38 (getfield (fieldCP "size" "java.io.ObjectInputStream$HandleTable" int))) 
                                      (41 (dup_x1)) 
                                      (42 (iconst_1)) 
                                      (43 (iadd)) 
                                      (44 (putfield (fieldCP "size" "java.io.ObjectInputStream$HandleTable" int))) 
                                      (47 (ireturn)) 
                                      (endofcode 48))
                                   (Exceptions )
                                   (StackMap )))
                        (method "markDependency"
                              (parameters int int)
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 171)
                                   (parsedcode
                                      (0 (iload_1)) 
                                      (1 (iconst_m1)) 
                                      (2 (if_icmpeq 10)) ;;to TAG_0
                                      (5 (iload_2)) 
                                      (6 (iconst_m1)) 
                                      (7 (if_icmpne 11))  ;;to TAG_1
                                      (10 (return)) ;;at TAG_0
                                      (11 (aload_0)) ;;at TAG_1
                                      (12 (getfield (fieldCP "status" "java.io.ObjectInputStream$HandleTable" (array byte)))) 
                                      (15 (iload_1)) 
                                      (16 (baload)) 
                                      (17 (lookupswitch (lookupswitchinfo 162 2 ((2 . 44) (3 . 159))))) ;;to TAG_2;;to TAG_3;;to TAG_4
                                      (44 (aload_0)) ;;at TAG_3
                                      (45 (getfield (fieldCP "status" "java.io.ObjectInputStream$HandleTable" (array byte)))) 
                                      (48 (iload_2)) 
                                      (49 (baload)) 
                                      (50 (tableswitch (tableswitchinfo 151 (1 . 3) (76 96 79)))) ;;to TAG_5;;to TAG_6;;to TAG_7;;to TAG_8
                                      (76 (goto 170)) ;;to TAG_9;;at TAG_6
                                      (79 (aload_0)) ;;at TAG_8
                                      (80 (iload_1)) 
                                      (81 (aload_0)) 
                                      (82 (getfield (fieldCP "entries" "java.io.ObjectInputStream$HandleTable" (array (class "java.lang.Object"))))) 
                                      (85 (iload_2)) 
                                      (86 (aaload)) 
                                      (87 (checkcast (class "java.lang.ClassNotFoundException"))) 
                                      (90 (invokevirtual (methodCP "markException" "java.io.ObjectInputStream$HandleTable" (int (class "java.lang.ClassNotFoundException")) void))) 
                                      (93 (goto 170)) ;;to TAG_9
                                      (96 (aload_0)) ;;at TAG_7
                                      (97 (getfield (fieldCP "deps" "java.io.ObjectInputStream$HandleTable" (array (class "java.io.ObjectInputStream$HandleTable$HandleList"))))) 
                                      (100 (iload_2)) 
                                      (101 (aaload)) 
                                      (102 (ifnonnull 118)) ;;to TAG_10
                                      (105 (aload_0)) 
                                      (106 (getfield (fieldCP "deps" "java.io.ObjectInputStream$HandleTable" (array (class "java.io.ObjectInputStream$HandleTable$HandleList"))))) 
                                      (109 (iload_2)) 
                                      (110 (new (class "java.io.ObjectInputStream$HandleTable$HandleList"))) 
                                      (113 (dup)) 
                                      (114 (invokespecial (methodCP "<init>" "java.io.ObjectInputStream$HandleTable$HandleList" () void))) 
                                      (117 (aastore)) 
                                      (118 (aload_0)) ;;at TAG_10
                                      (119 (getfield (fieldCP "deps" "java.io.ObjectInputStream$HandleTable" (array (class "java.io.ObjectInputStream$HandleTable$HandleList"))))) 
                                      (122 (iload_2)) 
                                      (123 (aaload)) 
                                      (124 (iload_1)) 
                                      (125 (invokevirtual (methodCP "add" "java.io.ObjectInputStream$HandleTable$HandleList" (int) void))) 
                                      (128 (aload_0)) 
                                      (129 (getfield (fieldCP "lowDep" "java.io.ObjectInputStream$HandleTable" int))) 
                                      (132 (iflt 143)) ;;to TAG_11
                                      (135 (aload_0)) 
                                      (136 (getfield (fieldCP "lowDep" "java.io.ObjectInputStream$HandleTable" int))) 
                                      (139 (iload_2)) 
                                      (140 (if_icmple 170)) ;;to TAG_9
                                      (143 (aload_0)) ;;at TAG_11
                                      (144 (iload_2)) 
                                      (145 (putfield (fieldCP "lowDep" "java.io.ObjectInputStream$HandleTable" int))) 
                                      (148 (goto 170)) ;;to TAG_9
                                      (151 (new (class "java.lang.InternalError"))) ;;at TAG_5
                                      (154 (dup)) 
                                      (155 (invokespecial (methodCP "<init>" "java.lang.InternalError" () void))) 
                                      (158 (athrow)) 
                                      (159 (goto 170)) ;;to TAG_9;;at TAG_4
                                      (162 (new (class "java.lang.InternalError"))) ;;at TAG_2
                                      (165 (dup)) 
                                      (166 (invokespecial (methodCP "<init>" "java.lang.InternalError" () void))) 
                                      (169 (athrow)) 
                                      (170 (return)) ;;at TAG_9
                                      (endofcode 171))
                                   (Exceptions )
                                   (StackMap )))
                        (method "markException"
                              (parameters int (class "java.lang.ClassNotFoundException"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 3) (max_locals . 6) (code_length . 112)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "status" "java.io.ObjectInputStream$HandleTable" (array byte)))) 
                                      (4 (iload_1)) 
                                      (5 (baload)) 
                                      (6 (lookupswitch (lookupswitchinfo 103 2 ((2 . 32) (3 . 100)))))  ;;to TAG_2;;to TAG_0;;to TAG_1
                                      (32 (aload_0)) ;;at TAG_1
                                      (33 (getfield (fieldCP "status" "java.io.ObjectInputStream$HandleTable" (array byte)))) 
                                      (36 (iload_1)) 
                                      (37 (iconst_3)) 
                                      (38 (bastore)) 
                                      (39 (aload_0)) 
                                      (40 (getfield (fieldCP "entries" "java.io.ObjectInputStream$HandleTable" (array (class "java.lang.Object"))))) 
                                      (43 (iload_1)) 
                                      (44 (aload_2)) 
                                      (45 (aastore)) 
                                      (46 (aload_0)) 
                                      (47 (getfield (fieldCP "deps" "java.io.ObjectInputStream$HandleTable" (array (class "java.io.ObjectInputStream$HandleTable$HandleList"))))) 
                                      (50 (iload_1)) 
                                      (51 (aaload)) 
                                      (52 (astore_3)) 
                                      (53 (aload_3)) 
                                      (54 (ifnull 111)) ;;to TAG_3
                                      (57 (aload_3)) 
                                      (58 (invokevirtual (methodCP "size" "java.io.ObjectInputStream$HandleTable$HandleList" () int))) 
                                      (61 (istore 4)) 
                                      (63 (iconst_0)) 
                                      (64 (istore 5)) 
                                      (66 (iload 5)) ;;at TAG_5
                                      (68 (iload 4)) 
                                      (70 (if_icmpge 90)) ;;to TAG_4
                                      (73 (aload_0)) 
                                      (74 (aload_3)) 
                                      (75 (iload 5)) 
                                      (77 (invokevirtual (methodCP "get" "java.io.ObjectInputStream$HandleTable$HandleList" (int) int))) 
                                      (80 (aload_2)) 
                                      (81 (invokevirtual (methodCP "markException" "java.io.ObjectInputStream$HandleTable" (int (class "java.lang.ClassNotFoundException")) void))) 
                                      (84 (iinc 5 1)) 
                                      (87 (goto 66)) ;;to TAG_5
                                      (90 (aload_0)) ;;at TAG_4
                                      (91 (getfield (fieldCP "deps" "java.io.ObjectInputStream$HandleTable" (array (class "java.io.ObjectInputStream$HandleTable$HandleList"))))) 
                                      (94 (iload_1)) 
                                      (95 (aconst_null)) 
                                      (96 (aastore)) 
                                      (97 (goto 111)) ;;to TAG_3
                                      (100 (goto 111)) ;;to TAG_3;;at TAG_2
                                      (103 (new (class "java.lang.InternalError"))) ;;at TAG_0
                                      (106 (dup)) 
                                      (107 (invokespecial (methodCP "<init>" "java.lang.InternalError" () void))) 
                                      (110 (athrow)) 
                                      (111 (return)) ;;at TAG_3
                                      (endofcode 112))
                                   (Exceptions )
                                   (StackMap )))
                        (method "finish"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 3) (max_locals . 4) (code_length . 111)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "lowDep" "java.io.ObjectInputStream$HandleTable" int))) 
                                      (4 (ifge 14)) ;;to TAG_0
                                      (7 (iload_1)) 
                                      (8 (iconst_1)) 
                                      (9 (iadd)) 
                                      (10 (istore_2)) 
                                      (11 (goto 36))  ;;to TAG_1
                                      (14 (aload_0)) ;;at TAG_0
                                      (15 (getfield (fieldCP "lowDep" "java.io.ObjectInputStream$HandleTable" int))) 
                                      (18 (iload_1)) 
                                      (19 (if_icmplt 35)) ;;to TAG_2
                                      (22 (aload_0)) 
                                      (23 (getfield (fieldCP "size" "java.io.ObjectInputStream$HandleTable" int))) 
                                      (26 (istore_2)) 
                                      (27 (aload_0)) 
                                      (28 (iconst_m1)) 
                                      (29 (putfield (fieldCP "lowDep" "java.io.ObjectInputStream$HandleTable" int))) 
                                      (32 (goto 36))  ;;to TAG_1
                                      (35 (return)) ;;at TAG_2
                                      (36 (iload_1)) ;;at TAG_1
                                      (37 (istore_3)) 
                                      (38 (iload_3)) ;;at TAG_8
                                      (39 (iload_2)) 
                                      (40 (if_icmpge 110)) ;;to TAG_3
                                      (43 (aload_0)) 
                                      (44 (getfield (fieldCP "status" "java.io.ObjectInputStream$HandleTable" (array byte)))) 
                                      (47 (iload_3)) 
                                      (48 (baload)) 
                                      (49 (tableswitch (tableswitchinfo 96 (1 . 3) (93 76 93)))) ;;to TAG_4;;to TAG_5;;to TAG_6
                                      (76 (aload_0)) ;;at TAG_6
                                      (77 (getfield (fieldCP "status" "java.io.ObjectInputStream$HandleTable" (array byte)))) 
                                      (80 (iload_3)) 
                                      (81 (iconst_1)) 
                                      (82 (bastore)) 
                                      (83 (aload_0)) 
                                      (84 (getfield (fieldCP "deps" "java.io.ObjectInputStream$HandleTable" (array (class "java.io.ObjectInputStream$HandleTable$HandleList"))))) 
                                      (87 (iload_3)) 
                                      (88 (aconst_null)) 
                                      (89 (aastore)) 
                                      (90 (goto 104)) ;;to TAG_7
                                      (93 (goto 104)) ;;to TAG_7;;at TAG_5
                                      (96 (new (class "java.lang.InternalError"))) ;;at TAG_4
                                      (99 (dup)) 
                                      (100 (invokespecial (methodCP "<init>" "java.lang.InternalError" () void))) 
                                      (103 (athrow)) 
                                      (104 (iinc 3 1)) ;;at TAG_7
                                      (107 (goto 38)) ;;to TAG_8
                                      (110 (return)) ;;at TAG_3
                                      (endofcode 111))
                                   (Exceptions )
                                   (StackMap )))
                        (method "setObject"
                              (parameters int (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 54)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "status" "java.io.ObjectInputStream$HandleTable" (array byte)))) 
                                      (4 (iload_1)) 
                                      (5 (baload)) 
                                      (6 (tableswitch (tableswitchinfo 45 (1 . 3) (32 32 42))))  ;;to TAG_2;;to TAG_0;;to TAG_1
                                      (32 (aload_0)) ;;at TAG_1
                                      (33 (getfield (fieldCP "entries" "java.io.ObjectInputStream$HandleTable" (array (class "java.lang.Object"))))) 
                                      (36 (iload_1)) 
                                      (37 (aload_2)) 
                                      (38 (aastore)) 
                                      (39 (goto 53)) ;;to TAG_3
                                      (42 (goto 53)) ;;to TAG_3;;at TAG_2
                                      (45 (new (class "java.lang.InternalError"))) ;;at TAG_0
                                      (48 (dup)) 
                                      (49 (invokespecial (methodCP "<init>" "java.lang.InternalError" () void))) 
                                      (52 (athrow)) 
                                      (53 (return)) ;;at TAG_3
                                      (endofcode 54))
                                   (Exceptions )
                                   (StackMap )))
                        (method "lookupObject"
                              (parameters int)
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 26)
                                   (parsedcode
                                      (0 (iload_1)) 
                                      (1 (iconst_m1)) 
                                      (2 (if_icmpeq 24))  ;;to TAG_0
                                      (5 (aload_0)) 
                                      (6 (getfield (fieldCP "status" "java.io.ObjectInputStream$HandleTable" (array byte)))) 
                                      (9 (iload_1)) 
                                      (10 (baload)) 
                                      (11 (iconst_3)) 
                                      (12 (if_icmpeq 24))  ;;to TAG_0
                                      (15 (aload_0)) 
                                      (16 (getfield (fieldCP "entries" "java.io.ObjectInputStream$HandleTable" (array (class "java.lang.Object"))))) 
                                      (19 (iload_1)) 
                                      (20 (aaload)) 
                                      (21 (goto 25)) ;;to TAG_1
                                      (24 (aconst_null)) ;;at TAG_0
                                      (25 (areturn)) ;;at TAG_1
                                      (endofcode 26))
                                   (Exceptions )
                                   (StackMap )))
                        (method "lookupException"
                              (parameters int)
                              (returntype . (class "java.lang.ClassNotFoundException"))
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 29)
                                   (parsedcode
                                      (0 (iload_1)) 
                                      (1 (iconst_m1)) 
                                      (2 (if_icmpeq 27))  ;;to TAG_0
                                      (5 (aload_0)) 
                                      (6 (getfield (fieldCP "status" "java.io.ObjectInputStream$HandleTable" (array byte)))) 
                                      (9 (iload_1)) 
                                      (10 (baload)) 
                                      (11 (iconst_3)) 
                                      (12 (if_icmpne 27))  ;;to TAG_0
                                      (15 (aload_0)) 
                                      (16 (getfield (fieldCP "entries" "java.io.ObjectInputStream$HandleTable" (array (class "java.lang.Object"))))) 
                                      (19 (iload_1)) 
                                      (20 (aaload)) 
                                      (21 (checkcast (class "java.lang.ClassNotFoundException"))) 
                                      (24 (goto 28)) ;;to TAG_1
                                      (27 (aconst_null)) ;;at TAG_0
                                      (28 (areturn)) ;;at TAG_1
                                      (endofcode 29))
                                   (Exceptions )
                                   (StackMap )))
                        (method "clear"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 4) (max_locals . 1) (code_length . 50)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "status" "java.io.ObjectInputStream$HandleTable" (array byte))))
                                      (4 (iconst_0))
                                      (5 (aload_0))
                                      (6 (getfield (fieldCP "size" "java.io.ObjectInputStream$HandleTable" int)))
                                      (9 (iconst_0))
                                      (10 (invokestatic
					(methodCP "fill" "java.util.Arrays" ((array byte) int int byte) void)))
                                      (13 (aload_0))
                                      (14 (getfield (fieldCP "entries" "java.io.ObjectInputStream$HandleTable" (array (class "java.lang.Object")))))
                                      (17 (iconst_0))
                                      (18 (aload_0))
                                      (19 (getfield (fieldCP "size" "java.io.ObjectInputStream$HandleTable" int)))
                                      (22 (aconst_null))
                                      (23 (invokestatic
					(methodCP "fill" "java.util.Arrays" ((array (class "java.lang.Object")) int int (class "java.lang.Object")) void)))
                                      (26 (aload_0))
                                      (27 (getfield (fieldCP "deps" "java.io.ObjectInputStream$HandleTable" (array (class "java.io.ObjectInputStream$HandleTable$HandleList")))))
                                      (30 (iconst_0))
                                      (31 (aload_0))
                                      (32 (getfield (fieldCP "size" "java.io.ObjectInputStream$HandleTable" int)))
                                      (35 (aconst_null))
                                      (36 (invokestatic
					(methodCP "fill" "java.util.Arrays" ((array (class "java.lang.Object")) int int (class "java.lang.Object")) void)))
                                      (39 (aload_0))
                                      (40 (iconst_m1))
                                      (41 (putfield (fieldCP "lowDep" "java.io.ObjectInputStream$HandleTable" int)))
                                      (44 (aload_0))
                                      (45 (iconst_0))
                                      (46 (putfield (fieldCP "size" "java.io.ObjectInputStream$HandleTable" int)))
                                      (49 (return))
                                      (endofcode 50))
                                   (Exceptions )
                                   (StackMap )))
                        (method "size"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "size" "java.io.ObjectInputStream$HandleTable" int)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "grow"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 5) (max_locals . 5) (code_length . 85)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "entries" "java.io.ObjectInputStream$HandleTable" (array (class "java.lang.Object")))))
                                      (4 (arraylength))
                                      (5 (iconst_1))
                                      (6 (ishl))
                                      (7 (iconst_1))
                                      (8 (iadd))
                                      (9 (istore_1))
                                      (10 (iload_1))
                                      (11 (newarray BYTE))
                                      (13 (astore_2))
                                      (14 (iload_1))
                                      (15 (anewarray (class "java.lang.Object")))
                                      (18 (astore_3))
                                      (19 (iload_1))
                                      (20 (anewarray (class "java.io.ObjectInputStream$HandleTable$HandleList")))
                                      (23 (astore 4))
                                      (25 (aload_0))
                                      (26 (getfield (fieldCP "status" "java.io.ObjectInputStream$HandleTable" (array byte))))
                                      (29 (iconst_0))
                                      (30 (aload_2))
                                      (31 (iconst_0))
                                      (32 (aload_0))
                                      (33 (getfield (fieldCP "size" "java.io.ObjectInputStream$HandleTable" int)))
                                      (36 (invokestatic
					(methodCP "arraycopy" "java.lang.System" ((class "java.lang.Object") int (class "java.lang.Object") int int) void)))
                                      (39 (aload_0))
                                      (40 (getfield (fieldCP "entries" "java.io.ObjectInputStream$HandleTable" (array (class "java.lang.Object")))))
                                      (43 (iconst_0))
                                      (44 (aload_3))
                                      (45 (iconst_0))
                                      (46 (aload_0))
                                      (47 (getfield (fieldCP "size" "java.io.ObjectInputStream$HandleTable" int)))
                                      (50 (invokestatic
					(methodCP "arraycopy" "java.lang.System" ((class "java.lang.Object") int (class "java.lang.Object") int int) void)))
                                      (53 (aload_0))
                                      (54 (getfield (fieldCP "deps" "java.io.ObjectInputStream$HandleTable" (array (class "java.io.ObjectInputStream$HandleTable$HandleList")))))
                                      (57 (iconst_0))
                                      (58 (aload 4))
                                      (60 (iconst_0))
                                      (61 (aload_0))
                                      (62 (getfield (fieldCP "size" "java.io.ObjectInputStream$HandleTable" int)))
                                      (65 (invokestatic
					(methodCP "arraycopy" "java.lang.System" ((class "java.lang.Object") int (class "java.lang.Object") int int) void)))
                                      (68 (aload_0))
                                      (69 (aload_2))
                                      (70 (putfield (fieldCP "status" "java.io.ObjectInputStream$HandleTable" (array byte))))
                                      (73 (aload_0))
                                      (74 (aload_3))
                                      (75 (putfield (fieldCP "entries" "java.io.ObjectInputStream$HandleTable" (array (class "java.lang.Object")))))
                                      (78 (aload_0))
                                      (79 (aload 4))
                                      (81 (putfield (fieldCP "deps" "java.io.ObjectInputStream$HandleTable" (array (class "java.io.ObjectInputStream$HandleTable$HandleList")))))
                                      (84 (return))
                                      (endofcode 85))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *ObjectInputStream$HandleTable-class-table*
  (make-static-class-decls 
   *java.io.ObjectInputStream$HandleTable*))

(defconst *package-name-map* 
  ("java.io.ObjectInputStream$HandleTable" . "java.io"))
