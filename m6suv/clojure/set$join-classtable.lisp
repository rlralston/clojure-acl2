; set$join-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:58 CDT 2014.
;

(defconst *clojure.set$join*
 (make-class-def
      '(class "clojure.set$join"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "seq")
                        (STRING  "clojure.set")
                        (STRING  "intersection")
                        (STRING  "set")
                        (STRING  "keys")
                        (STRING  "first")
                        (STRING  "<=")
                        (STRING  "count")
                        (STRING  "nth")
                        (STRING  "index")
                        (STRING  "reduce")
                        (STRING  "map-invert")
                        (LONG 2)
                        (STRING  "vals"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__9" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__10" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__11" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__12" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__13" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__14" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 180)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "seq"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.set$join" (class "clojure.lang.Var"))))
                                      (13 (ldc 2))        ;;STRING:: "clojure.set"
                                      (15 (ldc 3))        ;;STRING:: "intersection"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.set$join" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 4))        ;;STRING:: "set"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.set$join" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 5))        ;;STRING:: "keys"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.set$join" (class "clojure.lang.Var"))))
                                      (52 (ldc 0))        ;;STRING:: "clojure.core"
                                      (54 (ldc 6))        ;;STRING:: "first"
                                      (56 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (59 (checkcast (class "clojure.lang.Var")))
                                      (62 (putstatic (fieldCP "const__4" "clojure.set$join" (class "clojure.lang.Var"))))
                                      (65 (ldc 0))        ;;STRING:: "clojure.core"
                                      (67 (ldc 7))        ;;STRING:: "<="
                                      (69 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (72 (checkcast (class "clojure.lang.Var")))
                                      (75 (putstatic (fieldCP "const__5" "clojure.set$join" (class "clojure.lang.Var"))))
                                      (78 (ldc 0))        ;;STRING:: "clojure.core"
                                      (80 (ldc 8))        ;;STRING:: "count"
                                      (82 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (85 (checkcast (class "clojure.lang.Var")))
                                      (88 (putstatic (fieldCP "const__6" "clojure.set$join" (class "clojure.lang.Var"))))
                                      (91 (ldc 0))        ;;STRING:: "clojure.core"
                                      (93 (ldc 9))        ;;STRING:: "nth"
                                      (95 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (98 (checkcast (class "clojure.lang.Var")))
                                      (101 (putstatic (fieldCP "const__7" "clojure.set$join" (class "clojure.lang.Var"))))
                                      (104 (lconst_0))
                                      (105 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (108 (putstatic (fieldCP "const__8" "clojure.set$join" (class "java.lang.Object"))))
                                      (111 (lconst_1))
                                      (112 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (115 (putstatic (fieldCP "const__9" "clojure.set$join" (class "java.lang.Object"))))
                                      (118 (ldc 2))       ;;STRING:: "clojure.set"
                                      (120 (ldc 10))      ;;STRING:: "index"
                                      (122 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (125 (checkcast (class "clojure.lang.Var")))
                                      (128 (putstatic (fieldCP "const__10" "clojure.set$join" (class "clojure.lang.Var"))))
                                      (131 (ldc 0))       ;;STRING:: "clojure.core"
                                      (133 (ldc 11))      ;;STRING:: "reduce"
                                      (135 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (138 (checkcast (class "clojure.lang.Var")))
                                      (141 (putstatic (fieldCP "const__11" "clojure.set$join" (class "clojure.lang.Var"))))
                                      (144 (ldc 2))       ;;STRING:: "clojure.set"
                                      (146 (ldc 12))      ;;STRING:: "map-invert"
                                      (148 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (151 (checkcast (class "clojure.lang.Var")))
                                      (154 (putstatic (fieldCP "const__12" "clojure.set$join" (class "clojure.lang.Var"))))
                                      (157 (ldc2_w 13))   ;; LONG:: "2"
                                      (160 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (163 (putstatic (fieldCP "const__13" "clojure.set$join" (class "java.lang.Object"))))
                                      (166 (ldc 0))       ;;STRING:: "clojure.core"
                                      (168 (ldc 14))      ;;STRING:: "vals"
                                      (170 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (173 (checkcast (class "clojure.lang.Var")))
                                      (176 (putstatic (fieldCP "const__14" "clojure.set$join" (class "clojure.lang.Var"))))
                                      (179 (return))
                                      (endofcode 180))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.AFunction" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 9) (code_length . 202)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (invokestatic (methodCP "count" "clojure.lang.RT" ((class "java.lang.Object")) int))) 
                                      (4 (i2l)) 
                                      (5 (aload_2)) 
                                      (6 (invokestatic (methodCP "count" "clojure.lang.RT" ((class "java.lang.Object")) int))) 
                                      (9 (i2l)) 
                                      (10 (lcmp)) 
                                      (11 (ifgt 57))  ;;to TAG_0
                                      (14 (iconst_3)) 
                                      (15 (anewarray (class "java.lang.Object"))) 
                                      (18 (dup)) 
                                      (19 (iconst_0)) 
                                      (20 (aload_1)) 
                                      (21 (aconst_null)) 
                                      (22 (astore_1)) 
                                      (23 (aastore)) 
                                      (24 (dup)) 
                                      (25 (iconst_1)) 
                                      (26 (aload_2)) 
                                      (27 (aconst_null)) 
                                      (28 (astore_2)) 
                                      (29 (aastore)) 
                                      (30 (dup)) 
                                      (31 (iconst_2)) 
                                      (32 (getstatic (fieldCP "const__12" "clojure.set$join" (class "clojure.lang.Var")))) 
                                      (35 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (38 (checkcast (class "clojure.lang.IFn"))) 
                                      (41 (aload_3)) 
                                      (42 (aconst_null)) 
                                      (43 (astore_3)) 
                                      (44 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (49 (aastore)) 
                                      (50 (invokestatic (methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector")))) 
                                      (53 (goto 82)) ;;to TAG_1
                                      (56 (pop)) 
                                      (57 (iconst_3)) ;;at TAG_0
                                      (58 (anewarray (class "java.lang.Object"))) 
                                      (61 (dup)) 
                                      (62 (iconst_0)) 
                                      (63 (aload_2)) 
                                      (64 (aconst_null)) 
                                      (65 (astore_2)) 
                                      (66 (aastore)) 
                                      (67 (dup)) 
                                      (68 (iconst_1)) 
                                      (69 (aload_1)) 
                                      (70 (aconst_null)) 
                                      (71 (astore_1)) 
                                      (72 (aastore)) 
                                      (73 (dup)) 
                                      (74 (iconst_2)) 
                                      (75 (aload_3)) 
                                      (76 (aconst_null)) 
                                      (77 (astore_3)) 
                                      (78 (aastore)) 
                                      (79 (invokestatic (methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector")))) 
                                      (82 (astore 4)) ;;at TAG_1
                                      (84 (aload 4)) 
                                      (86 (lconst_0)) 
                                      (87 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (90 (aconst_null)) 
                                      (91 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (94 (astore 5)) 
                                      (96 (aload 4)) 
                                      (98 (lconst_1)) 
                                      (99 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (102 (aconst_null)) 
                                      (103 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (106 (astore 6)) 
                                      (108 (aload 4)) 
                                      (110 (aconst_null)) 
                                      (111 (astore 4)) 
                                      (113 (ldc2_w 13)) ;; LONG:: "2"
                                      (116 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (119 (aconst_null)) 
                                      (120 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (123 (astore 7)) 
                                      (125 (getstatic (fieldCP "const__10" "clojure.set$join" (class "clojure.lang.Var")))) 
                                      (128 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (131 (checkcast (class "clojure.lang.IFn"))) 
                                      (134 (aload 5)) 
                                      (136 (aconst_null)) 
                                      (137 (astore 5)) 
                                      (139 (getstatic (fieldCP "const__14" "clojure.set$join" (class "clojure.lang.Var")))) 
                                      (142 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (145 (checkcast (class "clojure.lang.IFn"))) 
                                      (148 (aload 7)) 
                                      (150 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (155 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (160 (astore 8)) 
                                      (162 (getstatic (fieldCP "const__11" "clojure.set$join" (class "clojure.lang.Var")))) 
                                      (165 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (168 (checkcast (class "clojure.lang.IFn"))) 
                                      (171 (new (class "clojure.set$join$fn__6698"))) 
                                      (174 (dup)) 
                                      (175 (aload 7)) 
                                      (177 (aconst_null)) 
                                      (178 (astore 7)) 
                                      (180 (aload 8)) 
                                      (182 (aconst_null)) 
                                      (183 (astore 8)) 
                                      (185 (invokespecial (methodCP "<init>" "clojure.set$join$fn__6698" ((class "java.lang.Object") (class "java.lang.Object")) void))) 
                                      (188 (getstatic (fieldCP "EMPTY" "clojure.lang.PersistentHashSet" (class "clojure.lang.PersistentHashSet")))) 
                                      (191 (aload 6)) 
                                      (193 (aconst_null)) 
                                      (194 (astore 6)) 
                                      (196 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) 
                                      (201 (areturn)) 
                                      (endofcode 202))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 8) (code_length . 312)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.set$join" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (aload_1)) 
                                      (10 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (15 (astore_3)) 
                                      (16 (aload_3)) 
                                      (17 (dup)) 
                                      (18 (ifnull 45)) ;;to TAG_0
                                      (21 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (24 (if_acmpeq 46)) ;;to TAG_1
                                      (27 (getstatic (fieldCP "const__0" "clojure.set$join" (class "clojure.lang.Var")))) 
                                      (30 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (33 (checkcast (class "clojure.lang.IFn"))) 
                                      (36 (aload_2)) 
                                      (37 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (42 (goto 49))  ;;to TAG_2
                                      (45 (pop)) ;;at TAG_0
                                      (46 (aload_3)) ;;at TAG_1
                                      (47 (aconst_null)) 
                                      (48 (astore_3)) 
                                      (49 (dup)) ;;at TAG_2
                                      (50 (ifnull 307)) ;;to TAG_3
                                      (53 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (56 (if_acmpeq 308)) ;;to TAG_4
                                      (59 (getstatic (fieldCP "const__1" "clojure.set$join" (class "clojure.lang.Var")))) 
                                      (62 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (65 (checkcast (class "clojure.lang.IFn"))) 
                                      (68 (getstatic (fieldCP "const__2" "clojure.set$join" (class "clojure.lang.Var")))) 
                                      (71 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (74 (checkcast (class "clojure.lang.IFn"))) 
                                      (77 (getstatic (fieldCP "const__3" "clojure.set$join" (class "clojure.lang.Var")))) 
                                      (80 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (83 (checkcast (class "clojure.lang.IFn"))) 
                                      (86 (getstatic (fieldCP "const__4" "clojure.set$join" (class "clojure.lang.Var")))) 
                                      (89 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (92 (checkcast (class "clojure.lang.IFn"))) 
                                      (95 (aload_1)) 
                                      (96 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (101 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (106 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (111 (getstatic (fieldCP "const__2" "clojure.set$join" (class "clojure.lang.Var")))) 
                                      (114 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (117 (checkcast (class "clojure.lang.IFn"))) 
                                      (120 (getstatic (fieldCP "const__3" "clojure.set$join" (class "clojure.lang.Var")))) 
                                      (123 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (126 (checkcast (class "clojure.lang.IFn"))) 
                                      (129 (getstatic (fieldCP "const__4" "clojure.set$join" (class "clojure.lang.Var")))) 
                                      (132 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (135 (checkcast (class "clojure.lang.IFn"))) 
                                      (138 (aload_2)) 
                                      (139 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (144 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (149 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (154 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (159 (astore_3)) 
                                      (160 (aload_1)) 
                                      (161 (invokestatic (methodCP "count" "clojure.lang.RT" ((class "java.lang.Object")) int))) 
                                      (164 (i2l)) 
                                      (165 (aload_2)) 
                                      (166 (invokestatic (methodCP "count" "clojure.lang.RT" ((class "java.lang.Object")) int))) 
                                      (169 (i2l)) 
                                      (170 (lcmp)) 
                                      (171 (ifgt 197)) ;;to TAG_5
                                      (174 (iconst_2)) 
                                      (175 (anewarray (class "java.lang.Object"))) 
                                      (178 (dup)) 
                                      (179 (iconst_0)) 
                                      (180 (aload_1)) 
                                      (181 (aconst_null)) 
                                      (182 (astore_1)) 
                                      (183 (aastore)) 
                                      (184 (dup)) 
                                      (185 (iconst_1)) 
                                      (186 (aload_2)) 
                                      (187 (aconst_null)) 
                                      (188 (astore_2)) 
                                      (189 (aastore)) 
                                      (190 (invokestatic (methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector")))) 
                                      (193 (goto 216)) ;;to TAG_6
                                      (196 (pop)) 
                                      (197 (iconst_2)) ;;at TAG_5
                                      (198 (anewarray (class "java.lang.Object"))) 
                                      (201 (dup)) 
                                      (202 (iconst_0)) 
                                      (203 (aload_2)) 
                                      (204 (aconst_null)) 
                                      (205 (astore_2)) 
                                      (206 (aastore)) 
                                      (207 (dup)) 
                                      (208 (iconst_1)) 
                                      (209 (aload_1)) 
                                      (210 (aconst_null)) 
                                      (211 (astore_1)) 
                                      (212 (aastore)) 
                                      (213 (invokestatic (methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector")))) 
                                      (216 (astore 4)) ;;at TAG_6
                                      (218 (aload 4)) 
                                      (220 (lconst_0)) 
                                      (221 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (224 (aconst_null)) 
                                      (225 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (228 (astore 5)) 
                                      (230 (aload 4)) 
                                      (232 (aconst_null)) 
                                      (233 (astore 4)) 
                                      (235 (lconst_1)) 
                                      (236 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (239 (aconst_null)) 
                                      (240 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (243 (astore 6)) 
                                      (245 (getstatic (fieldCP "const__10" "clojure.set$join" (class "clojure.lang.Var")))) 
                                      (248 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (251 (checkcast (class "clojure.lang.IFn"))) 
                                      (254 (aload 5)) 
                                      (256 (aconst_null)) 
                                      (257 (astore 5)) 
                                      (259 (aload_3)) 
                                      (260 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (265 (astore 7)) 
                                      (267 (getstatic (fieldCP "const__11" "clojure.set$join" (class "clojure.lang.Var")))) 
                                      (270 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (273 (checkcast (class "clojure.lang.IFn"))) 
                                      (276 (new (class "clojure.set$join$fn__6693"))) 
                                      (279 (dup)) 
                                      (280 (aload_3)) 
                                      (281 (aconst_null)) 
                                      (282 (astore_3)) 
                                      (283 (aload 7)) 
                                      (285 (aconst_null)) 
                                      (286 (astore 7)) 
                                      (288 (invokespecial (methodCP "<init>" "clojure.set$join$fn__6693" ((class "java.lang.Object") (class "java.lang.Object")) void))) 
                                      (291 (getstatic (fieldCP "EMPTY" "clojure.lang.PersistentHashSet" (class "clojure.lang.PersistentHashSet")))) 
                                      (294 (aload 6)) 
                                      (296 (aconst_null)) 
                                      (297 (astore 6)) 
                                      (299 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) 
                                      (304 (goto 311)) ;;to TAG_7
                                      (307 (pop)) ;;at TAG_3
                                      (308 (getstatic (fieldCP "EMPTY" "clojure.lang.PersistentHashSet" (class "clojure.lang.PersistentHashSet")))) ;;at TAG_4
                                      (311 (areturn)) ;;at TAG_7
                                      (endofcode 312))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *set$join-class-table*
  (make-static-class-decls 
   *clojure.set$join*))

(defconst *package-name-map* 
  ("clojure.set$join" . "clojure"))

