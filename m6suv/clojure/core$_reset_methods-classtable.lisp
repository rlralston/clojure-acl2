; core$_reset_methods-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:46 CDT 2014.
;

(defconst *clojure.core$_reset_methods*
 (make-class-def
      '(class "clojure.core$_reset_methods"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "seq")
                        (STRING  "method-builders")
                        (STRING  "<")
                        (STRING  "nth")
                        (STRING  "keyword")
                        (STRING  "unchecked-inc")
                        (STRING  "chunked-seq?")
                        (STRING  "chunk-first")
                        (STRING  "chunk-rest")
                        (STRING  "int")
                        (STRING  "count")
                        (STRING  "first")
                        (STRING  "next"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__9" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__10" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__11" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__12" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__13" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__14" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "__site__0__" (class "clojure.lang.KeywordLookupSite") (accessflags  *class*  *final*  *static* ) -1)
                        (field "__thunk__0__" (class "clojure.lang.ILookupThunk") (accessflags  *class*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 0) (code_length . 203)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "seq"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$_reset_methods" (class "clojure.lang.Var"))))
                                      (13 (aconst_null))
                                      (14 (ldc 2))        ;;STRING:: "method-builders"
                                      (16 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (19 (checkcast (class "clojure.lang.Keyword")))
                                      (22 (putstatic (fieldCP "const__1" "clojure.core$_reset_methods" (class "clojure.lang.Keyword"))))
                                      (25 (lconst_0))
                                      (26 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (29 (putstatic (fieldCP "const__2" "clojure.core$_reset_methods" (class "java.lang.Object"))))
                                      (32 (ldc 0))        ;;STRING:: "clojure.core"
                                      (34 (ldc 3))        ;;STRING:: "<"
                                      (36 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (39 (checkcast (class "clojure.lang.Var")))
                                      (42 (putstatic (fieldCP "const__3" "clojure.core$_reset_methods" (class "clojure.lang.Var"))))
                                      (45 (ldc 0))        ;;STRING:: "clojure.core"
                                      (47 (ldc 4))        ;;STRING:: "nth"
                                      (49 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (52 (checkcast (class "clojure.lang.Var")))
                                      (55 (putstatic (fieldCP "const__4" "clojure.core$_reset_methods" (class "clojure.lang.Var"))))
                                      (58 (lconst_1))
                                      (59 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (62 (putstatic (fieldCP "const__5" "clojure.core$_reset_methods" (class "java.lang.Object"))))
                                      (65 (ldc 0))        ;;STRING:: "clojure.core"
                                      (67 (ldc 5))        ;;STRING:: "keyword"
                                      (69 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (72 (checkcast (class "clojure.lang.Var")))
                                      (75 (putstatic (fieldCP "const__6" "clojure.core$_reset_methods" (class "clojure.lang.Var"))))
                                      (78 (ldc 0))        ;;STRING:: "clojure.core"
                                      (80 (ldc 6))        ;;STRING:: "unchecked-inc"
                                      (82 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (85 (checkcast (class "clojure.lang.Var")))
                                      (88 (putstatic (fieldCP "const__7" "clojure.core$_reset_methods" (class "clojure.lang.Var"))))
                                      (91 (ldc 0))        ;;STRING:: "clojure.core"
                                      (93 (ldc 7))        ;;STRING:: "chunked-seq?"
                                      (95 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (98 (checkcast (class "clojure.lang.Var")))
                                      (101 (putstatic (fieldCP "const__8" "clojure.core$_reset_methods" (class "clojure.lang.Var"))))
                                      (104 (ldc 0))       ;;STRING:: "clojure.core"
                                      (106 (ldc 8))       ;;STRING:: "chunk-first"
                                      (108 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (111 (checkcast (class "clojure.lang.Var")))
                                      (114 (putstatic (fieldCP "const__9" "clojure.core$_reset_methods" (class "clojure.lang.Var"))))
                                      (117 (ldc 0))       ;;STRING:: "clojure.core"
                                      (119 (ldc 9))       ;;STRING:: "chunk-rest"
                                      (121 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (124 (checkcast (class "clojure.lang.Var")))
                                      (127 (putstatic (fieldCP "const__10" "clojure.core$_reset_methods" (class "clojure.lang.Var"))))
                                      (130 (ldc 0))       ;;STRING:: "clojure.core"
                                      (132 (ldc 10))      ;;STRING:: "int"
                                      (134 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (137 (checkcast (class "clojure.lang.Var")))
                                      (140 (putstatic (fieldCP "const__11" "clojure.core$_reset_methods" (class "clojure.lang.Var"))))
                                      (143 (ldc 0))       ;;STRING:: "clojure.core"
                                      (145 (ldc 11))      ;;STRING:: "count"
                                      (147 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (150 (checkcast (class "clojure.lang.Var")))
                                      (153 (putstatic (fieldCP "const__12" "clojure.core$_reset_methods" (class "clojure.lang.Var"))))
                                      (156 (ldc 0))       ;;STRING:: "clojure.core"
                                      (158 (ldc 12))      ;;STRING:: "first"
                                      (160 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (163 (checkcast (class "clojure.lang.Var")))
                                      (166 (putstatic (fieldCP "const__13" "clojure.core$_reset_methods" (class "clojure.lang.Var"))))
                                      (169 (ldc 0))       ;;STRING:: "clojure.core"
                                      (171 (ldc 13))      ;;STRING:: "next"
                                      (173 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (176 (checkcast (class "clojure.lang.Var")))
                                      (179 (putstatic (fieldCP "const__14" "clojure.core$_reset_methods" (class "clojure.lang.Var"))))
                                      (182 (new (class "clojure.lang.KeywordLookupSite")))
                                      (185 (dup))
                                      (186 (aconst_null))
                                      (187 (ldc 2))       ;;STRING:: "method-builders"
                                      (189 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (192 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (195 (dup))
                                      (196 (putstatic (fieldCP "__site__0__" "clojure.core$_reset_methods" (class "clojure.lang.KeywordLookupSite"))))
                                      (199 (putstatic (fieldCP "__thunk__0__" "clojure.core$_reset_methods" (class "clojure.lang.ILookupThunk"))))
                                      (202 (return))
                                      (endofcode 203))
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
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 8) (max_locals . 14) (code_length . 487)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$_reset_methods" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (getstatic (fieldCP "__thunk__0__" "clojure.core$_reset_methods" (class "clojure.lang.ILookupThunk")))) 
                                      (12 (dup)) 
                                      (13 (aload_1)) 
                                      (14 (dup_x2)) 
                                      (15 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (20 (dup_x2)) 
                                      (21 (if_acmpeq 28)) ;;to TAG_0
                                      (24 (pop)) 
                                      (25 (goto 50))  ;;to TAG_1
                                      (28 (swap)) ;;at TAG_0
                                      (29 (pop)) 
                                      (30 (dup)) 
                                      (31 (getstatic (fieldCP "__site__0__" "clojure.core$_reset_methods" (class "clojure.lang.KeywordLookupSite")))) 
                                      (34 (swap)) 
                                      (35 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (40 (dup)) 
                                      (41 (putstatic (fieldCP "__thunk__0__" "clojure.core$_reset_methods" (class "clojure.lang.ILookupThunk")))) 
                                      (44 (swap)) 
                                      (45 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (50 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) ;;at TAG_1
                                      (55 (astore_2)) 
                                      (56 (aconst_null)) 
                                      (57 (astore_3)) 
                                      (58 (lconst_0)) 
                                      (59 (lstore 4)) 
                                      (61 (lconst_0)) 
                                      (62 (lstore 6)) 
                                      (64 (lload 6)) ;;at TAG_3
                                      (66 (lload 4)) 
                                      (68 (lcmp)) 
                                      (69 (ifge 205)) ;;to TAG_2
                                      (72 (aload_3)) 
                                      (73 (checkcast (class "clojure.lang.Indexed"))) 
                                      (76 (lload 6)) 
                                      (78 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (81 (invokeinterface (methodCP "nth" "clojure.lang.Indexed" (int) (class "java.lang.Object")) 2)) 
                                      (86 (astore 8)) 
                                      (88 (aload 8)) 
                                      (90 (lconst_0)) 
                                      (91 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (94 (aconst_null)) 
                                      (95 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (98 (astore 9)) 
                                      (100 (aload 8)) 
                                      (102 (aconst_null)) 
                                      (103 (astore 8)) 
                                      (105 (lconst_1)) 
                                      (106 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (109 (aconst_null)) 
                                      (110 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (113 (astore 10)) 
                                      (115 (new (class "clojure.lang.MethodImplCache"))) 
                                      (118 (dup)) 
                                      (119 (aload_1)) 
                                      (120 (checkcast (class "clojure.lang.IPersistentMap"))) 
                                      (123 (getstatic (fieldCP "const__6" "clojure.core$_reset_methods" (class "clojure.lang.Var")))) 
                                      (126 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (129 (checkcast (class "clojure.lang.IFn"))) 
                                      (132 (aload 9)) 
                                      (134 (checkcast (class "clojure.lang.Var"))) 
                                      (137 (getfield (fieldCP "sym" "clojure.lang.Var" (class "clojure.lang.Symbol")))) 
                                      (140 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (145 (checkcast (class "clojure.lang.Keyword"))) 
                                      (148 (invokespecial (methodCP "<init>" "clojure.lang.MethodImplCache" ((class "clojure.lang.IPersistentMap") (class "clojure.lang.Keyword")) void))) 
                                      (151 (astore 11)) 
                                      (153 (aload 9)) 
                                      (155 (aconst_null)) 
                                      (156 (astore 9)) 
                                      (158 (checkcast (class "clojure.lang.Var"))) 
                                      (161 (aload 10)) 
                                      (163 (aconst_null)) 
                                      (164 (astore 10)) 
                                      (166 (checkcast (class "clojure.lang.IFn"))) 
                                      (169 (aload 11)) 
                                      (171 (aconst_null)) 
                                      (172 (astore 11)) 
                                      (174 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (179 (invokevirtual (methodCP "bindRoot" "clojure.lang.Var" ((class "java.lang.Object")) void))) 
                                      (182 (aconst_null)) 
                                      (183 (pop)) 
                                      (184 (aload_2)) 
                                      (185 (aload_3)) 
                                      (186 (lload 4)) 
                                      (188 (lload 6)) 
                                      (190 (lconst_1)) 
                                      (191 (ladd)) 
                                      (192 (lstore 6)) 
                                      (194 (lstore 4)) 
                                      (196 (astore_3)) 
                                      (197 (astore_2)) 
                                      (198 (goto 64)) ;;to TAG_3
                                      (201 (goto 486)) ;;to TAG_4
                                      (204 (pop)) 
                                      (205 (getstatic (fieldCP "const__0" "clojure.core$_reset_methods" (class "clojure.lang.Var")))) ;;at TAG_2
                                      (208 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (211 (checkcast (class "clojure.lang.IFn"))) 
                                      (214 (aload_2)) 
                                      (215 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (220 (astore 8)) 
                                      (222 (aload 8)) 
                                      (224 (dup)) 
                                      (225 (ifnull 484)) ;;to TAG_5
                                      (228 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (231 (if_acmpeq 485)) ;;to TAG_6
                                      (234 (aload 8)) 
                                      (236 (aconst_null)) 
                                      (237 (astore 8)) 
                                      (239 (astore 9)) 
                                      (241 (getstatic (fieldCP "const__8" "clojure.core$_reset_methods" (class "clojure.lang.Var")))) 
                                      (244 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (247 (checkcast (class "clojure.lang.IFn"))) 
                                      (250 (aload 9)) 
                                      (252 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (257 (dup)) 
                                      (258 (ifnull 335)) ;;to TAG_7
                                      (261 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (264 (if_acmpeq 336)) ;;to TAG_8
                                      (267 (getstatic (fieldCP "const__9" "clojure.core$_reset_methods" (class "clojure.lang.Var")))) 
                                      (270 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (273 (checkcast (class "clojure.lang.IFn"))) 
                                      (276 (aload 9)) 
                                      (278 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (283 (astore 10)) 
                                      (285 (getstatic (fieldCP "const__10" "clojure.core$_reset_methods" (class "clojure.lang.Var")))) 
                                      (288 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (291 (checkcast (class "clojure.lang.IFn"))) 
                                      (294 (aload 9)) 
                                      (296 (aconst_null)) 
                                      (297 (astore 9)) 
                                      (299 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (304 (aload 10)) 
                                      (306 (aload 10)) 
                                      (308 (aconst_null)) 
                                      (309 (astore 10)) 
                                      (311 (invokestatic (methodCP "count" "clojure.lang.RT" ((class "java.lang.Object")) int))) 
                                      (314 (invokestatic (methodCP "intCast" "clojure.lang.RT" (int) int))) 
                                      (317 (i2l)) 
                                      (318 (lconst_0)) 
                                      (319 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (322 (i2l)) 
                                      (323 (lstore 6)) 
                                      (325 (lstore 4)) 
                                      (327 (astore_3)) 
                                      (328 (astore_2)) 
                                      (329 (goto 64)) ;;to TAG_3
                                      (332 (goto 481)) ;;to TAG_9
                                      (335 (pop)) ;;at TAG_7
                                      (336 (getstatic (fieldCP "const__13" "clojure.core$_reset_methods" (class "clojure.lang.Var")))) ;;at TAG_8
                                      (339 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (342 (checkcast (class "clojure.lang.IFn"))) 
                                      (345 (aload 9)) 
                                      (347 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (352 (astore 10)) 
                                      (354 (aload 10)) 
                                      (356 (lconst_0)) 
                                      (357 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (360 (aconst_null)) 
                                      (361 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (364 (astore 11)) 
                                      (366 (aload 10)) 
                                      (368 (aconst_null)) 
                                      (369 (astore 10)) 
                                      (371 (lconst_1)) 
                                      (372 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (375 (aconst_null)) 
                                      (376 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (379 (astore 12)) 
                                      (381 (new (class "clojure.lang.MethodImplCache"))) 
                                      (384 (dup)) 
                                      (385 (aload_1)) 
                                      (386 (checkcast (class "clojure.lang.IPersistentMap"))) 
                                      (389 (getstatic (fieldCP "const__6" "clojure.core$_reset_methods" (class "clojure.lang.Var")))) 
                                      (392 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (395 (checkcast (class "clojure.lang.IFn"))) 
                                      (398 (aload 11)) 
                                      (400 (checkcast (class "clojure.lang.Var"))) 
                                      (403 (getfield (fieldCP "sym" "clojure.lang.Var" (class "clojure.lang.Symbol")))) 
                                      (406 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (411 (checkcast (class "clojure.lang.Keyword"))) 
                                      (414 (invokespecial (methodCP "<init>" "clojure.lang.MethodImplCache" ((class "clojure.lang.IPersistentMap") (class "clojure.lang.Keyword")) void))) 
                                      (417 (astore 13)) 
                                      (419 (aload 11)) 
                                      (421 (aconst_null)) 
                                      (422 (astore 11)) 
                                      (424 (checkcast (class "clojure.lang.Var"))) 
                                      (427 (aload 12)) 
                                      (429 (aconst_null)) 
                                      (430 (astore 12)) 
                                      (432 (checkcast (class "clojure.lang.IFn"))) 
                                      (435 (aload 13)) 
                                      (437 (aconst_null)) 
                                      (438 (astore 13)) 
                                      (440 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (445 (invokevirtual (methodCP "bindRoot" "clojure.lang.Var" ((class "java.lang.Object")) void))) 
                                      (448 (aconst_null)) 
                                      (449 (pop)) 
                                      (450 (getstatic (fieldCP "const__14" "clojure.core$_reset_methods" (class "clojure.lang.Var")))) 
                                      (453 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (456 (checkcast (class "clojure.lang.IFn"))) 
                                      (459 (aload 9)) 
                                      (461 (aconst_null)) 
                                      (462 (astore 9)) 
                                      (464 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (469 (aconst_null)) 
                                      (470 (lconst_0)) 
                                      (471 (lconst_0)) 
                                      (472 (lstore 6)) 
                                      (474 (lstore 4)) 
                                      (476 (astore_3)) 
                                      (477 (astore_2)) 
                                      (478 (goto 64)) ;;to TAG_3
                                      (481 (goto 486)) ;;to TAG_4;;at TAG_9
                                      (484 (pop)) ;;at TAG_5
                                      (485 (aconst_null)) ;;at TAG_6
                                      (486 (areturn)) ;;at TAG_4
                                      (endofcode 487))
                                   (Exceptions )
                                   (StackMap )))
                        (method "swapThunk"
                              (parameters int (class "clojure.lang.ILookupThunk"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 3) (code_length . 28)
                                   (parsedcode
                                      (0 (iload_1)) 
                                      (1 (tableswitch (tableswitchinfo 27 (0 . 0) (20))))  ;;to TAG_0;;to TAG_1
                                      (20 (aload_2)) ;;at TAG_1
                                      (21 (putstatic (fieldCP "__thunk__0__" "clojure.core$_reset_methods" (class "clojure.lang.ILookupThunk")))) 
                                      (24 (goto 27))  ;;to TAG_0
                                      (27 (return)) ;;at TAG_0
                                      (endofcode 28))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$_reset_methods-class-table*
  (make-static-class-decls 
   *clojure.core$_reset_methods*))

(defconst *package-name-map* 
  ("clojure.core$_reset_methods" . "clojure"))

