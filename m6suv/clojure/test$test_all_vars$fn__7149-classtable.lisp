; test$test_all_vars$fn__7149-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:59 CDT 2014.
;

(defconst *clojure.test$test_all_vars$fn__7149*
 (make-class-def
      '(class "clojure.test$test_all_vars$fn__7149"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "seq")
                        (STRING  "vals")
                        (STRING  "ns-interns")
                        (STRING  "<")
                        (STRING  "test")
                        (STRING  "meta")
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
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
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
                        (field "__thunk__0__" (class "clojure.lang.ILookupThunk") (accessflags  *class*  *static* ) -1)
                        (field "__site__1__" (class "clojure.lang.KeywordLookupSite") (accessflags  *class*  *final*  *static* ) -1)
                        (field "__thunk__1__" (class "clojure.lang.ILookupThunk") (accessflags  *class*  *static* ) -1)
                        (field "each_fixture_fn" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "ns" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 0) (code_length . 229)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "seq"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.test$test_all_vars$fn__7149" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "vals"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.test$test_all_vars$fn__7149" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "ns-interns"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.test$test_all_vars$fn__7149" (class "clojure.lang.Var"))))
                                      (39 (lconst_0))
                                      (40 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (43 (putstatic (fieldCP "const__3" "clojure.test$test_all_vars$fn__7149" (class "java.lang.Object"))))
                                      (46 (ldc 0))        ;;STRING:: "clojure.core"
                                      (48 (ldc 4))        ;;STRING:: "<"
                                      (50 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (53 (checkcast (class "clojure.lang.Var")))
                                      (56 (putstatic (fieldCP "const__4" "clojure.test$test_all_vars$fn__7149" (class "clojure.lang.Var"))))
                                      (59 (aconst_null))
                                      (60 (ldc 5))        ;;STRING:: "test"
                                      (62 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (65 (checkcast (class "clojure.lang.Keyword")))
                                      (68 (putstatic (fieldCP "const__5" "clojure.test$test_all_vars$fn__7149" (class "clojure.lang.Keyword"))))
                                      (71 (ldc 0))        ;;STRING:: "clojure.core"
                                      (73 (ldc 6))        ;;STRING:: "meta"
                                      (75 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (78 (checkcast (class "clojure.lang.Var")))
                                      (81 (putstatic (fieldCP "const__6" "clojure.test$test_all_vars$fn__7149" (class "clojure.lang.Var"))))
                                      (84 (ldc 0))        ;;STRING:: "clojure.core"
                                      (86 (ldc 7))        ;;STRING:: "unchecked-inc"
                                      (88 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (91 (checkcast (class "clojure.lang.Var")))
                                      (94 (putstatic (fieldCP "const__7" "clojure.test$test_all_vars$fn__7149" (class "clojure.lang.Var"))))
                                      (97 (ldc 0))        ;;STRING:: "clojure.core"
                                      (99 (ldc 8))        ;;STRING:: "chunked-seq?"
                                      (101 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (104 (checkcast (class "clojure.lang.Var")))
                                      (107 (putstatic (fieldCP "const__8" "clojure.test$test_all_vars$fn__7149" (class "clojure.lang.Var"))))
                                      (110 (ldc 0))       ;;STRING:: "clojure.core"
                                      (112 (ldc 9))       ;;STRING:: "chunk-first"
                                      (114 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (117 (checkcast (class "clojure.lang.Var")))
                                      (120 (putstatic (fieldCP "const__9" "clojure.test$test_all_vars$fn__7149" (class "clojure.lang.Var"))))
                                      (123 (ldc 0))       ;;STRING:: "clojure.core"
                                      (125 (ldc 10))      ;;STRING:: "chunk-rest"
                                      (127 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (130 (checkcast (class "clojure.lang.Var")))
                                      (133 (putstatic (fieldCP "const__10" "clojure.test$test_all_vars$fn__7149" (class "clojure.lang.Var"))))
                                      (136 (ldc 0))       ;;STRING:: "clojure.core"
                                      (138 (ldc 11))      ;;STRING:: "int"
                                      (140 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (143 (checkcast (class "clojure.lang.Var")))
                                      (146 (putstatic (fieldCP "const__11" "clojure.test$test_all_vars$fn__7149" (class "clojure.lang.Var"))))
                                      (149 (ldc 0))       ;;STRING:: "clojure.core"
                                      (151 (ldc 12))      ;;STRING:: "count"
                                      (153 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (156 (checkcast (class "clojure.lang.Var")))
                                      (159 (putstatic (fieldCP "const__12" "clojure.test$test_all_vars$fn__7149" (class "clojure.lang.Var"))))
                                      (162 (ldc 0))       ;;STRING:: "clojure.core"
                                      (164 (ldc 13))      ;;STRING:: "first"
                                      (166 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (169 (checkcast (class "clojure.lang.Var")))
                                      (172 (putstatic (fieldCP "const__13" "clojure.test$test_all_vars$fn__7149" (class "clojure.lang.Var"))))
                                      (175 (ldc 0))       ;;STRING:: "clojure.core"
                                      (177 (ldc 14))      ;;STRING:: "next"
                                      (179 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (182 (checkcast (class "clojure.lang.Var")))
                                      (185 (putstatic (fieldCP "const__14" "clojure.test$test_all_vars$fn__7149" (class "clojure.lang.Var"))))
                                      (188 (new (class "clojure.lang.KeywordLookupSite")))
                                      (191 (dup))
                                      (192 (aconst_null))
                                      (193 (ldc 5))       ;;STRING:: "test"
                                      (195 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (198 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (201 (dup))
                                      (202 (putstatic (fieldCP "__site__0__" "clojure.test$test_all_vars$fn__7149" (class "clojure.lang.KeywordLookupSite"))))
                                      (205 (putstatic (fieldCP "__thunk__0__" "clojure.test$test_all_vars$fn__7149" (class "clojure.lang.ILookupThunk"))))
                                      (208 (new (class "clojure.lang.KeywordLookupSite")))
                                      (211 (dup))
                                      (212 (aconst_null))
                                      (213 (ldc 5))       ;;STRING:: "test"
                                      (215 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (218 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (221 (dup))
                                      (222 (putstatic (fieldCP "__site__1__" "clojure.test$test_all_vars$fn__7149" (class "clojure.lang.KeywordLookupSite"))))
                                      (225 (putstatic (fieldCP "__thunk__1__" "clojure.test$test_all_vars$fn__7149" (class "clojure.lang.ILookupThunk"))))
                                      (228 (return))
                                      (endofcode 229))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.AFunction" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "each_fixture_fn" "clojure.test$test_all_vars$fn__7149" (class "java.lang.Object"))))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "ns" "clojure.test$test_all_vars$fn__7149" (class "java.lang.Object"))))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 8) (max_locals . 10) (code_length . 474)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.test$test_all_vars$fn__7149" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (getstatic (fieldCP "const__1" "clojure.test$test_all_vars$fn__7149" (class "clojure.lang.Var")))) 
                                      (12 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (15 (checkcast (class "clojure.lang.IFn"))) 
                                      (18 (getstatic (fieldCP "const__2" "clojure.test$test_all_vars$fn__7149" (class "clojure.lang.Var")))) 
                                      (21 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (24 (checkcast (class "clojure.lang.IFn"))) 
                                      (27 (aload_0)) 
                                      (28 (getfield (fieldCP "ns" "clojure.test$test_all_vars$fn__7149" (class "java.lang.Object")))) 
                                      (31 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (36 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (41 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (46 (astore_1)) 
                                      (47 (aconst_null)) 
                                      (48 (astore_2)) 
                                      (49 (lconst_0)) 
                                      (50 (lstore_3)) 
                                      (51 (lconst_0)) 
                                      (52 (lstore 5)) 
                                      (54 (lload 5)) ;;at TAG_6
                                      (56 (lload_3)) 
                                      (57 (lcmp)) 
                                      (58 (ifge 193)) ;;to TAG_0
                                      (61 (aload_2)) 
                                      (62 (checkcast (class "clojure.lang.Indexed"))) 
                                      (65 (lload 5)) 
                                      (67 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (70 (invokeinterface (methodCP "nth" "clojure.lang.Indexed" (int) (class "java.lang.Object")) 2)) 
                                      (75 (astore 7)) 
                                      (77 (getstatic (fieldCP "__thunk__0__" "clojure.test$test_all_vars$fn__7149" (class "clojure.lang.ILookupThunk")))) 
                                      (80 (dup)) 
                                      (81 (getstatic (fieldCP "const__6" "clojure.test$test_all_vars$fn__7149" (class "clojure.lang.Var")))) 
                                      (84 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (87 (checkcast (class "clojure.lang.IFn"))) 
                                      (90 (aload 7)) 
                                      (92 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (97 (dup_x2)) 
                                      (98 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (103 (dup_x2)) 
                                      (104 (if_acmpeq 111)) ;;to TAG_1
                                      (107 (pop)) 
                                      (108 (goto 133)) ;;to TAG_2
                                      (111 (swap)) ;;at TAG_1
                                      (112 (pop)) 
                                      (113 (dup)) 
                                      (114 (getstatic (fieldCP "__site__0__" "clojure.test$test_all_vars$fn__7149" (class "clojure.lang.KeywordLookupSite")))) 
                                      (117 (swap)) 
                                      (118 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (123 (dup)) 
                                      (124 (putstatic (fieldCP "__thunk__0__" "clojure.test$test_all_vars$fn__7149" (class "clojure.lang.ILookupThunk")))) 
                                      (127 (swap)) 
                                      (128 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (133 (dup)) ;;at TAG_2
                                      (134 (ifnull 171)) ;;to TAG_3
                                      (137 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (140 (if_acmpeq 172)) ;;to TAG_4
                                      (143 (aload_0)) 
                                      (144 (getfield (fieldCP "each_fixture_fn" "clojure.test$test_all_vars$fn__7149" (class "java.lang.Object")))) 
                                      (147 (checkcast (class "clojure.lang.IFn"))) 
                                      (150 (new (class "clojure.test$test_all_vars$fn__7149$fn__7154"))) 
                                      (153 (dup)) 
                                      (154 (aload 7)) 
                                      (156 (aconst_null)) 
                                      (157 (astore 7)) 
                                      (159 (invokespecial (methodCP "<init>" "clojure.test$test_all_vars$fn__7149$fn__7154" ((class "java.lang.Object")) void))) 
                                      (162 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (167 (pop)) 
                                      (168 (goto 174)) ;;to TAG_5
                                      (171 (pop)) ;;at TAG_3
                                      (172 (aconst_null)) ;;at TAG_4
                                      (173 (pop)) 
                                      (174 (aload_1)) ;;at TAG_5
                                      (175 (aload_2)) 
                                      (176 (lload_3)) 
                                      (177 (lload 5)) 
                                      (179 (lconst_1)) 
                                      (180 (ladd)) 
                                      (181 (lstore 5)) 
                                      (183 (lstore_3)) 
                                      (184 (astore_2)) 
                                      (185 (astore_1)) 
                                      (186 (goto 54)) ;;to TAG_6
                                      (189 (goto 473)) ;;to TAG_7
                                      (192 (pop)) 
                                      (193 (getstatic (fieldCP "const__0" "clojure.test$test_all_vars$fn__7149" (class "clojure.lang.Var")))) ;;at TAG_0
                                      (196 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (199 (checkcast (class "clojure.lang.IFn"))) 
                                      (202 (aload_1)) 
                                      (203 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (208 (astore 7)) 
                                      (210 (aload 7)) 
                                      (212 (dup)) 
                                      (213 (ifnull 471)) ;;to TAG_8
                                      (216 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (219 (if_acmpeq 472)) ;;to TAG_9
                                      (222 (aload 7)) 
                                      (224 (aconst_null)) 
                                      (225 (astore 7)) 
                                      (227 (astore 8)) 
                                      (229 (getstatic (fieldCP "const__8" "clojure.test$test_all_vars$fn__7149" (class "clojure.lang.Var")))) 
                                      (232 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (235 (checkcast (class "clojure.lang.IFn"))) 
                                      (238 (aload 8)) 
                                      (240 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (245 (dup)) 
                                      (246 (ifnull 322)) ;;to TAG_10
                                      (249 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (252 (if_acmpeq 323)) ;;to TAG_11
                                      (255 (getstatic (fieldCP "const__9" "clojure.test$test_all_vars$fn__7149" (class "clojure.lang.Var")))) 
                                      (258 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (261 (checkcast (class "clojure.lang.IFn"))) 
                                      (264 (aload 8)) 
                                      (266 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (271 (astore 9)) 
                                      (273 (getstatic (fieldCP "const__10" "clojure.test$test_all_vars$fn__7149" (class "clojure.lang.Var")))) 
                                      (276 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (279 (checkcast (class "clojure.lang.IFn"))) 
                                      (282 (aload 8)) 
                                      (284 (aconst_null)) 
                                      (285 (astore 8)) 
                                      (287 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (292 (aload 9)) 
                                      (294 (aload 9)) 
                                      (296 (aconst_null)) 
                                      (297 (astore 9)) 
                                      (299 (invokestatic (methodCP "count" "clojure.lang.RT" ((class "java.lang.Object")) int))) 
                                      (302 (invokestatic (methodCP "intCast" "clojure.lang.RT" (int) int))) 
                                      (305 (i2l)) 
                                      (306 (lconst_0)) 
                                      (307 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (310 (i2l)) 
                                      (311 (lstore 5)) 
                                      (313 (lstore_3)) 
                                      (314 (astore_2)) 
                                      (315 (astore_1)) 
                                      (316 (goto 54)) ;;to TAG_6
                                      (319 (goto 468)) ;;to TAG_12
                                      (322 (pop)) ;;at TAG_10
                                      (323 (getstatic (fieldCP "const__13" "clojure.test$test_all_vars$fn__7149" (class "clojure.lang.Var")))) ;;at TAG_11
                                      (326 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (329 (checkcast (class "clojure.lang.IFn"))) 
                                      (332 (aload 8)) 
                                      (334 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (339 (astore 9)) 
                                      (341 (getstatic (fieldCP "__thunk__1__" "clojure.test$test_all_vars$fn__7149" (class "clojure.lang.ILookupThunk")))) 
                                      (344 (dup)) 
                                      (345 (getstatic (fieldCP "const__6" "clojure.test$test_all_vars$fn__7149" (class "clojure.lang.Var")))) 
                                      (348 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (351 (checkcast (class "clojure.lang.IFn"))) 
                                      (354 (aload 9)) 
                                      (356 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (361 (dup_x2)) 
                                      (362 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (367 (dup_x2)) 
                                      (368 (if_acmpeq 375))  ;;to TAG_13
                                      (371 (pop)) 
                                      (372 (goto 397)) ;;to TAG_14
                                      (375 (swap)) ;;at TAG_13
                                      (376 (pop)) 
                                      (377 (dup)) 
                                      (378 (getstatic (fieldCP "__site__1__" "clojure.test$test_all_vars$fn__7149" (class "clojure.lang.KeywordLookupSite")))) 
                                      (381 (swap)) 
                                      (382 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (387 (dup)) 
                                      (388 (putstatic (fieldCP "__thunk__1__" "clojure.test$test_all_vars$fn__7149" (class "clojure.lang.ILookupThunk")))) 
                                      (391 (swap)) 
                                      (392 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (397 (dup)) ;;at TAG_14
                                      (398 (ifnull 435)) ;;to TAG_15
                                      (401 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (404 (if_acmpeq 436)) ;;to TAG_16
                                      (407 (aload_0)) 
                                      (408 (getfield (fieldCP "each_fixture_fn" "clojure.test$test_all_vars$fn__7149" (class "java.lang.Object")))) 
                                      (411 (checkcast (class "clojure.lang.IFn"))) 
                                      (414 (new (class "clojure.test$test_all_vars$fn__7149$fn__7156"))) 
                                      (417 (dup)) 
                                      (418 (aload 9)) 
                                      (420 (aconst_null)) 
                                      (421 (astore 9)) 
                                      (423 (invokespecial (methodCP "<init>" "clojure.test$test_all_vars$fn__7149$fn__7156" ((class "java.lang.Object")) void))) 
                                      (426 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (431 (pop)) 
                                      (432 (goto 438)) ;;to TAG_17
                                      (435 (pop)) ;;at TAG_15
                                      (436 (aconst_null)) ;;at TAG_16
                                      (437 (pop)) 
                                      (438 (getstatic (fieldCP "const__14" "clojure.test$test_all_vars$fn__7149" (class "clojure.lang.Var")))) ;;at TAG_17
                                      (441 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (444 (checkcast (class "clojure.lang.IFn"))) 
                                      (447 (aload 8)) 
                                      (449 (aconst_null)) 
                                      (450 (astore 8)) 
                                      (452 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (457 (aconst_null)) 
                                      (458 (lconst_0)) 
                                      (459 (lconst_0)) 
                                      (460 (lstore 5)) 
                                      (462 (lstore_3)) 
                                      (463 (astore_2)) 
                                      (464 (astore_1)) 
                                      (465 (goto 54)) ;;to TAG_6
                                      (468 (goto 473)) ;;to TAG_7;;at TAG_12
                                      (471 (pop)) ;;at TAG_8
                                      (472 (aconst_null)) ;;at TAG_9
                                      (473 (areturn)) ;;at TAG_7
                                      (endofcode 474))
                                   (Exceptions )
                                   (StackMap )))
                        (method "swapThunk"
                              (parameters int (class "clojure.lang.ILookupThunk"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 3) (code_length . 39)
                                   (parsedcode
                                      (0 (iload_1)) 
                                      (1 (tableswitch (tableswitchinfo 38 (0 . 1) (24 31))))  ;;to TAG_2;;to TAG_0;;to TAG_1
                                      (24 (aload_2)) ;;at TAG_1
                                      (25 (putstatic (fieldCP "__thunk__0__" "clojure.test$test_all_vars$fn__7149" (class "clojure.lang.ILookupThunk")))) 
                                      (28 (goto 38)) ;;to TAG_0
                                      (31 (aload_2)) ;;at TAG_2
                                      (32 (putstatic (fieldCP "__thunk__1__" "clojure.test$test_all_vars$fn__7149" (class "clojure.lang.ILookupThunk")))) 
                                      (35 (goto 38)) ;;to TAG_0
                                      (38 (return)) ;;at TAG_0
                                      (endofcode 39))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *test$test_all_vars$fn__7149-class-table*
  (make-static-class-decls 
   *clojure.test$test_all_vars$fn__7149*))

(defconst *package-name-map* 
  ("clojure.test$test_all_vars$fn__7149" . "clojure"))

