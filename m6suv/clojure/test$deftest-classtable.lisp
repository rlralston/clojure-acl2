; test$deftest-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:58 CDT 2014.
;

(defconst *clojure.test$deftest*
 (make-class-def
      '(class "clojure.test$deftest"
            "clojure.lang.RestFn"
            (constant_pool
                        (STRING  "clojure.test")
                        (STRING  "*load-tests*")
                        (STRING  "clojure.core")
                        (STRING  "seq")
                        (STRING  "concat")
                        (STRING  "list")
                        (STRING  "def")
                        (STRING  "vary-meta")
                        (STRING  "assoc")
                        (STRING  "test")
                        (STRING  "fn")
                        (STRING  "apply")
                        (STRING  "vector")
                        (STRING  "test-var")
                        (STRING  "var"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__9" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__10" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__11" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__12" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__13" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 180)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.test"
                                      (2 (ldc 1))         ;;STRING:: "*load-tests*"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.test$deftest" (class "clojure.lang.Var"))))
                                      (13 (ldc 2))        ;;STRING:: "clojure.core"
                                      (15 (ldc 3))        ;;STRING:: "seq"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.test$deftest" (class "clojure.lang.Var"))))
                                      (26 (ldc 2))        ;;STRING:: "clojure.core"
                                      (28 (ldc 4))        ;;STRING:: "concat"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.test$deftest" (class "clojure.lang.Var"))))
                                      (39 (ldc 2))        ;;STRING:: "clojure.core"
                                      (41 (ldc 5))        ;;STRING:: "list"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.test$deftest" (class "clojure.lang.Var"))))
                                      (52 (aconst_null))
                                      (53 (ldc 6))        ;;STRING:: "def"
                                      (55 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (58 (checkcast (class "clojure.lang.AFn")))
                                      (61 (putstatic (fieldCP "const__4" "clojure.test$deftest" (class "clojure.lang.AFn"))))
                                      (64 (ldc 2))        ;;STRING:: "clojure.core"
                                      (66 (ldc 7))        ;;STRING:: "vary-meta"
                                      (68 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (71 (checkcast (class "clojure.lang.Var")))
                                      (74 (putstatic (fieldCP "const__5" "clojure.test$deftest" (class "clojure.lang.Var"))))
                                      (77 (ldc 2))        ;;STRING:: "clojure.core"
                                      (79 (ldc 8))        ;;STRING:: "assoc"
                                      (81 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (84 (checkcast (class "clojure.lang.Var")))
                                      (87 (putstatic (fieldCP "const__6" "clojure.test$deftest" (class "clojure.lang.Var"))))
                                      (90 (aconst_null))
                                      (91 (ldc 9))        ;;STRING:: "test"
                                      (93 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (96 (checkcast (class "clojure.lang.Keyword")))
                                      (99 (putstatic (fieldCP "const__7" "clojure.test$deftest" (class "clojure.lang.Keyword"))))
                                      (102 (ldc 2))       ;;STRING:: "clojure.core"
                                      (104 (ldc 10))      ;;STRING:: "fn"
                                      (106 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (109 (checkcast (class "clojure.lang.AFn")))
                                      (112 (putstatic (fieldCP "const__8" "clojure.test$deftest" (class "clojure.lang.AFn"))))
                                      (115 (ldc 2))       ;;STRING:: "clojure.core"
                                      (117 (ldc 11))      ;;STRING:: "apply"
                                      (119 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (122 (checkcast (class "clojure.lang.Var")))
                                      (125 (putstatic (fieldCP "const__9" "clojure.test$deftest" (class "clojure.lang.Var"))))
                                      (128 (ldc 2))       ;;STRING:: "clojure.core"
                                      (130 (ldc 12))      ;;STRING:: "vector"
                                      (132 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (135 (checkcast (class "clojure.lang.Var")))
                                      (138 (putstatic (fieldCP "const__10" "clojure.test$deftest" (class "clojure.lang.Var"))))
                                      (141 (ldc 2))       ;;STRING:: "clojure.core"
                                      (143 (ldc 10))      ;;STRING:: "fn"
                                      (145 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (148 (checkcast (class "clojure.lang.AFn")))
                                      (151 (putstatic (fieldCP "const__11" "clojure.test$deftest" (class "clojure.lang.AFn"))))
                                      (154 (ldc 0))       ;;STRING:: "clojure.test"
                                      (156 (ldc 13))      ;;STRING:: "test-var"
                                      (158 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (161 (checkcast (class "clojure.lang.AFn")))
                                      (164 (putstatic (fieldCP "const__12" "clojure.test$deftest" (class "clojure.lang.AFn"))))
                                      (167 (aconst_null))
                                      (168 (ldc 14))      ;;STRING:: "var"
                                      (170 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (173 (checkcast (class "clojure.lang.AFn")))
                                      (176 (putstatic (fieldCP "const__13" "clojure.test$deftest" (class "clojure.lang.AFn"))))
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
					(methodCP "<init>" "clojure.lang.RestFn" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "doInvoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 20) (max_locals . 5) (code_length . 473)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.test$deftest" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "get" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (dup)) 
                                      (7 (ifnull 470)) ;;to TAG_0
                                      (10 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (13 (if_acmpeq 471)) ;;to TAG_1
                                      (16 (getstatic (fieldCP "const__1" "clojure.test$deftest" (class "clojure.lang.Var")))) 
                                      (19 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (22 (checkcast (class "clojure.lang.IFn"))) 
                                      (25 (getstatic (fieldCP "const__2" "clojure.test$deftest" (class "clojure.lang.Var")))) 
                                      (28 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (31 (checkcast (class "clojure.lang.IFn"))) 
                                      (34 (getstatic (fieldCP "const__3" "clojure.test$deftest" (class "clojure.lang.Var")))) 
                                      (37 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (40 (checkcast (class "clojure.lang.IFn"))) 
                                      (43 (getstatic (fieldCP "const__4" "clojure.test$deftest" (class "clojure.lang.AFn")))) 
                                      (46 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (51 (getstatic (fieldCP "const__3" "clojure.test$deftest" (class "clojure.lang.Var")))) 
                                      (54 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (57 (checkcast (class "clojure.lang.IFn"))) 
                                      (60 (getstatic (fieldCP "const__5" "clojure.test$deftest" (class "clojure.lang.Var")))) 
                                      (63 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (66 (checkcast (class "clojure.lang.IFn"))) 
                                      (69 (aload_3)) 
                                      (70 (getstatic (fieldCP "const__6" "clojure.test$deftest" (class "clojure.lang.Var")))) 
                                      (73 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (76 (getstatic (fieldCP "const__7" "clojure.test$deftest" (class "clojure.lang.Keyword")))) 
                                      (79 (getstatic (fieldCP "const__1" "clojure.test$deftest" (class "clojure.lang.Var")))) 
                                      (82 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (85 (checkcast (class "clojure.lang.IFn"))) 
                                      (88 (getstatic (fieldCP "const__2" "clojure.test$deftest" (class "clojure.lang.Var")))) 
                                      (91 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (94 (checkcast (class "clojure.lang.IFn"))) 
                                      (97 (getstatic (fieldCP "const__3" "clojure.test$deftest" (class "clojure.lang.Var")))) 
                                      (100 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (103 (checkcast (class "clojure.lang.IFn"))) 
                                      (106 (getstatic (fieldCP "const__8" "clojure.test$deftest" (class "clojure.lang.AFn")))) 
                                      (109 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (114 (getstatic (fieldCP "const__3" "clojure.test$deftest" (class "clojure.lang.Var")))) 
                                      (117 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (120 (checkcast (class "clojure.lang.IFn"))) 
                                      (123 (getstatic (fieldCP "const__9" "clojure.test$deftest" (class "clojure.lang.Var")))) 
                                      (126 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (129 (checkcast (class "clojure.lang.IFn"))) 
                                      (132 (getstatic (fieldCP "const__10" "clojure.test$deftest" (class "clojure.lang.Var")))) 
                                      (135 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (138 (getstatic (fieldCP "const__1" "clojure.test$deftest" (class "clojure.lang.Var")))) 
                                      (141 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (144 (checkcast (class "clojure.lang.IFn"))) 
                                      (147 (getstatic (fieldCP "const__2" "clojure.test$deftest" (class "clojure.lang.Var")))) 
                                      (150 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (153 (checkcast (class "clojure.lang.IFn"))) 
                                      (156 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1)) 
                                      (161 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (166 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (171 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (176 (aload 4)) 
                                      (178 (aconst_null)) 
                                      (179 (astore 4)) 
                                      (181 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) 
                                      (186 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (191 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 5)) 
                                      (196 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (201 (getstatic (fieldCP "const__3" "clojure.test$deftest" (class "clojure.lang.Var")))) 
                                      (204 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (207 (checkcast (class "clojure.lang.IFn"))) 
                                      (210 (getstatic (fieldCP "const__1" "clojure.test$deftest" (class "clojure.lang.Var")))) 
                                      (213 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (216 (checkcast (class "clojure.lang.IFn"))) 
                                      (219 (getstatic (fieldCP "const__2" "clojure.test$deftest" (class "clojure.lang.Var")))) 
                                      (222 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (225 (checkcast (class "clojure.lang.IFn"))) 
                                      (228 (getstatic (fieldCP "const__3" "clojure.test$deftest" (class "clojure.lang.Var")))) 
                                      (231 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (234 (checkcast (class "clojure.lang.IFn"))) 
                                      (237 (getstatic (fieldCP "const__11" "clojure.test$deftest" (class "clojure.lang.AFn")))) 
                                      (240 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (245 (getstatic (fieldCP "const__3" "clojure.test$deftest" (class "clojure.lang.Var")))) 
                                      (248 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (251 (checkcast (class "clojure.lang.IFn"))) 
                                      (254 (getstatic (fieldCP "const__9" "clojure.test$deftest" (class "clojure.lang.Var")))) 
                                      (257 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (260 (checkcast (class "clojure.lang.IFn"))) 
                                      (263 (getstatic (fieldCP "const__10" "clojure.test$deftest" (class "clojure.lang.Var")))) 
                                      (266 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (269 (getstatic (fieldCP "const__1" "clojure.test$deftest" (class "clojure.lang.Var")))) 
                                      (272 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (275 (checkcast (class "clojure.lang.IFn"))) 
                                      (278 (getstatic (fieldCP "const__2" "clojure.test$deftest" (class "clojure.lang.Var")))) 
                                      (281 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (284 (checkcast (class "clojure.lang.IFn"))) 
                                      (287 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1)) 
                                      (292 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (297 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (302 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (307 (getstatic (fieldCP "const__3" "clojure.test$deftest" (class "clojure.lang.Var")))) 
                                      (310 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (313 (checkcast (class "clojure.lang.IFn"))) 
                                      (316 (getstatic (fieldCP "const__1" "clojure.test$deftest" (class "clojure.lang.Var")))) 
                                      (319 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (322 (checkcast (class "clojure.lang.IFn"))) 
                                      (325 (getstatic (fieldCP "const__2" "clojure.test$deftest" (class "clojure.lang.Var")))) 
                                      (328 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (331 (checkcast (class "clojure.lang.IFn"))) 
                                      (334 (getstatic (fieldCP "const__3" "clojure.test$deftest" (class "clojure.lang.Var")))) 
                                      (337 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (340 (checkcast (class "clojure.lang.IFn"))) 
                                      (343 (getstatic (fieldCP "const__12" "clojure.test$deftest" (class "clojure.lang.AFn")))) 
                                      (346 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (351 (getstatic (fieldCP "const__3" "clojure.test$deftest" (class "clojure.lang.Var")))) 
                                      (354 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (357 (checkcast (class "clojure.lang.IFn"))) 
                                      (360 (getstatic (fieldCP "const__1" "clojure.test$deftest" (class "clojure.lang.Var")))) 
                                      (363 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (366 (checkcast (class "clojure.lang.IFn"))) 
                                      (369 (getstatic (fieldCP "const__2" "clojure.test$deftest" (class "clojure.lang.Var")))) 
                                      (372 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (375 (checkcast (class "clojure.lang.IFn"))) 
                                      (378 (getstatic (fieldCP "const__3" "clojure.test$deftest" (class "clojure.lang.Var")))) 
                                      (381 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (384 (checkcast (class "clojure.lang.IFn"))) 
                                      (387 (getstatic (fieldCP "const__13" "clojure.test$deftest" (class "clojure.lang.AFn")))) 
                                      (390 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (395 (getstatic (fieldCP "const__3" "clojure.test$deftest" (class "clojure.lang.Var")))) 
                                      (398 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (401 (checkcast (class "clojure.lang.IFn"))) 
                                      (404 (aload_3)) 
                                      (405 (aconst_null)) 
                                      (406 (astore_3)) 
                                      (407 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (412 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (417 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (422 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (427 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (432 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (437 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (442 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) 
                                      (447 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (452 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (457 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) 
                                      (462 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (467 (goto 472))  ;;to TAG_2
                                      (470 (pop)) ;;at TAG_0
                                      (471 (aconst_null)) ;;at TAG_1
                                      (472 (areturn)) ;;at TAG_2
                                      (endofcode 473))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getRequiredArity"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 2)
                                   (parsedcode
                                      (0 (iconst_3))
                                      (1 (ireturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *test$deftest-class-table*
  (make-static-class-decls 
   *clojure.test$deftest*))

(defconst *package-name-map* 
  ("clojure.test$deftest" . "clojure"))
