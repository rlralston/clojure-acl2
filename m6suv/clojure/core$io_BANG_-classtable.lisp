; core$io_BANG_-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:44 CDT 2014.
;

(defconst *clojure.core$io_BANG_*
 (make-class-def
      '(class "clojure.core$io_BANG_"
            "clojure.lang.RestFn"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "string?")
                        (STRING  "first")
                        (STRING  "next")
                        (STRING  "seq")
                        (STRING  "concat")
                        (STRING  "list")
                        (STRING  "if")
                        (STRING  "clojure.lang.LockingTransaction")
                        (STRING  "isRunning")
                        (STRING  "throw")
                        (STRING  "new")
                        (STRING  "java.lang.IllegalStateException")
                        (STRING  "do")
                        (STRING  "I/O in transaction"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__9" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__10" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__11" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 152)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "string?"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$io_BANG_" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "first"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$io_BANG_" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "next"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.core$io_BANG_" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 4))        ;;STRING:: "seq"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.core$io_BANG_" (class "clojure.lang.Var"))))
                                      (52 (ldc 0))        ;;STRING:: "clojure.core"
                                      (54 (ldc 5))        ;;STRING:: "concat"
                                      (56 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (59 (checkcast (class "clojure.lang.Var")))
                                      (62 (putstatic (fieldCP "const__4" "clojure.core$io_BANG_" (class "clojure.lang.Var"))))
                                      (65 (ldc 0))        ;;STRING:: "clojure.core"
                                      (67 (ldc 6))        ;;STRING:: "list"
                                      (69 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (72 (checkcast (class "clojure.lang.Var")))
                                      (75 (putstatic (fieldCP "const__5" "clojure.core$io_BANG_" (class "clojure.lang.Var"))))
                                      (78 (aconst_null))
                                      (79 (ldc 7))        ;;STRING:: "if"
                                      (81 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (84 (checkcast (class "clojure.lang.AFn")))
                                      (87 (putstatic (fieldCP "const__6" "clojure.core$io_BANG_" (class "clojure.lang.AFn"))))
                                      (90 (ldc 8))        ;;STRING:: "clojure.lang.LockingTransaction"
                                      (92 (ldc 9))        ;;STRING:: "isRunning"
                                      (94 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (97 (checkcast (class "clojure.lang.AFn")))
                                      (100 (putstatic (fieldCP "const__7" "clojure.core$io_BANG_" (class "clojure.lang.AFn"))))
                                      (103 (aconst_null))
                                      (104 (ldc 10))      ;;STRING:: "throw"
                                      (106 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (109 (checkcast (class "clojure.lang.AFn")))
                                      (112 (putstatic (fieldCP "const__8" "clojure.core$io_BANG_" (class "clojure.lang.AFn"))))
                                      (115 (aconst_null))
                                      (116 (ldc 11))      ;;STRING:: "new"
                                      (118 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (121 (checkcast (class "clojure.lang.AFn")))
                                      (124 (putstatic (fieldCP "const__9" "clojure.core$io_BANG_" (class "clojure.lang.AFn"))))
                                      (127 (aconst_null))
                                      (128 (ldc 12))      ;;STRING:: "java.lang.IllegalStateException"
                                      (130 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (133 (checkcast (class "clojure.lang.AFn")))
                                      (136 (putstatic (fieldCP "const__10" "clojure.core$io_BANG_" (class "clojure.lang.AFn"))))
                                      (139 (aconst_null))
                                      (140 (ldc 13))      ;;STRING:: "do"
                                      (142 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (145 (checkcast (class "clojure.lang.AFn")))
                                      (148 (putstatic (fieldCP "const__11" "clojure.core$io_BANG_" (class "clojure.lang.AFn"))))
                                      (151 (return))
                                      (endofcode 152))
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
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 16) (max_locals . 7) (code_length . 447)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$io_BANG_" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (getstatic (fieldCP "const__1" "clojure.core$io_BANG_" (class "clojure.lang.Var")))) 
                                      (12 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (15 (checkcast (class "clojure.lang.IFn"))) 
                                      (18 (aload_3)) 
                                      (19 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (24 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (29 (dup)) 
                                      (30 (ifnull 57)) ;;to TAG_0
                                      (33 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (36 (if_acmpeq 58))  ;;to TAG_1
                                      (39 (getstatic (fieldCP "const__1" "clojure.core$io_BANG_" (class "clojure.lang.Var")))) 
                                      (42 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (45 (checkcast (class "clojure.lang.IFn"))) 
                                      (48 (aload_3)) 
                                      (49 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (54 (goto 59)) ;;to TAG_2
                                      (57 (pop)) ;;at TAG_0
                                      (58 (aconst_null)) ;;at TAG_1
                                      (59 (astore 4)) ;;at TAG_2
                                      (61 (aload 4)) 
                                      (63 (dup)) 
                                      (64 (ifnull 93)) ;;to TAG_3
                                      (67 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (70 (if_acmpeq 94)) ;;to TAG_4
                                      (73 (getstatic (fieldCP "const__2" "clojure.core$io_BANG_" (class "clojure.lang.Var")))) 
                                      (76 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (79 (checkcast (class "clojure.lang.IFn"))) 
                                      (82 (aload_3)) 
                                      (83 (aconst_null)) 
                                      (84 (astore_3)) 
                                      (85 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (90 (goto 97)) ;;to TAG_5
                                      (93 (pop)) ;;at TAG_3
                                      (94 (aload_3)) ;;at TAG_4
                                      (95 (aconst_null)) 
                                      (96 (astore_3)) 
                                      (97 (astore 5)) ;;at TAG_5
                                      (99 (getstatic (fieldCP "const__3" "clojure.core$io_BANG_" (class "clojure.lang.Var")))) 
                                      (102 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (105 (checkcast (class "clojure.lang.IFn"))) 
                                      (108 (getstatic (fieldCP "const__4" "clojure.core$io_BANG_" (class "clojure.lang.Var")))) 
                                      (111 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (114 (checkcast (class "clojure.lang.IFn"))) 
                                      (117 (getstatic (fieldCP "const__5" "clojure.core$io_BANG_" (class "clojure.lang.Var")))) 
                                      (120 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (123 (checkcast (class "clojure.lang.IFn"))) 
                                      (126 (getstatic (fieldCP "const__6" "clojure.core$io_BANG_" (class "clojure.lang.AFn")))) 
                                      (129 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (134 (getstatic (fieldCP "const__5" "clojure.core$io_BANG_" (class "clojure.lang.Var")))) 
                                      (137 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (140 (checkcast (class "clojure.lang.IFn"))) 
                                      (143 (getstatic (fieldCP "const__3" "clojure.core$io_BANG_" (class "clojure.lang.Var")))) 
                                      (146 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (149 (checkcast (class "clojure.lang.IFn"))) 
                                      (152 (getstatic (fieldCP "const__4" "clojure.core$io_BANG_" (class "clojure.lang.Var")))) 
                                      (155 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (158 (checkcast (class "clojure.lang.IFn"))) 
                                      (161 (getstatic (fieldCP "const__5" "clojure.core$io_BANG_" (class "clojure.lang.Var")))) 
                                      (164 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (167 (checkcast (class "clojure.lang.IFn"))) 
                                      (170 (getstatic (fieldCP "const__7" "clojure.core$io_BANG_" (class "clojure.lang.AFn")))) 
                                      (173 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (178 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (183 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (188 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (193 (getstatic (fieldCP "const__5" "clojure.core$io_BANG_" (class "clojure.lang.Var")))) 
                                      (196 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (199 (checkcast (class "clojure.lang.IFn"))) 
                                      (202 (getstatic (fieldCP "const__3" "clojure.core$io_BANG_" (class "clojure.lang.Var")))) 
                                      (205 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (208 (checkcast (class "clojure.lang.IFn"))) 
                                      (211 (getstatic (fieldCP "const__4" "clojure.core$io_BANG_" (class "clojure.lang.Var")))) 
                                      (214 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (217 (checkcast (class "clojure.lang.IFn"))) 
                                      (220 (getstatic (fieldCP "const__5" "clojure.core$io_BANG_" (class "clojure.lang.Var")))) 
                                      (223 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (226 (checkcast (class "clojure.lang.IFn"))) 
                                      (229 (getstatic (fieldCP "const__8" "clojure.core$io_BANG_" (class "clojure.lang.AFn")))) 
                                      (232 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (237 (getstatic (fieldCP "const__5" "clojure.core$io_BANG_" (class "clojure.lang.Var")))) 
                                      (240 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (243 (checkcast (class "clojure.lang.IFn"))) 
                                      (246 (getstatic (fieldCP "const__3" "clojure.core$io_BANG_" (class "clojure.lang.Var")))) 
                                      (249 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (252 (checkcast (class "clojure.lang.IFn"))) 
                                      (255 (getstatic (fieldCP "const__4" "clojure.core$io_BANG_" (class "clojure.lang.Var")))) 
                                      (258 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (261 (checkcast (class "clojure.lang.IFn"))) 
                                      (264 (getstatic (fieldCP "const__5" "clojure.core$io_BANG_" (class "clojure.lang.Var")))) 
                                      (267 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (270 (checkcast (class "clojure.lang.IFn"))) 
                                      (273 (getstatic (fieldCP "const__9" "clojure.core$io_BANG_" (class "clojure.lang.AFn")))) 
                                      (276 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (281 (getstatic (fieldCP "const__5" "clojure.core$io_BANG_" (class "clojure.lang.Var")))) 
                                      (284 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (287 (checkcast (class "clojure.lang.IFn"))) 
                                      (290 (getstatic (fieldCP "const__10" "clojure.core$io_BANG_" (class "clojure.lang.AFn")))) 
                                      (293 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (298 (getstatic (fieldCP "const__5" "clojure.core$io_BANG_" (class "clojure.lang.Var")))) 
                                      (301 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (304 (checkcast (class "clojure.lang.IFn"))) 
                                      (307 (aload 4)) 
                                      (309 (aconst_null)) 
                                      (310 (astore 4)) 
                                      (312 (astore 6)) 
                                      (314 (aload 6)) 
                                      (316 (dup)) 
                                      (317 (ifnull 334)) ;;to TAG_6
                                      (320 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (323 (if_acmpeq 335)) ;;to TAG_7
                                      (326 (aload 6)) 
                                      (328 (aconst_null)) 
                                      (329 (astore 6)) 
                                      (331 (goto 337)) ;;to TAG_8
                                      (334 (pop)) ;;at TAG_6
                                      (335 (ldc 14)) ;;at TAG_7;;STRING:: "I/O in transaction"
                                      (337 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) ;;at TAG_8
                                      (342 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) 
                                      (347 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (352 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (357 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (362 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (367 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (372 (getstatic (fieldCP "const__5" "clojure.core$io_BANG_" (class "clojure.lang.Var")))) 
                                      (375 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (378 (checkcast (class "clojure.lang.IFn"))) 
                                      (381 (getstatic (fieldCP "const__3" "clojure.core$io_BANG_" (class "clojure.lang.Var")))) 
                                      (384 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (387 (checkcast (class "clojure.lang.IFn"))) 
                                      (390 (getstatic (fieldCP "const__4" "clojure.core$io_BANG_" (class "clojure.lang.Var")))) 
                                      (393 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (396 (checkcast (class "clojure.lang.IFn"))) 
                                      (399 (getstatic (fieldCP "const__5" "clojure.core$io_BANG_" (class "clojure.lang.Var")))) 
                                      (402 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (405 (checkcast (class "clojure.lang.IFn"))) 
                                      (408 (getstatic (fieldCP "const__11" "clojure.core$io_BANG_" (class "clojure.lang.AFn")))) 
                                      (411 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (416 (aload 5)) 
                                      (418 (aconst_null)) 
                                      (419 (astore 5)) 
                                      (421 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (426 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (431 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (436 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 5)) 
                                      (441 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (446 (areturn)) 
                                      (endofcode 447))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getRequiredArity"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 2)
                                   (parsedcode
                                      (0 (iconst_2))
                                      (1 (ireturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$io_BANG_-class-table*
  (make-static-class-decls 
   *clojure.core$io_BANG_*))

(defconst *package-name-map* 
  ("clojure.core$io_BANG_" . "clojure"))

