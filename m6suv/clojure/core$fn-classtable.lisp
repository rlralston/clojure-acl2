; core$fn-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:42 CDT 2014.
;

(defconst *clojure.core$fn*
 (make-class-def
      '(class "clojure.core$fn"
            "clojure.lang.RestFn"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "symbol?")
                        (STRING  "first")
                        (STRING  "next")
                        (STRING  "vector?")
                        (STRING  "list")
                        (STRING  "seq?")
                        (STRING  "seq")
                        (STRING  "str")
                        (STRING  "map")
                        (STRING  "with-meta")
                        (STRING  "list*")
                        (STRING  "fn*")
                        (STRING  "cons")
                        (STRING  "meta")
                        (STRING  "Parameter declaration ")
                        (STRING  " should be a vector")
                        (STRING  "Parameter declaration missing"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__9" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__10" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__11" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__12" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__13" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__14" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 194)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "symbol?"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$fn" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "first"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$fn" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "next"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.core$fn" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 4))        ;;STRING:: "vector?"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.core$fn" (class "clojure.lang.Var"))))
                                      (52 (ldc 0))        ;;STRING:: "clojure.core"
                                      (54 (ldc 5))        ;;STRING:: "list"
                                      (56 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (59 (checkcast (class "clojure.lang.Var")))
                                      (62 (putstatic (fieldCP "const__4" "clojure.core$fn" (class "clojure.lang.Var"))))
                                      (65 (ldc 0))        ;;STRING:: "clojure.core"
                                      (67 (ldc 6))        ;;STRING:: "seq?"
                                      (69 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (72 (checkcast (class "clojure.lang.Var")))
                                      (75 (putstatic (fieldCP "const__5" "clojure.core$fn" (class "clojure.lang.Var"))))
                                      (78 (ldc 0))        ;;STRING:: "clojure.core"
                                      (80 (ldc 7))        ;;STRING:: "seq"
                                      (82 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (85 (checkcast (class "clojure.lang.Var")))
                                      (88 (putstatic (fieldCP "const__6" "clojure.core$fn" (class "clojure.lang.Var"))))
                                      (91 (ldc 0))        ;;STRING:: "clojure.core"
                                      (93 (ldc 8))        ;;STRING:: "str"
                                      (95 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (98 (checkcast (class "clojure.lang.Var")))
                                      (101 (putstatic (fieldCP "const__7" "clojure.core$fn" (class "clojure.lang.Var"))))
                                      (104 (ldc 0))       ;;STRING:: "clojure.core"
                                      (106 (ldc 9))       ;;STRING:: "map"
                                      (108 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (111 (checkcast (class "clojure.lang.Var")))
                                      (114 (putstatic (fieldCP "const__8" "clojure.core$fn" (class "clojure.lang.Var"))))
                                      (117 (ldc 0))       ;;STRING:: "clojure.core"
                                      (119 (ldc 10))      ;;STRING:: "with-meta"
                                      (121 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (124 (checkcast (class "clojure.lang.Var")))
                                      (127 (putstatic (fieldCP "const__9" "clojure.core$fn" (class "clojure.lang.Var"))))
                                      (130 (ldc 0))       ;;STRING:: "clojure.core"
                                      (132 (ldc 11))      ;;STRING:: "list*"
                                      (134 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (137 (checkcast (class "clojure.lang.Var")))
                                      (140 (putstatic (fieldCP "const__10" "clojure.core$fn" (class "clojure.lang.Var"))))
                                      (143 (aconst_null))
                                      (144 (ldc 12))      ;;STRING:: "fn*"
                                      (146 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (149 (checkcast (class "clojure.lang.AFn")))
                                      (152 (putstatic (fieldCP "const__11" "clojure.core$fn" (class "clojure.lang.AFn"))))
                                      (155 (ldc 0))       ;;STRING:: "clojure.core"
                                      (157 (ldc 13))      ;;STRING:: "cons"
                                      (159 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (162 (checkcast (class "clojure.lang.Var")))
                                      (165 (putstatic (fieldCP "const__12" "clojure.core$fn" (class "clojure.lang.Var"))))
                                      (168 (aconst_null))
                                      (169 (ldc 12))      ;;STRING:: "fn*"
                                      (171 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (174 (checkcast (class "clojure.lang.AFn")))
                                      (177 (putstatic (fieldCP "const__13" "clojure.core$fn" (class "clojure.lang.AFn"))))
                                      (180 (ldc 0))       ;;STRING:: "clojure.core"
                                      (182 (ldc 14))      ;;STRING:: "meta"
                                      (184 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (187 (checkcast (class "clojure.lang.Var")))
                                      (190 (putstatic (fieldCP "const__14" "clojure.core$fn" (class "clojure.lang.Var"))))
                                      (193 (return))
                                      (endofcode 194))
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
                                   (max_stack . 7) (max_locals . 9) (code_length . 444)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$fn" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (getstatic (fieldCP "const__1" "clojure.core$fn" (class "clojure.lang.Var")))) 
                                      (12 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (15 (checkcast (class "clojure.lang.IFn"))) 
                                      (18 (aload_3)) 
                                      (19 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (24 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (29 (dup)) 
                                      (30 (ifnull 57)) ;;to TAG_0
                                      (33 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (36 (if_acmpeq 58))  ;;to TAG_1
                                      (39 (getstatic (fieldCP "const__1" "clojure.core$fn" (class "clojure.lang.Var")))) 
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
                                      (73 (getstatic (fieldCP "const__2" "clojure.core$fn" (class "clojure.lang.Var")))) 
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
                                      (99 (getstatic (fieldCP "const__3" "clojure.core$fn" (class "clojure.lang.Var")))) 
                                      (102 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (105 (checkcast (class "clojure.lang.IFn"))) 
                                      (108 (getstatic (fieldCP "const__1" "clojure.core$fn" (class "clojure.lang.Var")))) 
                                      (111 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (114 (checkcast (class "clojure.lang.IFn"))) 
                                      (117 (aload 5)) 
                                      (119 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (124 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (129 (dup)) 
                                      (130 (ifnull 161)) ;;to TAG_6
                                      (133 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (136 (if_acmpeq 162)) ;;to TAG_7
                                      (139 (getstatic (fieldCP "const__4" "clojure.core$fn" (class "clojure.lang.Var")))) 
                                      (142 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (145 (checkcast (class "clojure.lang.IFn"))) 
                                      (148 (aload 5)) 
                                      (150 (aconst_null)) 
                                      (151 (astore 5)) 
                                      (153 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (158 (goto 308)) ;;to TAG_8
                                      (161 (pop)) ;;at TAG_6
                                      (162 (getstatic (fieldCP "const__5" "clojure.core$fn" (class "clojure.lang.Var")))) ;;at TAG_7
                                      (165 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (168 (checkcast (class "clojure.lang.IFn"))) 
                                      (171 (getstatic (fieldCP "const__1" "clojure.core$fn" (class "clojure.lang.Var")))) 
                                      (174 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (177 (checkcast (class "clojure.lang.IFn"))) 
                                      (180 (aload 5)) 
                                      (182 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (187 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (192 (dup)) 
                                      (193 (ifnull 210)) ;;to TAG_9
                                      (196 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (199 (if_acmpeq 211)) ;;to TAG_10
                                      (202 (aload 5)) 
                                      (204 (aconst_null)) 
                                      (205 (astore 5)) 
                                      (207 (goto 308)) ;;to TAG_8
                                      (210 (pop)) ;;at TAG_9
                                      (211 (new (class "java.lang.IllegalArgumentException"))) ;;at TAG_10
                                      (214 (dup)) 
                                      (215 (getstatic (fieldCP "const__6" "clojure.core$fn" (class "clojure.lang.Var")))) 
                                      (218 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (221 (checkcast (class "clojure.lang.IFn"))) 
                                      (224 (aload 5)) 
                                      (226 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (231 (dup)) 
                                      (232 (ifnull 281)) ;;to TAG_11
                                      (235 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (238 (if_acmpeq 282)) ;;to TAG_12
                                      (241 (getstatic (fieldCP "const__7" "clojure.core$fn" (class "clojure.lang.Var")))) 
                                      (244 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (247 (checkcast (class "clojure.lang.IFn"))) 
                                      (250 (ldc 15)) ;;STRING:: "Parameter declaration "
                                      (252 (getstatic (fieldCP "const__1" "clojure.core$fn" (class "clojure.lang.Var")))) 
                                      (255 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (258 (checkcast (class "clojure.lang.IFn"))) 
                                      (261 (aload 5)) 
                                      (263 (aconst_null)) 
                                      (264 (astore 5)) 
                                      (266 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (271 (ldc 16)) ;;STRING:: " should be a vector"
                                      (273 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) 
                                      (278 (goto 298)) ;;to TAG_13
                                      (281 (pop)) ;;at TAG_11
                                      (282 (getstatic (fieldCP "const__7" "clojure.core$fn" (class "clojure.lang.Var")))) ;;at TAG_12
                                      (285 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (288 (checkcast (class "clojure.lang.IFn"))) 
                                      (291 (ldc 17)) ;;STRING:: "Parameter declaration missing"
                                      (293 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (298 (checkcast (class "java.lang.String"))) ;;at TAG_13
                                      (301 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (304 (checkcast (class "java.lang.Throwable"))) 
                                      (307 (athrow)) 
                                      (308 (astore 6)) ;;at TAG_8
                                      (310 (new (class "clojure.core$fn$psig__4562"))) 
                                      (313 (dup)) 
                                      (314 (aload 6)) 
                                      (316 (invokespecial (methodCP "<init>" "clojure.core$fn$psig__4562" ((class "java.lang.Object")) void))) 
                                      (319 (astore 7)) 
                                      (321 (getstatic (fieldCP "const__8" "clojure.core$fn" (class "clojure.lang.Var")))) 
                                      (324 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (327 (checkcast (class "clojure.lang.IFn"))) 
                                      (330 (aload 7)) 
                                      (332 (aconst_null)) 
                                      (333 (astore 7)) 
                                      (335 (aload 6)) 
                                      (337 (aconst_null)) 
                                      (338 (astore 6)) 
                                      (340 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (345 (astore 8)) 
                                      (347 (getstatic (fieldCP "const__9" "clojure.core$fn" (class "clojure.lang.Var")))) 
                                      (350 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (353 (checkcast (class "clojure.lang.IFn"))) 
                                      (356 (aload 4)) 
                                      (358 (dup)) 
                                      (359 (ifnull 398)) ;;to TAG_14
                                      (362 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (365 (if_acmpeq 399)) ;;to TAG_15
                                      (368 (getstatic (fieldCP "const__10" "clojure.core$fn" (class "clojure.lang.Var")))) 
                                      (371 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (374 (checkcast (class "clojure.lang.IFn"))) 
                                      (377 (getstatic (fieldCP "const__11" "clojure.core$fn" (class "clojure.lang.AFn")))) 
                                      (380 (aload 4)) 
                                      (382 (aconst_null)) 
                                      (383 (astore 4)) 
                                      (385 (aload 8)) 
                                      (387 (aconst_null)) 
                                      (388 (astore 8)) 
                                      (390 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) 
                                      (395 (goto 421)) ;;to TAG_16
                                      (398 (pop)) ;;at TAG_14
                                      (399 (getstatic (fieldCP "const__12" "clojure.core$fn" (class "clojure.lang.Var")))) ;;at TAG_15
                                      (402 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (405 (checkcast (class "clojure.lang.IFn"))) 
                                      (408 (getstatic (fieldCP "const__13" "clojure.core$fn" (class "clojure.lang.AFn")))) 
                                      (411 (aload 8)) 
                                      (413 (aconst_null)) 
                                      (414 (astore 8)) 
                                      (416 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (421 (getstatic (fieldCP "const__14" "clojure.core$fn" (class "clojure.lang.Var")))) ;;at TAG_16
                                      (424 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (427 (checkcast (class "clojure.lang.IFn"))) 
                                      (430 (aload_1)) 
                                      (431 (aconst_null)) 
                                      (432 (astore_1)) 
                                      (433 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (438 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (443 (areturn)) 
                                      (endofcode 444))
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


(defconst *core$fn-class-table*
  (make-static-class-decls 
   *clojure.core$fn*))

(defconst *package-name-map* 
  ("clojure.core$fn" . "clojure"))

