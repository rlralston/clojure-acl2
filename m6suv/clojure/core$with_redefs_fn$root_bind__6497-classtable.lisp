; core$with_redefs_fn$root_bind__6497-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:46 CDT 2014.
;

(defconst *clojure.core$with_redefs_fn$root_bind__6497*
 (make-class-def
      '(class "clojure.core$with_redefs_fn$root_bind__6497"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "seq")
                        (STRING  "<")
                        (STRING  "nth")
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
                        (field "const__1" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__9" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__10" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__11" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__12" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 158)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "seq"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$with_redefs_fn$root_bind__6497" (class "clojure.lang.Var"))))
                                      (13 (lconst_0))
                                      (14 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (17 (putstatic (fieldCP "const__1" "clojure.core$with_redefs_fn$root_bind__6497" (class "java.lang.Object"))))
                                      (20 (ldc 0))        ;;STRING:: "clojure.core"
                                      (22 (ldc 2))        ;;STRING:: "<"
                                      (24 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (27 (checkcast (class "clojure.lang.Var")))
                                      (30 (putstatic (fieldCP "const__2" "clojure.core$with_redefs_fn$root_bind__6497" (class "clojure.lang.Var"))))
                                      (33 (ldc 0))        ;;STRING:: "clojure.core"
                                      (35 (ldc 3))        ;;STRING:: "nth"
                                      (37 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (40 (checkcast (class "clojure.lang.Var")))
                                      (43 (putstatic (fieldCP "const__3" "clojure.core$with_redefs_fn$root_bind__6497" (class "clojure.lang.Var"))))
                                      (46 (lconst_1))
                                      (47 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (50 (putstatic (fieldCP "const__4" "clojure.core$with_redefs_fn$root_bind__6497" (class "java.lang.Object"))))
                                      (53 (ldc 0))        ;;STRING:: "clojure.core"
                                      (55 (ldc 4))        ;;STRING:: "unchecked-inc"
                                      (57 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (60 (checkcast (class "clojure.lang.Var")))
                                      (63 (putstatic (fieldCP "const__5" "clojure.core$with_redefs_fn$root_bind__6497" (class "clojure.lang.Var"))))
                                      (66 (ldc 0))        ;;STRING:: "clojure.core"
                                      (68 (ldc 5))        ;;STRING:: "chunked-seq?"
                                      (70 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (73 (checkcast (class "clojure.lang.Var")))
                                      (76 (putstatic (fieldCP "const__6" "clojure.core$with_redefs_fn$root_bind__6497" (class "clojure.lang.Var"))))
                                      (79 (ldc 0))        ;;STRING:: "clojure.core"
                                      (81 (ldc 6))        ;;STRING:: "chunk-first"
                                      (83 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (86 (checkcast (class "clojure.lang.Var")))
                                      (89 (putstatic (fieldCP "const__7" "clojure.core$with_redefs_fn$root_bind__6497" (class "clojure.lang.Var"))))
                                      (92 (ldc 0))        ;;STRING:: "clojure.core"
                                      (94 (ldc 7))        ;;STRING:: "chunk-rest"
                                      (96 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (99 (checkcast (class "clojure.lang.Var")))
                                      (102 (putstatic (fieldCP "const__8" "clojure.core$with_redefs_fn$root_bind__6497" (class "clojure.lang.Var"))))
                                      (105 (ldc 0))       ;;STRING:: "clojure.core"
                                      (107 (ldc 8))       ;;STRING:: "int"
                                      (109 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (112 (checkcast (class "clojure.lang.Var")))
                                      (115 (putstatic (fieldCP "const__9" "clojure.core$with_redefs_fn$root_bind__6497" (class "clojure.lang.Var"))))
                                      (118 (ldc 0))       ;;STRING:: "clojure.core"
                                      (120 (ldc 9))       ;;STRING:: "count"
                                      (122 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (125 (checkcast (class "clojure.lang.Var")))
                                      (128 (putstatic (fieldCP "const__10" "clojure.core$with_redefs_fn$root_bind__6497" (class "clojure.lang.Var"))))
                                      (131 (ldc 0))       ;;STRING:: "clojure.core"
                                      (133 (ldc 10))      ;;STRING:: "first"
                                      (135 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (138 (checkcast (class "clojure.lang.Var")))
                                      (141 (putstatic (fieldCP "const__11" "clojure.core$with_redefs_fn$root_bind__6497" (class "clojure.lang.Var"))))
                                      (144 (ldc 0))       ;;STRING:: "clojure.core"
                                      (146 (ldc 11))      ;;STRING:: "next"
                                      (148 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (151 (checkcast (class "clojure.lang.Var")))
                                      (154 (putstatic (fieldCP "const__12" "clojure.core$with_redefs_fn$root_bind__6497" (class "clojure.lang.Var"))))
                                      (157 (return))
                                      (endofcode 158))
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
                                   (max_stack . 8) (max_locals . 13) (code_length . 347)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$with_redefs_fn$root_bind__6497" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (aload_1)) 
                                      (10 (aconst_null)) 
                                      (11 (astore_1)) 
                                      (12 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (17 (astore_2)) 
                                      (18 (aconst_null)) 
                                      (19 (astore_3)) 
                                      (20 (lconst_0)) 
                                      (21 (lstore 4)) 
                                      (23 (lconst_0)) 
                                      (24 (lstore 6)) 
                                      (26 (lload 6)) ;;at TAG_1
                                      (28 (lload 4)) 
                                      (30 (lcmp)) 
                                      (31 (ifge 116)) ;;to TAG_0
                                      (34 (aload_3)) 
                                      (35 (checkcast (class "clojure.lang.Indexed"))) 
                                      (38 (lload 6)) 
                                      (40 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (43 (invokeinterface (methodCP "nth" "clojure.lang.Indexed" (int) (class "java.lang.Object")) 2)) 
                                      (48 (astore 8)) 
                                      (50 (aload 8)) 
                                      (52 (lconst_0)) 
                                      (53 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (56 (aconst_null)) 
                                      (57 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (60 (astore 9)) 
                                      (62 (aload 8)) 
                                      (64 (aconst_null)) 
                                      (65 (astore 8)) 
                                      (67 (lconst_1)) 
                                      (68 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (71 (aconst_null)) 
                                      (72 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (75 (astore 10)) 
                                      (77 (aload 9)) 
                                      (79 (aconst_null)) 
                                      (80 (astore 9)) 
                                      (82 (checkcast (class "clojure.lang.Var"))) 
                                      (85 (aload 10)) 
                                      (87 (aconst_null)) 
                                      (88 (astore 10)) 
                                      (90 (invokevirtual (methodCP "bindRoot" "clojure.lang.Var" ((class "java.lang.Object")) void))) 
                                      (93 (aconst_null)) 
                                      (94 (pop)) 
                                      (95 (aload_2)) 
                                      (96 (aload_3)) 
                                      (97 (lload 4)) 
                                      (99 (lload 6)) 
                                      (101 (lconst_1)) 
                                      (102 (ladd)) 
                                      (103 (lstore 6)) 
                                      (105 (lstore 4)) 
                                      (107 (astore_3)) 
                                      (108 (astore_2)) 
                                      (109 (goto 26)) ;;to TAG_1
                                      (112 (goto 346))  ;;to TAG_2
                                      (115 (pop)) 
                                      (116 (getstatic (fieldCP "const__0" "clojure.core$with_redefs_fn$root_bind__6497" (class "clojure.lang.Var")))) ;;at TAG_0
                                      (119 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (122 (checkcast (class "clojure.lang.IFn"))) 
                                      (125 (aload_2)) 
                                      (126 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (131 (astore 8)) 
                                      (133 (aload 8)) 
                                      (135 (dup)) 
                                      (136 (ifnull 344)) ;;to TAG_3
                                      (139 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (142 (if_acmpeq 345)) ;;to TAG_4
                                      (145 (aload 8)) 
                                      (147 (aconst_null)) 
                                      (148 (astore 8)) 
                                      (150 (astore 9)) 
                                      (152 (getstatic (fieldCP "const__6" "clojure.core$with_redefs_fn$root_bind__6497" (class "clojure.lang.Var")))) 
                                      (155 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (158 (checkcast (class "clojure.lang.IFn"))) 
                                      (161 (aload 9)) 
                                      (163 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (168 (dup)) 
                                      (169 (ifnull 246)) ;;to TAG_5
                                      (172 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (175 (if_acmpeq 247)) ;;to TAG_6
                                      (178 (getstatic (fieldCP "const__7" "clojure.core$with_redefs_fn$root_bind__6497" (class "clojure.lang.Var")))) 
                                      (181 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (184 (checkcast (class "clojure.lang.IFn"))) 
                                      (187 (aload 9)) 
                                      (189 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (194 (astore 10)) 
                                      (196 (getstatic (fieldCP "const__8" "clojure.core$with_redefs_fn$root_bind__6497" (class "clojure.lang.Var")))) 
                                      (199 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (202 (checkcast (class "clojure.lang.IFn"))) 
                                      (205 (aload 9)) 
                                      (207 (aconst_null)) 
                                      (208 (astore 9)) 
                                      (210 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (215 (aload 10)) 
                                      (217 (aload 10)) 
                                      (219 (aconst_null)) 
                                      (220 (astore 10)) 
                                      (222 (invokestatic (methodCP "count" "clojure.lang.RT" ((class "java.lang.Object")) int))) 
                                      (225 (invokestatic (methodCP "intCast" "clojure.lang.RT" (int) int))) 
                                      (228 (i2l)) 
                                      (229 (lconst_0)) 
                                      (230 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (233 (i2l)) 
                                      (234 (lstore 6)) 
                                      (236 (lstore 4)) 
                                      (238 (astore_3)) 
                                      (239 (astore_2)) 
                                      (240 (goto 26)) ;;to TAG_1
                                      (243 (goto 341)) ;;to TAG_7
                                      (246 (pop)) ;;at TAG_5
                                      (247 (getstatic (fieldCP "const__11" "clojure.core$with_redefs_fn$root_bind__6497" (class "clojure.lang.Var")))) ;;at TAG_6
                                      (250 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (253 (checkcast (class "clojure.lang.IFn"))) 
                                      (256 (aload 9)) 
                                      (258 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (263 (astore 10)) 
                                      (265 (aload 10)) 
                                      (267 (lconst_0)) 
                                      (268 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (271 (aconst_null)) 
                                      (272 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (275 (astore 11)) 
                                      (277 (aload 10)) 
                                      (279 (aconst_null)) 
                                      (280 (astore 10)) 
                                      (282 (lconst_1)) 
                                      (283 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (286 (aconst_null)) 
                                      (287 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (290 (astore 12)) 
                                      (292 (aload 11)) 
                                      (294 (aconst_null)) 
                                      (295 (astore 11)) 
                                      (297 (checkcast (class "clojure.lang.Var"))) 
                                      (300 (aload 12)) 
                                      (302 (aconst_null)) 
                                      (303 (astore 12)) 
                                      (305 (invokevirtual (methodCP "bindRoot" "clojure.lang.Var" ((class "java.lang.Object")) void))) 
                                      (308 (aconst_null)) 
                                      (309 (pop)) 
                                      (310 (getstatic (fieldCP "const__12" "clojure.core$with_redefs_fn$root_bind__6497" (class "clojure.lang.Var")))) 
                                      (313 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (316 (checkcast (class "clojure.lang.IFn"))) 
                                      (319 (aload 9)) 
                                      (321 (aconst_null)) 
                                      (322 (astore 9)) 
                                      (324 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (329 (aconst_null)) 
                                      (330 (lconst_0)) 
                                      (331 (lconst_0)) 
                                      (332 (lstore 6)) 
                                      (334 (lstore 4)) 
                                      (336 (astore_3)) 
                                      (337 (astore_2)) 
                                      (338 (goto 26)) ;;to TAG_1
                                      (341 (goto 346))  ;;to TAG_2;;at TAG_7
                                      (344 (pop)) ;;at TAG_3
                                      (345 (aconst_null)) ;;at TAG_4
                                      (346 (areturn)) ;;at TAG_2
                                      (endofcode 347))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$with_redefs_fn$root_bind__6497-class-table*
  (make-static-class-decls 
   *clojure.core$with_redefs_fn$root_bind__6497*))

(defconst *package-name-map* 
  ("clojure.core$with_redefs_fn$root_bind__6497" . "clojure"))
