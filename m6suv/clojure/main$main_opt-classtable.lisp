; main$main_opt-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:53 CDT 2014.
;

(defconst *clojure.main$main_opt*
 (make-class-def
      '(class "clojure.main$main_opt"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "nth")
                        (STRING  "nthnext")
                        (LONG 2)
                        (STRING  "push-thread-bindings")
                        (STRING  "hash-map")
                        (STRING  "*ns*")
                        (STRING  "*warn-on-reflection*")
                        (STRING  "*math-context*")
                        (STRING  "*print-meta*")
                        (STRING  "*print-length*")
                        (STRING  "*print-level*")
                        (STRING  "*data-readers*")
                        (STRING  "*compile-path*")
                        (STRING  "*command-line-args*")
                        (STRING  "*unchecked-math*")
                        (STRING  "*assert*")
                        (STRING  "*1")
                        (STRING  "*2")
                        (STRING  "*3")
                        (STRING  "*e")
                        (STRING  "clojure.main")
                        (STRING  "initialize")
                        (STRING  "apply")
                        (STRING  "ns-resolve")
                        (STRING  "symbol")
                        (STRING  "require")
                        (STRING  "-main")
                        (STRING  "pop-thread-bindings")
                        (STRING  "clojure.compile.path")
                        (STRING  "classes"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__9" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__10" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__11" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__12" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__13" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__14" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__15" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__16" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__17" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__18" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__19" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__20" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__21" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__22" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__23" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__24" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__25" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__26" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__27" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__28" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 361)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "nth"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.main$main_opt" (class "clojure.lang.Var"))))
                                      (13 (lconst_0))
                                      (14 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (17 (putstatic (fieldCP "const__1" "clojure.main$main_opt" (class "java.lang.Object"))))
                                      (20 (lconst_1))
                                      (21 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (24 (putstatic (fieldCP "const__2" "clojure.main$main_opt" (class "java.lang.Object"))))
                                      (27 (ldc 0))        ;;STRING:: "clojure.core"
                                      (29 (ldc 2))        ;;STRING:: "nthnext"
                                      (31 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (34 (checkcast (class "clojure.lang.Var")))
                                      (37 (putstatic (fieldCP "const__3" "clojure.main$main_opt" (class "clojure.lang.Var"))))
                                      (40 (ldc2_w 3))     ;; LONG:: "2"
                                      (43 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (46 (putstatic (fieldCP "const__4" "clojure.main$main_opt" (class "java.lang.Object"))))
                                      (49 (ldc 0))        ;;STRING:: "clojure.core"
                                      (51 (ldc 4))        ;;STRING:: "push-thread-bindings"
                                      (53 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (56 (checkcast (class "clojure.lang.Var")))
                                      (59 (putstatic (fieldCP "const__5" "clojure.main$main_opt" (class "clojure.lang.Var"))))
                                      (62 (ldc 0))        ;;STRING:: "clojure.core"
                                      (64 (ldc 5))        ;;STRING:: "hash-map"
                                      (66 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (69 (checkcast (class "clojure.lang.Var")))
                                      (72 (putstatic (fieldCP "const__6" "clojure.main$main_opt" (class "clojure.lang.Var"))))
                                      (75 (ldc 0))        ;;STRING:: "clojure.core"
                                      (77 (ldc 6))        ;;STRING:: "*ns*"
                                      (79 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (82 (checkcast (class "clojure.lang.Var")))
                                      (85 (putstatic (fieldCP "const__7" "clojure.main$main_opt" (class "clojure.lang.Var"))))
                                      (88 (ldc 0))        ;;STRING:: "clojure.core"
                                      (90 (ldc 7))        ;;STRING:: "*warn-on-reflection*"
                                      (92 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (95 (checkcast (class "clojure.lang.Var")))
                                      (98 (putstatic (fieldCP "const__8" "clojure.main$main_opt" (class "clojure.lang.Var"))))
                                      (101 (ldc 0))       ;;STRING:: "clojure.core"
                                      (103 (ldc 8))       ;;STRING:: "*math-context*"
                                      (105 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (108 (checkcast (class "clojure.lang.Var")))
                                      (111 (putstatic (fieldCP "const__9" "clojure.main$main_opt" (class "clojure.lang.Var"))))
                                      (114 (ldc 0))       ;;STRING:: "clojure.core"
                                      (116 (ldc 9))       ;;STRING:: "*print-meta*"
                                      (118 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (121 (checkcast (class "clojure.lang.Var")))
                                      (124 (putstatic (fieldCP "const__10" "clojure.main$main_opt" (class "clojure.lang.Var"))))
                                      (127 (ldc 0))       ;;STRING:: "clojure.core"
                                      (129 (ldc 10))      ;;STRING:: "*print-length*"
                                      (131 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (134 (checkcast (class "clojure.lang.Var")))
                                      (137 (putstatic (fieldCP "const__11" "clojure.main$main_opt" (class "clojure.lang.Var"))))
                                      (140 (ldc 0))       ;;STRING:: "clojure.core"
                                      (142 (ldc 11))      ;;STRING:: "*print-level*"
                                      (144 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (147 (checkcast (class "clojure.lang.Var")))
                                      (150 (putstatic (fieldCP "const__12" "clojure.main$main_opt" (class "clojure.lang.Var"))))
                                      (153 (ldc 0))       ;;STRING:: "clojure.core"
                                      (155 (ldc 12))      ;;STRING:: "*data-readers*"
                                      (157 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (160 (checkcast (class "clojure.lang.Var")))
                                      (163 (putstatic (fieldCP "const__13" "clojure.main$main_opt" (class "clojure.lang.Var"))))
                                      (166 (ldc 0))       ;;STRING:: "clojure.core"
                                      (168 (ldc 13))      ;;STRING:: "*compile-path*"
                                      (170 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (173 (checkcast (class "clojure.lang.Var")))
                                      (176 (putstatic (fieldCP "const__14" "clojure.main$main_opt" (class "clojure.lang.Var"))))
                                      (179 (ldc 0))       ;;STRING:: "clojure.core"
                                      (181 (ldc 14))      ;;STRING:: "*command-line-args*"
                                      (183 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (186 (checkcast (class "clojure.lang.Var")))
                                      (189 (putstatic (fieldCP "const__15" "clojure.main$main_opt" (class "clojure.lang.Var"))))
                                      (192 (ldc 0))       ;;STRING:: "clojure.core"
                                      (194 (ldc 15))      ;;STRING:: "*unchecked-math*"
                                      (196 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (199 (checkcast (class "clojure.lang.Var")))
                                      (202 (putstatic (fieldCP "const__16" "clojure.main$main_opt" (class "clojure.lang.Var"))))
                                      (205 (ldc 0))       ;;STRING:: "clojure.core"
                                      (207 (ldc 16))      ;;STRING:: "*assert*"
                                      (209 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (212 (checkcast (class "clojure.lang.Var")))
                                      (215 (putstatic (fieldCP "const__17" "clojure.main$main_opt" (class "clojure.lang.Var"))))
                                      (218 (ldc 0))       ;;STRING:: "clojure.core"
                                      (220 (ldc 17))      ;;STRING:: "*1"
                                      (222 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (225 (checkcast (class "clojure.lang.Var")))
                                      (228 (putstatic (fieldCP "const__18" "clojure.main$main_opt" (class "clojure.lang.Var"))))
                                      (231 (ldc 0))       ;;STRING:: "clojure.core"
                                      (233 (ldc 18))      ;;STRING:: "*2"
                                      (235 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (238 (checkcast (class "clojure.lang.Var")))
                                      (241 (putstatic (fieldCP "const__19" "clojure.main$main_opt" (class "clojure.lang.Var"))))
                                      (244 (ldc 0))       ;;STRING:: "clojure.core"
                                      (246 (ldc 19))      ;;STRING:: "*3"
                                      (248 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (251 (checkcast (class "clojure.lang.Var")))
                                      (254 (putstatic (fieldCP "const__20" "clojure.main$main_opt" (class "clojure.lang.Var"))))
                                      (257 (ldc 0))       ;;STRING:: "clojure.core"
                                      (259 (ldc 20))      ;;STRING:: "*e"
                                      (261 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (264 (checkcast (class "clojure.lang.Var")))
                                      (267 (putstatic (fieldCP "const__21" "clojure.main$main_opt" (class "clojure.lang.Var"))))
                                      (270 (ldc 21))      ;;STRING:: "clojure.main"
                                      (272 (ldc 22))      ;;STRING:: "initialize"
                                      (274 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (277 (checkcast (class "clojure.lang.Var")))
                                      (280 (putstatic (fieldCP "const__22" "clojure.main$main_opt" (class "clojure.lang.Var"))))
                                      (283 (ldc 0))       ;;STRING:: "clojure.core"
                                      (285 (ldc 23))      ;;STRING:: "apply"
                                      (287 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (290 (checkcast (class "clojure.lang.Var")))
                                      (293 (putstatic (fieldCP "const__23" "clojure.main$main_opt" (class "clojure.lang.Var"))))
                                      (296 (ldc 0))       ;;STRING:: "clojure.core"
                                      (298 (ldc 24))      ;;STRING:: "ns-resolve"
                                      (300 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (303 (checkcast (class "clojure.lang.Var")))
                                      (306 (putstatic (fieldCP "const__24" "clojure.main$main_opt" (class "clojure.lang.Var"))))
                                      (309 (ldc 0))       ;;STRING:: "clojure.core"
                                      (311 (ldc 25))      ;;STRING:: "symbol"
                                      (313 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (316 (checkcast (class "clojure.lang.Var")))
                                      (319 (putstatic (fieldCP "const__25" "clojure.main$main_opt" (class "clojure.lang.Var"))))
                                      (322 (ldc 0))       ;;STRING:: "clojure.core"
                                      (324 (ldc 26))      ;;STRING:: "require"
                                      (326 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (329 (checkcast (class "clojure.lang.Var")))
                                      (332 (putstatic (fieldCP "const__26" "clojure.main$main_opt" (class "clojure.lang.Var"))))
                                      (335 (aconst_null))
                                      (336 (ldc 27))      ;;STRING:: "-main"
                                      (338 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (341 (checkcast (class "clojure.lang.AFn")))
                                      (344 (putstatic (fieldCP "const__27" "clojure.main$main_opt" (class "clojure.lang.AFn"))))
                                      (347 (ldc 0))       ;;STRING:: "clojure.core"
                                      (349 (ldc 28))      ;;STRING:: "pop-thread-bindings"
                                      (351 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (354 (checkcast (class "clojure.lang.Var")))
                                      (357 (putstatic (fieldCP "const__28" "clojure.main$main_opt" (class "clojure.lang.Var"))))
                                      (360 (return))
                                      (endofcode 361))
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
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 26) (max_locals . 10) (code_length . 380)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (aconst_null)) 
                                      (2 (astore_1)) 
                                      (3 (astore_3)) 
                                      (4 (aload_3)) 
                                      (5 (lconst_0)) 
                                      (6 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (9 (aconst_null)) 
                                      (10 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (13 (astore 4)) 
                                      (15 (aload_3)) 
                                      (16 (lconst_1)) 
                                      (17 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (20 (aconst_null)) 
                                      (21 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (24 (astore 5)) 
                                      (26 (getstatic (fieldCP "const__3" "clojure.main$main_opt" (class "clojure.lang.Var")))) 
                                      (29 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (32 (checkcast (class "clojure.lang.IFn"))) 
                                      (35 (aload_3)) 
                                      (36 (aconst_null)) 
                                      (37 (astore_3)) 
                                      (38 (getstatic (fieldCP "const__4" "clojure.main$main_opt" (class "java.lang.Object")))) 
                                      (41 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (46 (astore 6)) 
                                      (48 (getstatic (fieldCP "const__5" "clojure.main$main_opt" (class "clojure.lang.Var")))) 
                                      (51 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (54 (checkcast (class "clojure.lang.IFn"))) 
                                      (57 (getstatic (fieldCP "const__6" "clojure.main$main_opt" (class "clojure.lang.Var")))) 
                                      (60 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (63 (checkcast (class "clojure.lang.IFn"))) 
                                      (66 (getstatic (fieldCP "const__7" "clojure.main$main_opt" (class "clojure.lang.Var")))) 
                                      (69 (getstatic (fieldCP "const__7" "clojure.main$main_opt" (class "clojure.lang.Var")))) 
                                      (72 (invokevirtual (methodCP "get" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (75 (getstatic (fieldCP "const__8" "clojure.main$main_opt" (class "clojure.lang.Var")))) 
                                      (78 (getstatic (fieldCP "const__8" "clojure.main$main_opt" (class "clojure.lang.Var")))) 
                                      (81 (invokevirtual (methodCP "get" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (84 (getstatic (fieldCP "const__9" "clojure.main$main_opt" (class "clojure.lang.Var")))) 
                                      (87 (getstatic (fieldCP "const__9" "clojure.main$main_opt" (class "clojure.lang.Var")))) 
                                      (90 (invokevirtual (methodCP "get" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (93 (getstatic (fieldCP "const__10" "clojure.main$main_opt" (class "clojure.lang.Var")))) 
                                      (96 (getstatic (fieldCP "const__10" "clojure.main$main_opt" (class "clojure.lang.Var")))) 
                                      (99 (invokevirtual (methodCP "get" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (102 (getstatic (fieldCP "const__11" "clojure.main$main_opt" (class "clojure.lang.Var")))) 
                                      (105 (getstatic (fieldCP "const__11" "clojure.main$main_opt" (class "clojure.lang.Var")))) 
                                      (108 (invokevirtual (methodCP "get" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (111 (getstatic (fieldCP "const__12" "clojure.main$main_opt" (class "clojure.lang.Var")))) 
                                      (114 (getstatic (fieldCP "const__12" "clojure.main$main_opt" (class "clojure.lang.Var")))) 
                                      (117 (invokevirtual (methodCP "get" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (120 (getstatic (fieldCP "const__13" "clojure.main$main_opt" (class "clojure.lang.Var")))) 
                                      (123 (getstatic (fieldCP "const__13" "clojure.main$main_opt" (class "clojure.lang.Var")))) 
                                      (126 (invokevirtual (methodCP "get" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (129 (getstatic (fieldCP "const__14" "clojure.main$main_opt" (class "clojure.lang.Var")))) 
                                      (132 (ldc 29)) ;;STRING:: "clojure.compile.path"
                                      (134 (checkcast (class "java.lang.String"))) 
                                      (137 (ldc 30)) ;;STRING:: "classes"
                                      (139 (checkcast (class "java.lang.String"))) 
                                      (142 (invokestatic (methodCP "getProperty" "java.lang.System" ((class "java.lang.String") (class "java.lang.String")) (class "java.lang.String")))) 
                                      (145 (getstatic (fieldCP "const__15" "clojure.main$main_opt" (class "clojure.lang.Var")))) 
                                      (148 (getstatic (fieldCP "const__15" "clojure.main$main_opt" (class "clojure.lang.Var")))) 
                                      (151 (invokevirtual (methodCP "get" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (154 (getstatic (fieldCP "const__16" "clojure.main$main_opt" (class "clojure.lang.Var")))) 
                                      (157 (getstatic (fieldCP "const__16" "clojure.main$main_opt" (class "clojure.lang.Var")))) 
                                      (160 (invokevirtual (methodCP "get" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (163 (bipush 10)) 
                                      (165 (anewarray (class "java.lang.Object"))) 
                                      (168 (dup)) 
                                      (169 (iconst_0)) 
                                      (170 (getstatic (fieldCP "const__17" "clojure.main$main_opt" (class "clojure.lang.Var")))) 
                                      (173 (aastore)) 
                                      (174 (dup)) 
                                      (175 (iconst_1)) 
                                      (176 (getstatic (fieldCP "const__17" "clojure.main$main_opt" (class "clojure.lang.Var")))) 
                                      (179 (invokevirtual (methodCP "get" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (182 (aastore)) 
                                      (183 (dup)) 
                                      (184 (iconst_2)) 
                                      (185 (getstatic (fieldCP "const__18" "clojure.main$main_opt" (class "clojure.lang.Var")))) 
                                      (188 (aastore)) 
                                      (189 (dup)) 
                                      (190 (iconst_3)) 
                                      (191 (aconst_null)) 
                                      (192 (aastore)) 
                                      (193 (dup)) 
                                      (194 (iconst_4)) 
                                      (195 (getstatic (fieldCP "const__19" "clojure.main$main_opt" (class "clojure.lang.Var")))) 
                                      (198 (aastore)) 
                                      (199 (dup)) 
                                      (200 (iconst_5)) 
                                      (201 (aconst_null)) 
                                      (202 (aastore)) 
                                      (203 (dup)) 
                                      (204 (bipush 6)) 
                                      (206 (getstatic (fieldCP "const__20" "clojure.main$main_opt" (class "clojure.lang.Var")))) 
                                      (209 (aastore)) 
                                      (210 (dup)) 
                                      (211 (bipush 7)) 
                                      (213 (aconst_null)) 
                                      (214 (aastore)) 
                                      (215 (dup)) 
                                      (216 (bipush 8)) 
                                      (218 (getstatic (fieldCP "const__21" "clojure.main$main_opt" (class "clojure.lang.Var")))) 
                                      (221 (aastore)) 
                                      (222 (dup)) 
                                      (223 (bipush 9)) 
                                      (225 (aconst_null)) 
                                      (226 (aastore)) 
                                      (227 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (array (class "java.lang.Object"))) (class "java.lang.Object")) 22)) 
                                      (232 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (237 (pop)) 
                                      (238 (getstatic (fieldCP "const__22" "clojure.main$main_opt" (class "clojure.lang.Var")))) ;;at TAG_1
                                      (241 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (244 (checkcast (class "clojure.lang.IFn"))) 
                                      (247 (aload 6)) 
                                      (249 (aload_2)) 
                                      (250 (aconst_null)) 
                                      (251 (astore_2)) 
                                      (252 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (257 (pop)) 
                                      (258 (getstatic (fieldCP "const__23" "clojure.main$main_opt" (class "clojure.lang.Var")))) 
                                      (261 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (264 (checkcast (class "clojure.lang.IFn"))) 
                                      (267 (getstatic (fieldCP "const__24" "clojure.main$main_opt" (class "clojure.lang.Var")))) 
                                      (270 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (273 (checkcast (class "clojure.lang.IFn"))) 
                                      (276 (getstatic (fieldCP "const__25" "clojure.main$main_opt" (class "clojure.lang.Var")))) 
                                      (279 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (282 (checkcast (class "clojure.lang.IFn"))) 
                                      (285 (aload 5)) 
                                      (287 (aconst_null)) 
                                      (288 (astore 5)) 
                                      (290 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (295 (astore 7)) 
                                      (297 (getstatic (fieldCP "const__26" "clojure.main$main_opt" (class "clojure.lang.Var")))) 
                                      (300 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (303 (checkcast (class "clojure.lang.IFn"))) 
                                      (306 (aload 7)) 
                                      (308 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (313 (pop)) 
                                      (314 (aload 7)) 
                                      (316 (aconst_null)) 
                                      (317 (astore 7)) 
                                      (319 (getstatic (fieldCP "const__27" "clojure.main$main_opt" (class "clojure.lang.AFn")))) 
                                      (322 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (327 (aload 6)) 
                                      (329 (aconst_null)) 
                                      (330 (astore 6)) 
                                      (332 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (337 (astore 8)) 
                                      (339 (getstatic (fieldCP "const__28" "clojure.main$main_opt" (class "clojure.lang.Var")))) ;;at TAG_2
                                      (342 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (345 (checkcast (class "clojure.lang.IFn"))) 
                                      (348 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1)) 
                                      (353 (pop)) 
                                      (354 (goto 377)) ;;to TAG_0
                                      (357 (astore 9)) ;;at TAG_3
                                      (359 (getstatic (fieldCP "const__28" "clojure.main$main_opt" (class "clojure.lang.Var")))) 
                                      (362 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (365 (checkcast (class "clojure.lang.IFn"))) 
                                      (368 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1)) 
                                      (373 (pop)) 
                                      (374 (aload 9)) 
                                      (376 (athrow)) 
                                      (377 (aload 8)) ;;at TAG_0
                                      (379 (areturn)) 
                                      (endofcode 380))
                                   (Exceptions 
                                     (handler 238 339  357 (class "java.lang.Throwable")))
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *main$main_opt-class-table*
  (make-static-class-decls 
   *clojure.main$main_opt*))

(defconst *package-name-map* 
  ("clojure.main$main_opt" . "clojure"))

