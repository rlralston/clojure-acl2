; pprint$pprint_defn-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:56 CDT 2014.
;

(defconst *clojure.pprint$pprint_defn*
 (make-class-def
      '(class "clojure.pprint$pprint_defn"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "next")
                        (STRING  "nth")
                        (STRING  "nthnext")
                        (LONG 2)
                        (STRING  "string?")
                        (STRING  "first")
                        (STRING  "map?")
                        (STRING  "clojure.pprint")
                        (STRING  "level-exceeded")
                        (STRING  "*out*")
                        (STRING  "push-thread-bindings")
                        (STRING  "*current-level*")
                        (STRING  "inc")
                        (STRING  "var-get")
                        (STRING  "*current-length*")
                        (STRING  "pprint-simple-code-list")
                        (STRING  "#"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
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
                        (field "const__15" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__16" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 206)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "next"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.pprint$pprint_defn" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "nth"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.pprint$pprint_defn" (class "clojure.lang.Var"))))
                                      (26 (lconst_0))
                                      (27 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (30 (putstatic (fieldCP "const__2" "clojure.pprint$pprint_defn" (class "java.lang.Object"))))
                                      (33 (lconst_1))
                                      (34 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (37 (putstatic (fieldCP "const__3" "clojure.pprint$pprint_defn" (class "java.lang.Object"))))
                                      (40 (ldc 0))        ;;STRING:: "clojure.core"
                                      (42 (ldc 3))        ;;STRING:: "nthnext"
                                      (44 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (47 (checkcast (class "clojure.lang.Var")))
                                      (50 (putstatic (fieldCP "const__4" "clojure.pprint$pprint_defn" (class "clojure.lang.Var"))))
                                      (53 (ldc2_w 4))     ;; LONG:: "2"
                                      (56 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (59 (putstatic (fieldCP "const__5" "clojure.pprint$pprint_defn" (class "java.lang.Object"))))
                                      (62 (ldc 0))        ;;STRING:: "clojure.core"
                                      (64 (ldc 5))        ;;STRING:: "string?"
                                      (66 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (69 (checkcast (class "clojure.lang.Var")))
                                      (72 (putstatic (fieldCP "const__6" "clojure.pprint$pprint_defn" (class "clojure.lang.Var"))))
                                      (75 (ldc 0))        ;;STRING:: "clojure.core"
                                      (77 (ldc 6))        ;;STRING:: "first"
                                      (79 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (82 (checkcast (class "clojure.lang.Var")))
                                      (85 (putstatic (fieldCP "const__7" "clojure.pprint$pprint_defn" (class "clojure.lang.Var"))))
                                      (88 (ldc 0))        ;;STRING:: "clojure.core"
                                      (90 (ldc 7))        ;;STRING:: "map?"
                                      (92 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (95 (checkcast (class "clojure.lang.Var")))
                                      (98 (putstatic (fieldCP "const__8" "clojure.pprint$pprint_defn" (class "clojure.lang.Var"))))
                                      (101 (ldc 8))       ;;STRING:: "clojure.pprint"
                                      (103 (ldc 9))       ;;STRING:: "level-exceeded"
                                      (105 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (108 (checkcast (class "clojure.lang.Var")))
                                      (111 (putstatic (fieldCP "const__9" "clojure.pprint$pprint_defn" (class "clojure.lang.Var"))))
                                      (114 (ldc 0))       ;;STRING:: "clojure.core"
                                      (116 (ldc 10))      ;;STRING:: "*out*"
                                      (118 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (121 (checkcast (class "clojure.lang.Var")))
                                      (124 (putstatic (fieldCP "const__10" "clojure.pprint$pprint_defn" (class "clojure.lang.Var"))))
                                      (127 (ldc 0))       ;;STRING:: "clojure.core"
                                      (129 (ldc 11))      ;;STRING:: "push-thread-bindings"
                                      (131 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (134 (checkcast (class "clojure.lang.Var")))
                                      (137 (putstatic (fieldCP "const__11" "clojure.pprint$pprint_defn" (class "clojure.lang.Var"))))
                                      (140 (ldc 8))       ;;STRING:: "clojure.pprint"
                                      (142 (ldc 12))      ;;STRING:: "*current-level*"
                                      (144 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (147 (checkcast (class "clojure.lang.Var")))
                                      (150 (putstatic (fieldCP "const__12" "clojure.pprint$pprint_defn" (class "clojure.lang.Var"))))
                                      (153 (ldc 0))       ;;STRING:: "clojure.core"
                                      (155 (ldc 13))      ;;STRING:: "inc"
                                      (157 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (160 (checkcast (class "clojure.lang.Var")))
                                      (163 (putstatic (fieldCP "const__13" "clojure.pprint$pprint_defn" (class "clojure.lang.Var"))))
                                      (166 (ldc 0))       ;;STRING:: "clojure.core"
                                      (168 (ldc 14))      ;;STRING:: "var-get"
                                      (170 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (173 (checkcast (class "clojure.lang.Var")))
                                      (176 (putstatic (fieldCP "const__14" "clojure.pprint$pprint_defn" (class "clojure.lang.Var"))))
                                      (179 (ldc 8))       ;;STRING:: "clojure.pprint"
                                      (181 (ldc 15))      ;;STRING:: "*current-length*"
                                      (183 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (186 (checkcast (class "clojure.lang.Var")))
                                      (189 (putstatic (fieldCP "const__15" "clojure.pprint$pprint_defn" (class "clojure.lang.Var"))))
                                      (192 (ldc 8))       ;;STRING:: "clojure.pprint"
                                      (194 (ldc 16))      ;;STRING:: "pprint-simple-code-list"
                                      (196 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (199 (checkcast (class "clojure.lang.Var")))
                                      (202 (putstatic (fieldCP "const__16" "clojure.pprint$pprint_defn" (class "clojure.lang.Var"))))
                                      (205 (return))
                                      (endofcode 206))
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
                                   (max_stack . 8) (max_locals . 12) (code_length . 521)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.pprint$pprint_defn" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (aload_1)) 
                                      (10 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (15 (dup)) 
                                      (16 (ifnull 502)) ;;to TAG_0
                                      (19 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (22 (if_acmpeq 503))  ;;to TAG_1
                                      (25 (aload_1)) 
                                      (26 (aconst_null)) 
                                      (27 (astore_1)) 
                                      (28 (astore_2)) 
                                      (29 (aload_2)) 
                                      (30 (lconst_0)) 
                                      (31 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (34 (aconst_null)) 
                                      (35 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (38 (astore_3)) 
                                      (39 (aload_2)) 
                                      (40 (lconst_1)) 
                                      (41 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (44 (aconst_null)) 
                                      (45 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (48 (astore 4)) 
                                      (50 (getstatic (fieldCP "const__4" "clojure.pprint$pprint_defn" (class "clojure.lang.Var")))) 
                                      (53 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (56 (checkcast (class "clojure.lang.IFn"))) 
                                      (59 (aload_2)) 
                                      (60 (aconst_null)) 
                                      (61 (astore_2)) 
                                      (62 (getstatic (fieldCP "const__5" "clojure.pprint$pprint_defn" (class "java.lang.Object")))) 
                                      (65 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (70 (astore 5)) 
                                      (72 (getstatic (fieldCP "const__6" "clojure.pprint$pprint_defn" (class "clojure.lang.Var")))) 
                                      (75 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (78 (checkcast (class "clojure.lang.IFn"))) 
                                      (81 (getstatic (fieldCP "const__7" "clojure.pprint$pprint_defn" (class "clojure.lang.Var")))) 
                                      (84 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (87 (checkcast (class "clojure.lang.IFn"))) 
                                      (90 (aload 5)) 
                                      (92 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (97 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (102 (dup)) 
                                      (103 (ifnull 163)) ;;to TAG_2
                                      (106 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (109 (if_acmpeq 164)) ;;to TAG_3
                                      (112 (iconst_2)) 
                                      (113 (anewarray (class "java.lang.Object"))) 
                                      (116 (dup)) 
                                      (117 (iconst_0)) 
                                      (118 (getstatic (fieldCP "const__7" "clojure.pprint$pprint_defn" (class "clojure.lang.Var")))) 
                                      (121 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (124 (checkcast (class "clojure.lang.IFn"))) 
                                      (127 (aload 5)) 
                                      (129 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (134 (aastore)) 
                                      (135 (dup)) 
                                      (136 (iconst_1)) 
                                      (137 (getstatic (fieldCP "const__0" "clojure.pprint$pprint_defn" (class "clojure.lang.Var")))) 
                                      (140 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (143 (checkcast (class "clojure.lang.IFn"))) 
                                      (146 (aload 5)) 
                                      (148 (aconst_null)) 
                                      (149 (astore 5)) 
                                      (151 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (156 (aastore)) 
                                      (157 (invokestatic (methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector")))) 
                                      (160 (goto 183)) ;;to TAG_4
                                      (163 (pop)) ;;at TAG_2
                                      (164 (iconst_2)) ;;at TAG_3
                                      (165 (anewarray (class "java.lang.Object"))) 
                                      (168 (dup)) 
                                      (169 (iconst_0)) 
                                      (170 (aconst_null)) 
                                      (171 (aastore)) 
                                      (172 (dup)) 
                                      (173 (iconst_1)) 
                                      (174 (aload 5)) 
                                      (176 (aconst_null)) 
                                      (177 (astore 5)) 
                                      (179 (aastore)) 
                                      (180 (invokestatic (methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector")))) 
                                      (183 (astore 6)) ;;at TAG_4
                                      (185 (aload 6)) 
                                      (187 (lconst_0)) 
                                      (188 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (191 (aconst_null)) 
                                      (192 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (195 (astore 7)) 
                                      (197 (aload 6)) 
                                      (199 (aconst_null)) 
                                      (200 (astore 6)) 
                                      (202 (lconst_1)) 
                                      (203 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (206 (aconst_null)) 
                                      (207 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (210 (astore 8)) 
                                      (212 (getstatic (fieldCP "const__8" "clojure.pprint$pprint_defn" (class "clojure.lang.Var")))) 
                                      (215 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (218 (checkcast (class "clojure.lang.IFn"))) 
                                      (221 (getstatic (fieldCP "const__7" "clojure.pprint$pprint_defn" (class "clojure.lang.Var")))) 
                                      (224 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (227 (checkcast (class "clojure.lang.IFn"))) 
                                      (230 (aload 8)) 
                                      (232 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (237 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (242 (dup)) 
                                      (243 (ifnull 303)) ;;to TAG_5
                                      (246 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (249 (if_acmpeq 304)) ;;to TAG_6
                                      (252 (iconst_2)) 
                                      (253 (anewarray (class "java.lang.Object"))) 
                                      (256 (dup)) 
                                      (257 (iconst_0)) 
                                      (258 (getstatic (fieldCP "const__7" "clojure.pprint$pprint_defn" (class "clojure.lang.Var")))) 
                                      (261 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (264 (checkcast (class "clojure.lang.IFn"))) 
                                      (267 (aload 8)) 
                                      (269 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (274 (aastore)) 
                                      (275 (dup)) 
                                      (276 (iconst_1)) 
                                      (277 (getstatic (fieldCP "const__0" "clojure.pprint$pprint_defn" (class "clojure.lang.Var")))) 
                                      (280 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (283 (checkcast (class "clojure.lang.IFn"))) 
                                      (286 (aload 8)) 
                                      (288 (aconst_null)) 
                                      (289 (astore 8)) 
                                      (291 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (296 (aastore)) 
                                      (297 (invokestatic (methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector")))) 
                                      (300 (goto 323)) ;;to TAG_7
                                      (303 (pop)) ;;at TAG_5
                                      (304 (iconst_2)) ;;at TAG_6
                                      (305 (anewarray (class "java.lang.Object"))) 
                                      (308 (dup)) 
                                      (309 (iconst_0)) 
                                      (310 (aconst_null)) 
                                      (311 (aastore)) 
                                      (312 (dup)) 
                                      (313 (iconst_1)) 
                                      (314 (aload 8)) 
                                      (316 (aconst_null)) 
                                      (317 (astore 8)) 
                                      (319 (aastore)) 
                                      (320 (invokestatic (methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector")))) 
                                      (323 (astore 9)) ;;at TAG_7
                                      (325 (aload 9)) 
                                      (327 (lconst_0)) 
                                      (328 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (331 (aconst_null)) 
                                      (332 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (335 (astore 10)) 
                                      (337 (aload 9)) 
                                      (339 (aconst_null)) 
                                      (340 (astore 9)) 
                                      (342 (lconst_1)) 
                                      (343 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (346 (aconst_null)) 
                                      (347 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (350 (astore 11)) 
                                      (352 (getstatic (fieldCP "const__9" "clojure.pprint$pprint_defn" (class "clojure.lang.Var")))) 
                                      (355 (checkcast (class "clojure.lang.IFn"))) 
                                      (358 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1)) 
                                      (363 (dup)) 
                                      (364 (ifnull 395)) ;;to TAG_8
                                      (367 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (370 (if_acmpeq 396)) ;;to TAG_9
                                      (373 (getstatic (fieldCP "const__10" "clojure.pprint$pprint_defn" (class "clojure.lang.Var")))) 
                                      (376 (invokevirtual (methodCP "get" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (379 (checkcast (class "java.io.Writer"))) 
                                      (382 (ldc 17)) ;;STRING:: "#"
                                      (384 (checkcast (class "java.lang.String"))) 
                                      (387 (invokevirtual (methodCP "write" "java.io.Writer" ((class "java.lang.String")) void))) 
                                      (390 (aconst_null)) 
                                      (391 (pop)) 
                                      (392 (goto 498)) ;;to TAG_10
                                      (395 (pop)) ;;at TAG_8
                                      (396 (getstatic (fieldCP "const__11" "clojure.pprint$pprint_defn" (class "clojure.lang.Var")))) ;;at TAG_9
                                      (399 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (402 (checkcast (class "clojure.lang.IFn"))) 
                                      (405 (iconst_4)) 
                                      (406 (anewarray (class "java.lang.Object"))) 
                                      (409 (dup)) 
                                      (410 (iconst_0)) 
                                      (411 (getstatic (fieldCP "const__12" "clojure.pprint$pprint_defn" (class "clojure.lang.Var")))) 
                                      (414 (aastore)) 
                                      (415 (dup)) 
                                      (416 (iconst_1)) 
                                      (417 (getstatic (fieldCP "const__14" "clojure.pprint$pprint_defn" (class "clojure.lang.Var")))) 
                                      (420 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (423 (checkcast (class "clojure.lang.IFn"))) 
                                      (426 (getstatic (fieldCP "const__12" "clojure.pprint$pprint_defn" (class "clojure.lang.Var")))) 
                                      (429 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (434 (invokestatic (methodCP "inc" "clojure.lang.Numbers" ((class "java.lang.Object")) (class "java.lang.Number")))) 
                                      (437 (aastore)) 
                                      (438 (dup)) 
                                      (439 (iconst_2)) 
                                      (440 (getstatic (fieldCP "const__15" "clojure.pprint$pprint_defn" (class "clojure.lang.Var")))) 
                                      (443 (aastore)) 
                                      (444 (dup)) 
                                      (445 (iconst_3)) 
                                      (446 (getstatic (fieldCP "const__2" "clojure.pprint$pprint_defn" (class "java.lang.Object")))) 
                                      (449 (aastore)) 
                                      (450 (invokestatic (methodCP "map" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentMap")))) 
                                      (453 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (458 (pop)) 
                                      (459 (new (class "clojure.pprint$pprint_defn$fn__8336"))) 
                                      (462 (dup)) 
                                      (463 (aload_3)) 
                                      (464 (aconst_null)) 
                                      (465 (astore_3)) 
                                      (466 (aload 10)) 
                                      (468 (aconst_null)) 
                                      (469 (astore 10)) 
                                      (471 (aload 4)) 
                                      (473 (aconst_null)) 
                                      (474 (astore 4)) 
                                      (476 (aload 7)) 
                                      (478 (aconst_null)) 
                                      (479 (astore 7)) 
                                      (481 (aload 11)) 
                                      (483 (aconst_null)) 
                                      (484 (astore 11)) 
                                      (486 (invokespecial (methodCP "<init>" "clojure.pprint$pprint_defn$fn__8336" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) void))) 
                                      (489 (checkcast (class "clojure.lang.IFn"))) 
                                      (492 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1)) 
                                      (497 (pop)) 
                                      (498 (aconst_null)) ;;at TAG_10
                                      (499 (goto 520)) ;;to TAG_11
                                      (502 (pop)) ;;at TAG_0
                                      (503 (getstatic (fieldCP "const__16" "clojure.pprint$pprint_defn" (class "clojure.lang.Var")))) ;;at TAG_1
                                      (506 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (509 (checkcast (class "clojure.lang.IFn"))) 
                                      (512 (aload_1)) 
                                      (513 (aconst_null)) 
                                      (514 (astore_1)) 
                                      (515 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (520 (areturn)) ;;at TAG_11
                                      (endofcode 521))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *pprint$pprint_defn-class-table*
  (make-static-class-decls 
   *clojure.pprint$pprint_defn*))

(defconst *package-name-map* 
  ("clojure.pprint$pprint_defn" . "clojure"))

