; core$fn__6302-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:42 CDT 2014.
;

(defconst *clojure.core$fn__6302*
 (make-class-def
      '(class "clojure.core$fn__6302"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "re-matches")
                        (STRING  "(\\d+)\\.(\\d+)\\.(\\d+)(?:-([a-zA-Z0-9_]+))?(?:-(SNAPSHOT))?")
                        (STRING  "nth")
                        (LONG 2)
                        (LONG 3)
                        (LONG 4)
                        (LONG 5)
                        (STRING  "major")
                        (STRING  "minor")
                        (STRING  "incremental")
                        (STRING  "qualifier")
                        (STRING  "=")
                        (STRING  "*clojure-version*")
                        (STRING  "file")
                        (STRING  "column")
                        (STRING  "line")
                        (STRING  "dynamic")
                        (STRING  "clojure/core.clj")
                        (STRING  "interim")
                        (STRING  "clojure/version.properties")
                        (STRING  "getProperty")
                        (STRING  "version")
                        (STRING  "SNAPSHOT")
                        (STRING  "contains"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__9" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__10" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__11" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__12" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__13" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__14" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__15" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__16" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__17" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__18" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__19" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__20" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__21" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__22" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 5) (max_locals . 0) (code_length . 314)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "re-matches"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$fn__6302" (class "clojure.lang.Var"))))
                                      (13 (ldc 2))        ;;STRING:: "(\\d+)\\.(\\d+)\\.(\\d+)(?:-([a-zA-Z0-9_]+))?(?:-(SNAPSHOT))?"
                                      (15 (invokestatic
					(methodCP "compile" "java.util.regex.Pattern" ((class "java.lang.String")) (class "java.util.regex.Pattern"))))
                                      (18 (putstatic (fieldCP "const__1" "clojure.core$fn__6302" (class "java.lang.Object"))))
                                      (21 (ldc 0))        ;;STRING:: "clojure.core"
                                      (23 (ldc 3))        ;;STRING:: "nth"
                                      (25 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (28 (checkcast (class "clojure.lang.Var")))
                                      (31 (putstatic (fieldCP "const__2" "clojure.core$fn__6302" (class "clojure.lang.Var"))))
                                      (34 (lconst_0))
                                      (35 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (38 (putstatic (fieldCP "const__3" "clojure.core$fn__6302" (class "java.lang.Object"))))
                                      (41 (lconst_1))
                                      (42 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (45 (putstatic (fieldCP "const__4" "clojure.core$fn__6302" (class "java.lang.Object"))))
                                      (48 (ldc2_w 4))     ;; LONG:: "2"
                                      (51 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (54 (putstatic (fieldCP "const__5" "clojure.core$fn__6302" (class "java.lang.Object"))))
                                      (57 (ldc2_w 5))     ;; LONG:: "3"
                                      (60 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (63 (putstatic (fieldCP "const__6" "clojure.core$fn__6302" (class "java.lang.Object"))))
                                      (66 (ldc2_w 6))     ;; LONG:: "4"
                                      (69 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (72 (putstatic (fieldCP "const__7" "clojure.core$fn__6302" (class "java.lang.Object"))))
                                      (75 (ldc2_w 7))     ;; LONG:: "5"
                                      (78 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (81 (putstatic (fieldCP "const__8" "clojure.core$fn__6302" (class "java.lang.Object"))))
                                      (84 (aconst_null))
                                      (85 (ldc 8))        ;;STRING:: "major"
                                      (87 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (90 (checkcast (class "clojure.lang.Keyword")))
                                      (93 (putstatic (fieldCP "const__9" "clojure.core$fn__6302" (class "clojure.lang.Keyword"))))
                                      (96 (aconst_null))
                                      (97 (ldc 9))        ;;STRING:: "minor"
                                      (99 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (102 (checkcast (class "clojure.lang.Keyword")))
                                      (105 (putstatic (fieldCP "const__10" "clojure.core$fn__6302" (class "clojure.lang.Keyword"))))
                                      (108 (aconst_null))
                                      (109 (ldc 10))      ;;STRING:: "incremental"
                                      (111 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (114 (checkcast (class "clojure.lang.Keyword")))
                                      (117 (putstatic (fieldCP "const__11" "clojure.core$fn__6302" (class "clojure.lang.Keyword"))))
                                      (120 (aconst_null))
                                      (121 (ldc 11))      ;;STRING:: "qualifier"
                                      (123 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (126 (checkcast (class "clojure.lang.Keyword")))
                                      (129 (putstatic (fieldCP "const__12" "clojure.core$fn__6302" (class "clojure.lang.Keyword"))))
                                      (132 (ldc 0))       ;;STRING:: "clojure.core"
                                      (134 (ldc 12))      ;;STRING:: "="
                                      (136 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (139 (checkcast (class "clojure.lang.Var")))
                                      (142 (putstatic (fieldCP "const__13" "clojure.core$fn__6302" (class "clojure.lang.Var"))))
                                      (145 (ldc 0))       ;;STRING:: "clojure.core"
                                      (147 (ldc 13))      ;;STRING:: "*clojure-version*"
                                      (149 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (152 (checkcast (class "clojure.lang.Var")))
                                      (155 (putstatic (fieldCP "const__14" "clojure.core$fn__6302" (class "clojure.lang.Var"))))
                                      (158 (aconst_null))
                                      (159 (ldc 14))      ;;STRING:: "file"
                                      (161 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (164 (checkcast (class "clojure.lang.Keyword")))
                                      (167 (putstatic (fieldCP "const__15" "clojure.core$fn__6302" (class "clojure.lang.Keyword"))))
                                      (170 (aconst_null))
                                      (171 (ldc 15))      ;;STRING:: "column"
                                      (173 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (176 (checkcast (class "clojure.lang.Keyword")))
                                      (179 (putstatic (fieldCP "const__16" "clojure.core$fn__6302" (class "clojure.lang.Keyword"))))
                                      (182 (iconst_3))
                                      (183 (invokestatic
					(methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer"))))
                                      (186 (putstatic (fieldCP "const__17" "clojure.core$fn__6302" (class "java.lang.Object"))))
                                      (189 (aconst_null))
                                      (190 (ldc 16))      ;;STRING:: "line"
                                      (192 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (195 (checkcast (class "clojure.lang.Keyword")))
                                      (198 (putstatic (fieldCP "const__18" "clojure.core$fn__6302" (class "clojure.lang.Keyword"))))
                                      (201 (sipush 6401))
                                      (204 (invokestatic
					(methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer"))))
                                      (207 (putstatic (fieldCP "const__19" "clojure.core$fn__6302" (class "java.lang.Object"))))
                                      (210 (aconst_null))
                                      (211 (ldc 17))      ;;STRING:: "dynamic"
                                      (213 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (216 (checkcast (class "clojure.lang.Keyword")))
                                      (219 (putstatic (fieldCP "const__20" "clojure.core$fn__6302" (class "clojure.lang.Keyword"))))
                                      (222 (bipush 8))
                                      (224 (anewarray (class "java.lang.Object")))
                                      (227 (dup))
                                      (228 (iconst_0))
                                      (229 (aconst_null))
                                      (230 (ldc 17))      ;;STRING:: "dynamic"
                                      (232 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (235 (aastore))
                                      (236 (dup))
                                      (237 (iconst_1))
                                      (238 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean"))))
                                      (241 (aastore))
                                      (242 (dup))
                                      (243 (iconst_2))
                                      (244 (aconst_null))
                                      (245 (ldc 15))      ;;STRING:: "column"
                                      (247 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (250 (aastore))
                                      (251 (dup))
                                      (252 (iconst_3))
                                      (253 (iconst_3))
                                      (254 (invokestatic
					(methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer"))))
                                      (257 (aastore))
                                      (258 (dup))
                                      (259 (iconst_4))
                                      (260 (aconst_null))
                                      (261 (ldc 16))      ;;STRING:: "line"
                                      (263 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (266 (aastore))
                                      (267 (dup))
                                      (268 (iconst_5))
                                      (269 (sipush 6401))
                                      (272 (invokestatic
					(methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer"))))
                                      (275 (aastore))
                                      (276 (dup))
                                      (277 (bipush 6))
                                      (279 (aconst_null))
                                      (280 (ldc 14))      ;;STRING:: "file"
                                      (282 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (285 (aastore))
                                      (286 (dup))
                                      (287 (bipush 7))
                                      (289 (ldc 18))      ;;STRING:: "clojure/core.clj"
                                      (291 (aastore))
                                      (292 (invokestatic
					(methodCP "map" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentMap"))))
                                      (295 (checkcast (class "clojure.lang.AFn")))
                                      (298 (putstatic (fieldCP "const__21" "clojure.core$fn__6302" (class "clojure.lang.AFn"))))
                                      (301 (aconst_null))
                                      (302 (ldc 19))      ;;STRING:: "interim"
                                      (304 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (307 (checkcast (class "clojure.lang.Keyword")))
                                      (310 (putstatic (fieldCP "const__22" "clojure.core$fn__6302" (class "clojure.lang.Keyword"))))
                                      (313 (return))
                                      (endofcode 314))
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
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 8) (max_locals . 11) (code_length . 320)
                                   (parsedcode
                                      (0 (invokestatic (methodCP "baseLoader" "clojure.lang.RT" () (class "java.lang.ClassLoader")))) 
                                      (3 (checkcast (class "java.lang.ClassLoader"))) 
                                      (6 (ldc 20)) ;;STRING:: "clojure/version.properties"
                                      (8 (checkcast (class "java.lang.String"))) 
                                      (11 (invokevirtual (methodCP "getResourceAsStream" "java.lang.ClassLoader" ((class "java.lang.String")) (class "java.io.InputStream")))) 
                                      (14 (astore_1)) 
                                      (15 (new (class "clojure.core$fn__6302$fn__6303"))) 
                                      (18 (dup)) 
                                      (19 (aload_1)) 
                                      (20 (aconst_null)) 
                                      (21 (astore_1)) 
                                      (22 (invokespecial (methodCP "<init>" "clojure.core$fn__6302$fn__6303" ((class "java.lang.Object")) void))) 
                                      (25 (checkcast (class "clojure.lang.IFn"))) 
                                      (28 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1)) 
                                      (33 (astore_1)) 
                                      (34 (aload_1)) 
                                      (35 (aconst_null)) 
                                      (36 (astore_1)) 
                                      (37 (ldc 21)) ;;STRING:: "getProperty"
                                      (39 (iconst_1)) 
                                      (40 (anewarray (class "java.lang.Object"))) 
                                      (43 (dup)) 
                                      (44 (iconst_0)) 
                                      (45 (ldc 22)) ;;STRING:: "version"
                                      (47 (aastore)) 
                                      (48 (invokestatic (methodCP "invokeInstanceMethod" "clojure.lang.Reflector" ((class "java.lang.Object") (class "java.lang.String") (array (class "java.lang.Object"))) (class "java.lang.Object")))) 
                                      (51 (astore_2)) 
                                      (52 (getstatic (fieldCP "const__0" "clojure.core$fn__6302" (class "clojure.lang.Var")))) 
                                      (55 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (58 (checkcast (class "clojure.lang.IFn"))) 
                                      (61 (getstatic (fieldCP "const__1" "clojure.core$fn__6302" (class "java.lang.Object")))) 
                                      (64 (aload_2)) 
                                      (65 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (70 (astore_3)) 
                                      (71 (aload_3)) 
                                      (72 (lconst_0)) 
                                      (73 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (76 (aconst_null)) 
                                      (77 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (80 (astore 4)) 
                                      (82 (aload_3)) 
                                      (83 (lconst_1)) 
                                      (84 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (87 (aconst_null)) 
                                      (88 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (91 (astore 5)) 
                                      (93 (aload_3)) 
                                      (94 (ldc2_w 4)) ;; LONG:: "2"
                                      (97 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (100 (aconst_null)) 
                                      (101 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (104 (astore 6)) 
                                      (106 (aload_3)) 
                                      (107 (ldc2_w 5)) ;; LONG:: "3"
                                      (110 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (113 (aconst_null)) 
                                      (114 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (117 (astore 7)) 
                                      (119 (aload_3)) 
                                      (120 (ldc2_w 6)) ;; LONG:: "4"
                                      (123 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (126 (aconst_null)) 
                                      (127 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (130 (astore 8)) 
                                      (132 (aload_3)) 
                                      (133 (aconst_null)) 
                                      (134 (astore_3)) 
                                      (135 (ldc2_w 7)) ;; LONG:: "5"
                                      (138 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (141 (aconst_null)) 
                                      (142 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (145 (astore 9)) 
                                      (147 (bipush 8)) 
                                      (149 (anewarray (class "java.lang.Object"))) 
                                      (152 (dup)) 
                                      (153 (iconst_0)) 
                                      (154 (getstatic (fieldCP "const__9" "clojure.core$fn__6302" (class "clojure.lang.Keyword")))) 
                                      (157 (aastore)) 
                                      (158 (dup)) 
                                      (159 (iconst_1)) 
                                      (160 (aload 5)) 
                                      (162 (aconst_null)) 
                                      (163 (astore 5)) 
                                      (165 (checkcast (class "java.lang.String"))) 
                                      (168 (invokestatic (methodCP "valueOf" "java.lang.Integer" ((class "java.lang.String")) (class "java.lang.Integer")))) 
                                      (171 (aastore)) 
                                      (172 (dup)) 
                                      (173 (iconst_2)) 
                                      (174 (getstatic (fieldCP "const__10" "clojure.core$fn__6302" (class "clojure.lang.Keyword")))) 
                                      (177 (aastore)) 
                                      (178 (dup)) 
                                      (179 (iconst_3)) 
                                      (180 (aload 6)) 
                                      (182 (aconst_null)) 
                                      (183 (astore 6)) 
                                      (185 (checkcast (class "java.lang.String"))) 
                                      (188 (invokestatic (methodCP "valueOf" "java.lang.Integer" ((class "java.lang.String")) (class "java.lang.Integer")))) 
                                      (191 (aastore)) 
                                      (192 (dup)) 
                                      (193 (iconst_4)) 
                                      (194 (getstatic (fieldCP "const__11" "clojure.core$fn__6302" (class "clojure.lang.Keyword")))) 
                                      (197 (aastore)) 
                                      (198 (dup)) 
                                      (199 (iconst_5)) 
                                      (200 (aload 7)) 
                                      (202 (aconst_null)) 
                                      (203 (astore 7)) 
                                      (205 (checkcast (class "java.lang.String"))) 
                                      (208 (invokestatic (methodCP "valueOf" "java.lang.Integer" ((class "java.lang.String")) (class "java.lang.Integer")))) 
                                      (211 (aastore)) 
                                      (212 (dup)) 
                                      (213 (bipush 6)) 
                                      (215 (getstatic (fieldCP "const__12" "clojure.core$fn__6302" (class "clojure.lang.Keyword")))) 
                                      (218 (aastore)) 
                                      (219 (dup)) 
                                      (220 (bipush 7)) 
                                      (222 (aload 8)) 
                                      (224 (ldc 23)) ;;STRING:: "SNAPSHOT"
                                      (226 (invokestatic (methodCP "equiv" "clojure.lang.Util" ((class "java.lang.Object") (class "java.lang.Object")) boolean))) 
                                      (229 (ifeq 237)) ;;to TAG_0
                                      (232 (aconst_null)) 
                                      (233 (goto 242)) ;;to TAG_1
                                      (236 (pop)) 
                                      (237 (aload 8)) ;;at TAG_0
                                      (239 (aconst_null)) 
                                      (240 (astore 8)) 
                                      (242 (aastore)) ;;at TAG_1
                                      (243 (invokestatic (methodCP "mapUniqueKeys" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentMap")))) 
                                      (246 (astore 10)) 
                                      (248 (getstatic (fieldCP "const__14" "clojure.core$fn__6302" (class "clojure.lang.Var")))) 
                                      (251 (iconst_1)) 
                                      (252 (invokevirtual (methodCP "setDynamic" "clojure.lang.Var" (boolean) (class "clojure.lang.Var")))) 
                                      (255 (dup)) 
                                      (256 (getstatic (fieldCP "const__21" "clojure.core$fn__6302" (class "clojure.lang.AFn")))) 
                                      (259 (checkcast (class "clojure.lang.IPersistentMap"))) 
                                      (262 (invokevirtual (methodCP "setMeta" "clojure.lang.Var" ((class "clojure.lang.IPersistentMap")) void))) 
                                      (265 (dup)) 
                                      (266 (aload_2)) 
                                      (267 (aconst_null)) 
                                      (268 (astore_2)) 
                                      (269 (ldc 24)) ;;STRING:: "contains"
                                      (271 (iconst_1)) 
                                      (272 (anewarray (class "java.lang.Object"))) 
                                      (275 (dup)) 
                                      (276 (iconst_0)) 
                                      (277 (ldc 23)) ;;STRING:: "SNAPSHOT"
                                      (279 (aastore)) 
                                      (280 (invokestatic (methodCP "invokeInstanceMethod" "clojure.lang.Reflector" ((class "java.lang.Object") (class "java.lang.String") (array (class "java.lang.Object"))) (class "java.lang.Object")))) 
                                      (283 (dup)) 
                                      (284 (ifnull 310))  ;;to TAG_2
                                      (287 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (290 (if_acmpeq 311)) ;;to TAG_3
                                      (293 (aload 10)) 
                                      (295 (aconst_null)) 
                                      (296 (astore 10)) 
                                      (298 (getstatic (fieldCP "const__22" "clojure.core$fn__6302" (class "clojure.lang.Keyword")))) 
                                      (301 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (304 (invokestatic (methodCP "assoc" "clojure.lang.RT" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "clojure.lang.Associative")))) 
                                      (307 (goto 316)) ;;to TAG_4
                                      (310 (pop)) ;;at TAG_2
                                      (311 (aload 10)) ;;at TAG_3
                                      (313 (aconst_null)) 
                                      (314 (astore 10)) 
                                      (316 (invokevirtual (methodCP "bindRoot" "clojure.lang.Var" ((class "java.lang.Object")) void))) ;;at TAG_4
                                      (319 (areturn)) 
                                      (endofcode 320))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$fn__6302-class-table*
  (make-static-class-decls 
   *clojure.core$fn__6302*))

(defconst *package-name-map* 
  ("clojure.core$fn__6302" . "clojure"))
