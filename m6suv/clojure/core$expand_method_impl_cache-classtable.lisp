; core$expand_method_impl_cache-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:42 CDT 2014.
;

(defconst *clojure.core$expand_method_impl_cache*
 (make-class-def
      '(class "clojure.core$expand_method_impl_cache"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "assoc")
                        (STRING  "into1")
                        (STRING  "remove")
                        (STRING  "map")
                        (STRING  "vec")
                        (STRING  "partition")
                        (LONG 2)
                        (STRING  "maybe-min-hash")
                        (STRING  "hash")
                        (STRING  "keys")
                        (STRING  "nth")
                        (STRING  "make-array")
                        (STRING  "java.lang.Object")
                        (STRING  "*")
                        (STRING  "inc")
                        (STRING  "reduce1"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__9" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__10" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__11" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__12" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__13" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__14" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__15" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__16" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__17" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 214)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "assoc"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$expand_method_impl_cache" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "into1"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$expand_method_impl_cache" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "remove"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.core$expand_method_impl_cache" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 4))        ;;STRING:: "map"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.core$expand_method_impl_cache" (class "clojure.lang.Var"))))
                                      (52 (ldc 0))        ;;STRING:: "clojure.core"
                                      (54 (ldc 5))        ;;STRING:: "vec"
                                      (56 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (59 (checkcast (class "clojure.lang.Var")))
                                      (62 (putstatic (fieldCP "const__4" "clojure.core$expand_method_impl_cache" (class "clojure.lang.Var"))))
                                      (65 (ldc 0))        ;;STRING:: "clojure.core"
                                      (67 (ldc 6))        ;;STRING:: "partition"
                                      (69 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (72 (checkcast (class "clojure.lang.Var")))
                                      (75 (putstatic (fieldCP "const__5" "clojure.core$expand_method_impl_cache" (class "clojure.lang.Var"))))
                                      (78 (ldc2_w 7))     ;; LONG:: "2"
                                      (81 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (84 (putstatic (fieldCP "const__6" "clojure.core$expand_method_impl_cache" (class "java.lang.Object"))))
                                      (87 (ldc 0))        ;;STRING:: "clojure.core"
                                      (89 (ldc 8))        ;;STRING:: "maybe-min-hash"
                                      (91 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (94 (checkcast (class "clojure.lang.Var")))
                                      (97 (putstatic (fieldCP "const__7" "clojure.core$expand_method_impl_cache" (class "clojure.lang.Var"))))
                                      (100 (ldc 0))       ;;STRING:: "clojure.core"
                                      (102 (ldc 9))       ;;STRING:: "hash"
                                      (104 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (107 (checkcast (class "clojure.lang.Var")))
                                      (110 (putstatic (fieldCP "const__8" "clojure.core$expand_method_impl_cache" (class "clojure.lang.Var"))))
                                      (113 (ldc 0))       ;;STRING:: "clojure.core"
                                      (115 (ldc 10))      ;;STRING:: "keys"
                                      (117 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (120 (checkcast (class "clojure.lang.Var")))
                                      (123 (putstatic (fieldCP "const__9" "clojure.core$expand_method_impl_cache" (class "clojure.lang.Var"))))
                                      (126 (ldc 0))       ;;STRING:: "clojure.core"
                                      (128 (ldc 11))      ;;STRING:: "nth"
                                      (130 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (133 (checkcast (class "clojure.lang.Var")))
                                      (136 (putstatic (fieldCP "const__10" "clojure.core$expand_method_impl_cache" (class "clojure.lang.Var"))))
                                      (139 (lconst_0))
                                      (140 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (143 (putstatic (fieldCP "const__11" "clojure.core$expand_method_impl_cache" (class "java.lang.Object"))))
                                      (146 (lconst_1))
                                      (147 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (150 (putstatic (fieldCP "const__12" "clojure.core$expand_method_impl_cache" (class "java.lang.Object"))))
                                      (153 (ldc 0))       ;;STRING:: "clojure.core"
                                      (155 (ldc 12))      ;;STRING:: "make-array"
                                      (157 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (160 (checkcast (class "clojure.lang.Var")))
                                      (163 (putstatic (fieldCP "const__13" "clojure.core$expand_method_impl_cache" (class "clojure.lang.Var"))))
                                      (166 (ldc 13))      ;;STRING:: "java.lang.Object"
                                      (168 (invokestatic
					(methodCP "forName" "java.lang.Class" ((class "java.lang.String")) (class "java.lang.Class"))))
                                      (171 (putstatic (fieldCP "const__14" "clojure.core$expand_method_impl_cache" (class "java.lang.Object"))))
                                      (174 (ldc 0))       ;;STRING:: "clojure.core"
                                      (176 (ldc 14))      ;;STRING:: "*"
                                      (178 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (181 (checkcast (class "clojure.lang.Var")))
                                      (184 (putstatic (fieldCP "const__15" "clojure.core$expand_method_impl_cache" (class "clojure.lang.Var"))))
                                      (187 (ldc 0))       ;;STRING:: "clojure.core"
                                      (189 (ldc 15))      ;;STRING:: "inc"
                                      (191 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (194 (checkcast (class "clojure.lang.Var")))
                                      (197 (putstatic (fieldCP "const__16" "clojure.core$expand_method_impl_cache" (class "clojure.lang.Var"))))
                                      (200 (ldc 0))       ;;STRING:: "clojure.core"
                                      (202 (ldc 16))      ;;STRING:: "reduce1"
                                      (204 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (207 (checkcast (class "clojure.lang.Var")))
                                      (210 (putstatic (fieldCP "const__17" "clojure.core$expand_method_impl_cache" (class "clojure.lang.Var"))))
                                      (213 (return))
                                      (endofcode 214))
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
                                   (max_stack . 9) (max_locals . 12) (code_length . 492)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (checkcast (class "clojure.lang.MethodImplCache"))) 
                                      (4 (getfield (fieldCP "map" "clojure.lang.MethodImplCache" (class "java.util.Map")))) 
                                      (7 (dup)) 
                                      (8 (ifnull 100)) ;;to TAG_0
                                      (11 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (14 (if_acmpeq 101)) ;;to TAG_1
                                      (17 (getstatic (fieldCP "const__0" "clojure.core$expand_method_impl_cache" (class "clojure.lang.Var")))) 
                                      (20 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (23 (checkcast (class "clojure.lang.IFn"))) 
                                      (26 (aload_1)) 
                                      (27 (checkcast (class "clojure.lang.MethodImplCache"))) 
                                      (30 (getfield (fieldCP "map" "clojure.lang.MethodImplCache" (class "java.util.Map")))) 
                                      (33 (aload_2)) 
                                      (34 (new (class "clojure.lang.MethodImplCache$Entry"))) 
                                      (37 (dup)) 
                                      (38 (aload_2)) 
                                      (39 (aconst_null)) 
                                      (40 (astore_2)) 
                                      (41 (checkcast (class "java.lang.Class"))) 
                                      (44 (aload_3)) 
                                      (45 (aconst_null)) 
                                      (46 (astore_3)) 
                                      (47 (checkcast (class "clojure.lang.IFn"))) 
                                      (50 (invokespecial (methodCP "<init>" "clojure.lang.MethodImplCache$Entry" ((class "java.lang.Class") (class "clojure.lang.IFn")) void))) 
                                      (53 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) 
                                      (58 (astore 4)) 
                                      (60 (new (class "clojure.lang.MethodImplCache"))) 
                                      (63 (dup)) 
                                      (64 (aload_1)) 
                                      (65 (checkcast (class "clojure.lang.MethodImplCache"))) 
                                      (68 (getfield (fieldCP "protocol" "clojure.lang.MethodImplCache" (class "clojure.lang.IPersistentMap")))) 
                                      (71 (checkcast (class "clojure.lang.IPersistentMap"))) 
                                      (74 (aload_1)) 
                                      (75 (aconst_null)) 
                                      (76 (astore_1)) 
                                      (77 (checkcast (class "clojure.lang.MethodImplCache"))) 
                                      (80 (getfield (fieldCP "methodk" "clojure.lang.MethodImplCache" (class "clojure.lang.Keyword")))) 
                                      (83 (checkcast (class "clojure.lang.Keyword"))) 
                                      (86 (aload 4)) 
                                      (88 (aconst_null)) 
                                      (89 (astore 4)) 
                                      (91 (checkcast (class "java.util.Map"))) 
                                      (94 (invokespecial (methodCP "<init>" "clojure.lang.MethodImplCache" ((class "clojure.lang.IPersistentMap") (class "clojure.lang.Keyword") (class "java.util.Map")) void))) 
                                      (97 (goto 491))  ;;to TAG_2
                                      (100 (pop)) ;;at TAG_0
                                      (101 (getstatic (fieldCP "const__1" "clojure.core$expand_method_impl_cache" (class "clojure.lang.Var")))) ;;at TAG_1
                                      (104 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (107 (checkcast (class "clojure.lang.IFn"))) 
                                      (110 (getstatic (fieldCP "EMPTY" "clojure.lang.PersistentArrayMap" (class "clojure.lang.PersistentArrayMap")))) 
                                      (113 (getstatic (fieldCP "const__2" "clojure.core$expand_method_impl_cache" (class "clojure.lang.Var")))) 
                                      (116 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (119 (checkcast (class "clojure.lang.IFn"))) 
                                      (122 (new (class "clojure.core$expand_method_impl_cache$fn__5831"))) 
                                      (125 (dup)) 
                                      (126 (invokespecial (methodCP "<init>" "clojure.core$expand_method_impl_cache$fn__5831" () void))) 
                                      (129 (getstatic (fieldCP "const__3" "clojure.core$expand_method_impl_cache" (class "clojure.lang.Var")))) 
                                      (132 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (135 (checkcast (class "clojure.lang.IFn"))) 
                                      (138 (getstatic (fieldCP "const__4" "clojure.core$expand_method_impl_cache" (class "clojure.lang.Var")))) 
                                      (141 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (144 (getstatic (fieldCP "const__5" "clojure.core$expand_method_impl_cache" (class "clojure.lang.Var")))) 
                                      (147 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (150 (checkcast (class "clojure.lang.IFn"))) 
                                      (153 (getstatic (fieldCP "const__6" "clojure.core$expand_method_impl_cache" (class "java.lang.Object")))) 
                                      (156 (aload_1)) 
                                      (157 (checkcast (class "clojure.lang.MethodImplCache"))) 
                                      (160 (getfield (fieldCP "table" "clojure.lang.MethodImplCache" (array (class "java.lang.Object"))))) 
                                      (163 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (168 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (173 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (178 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (183 (astore 4)) 
                                      (185 (getstatic (fieldCP "const__0" "clojure.core$expand_method_impl_cache" (class "clojure.lang.Var")))) 
                                      (188 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (191 (checkcast (class "clojure.lang.IFn"))) 
                                      (194 (aload 4)) 
                                      (196 (aconst_null)) 
                                      (197 (astore 4)) 
                                      (199 (aload_2)) 
                                      (200 (new (class "clojure.lang.MethodImplCache$Entry"))) 
                                      (203 (dup)) 
                                      (204 (aload_2)) 
                                      (205 (aconst_null)) 
                                      (206 (astore_2)) 
                                      (207 (checkcast (class "java.lang.Class"))) 
                                      (210 (aload_3)) 
                                      (211 (aconst_null)) 
                                      (212 (astore_3)) 
                                      (213 (checkcast (class "clojure.lang.IFn"))) 
                                      (216 (invokespecial (methodCP "<init>" "clojure.lang.MethodImplCache$Entry" ((class "java.lang.Class") (class "clojure.lang.IFn")) void))) 
                                      (219 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) 
                                      (224 (astore 5)) 
                                      (226 (getstatic (fieldCP "const__7" "clojure.core$expand_method_impl_cache" (class "clojure.lang.Var")))) 
                                      (229 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (232 (checkcast (class "clojure.lang.IFn"))) 
                                      (235 (getstatic (fieldCP "const__3" "clojure.core$expand_method_impl_cache" (class "clojure.lang.Var")))) 
                                      (238 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (241 (checkcast (class "clojure.lang.IFn"))) 
                                      (244 (getstatic (fieldCP "const__8" "clojure.core$expand_method_impl_cache" (class "clojure.lang.Var")))) 
                                      (247 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (250 (getstatic (fieldCP "const__9" "clojure.core$expand_method_impl_cache" (class "clojure.lang.Var")))) 
                                      (253 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (256 (checkcast (class "clojure.lang.IFn"))) 
                                      (259 (aload 5)) 
                                      (261 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (266 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (271 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (276 (astore 6)) 
                                      (278 (aload 6)) 
                                      (280 (dup)) 
                                      (281 (ifnull 453)) ;;to TAG_3
                                      (284 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (287 (if_acmpeq 454)) ;;to TAG_4
                                      (290 (aload 6)) 
                                      (292 (aconst_null)) 
                                      (293 (astore 6)) 
                                      (295 (astore 7)) 
                                      (297 (aload 7)) 
                                      (299 (lconst_0)) 
                                      (300 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (303 (aconst_null)) 
                                      (304 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (307 (astore 8)) 
                                      (309 (aload 7)) 
                                      (311 (aconst_null)) 
                                      (312 (astore 7)) 
                                      (314 (lconst_1)) 
                                      (315 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (318 (aconst_null)) 
                                      (319 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (322 (astore 9)) 
                                      (324 (getstatic (fieldCP "const__13" "clojure.core$expand_method_impl_cache" (class "clojure.lang.Var")))) 
                                      (327 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (330 (checkcast (class "clojure.lang.IFn"))) 
                                      (333 (getstatic (fieldCP "const__14" "clojure.core$expand_method_impl_cache" (class "java.lang.Object")))) 
                                      (336 (ldc2_w 7)) ;; LONG:: "2"
                                      (339 (aload 9)) 
                                      (341 (invokestatic (methodCP "inc" "clojure.lang.Numbers" ((class "java.lang.Object")) (class "java.lang.Number")))) 
                                      (344 (invokestatic (methodCP "multiply" "clojure.lang.Numbers" (long (class "java.lang.Object")) (class "java.lang.Number")))) 
                                      (347 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (352 (astore 10)) 
                                      (354 (getstatic (fieldCP "const__17" "clojure.core$expand_method_impl_cache" (class "clojure.lang.Var")))) 
                                      (357 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (360 (checkcast (class "clojure.lang.IFn"))) 
                                      (363 (new (class "clojure.core$expand_method_impl_cache$fn__5836"))) 
                                      (366 (dup)) 
                                      (367 (aload 8)) 
                                      (369 (aload 9)) 
                                      (371 (invokespecial (methodCP "<init>" "clojure.core$expand_method_impl_cache$fn__5836" ((class "java.lang.Object") (class "java.lang.Object")) void))) 
                                      (374 (aload 10)) 
                                      (376 (aconst_null)) 
                                      (377 (astore 10)) 
                                      (379 (aload 5)) 
                                      (381 (aconst_null)) 
                                      (382 (astore 5)) 
                                      (384 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) 
                                      (389 (astore 11)) 
                                      (391 (new (class "clojure.lang.MethodImplCache"))) 
                                      (394 (dup)) 
                                      (395 (aload_1)) 
                                      (396 (checkcast (class "clojure.lang.MethodImplCache"))) 
                                      (399 (getfield (fieldCP "protocol" "clojure.lang.MethodImplCache" (class "clojure.lang.IPersistentMap")))) 
                                      (402 (checkcast (class "clojure.lang.IPersistentMap"))) 
                                      (405 (aload_1)) 
                                      (406 (aconst_null)) 
                                      (407 (astore_1)) 
                                      (408 (checkcast (class "clojure.lang.MethodImplCache"))) 
                                      (411 (getfield (fieldCP "methodk" "clojure.lang.MethodImplCache" (class "clojure.lang.Keyword")))) 
                                      (414 (checkcast (class "clojure.lang.Keyword"))) 
                                      (417 (aload 8)) 
                                      (419 (aconst_null)) 
                                      (420 (astore 8)) 
                                      (422 (checkcast (class "java.lang.Number"))) 
                                      (425 (invokestatic (methodCP "intCast" "clojure.lang.RT" ((class "java.lang.Object")) int))) 
                                      (428 (aload 9)) 
                                      (430 (aconst_null)) 
                                      (431 (astore 9)) 
                                      (433 (checkcast (class "java.lang.Number"))) 
                                      (436 (invokestatic (methodCP "intCast" "clojure.lang.RT" ((class "java.lang.Object")) int))) 
                                      (439 (aload 11)) 
                                      (441 (aconst_null)) 
                                      (442 (astore 11)) 
                                      (444 (checkcast (array (class "java.lang.Object")))) 
                                      (447 (invokespecial (methodCP "<init>" "clojure.lang.MethodImplCache" ((class "clojure.lang.IPersistentMap") (class "clojure.lang.Keyword") int int (array (class "java.lang.Object"))) void))) 
                                      (450 (goto 491))  ;;to TAG_2
                                      (453 (pop)) ;;at TAG_3
                                      (454 (new (class "clojure.lang.MethodImplCache"))) ;;at TAG_4
                                      (457 (dup)) 
                                      (458 (aload_1)) 
                                      (459 (checkcast (class "clojure.lang.MethodImplCache"))) 
                                      (462 (getfield (fieldCP "protocol" "clojure.lang.MethodImplCache" (class "clojure.lang.IPersistentMap")))) 
                                      (465 (checkcast (class "clojure.lang.IPersistentMap"))) 
                                      (468 (aload_1)) 
                                      (469 (aconst_null)) 
                                      (470 (astore_1)) 
                                      (471 (checkcast (class "clojure.lang.MethodImplCache"))) 
                                      (474 (getfield (fieldCP "methodk" "clojure.lang.MethodImplCache" (class "clojure.lang.Keyword")))) 
                                      (477 (checkcast (class "clojure.lang.Keyword"))) 
                                      (480 (aload 5)) 
                                      (482 (aconst_null)) 
                                      (483 (astore 5)) 
                                      (485 (checkcast (class "java.util.Map"))) 
                                      (488 (invokespecial (methodCP "<init>" "clojure.lang.MethodImplCache" ((class "clojure.lang.IPersistentMap") (class "clojure.lang.Keyword") (class "java.util.Map")) void))) 
                                      (491 (areturn)) ;;at TAG_2
                                      (endofcode 492))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$expand_method_impl_cache-class-table*
  (make-static-class-decls 
   *clojure.core$expand_method_impl_cache*))

(defconst *package-name-map* 
  ("clojure.core$expand_method_impl_cache" . "clojure"))

