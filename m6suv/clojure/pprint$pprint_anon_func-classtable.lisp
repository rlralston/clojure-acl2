; pprint$pprint_anon_func-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:56 CDT 2014.
;

(defconst *clojure.pprint$pprint_anon_func*
 (make-class-def
      '(class "clojure.pprint$pprint_anon_func"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "second")
                        (STRING  "first")
                        (STRING  "rest")
                        (STRING  "vector?")
                        (STRING  "push-thread-bindings")
                        (STRING  "hash-map")
                        (STRING  "clojure.pprint")
                        (STRING  "*symbol-map*")
                        (STRING  "=")
                        (STRING  "count")
                        (STRING  "into")
                        (STRING  "map")
                        (STRING  "range")
                        (STRING  "inc")
                        (STRING  "string?")
                        (STRING  "cached-compile")
                        (STRING  "pop-thread-bindings")
                        (STRING  "pprint-simple-code-list")
                        (STRING  "%")
                        (STRING  "~<#(~;~@{~w~^ ~_~}~;)~:>"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__9" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__10" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__11" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__12" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__13" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__14" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__15" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__16" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__17" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 229)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "second"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.pprint$pprint_anon_func" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "first"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.pprint$pprint_anon_func" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "rest"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.pprint$pprint_anon_func" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 4))        ;;STRING:: "vector?"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.pprint$pprint_anon_func" (class "clojure.lang.Var"))))
                                      (52 (ldc 0))        ;;STRING:: "clojure.core"
                                      (54 (ldc 5))        ;;STRING:: "push-thread-bindings"
                                      (56 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (59 (checkcast (class "clojure.lang.Var")))
                                      (62 (putstatic (fieldCP "const__4" "clojure.pprint$pprint_anon_func" (class "clojure.lang.Var"))))
                                      (65 (ldc 0))        ;;STRING:: "clojure.core"
                                      (67 (ldc 6))        ;;STRING:: "hash-map"
                                      (69 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (72 (checkcast (class "clojure.lang.Var")))
                                      (75 (putstatic (fieldCP "const__5" "clojure.pprint$pprint_anon_func" (class "clojure.lang.Var"))))
                                      (78 (ldc 7))        ;;STRING:: "clojure.pprint"
                                      (80 (ldc 8))        ;;STRING:: "*symbol-map*"
                                      (82 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (85 (checkcast (class "clojure.lang.Var")))
                                      (88 (putstatic (fieldCP "const__6" "clojure.pprint$pprint_anon_func" (class "clojure.lang.Var"))))
                                      (91 (ldc 0))        ;;STRING:: "clojure.core"
                                      (93 (ldc 9))        ;;STRING:: "="
                                      (95 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (98 (checkcast (class "clojure.lang.Var")))
                                      (101 (putstatic (fieldCP "const__7" "clojure.pprint$pprint_anon_func" (class "clojure.lang.Var"))))
                                      (104 (lconst_1))
                                      (105 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (108 (putstatic (fieldCP "const__8" "clojure.pprint$pprint_anon_func" (class "java.lang.Object"))))
                                      (111 (ldc 0))       ;;STRING:: "clojure.core"
                                      (113 (ldc 10))      ;;STRING:: "count"
                                      (115 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (118 (checkcast (class "clojure.lang.Var")))
                                      (121 (putstatic (fieldCP "const__9" "clojure.pprint$pprint_anon_func" (class "clojure.lang.Var"))))
                                      (124 (ldc 0))       ;;STRING:: "clojure.core"
                                      (126 (ldc 11))      ;;STRING:: "into"
                                      (128 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (131 (checkcast (class "clojure.lang.Var")))
                                      (134 (putstatic (fieldCP "const__10" "clojure.pprint$pprint_anon_func" (class "clojure.lang.Var"))))
                                      (137 (ldc 0))       ;;STRING:: "clojure.core"
                                      (139 (ldc 12))      ;;STRING:: "map"
                                      (141 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (144 (checkcast (class "clojure.lang.Var")))
                                      (147 (putstatic (fieldCP "const__11" "clojure.pprint$pprint_anon_func" (class "clojure.lang.Var"))))
                                      (150 (ldc 0))       ;;STRING:: "clojure.core"
                                      (152 (ldc 13))      ;;STRING:: "range"
                                      (154 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (157 (checkcast (class "clojure.lang.Var")))
                                      (160 (putstatic (fieldCP "const__12" "clojure.pprint$pprint_anon_func" (class "clojure.lang.Var"))))
                                      (163 (ldc 0))       ;;STRING:: "clojure.core"
                                      (165 (ldc 14))      ;;STRING:: "inc"
                                      (167 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (170 (checkcast (class "clojure.lang.Var")))
                                      (173 (putstatic (fieldCP "const__13" "clojure.pprint$pprint_anon_func" (class "clojure.lang.Var"))))
                                      (176 (ldc 0))       ;;STRING:: "clojure.core"
                                      (178 (ldc 15))      ;;STRING:: "string?"
                                      (180 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (183 (checkcast (class "clojure.lang.Var")))
                                      (186 (putstatic (fieldCP "const__14" "clojure.pprint$pprint_anon_func" (class "clojure.lang.Var"))))
                                      (189 (ldc 7))       ;;STRING:: "clojure.pprint"
                                      (191 (ldc 16))      ;;STRING:: "cached-compile"
                                      (193 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (196 (checkcast (class "clojure.lang.Var")))
                                      (199 (putstatic (fieldCP "const__15" "clojure.pprint$pprint_anon_func" (class "clojure.lang.Var"))))
                                      (202 (ldc 0))       ;;STRING:: "clojure.core"
                                      (204 (ldc 17))      ;;STRING:: "pop-thread-bindings"
                                      (206 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (209 (checkcast (class "clojure.lang.Var")))
                                      (212 (putstatic (fieldCP "const__16" "clojure.pprint$pprint_anon_func" (class "clojure.lang.Var"))))
                                      (215 (ldc 7))       ;;STRING:: "clojure.pprint"
                                      (217 (ldc 18))      ;;STRING:: "pprint-simple-code-list"
                                      (219 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (222 (checkcast (class "clojure.lang.Var")))
                                      (225 (putstatic (fieldCP "const__17" "clojure.pprint$pprint_anon_func" (class "clojure.lang.Var"))))
                                      (228 (return))
                                      (endofcode 229))
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
                                   (max_stack . 12) (max_locals . 8) (code_length . 376)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.pprint$pprint_anon_func" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (aload_1)) 
                                      (10 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (15 (astore_2)) 
                                      (16 (getstatic (fieldCP "const__1" "clojure.pprint$pprint_anon_func" (class "clojure.lang.Var")))) 
                                      (19 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (22 (checkcast (class "clojure.lang.IFn"))) 
                                      (25 (getstatic (fieldCP "const__2" "clojure.pprint$pprint_anon_func" (class "clojure.lang.Var")))) 
                                      (28 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (31 (checkcast (class "clojure.lang.IFn"))) 
                                      (34 (getstatic (fieldCP "const__2" "clojure.pprint$pprint_anon_func" (class "clojure.lang.Var")))) 
                                      (37 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (40 (checkcast (class "clojure.lang.IFn"))) 
                                      (43 (aload_1)) 
                                      (44 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (49 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (54 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (59 (astore_3)) 
                                      (60 (getstatic (fieldCP "const__3" "clojure.pprint$pprint_anon_func" (class "clojure.lang.Var")))) 
                                      (63 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (66 (checkcast (class "clojure.lang.IFn"))) 
                                      (69 (aload_2)) 
                                      (70 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (75 (dup)) 
                                      (76 (ifnull 357)) ;;to TAG_0
                                      (79 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (82 (if_acmpeq 358))  ;;to TAG_1
                                      (85 (getstatic (fieldCP "const__4" "clojure.pprint$pprint_anon_func" (class "clojure.lang.Var")))) 
                                      (88 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (91 (checkcast (class "clojure.lang.IFn"))) 
                                      (94 (getstatic (fieldCP "const__5" "clojure.pprint$pprint_anon_func" (class "clojure.lang.Var")))) 
                                      (97 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (100 (checkcast (class "clojure.lang.IFn"))) 
                                      (103 (getstatic (fieldCP "const__6" "clojure.pprint$pprint_anon_func" (class "clojure.lang.Var")))) 
                                      (106 (lconst_1)) 
                                      (107 (aload_2)) 
                                      (108 (invokestatic (methodCP "count" "clojure.lang.RT" ((class "java.lang.Object")) int))) 
                                      (111 (i2l)) 
                                      (112 (lcmp)) 
                                      (113 (ifne 152)) ;;to TAG_2
                                      (116 (iconst_2)) 
                                      (117 (anewarray (class "java.lang.Object"))) 
                                      (120 (dup)) 
                                      (121 (iconst_0)) 
                                      (122 (getstatic (fieldCP "const__1" "clojure.pprint$pprint_anon_func" (class "clojure.lang.Var")))) 
                                      (125 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (128 (checkcast (class "clojure.lang.IFn"))) 
                                      (131 (aload_2)) 
                                      (132 (aconst_null)) 
                                      (133 (astore_2)) 
                                      (134 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (139 (aastore)) 
                                      (140 (dup)) 
                                      (141 (iconst_1)) 
                                      (142 (ldc 19)) ;;STRING:: "%"
                                      (144 (aastore)) 
                                      (145 (invokestatic (methodCP "mapUniqueKeys" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentMap")))) 
                                      (148 (goto 221)) ;;to TAG_3
                                      (151 (pop)) 
                                      (152 (getstatic (fieldCP "const__10" "clojure.pprint$pprint_anon_func" (class "clojure.lang.Var")))) ;;at TAG_2
                                      (155 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (158 (checkcast (class "clojure.lang.IFn"))) 
                                      (161 (getstatic (fieldCP "EMPTY" "clojure.lang.PersistentArrayMap" (class "clojure.lang.PersistentArrayMap")))) 
                                      (164 (getstatic (fieldCP "const__11" "clojure.pprint$pprint_anon_func" (class "clojure.lang.Var")))) 
                                      (167 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (170 (checkcast (class "clojure.lang.IFn"))) 
                                      (173 (new (class "clojure.pprint$pprint_anon_func$fn__8405"))) 
                                      (176 (dup)) 
                                      (177 (invokespecial (methodCP "<init>" "clojure.pprint$pprint_anon_func$fn__8405" () void))) 
                                      (180 (aload_2)) 
                                      (181 (getstatic (fieldCP "const__12" "clojure.pprint$pprint_anon_func" (class "clojure.lang.Var")))) 
                                      (184 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (187 (checkcast (class "clojure.lang.IFn"))) 
                                      (190 (getstatic (fieldCP "const__8" "clojure.pprint$pprint_anon_func" (class "java.lang.Object")))) 
                                      (193 (aload_2)) 
                                      (194 (aconst_null)) 
                                      (195 (astore_2)) 
                                      (196 (invokestatic (methodCP "count" "clojure.lang.RT" ((class "java.lang.Object")) int))) 
                                      (199 (i2l)) 
                                      (200 (invokestatic (methodCP "inc" "clojure.lang.Numbers" (long) long))) 
                                      (203 (invokestatic (methodCP "num" "clojure.lang.Numbers" (long) (class "java.lang.Number")))) 
                                      (206 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (211 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) 
                                      (216 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (221 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) ;;at TAG_3
                                      (226 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (231 (pop)) 
                                      (232 (ldc 20)) ;;at TAG_9;;STRING:: "~<#(~;~@{~w~^ ~_~}~;)~:>"
                                      (234 (astore 4)) 
                                      (236 (getstatic (fieldCP "const__14" "clojure.pprint$pprint_anon_func" (class "clojure.lang.Var")))) 
                                      (239 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (242 (checkcast (class "clojure.lang.IFn"))) 
                                      (245 (aload 4)) 
                                      (247 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (252 (dup)) 
                                      (253 (ifnull 281)) ;;to TAG_4
                                      (256 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (259 (if_acmpeq 282)) ;;to TAG_5
                                      (262 (getstatic (fieldCP "const__15" "clojure.pprint$pprint_anon_func" (class "clojure.lang.Var")))) 
                                      (265 (checkcast (class "clojure.lang.IFn"))) 
                                      (268 (aload 4)) 
                                      (270 (aconst_null)) 
                                      (271 (astore 4)) 
                                      (273 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (278 (goto 287)) ;;to TAG_6
                                      (281 (pop)) ;;at TAG_4
                                      (282 (aload 4)) ;;at TAG_5
                                      (284 (aconst_null)) 
                                      (285 (astore 4)) 
                                      (287 (astore 5)) ;;at TAG_6
                                      (289 (new (class "clojure.pprint$pprint_anon_func$fn__8407"))) 
                                      (292 (dup)) 
                                      (293 (aload 5)) 
                                      (295 (aconst_null)) 
                                      (296 (astore 5)) 
                                      (298 (invokespecial (methodCP "<init>" "clojure.pprint$pprint_anon_func$fn__8407" ((class "java.lang.Object")) void))) 
                                      (301 (checkcast (class "clojure.lang.IFn"))) 
                                      (304 (aload_3)) 
                                      (305 (aconst_null)) 
                                      (306 (astore_3)) 
                                      (307 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (312 (astore 6)) 
                                      (314 (getstatic (fieldCP "const__16" "clojure.pprint$pprint_anon_func" (class "clojure.lang.Var")))) ;;at TAG_10
                                      (317 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (320 (checkcast (class "clojure.lang.IFn"))) 
                                      (323 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1)) 
                                      (328 (pop)) 
                                      (329 (goto 352)) ;;to TAG_7
                                      (332 (astore 7)) ;;at TAG_11
                                      (334 (getstatic (fieldCP "const__16" "clojure.pprint$pprint_anon_func" (class "clojure.lang.Var")))) 
                                      (337 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (340 (checkcast (class "clojure.lang.IFn"))) 
                                      (343 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1)) 
                                      (348 (pop)) 
                                      (349 (aload 7)) 
                                      (351 (athrow)) 
                                      (352 (aload 6)) ;;at TAG_7
                                      (354 (goto 375)) ;;to TAG_8
                                      (357 (pop)) ;;at TAG_0
                                      (358 (getstatic (fieldCP "const__17" "clojure.pprint$pprint_anon_func" (class "clojure.lang.Var")))) ;;at TAG_1
                                      (361 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (364 (checkcast (class "clojure.lang.IFn"))) 
                                      (367 (aload_1)) 
                                      (368 (aconst_null)) 
                                      (369 (astore_1)) 
                                      (370 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (375 (areturn)) ;;at TAG_8
                                      (endofcode 376))
                                   (Exceptions 
                                     (handler 232 314  332 (class "java.lang.Throwable")))
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *pprint$pprint_anon_func-class-table*
  (make-static-class-decls 
   *clojure.pprint$pprint_anon_func*))

(defconst *package-name-map* 
  ("clojure.pprint$pprint_anon_func" . "clojure"))
