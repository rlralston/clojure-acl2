; pprint$render_clauses-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:57 CDT 2014.
;

(defconst *clojure.pprint$render_clauses*
 (make-class-def
      '(class "clojure.pprint$render_clauses"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "empty?")
                        (STRING  "first")
                        (STRING  "push-thread-bindings")
                        (STRING  "hash-map")
                        (STRING  "*out*")
                        (STRING  "nth")
                        (STRING  "=")
                        (STRING  "up-arrow")
                        (STRING  "second")
                        (STRING  "next")
                        (STRING  "conj"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__9" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__10" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__11" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__12" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 157)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "empty?"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.pprint$render_clauses" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "first"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.pprint$render_clauses" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "push-thread-bindings"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.pprint$render_clauses" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 4))        ;;STRING:: "hash-map"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.pprint$render_clauses" (class "clojure.lang.Var"))))
                                      (52 (ldc 0))        ;;STRING:: "clojure.core"
                                      (54 (ldc 5))        ;;STRING:: "*out*"
                                      (56 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (59 (checkcast (class "clojure.lang.Var")))
                                      (62 (putstatic (fieldCP "const__4" "clojure.pprint$render_clauses" (class "clojure.lang.Var"))))
                                      (65 (ldc 0))        ;;STRING:: "clojure.core"
                                      (67 (ldc 6))        ;;STRING:: "nth"
                                      (69 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (72 (checkcast (class "clojure.lang.Var")))
                                      (75 (putstatic (fieldCP "const__5" "clojure.pprint$render_clauses" (class "clojure.lang.Var"))))
                                      (78 (lconst_0))
                                      (79 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (82 (putstatic (fieldCP "const__6" "clojure.pprint$render_clauses" (class "java.lang.Object"))))
                                      (85 (lconst_1))
                                      (86 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (89 (putstatic (fieldCP "const__7" "clojure.pprint$render_clauses" (class "java.lang.Object"))))
                                      (92 (ldc 0))        ;;STRING:: "clojure.core"
                                      (94 (ldc 7))        ;;STRING:: "="
                                      (96 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (99 (checkcast (class "clojure.lang.Var")))
                                      (102 (putstatic (fieldCP "const__8" "clojure.pprint$render_clauses" (class "clojure.lang.Var"))))
                                      (105 (aconst_null))
                                      (106 (ldc 8))       ;;STRING:: "up-arrow"
                                      (108 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (111 (checkcast (class "clojure.lang.Keyword")))
                                      (114 (putstatic (fieldCP "const__9" "clojure.pprint$render_clauses" (class "clojure.lang.Keyword"))))
                                      (117 (ldc 0))       ;;STRING:: "clojure.core"
                                      (119 (ldc 9))       ;;STRING:: "second"
                                      (121 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (124 (checkcast (class "clojure.lang.Var")))
                                      (127 (putstatic (fieldCP "const__10" "clojure.pprint$render_clauses" (class "clojure.lang.Var"))))
                                      (130 (ldc 0))       ;;STRING:: "clojure.core"
                                      (132 (ldc 10))      ;;STRING:: "next"
                                      (134 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (137 (checkcast (class "clojure.lang.Var")))
                                      (140 (putstatic (fieldCP "const__11" "clojure.pprint$render_clauses" (class "clojure.lang.Var"))))
                                      (143 (ldc 0))       ;;STRING:: "clojure.core"
                                      (145 (ldc 11))      ;;STRING:: "conj"
                                      (147 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (150 (checkcast (class "clojure.lang.Var")))
                                      (153 (putstatic (fieldCP "const__12" "clojure.pprint$render_clauses" (class "clojure.lang.Var"))))
                                      (156 (return))
                                      (endofcode 157))
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
                                   (max_stack . 6) (max_locals . 11) (code_length . 286)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (aconst_null)) 
                                      (2 (astore_1)) 
                                      (3 (astore 4)) 
                                      (5 (getstatic (fieldCP "EMPTY" "clojure.lang.PersistentVector" (class "clojure.lang.PersistentVector")))) 
                                      (8 (astore 5)) 
                                      (10 (aload_2)) 
                                      (11 (aconst_null)) 
                                      (12 (astore_2)) 
                                      (13 (astore 6)) 
                                      (15 (getstatic (fieldCP "const__0" "clojure.pprint$render_clauses" (class "clojure.lang.Var")))) ;;at TAG_4
                                      (18 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (21 (checkcast (class "clojure.lang.IFn"))) 
                                      (24 (aload 4)) 
                                      (26 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (31 (dup)) 
                                      (32 (ifnull 61)) ;;to TAG_0
                                      (35 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (38 (if_acmpeq 62)) ;;to TAG_1
                                      (41 (iconst_2)) 
                                      (42 (anewarray (class "java.lang.Object"))) 
                                      (45 (dup)) 
                                      (46 (iconst_0)) 
                                      (47 (aload 5)) 
                                      (49 (aastore)) 
                                      (50 (dup)) 
                                      (51 (iconst_1)) 
                                      (52 (aload 6)) 
                                      (54 (aastore)) 
                                      (55 (invokestatic (methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector")))) 
                                      (58 (goto 285))  ;;to TAG_2
                                      (61 (pop)) ;;at TAG_0
                                      (62 (getstatic (fieldCP "const__1" "clojure.pprint$render_clauses" (class "clojure.lang.Var")))) ;;at TAG_1
                                      (65 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (68 (checkcast (class "clojure.lang.IFn"))) 
                                      (71 (aload 4)) 
                                      (73 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (78 (astore 7)) 
                                      (80 (getstatic (fieldCP "const__2" "clojure.pprint$render_clauses" (class "clojure.lang.Var")))) 
                                      (83 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (86 (checkcast (class "clojure.lang.IFn"))) 
                                      (89 (getstatic (fieldCP "const__3" "clojure.pprint$render_clauses" (class "clojure.lang.Var")))) 
                                      (92 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (95 (checkcast (class "clojure.lang.IFn"))) 
                                      (98 (getstatic (fieldCP "const__4" "clojure.pprint$render_clauses" (class "clojure.lang.Var")))) 
                                      (101 (new (class "java.io.StringWriter"))) 
                                      (104 (dup)) 
                                      (105 (invokespecial (methodCP "<init>" "java.io.StringWriter" () void))) 
                                      (108 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (113 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (118 (pop)) 
                                      (119 (new (class "clojure.pprint$render_clauses$fn__7793"))) 
                                      (122 (dup)) 
                                      (123 (aload_3)) 
                                      (124 (aload 6)) 
                                      (126 (aload 7)) 
                                      (128 (aconst_null)) 
                                      (129 (astore 7)) 
                                      (131 (invokespecial (methodCP "<init>" "clojure.pprint$render_clauses$fn__7793" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) void))) 
                                      (134 (checkcast (class "clojure.lang.IFn"))) 
                                      (137 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1)) 
                                      (142 (astore 8)) 
                                      (144 (aload 8)) 
                                      (146 (lconst_0)) 
                                      (147 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (150 (aconst_null)) 
                                      (151 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (154 (astore 9)) 
                                      (156 (aload 8)) 
                                      (158 (aconst_null)) 
                                      (159 (astore 8)) 
                                      (161 (lconst_1)) 
                                      (162 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (165 (aconst_null)) 
                                      (166 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (169 (astore 10)) 
                                      (171 (getstatic (fieldCP "const__9" "clojure.pprint$render_clauses" (class "clojure.lang.Keyword")))) 
                                      (174 (getstatic (fieldCP "const__1" "clojure.pprint$render_clauses" (class "clojure.lang.Var")))) 
                                      (177 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (180 (checkcast (class "clojure.lang.IFn"))) 
                                      (183 (aload 9)) 
                                      (185 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (190 (invokestatic (methodCP "equiv" "clojure.lang.Util" ((class "java.lang.Object") (class "java.lang.Object")) boolean))) 
                                      (193 (ifeq 234)) ;;to TAG_3
                                      (196 (iconst_2)) 
                                      (197 (anewarray (class "java.lang.Object"))) 
                                      (200 (dup)) 
                                      (201 (iconst_0)) 
                                      (202 (aload 5)) 
                                      (204 (aastore)) 
                                      (205 (dup)) 
                                      (206 (iconst_1)) 
                                      (207 (getstatic (fieldCP "const__10" "clojure.pprint$render_clauses" (class "clojure.lang.Var")))) 
                                      (210 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (213 (checkcast (class "clojure.lang.IFn"))) 
                                      (216 (aload 9)) 
                                      (218 (aconst_null)) 
                                      (219 (astore 9)) 
                                      (221 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (226 (aastore)) 
                                      (227 (invokestatic (methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector")))) 
                                      (230 (goto 285))  ;;to TAG_2
                                      (233 (pop)) 
                                      (234 (getstatic (fieldCP "const__11" "clojure.pprint$render_clauses" (class "clojure.lang.Var")))) ;;at TAG_3
                                      (237 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (240 (checkcast (class "clojure.lang.IFn"))) 
                                      (243 (aload 4)) 
                                      (245 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (250 (getstatic (fieldCP "const__12" "clojure.pprint$render_clauses" (class "clojure.lang.Var")))) 
                                      (253 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (256 (checkcast (class "clojure.lang.IFn"))) 
                                      (259 (aload 5)) 
                                      (261 (aload 10)) 
                                      (263 (aconst_null)) 
                                      (264 (astore 10)) 
                                      (266 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (271 (aload 9)) 
                                      (273 (aconst_null)) 
                                      (274 (astore 9)) 
                                      (276 (astore 6)) 
                                      (278 (astore 5)) 
                                      (280 (astore 4)) 
                                      (282 (goto 15)) ;;to TAG_4
                                      (285 (areturn)) ;;at TAG_2
                                      (endofcode 286))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *pprint$render_clauses-class-table*
  (make-static-class-decls 
   *clojure.pprint$render_clauses*))

(defconst *package-name-map* 
  ("clojure.pprint$render_clauses" . "clojure"))
