; core$emit_protocol$fn__5905-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:42 CDT 2014.
;

(defconst *clojure.core$emit_protocol$fn__5905*
 (make-class-def
      '(class "clojure.core$emit_protocol$fn__5905"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "on")
                        (STRING  "clojure.core")
                        (STRING  "list")
                        (STRING  "quote")
                        (STRING  "on-interface")
                        (STRING  "first")
                        (STRING  "string?")
                        (STRING  "assoc")
                        (STRING  "doc")
                        (STRING  "next")
                        (STRING  "keyword?")
                        (STRING  "second")
                        (STRING  "nnext"))
            (fields
                        (field "const__0" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__9" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__10" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__11" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "opts_PLUS_sigs" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "iname" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 153)
                                   (parsedcode
                                      (0 (aconst_null))
                                      (1 (ldc 0))         ;;STRING:: "on"
                                      (3 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (6 (checkcast (class "clojure.lang.Keyword")))
                                      (9 (putstatic (fieldCP "const__0" "clojure.core$emit_protocol$fn__5905" (class "clojure.lang.Keyword"))))
                                      (12 (ldc 1))        ;;STRING:: "clojure.core"
                                      (14 (ldc 2))        ;;STRING:: "list"
                                      (16 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (19 (checkcast (class "clojure.lang.Var")))
                                      (22 (putstatic (fieldCP "const__1" "clojure.core$emit_protocol$fn__5905" (class "clojure.lang.Var"))))
                                      (25 (aconst_null))
                                      (26 (ldc 3))        ;;STRING:: "quote"
                                      (28 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (31 (checkcast (class "clojure.lang.AFn")))
                                      (34 (putstatic (fieldCP "const__2" "clojure.core$emit_protocol$fn__5905" (class "clojure.lang.AFn"))))
                                      (37 (aconst_null))
                                      (38 (ldc 4))        ;;STRING:: "on-interface"
                                      (40 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (43 (checkcast (class "clojure.lang.Keyword")))
                                      (46 (putstatic (fieldCP "const__3" "clojure.core$emit_protocol$fn__5905" (class "clojure.lang.Keyword"))))
                                      (49 (ldc 1))        ;;STRING:: "clojure.core"
                                      (51 (ldc 5))        ;;STRING:: "first"
                                      (53 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (56 (checkcast (class "clojure.lang.Var")))
                                      (59 (putstatic (fieldCP "const__4" "clojure.core$emit_protocol$fn__5905" (class "clojure.lang.Var"))))
                                      (62 (ldc 1))        ;;STRING:: "clojure.core"
                                      (64 (ldc 6))        ;;STRING:: "string?"
                                      (66 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (69 (checkcast (class "clojure.lang.Var")))
                                      (72 (putstatic (fieldCP "const__5" "clojure.core$emit_protocol$fn__5905" (class "clojure.lang.Var"))))
                                      (75 (ldc 1))        ;;STRING:: "clojure.core"
                                      (77 (ldc 7))        ;;STRING:: "assoc"
                                      (79 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (82 (checkcast (class "clojure.lang.Var")))
                                      (85 (putstatic (fieldCP "const__6" "clojure.core$emit_protocol$fn__5905" (class "clojure.lang.Var"))))
                                      (88 (aconst_null))
                                      (89 (ldc 8))        ;;STRING:: "doc"
                                      (91 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (94 (checkcast (class "clojure.lang.Keyword")))
                                      (97 (putstatic (fieldCP "const__7" "clojure.core$emit_protocol$fn__5905" (class "clojure.lang.Keyword"))))
                                      (100 (ldc 1))       ;;STRING:: "clojure.core"
                                      (102 (ldc 9))       ;;STRING:: "next"
                                      (104 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (107 (checkcast (class "clojure.lang.Var")))
                                      (110 (putstatic (fieldCP "const__8" "clojure.core$emit_protocol$fn__5905" (class "clojure.lang.Var"))))
                                      (113 (ldc 1))       ;;STRING:: "clojure.core"
                                      (115 (ldc 10))      ;;STRING:: "keyword?"
                                      (117 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (120 (checkcast (class "clojure.lang.Var")))
                                      (123 (putstatic (fieldCP "const__9" "clojure.core$emit_protocol$fn__5905" (class "clojure.lang.Var"))))
                                      (126 (ldc 1))       ;;STRING:: "clojure.core"
                                      (128 (ldc 11))      ;;STRING:: "second"
                                      (130 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (133 (checkcast (class "clojure.lang.Var")))
                                      (136 (putstatic (fieldCP "const__10" "clojure.core$emit_protocol$fn__5905" (class "clojure.lang.Var"))))
                                      (139 (ldc 1))       ;;STRING:: "clojure.core"
                                      (141 (ldc 12))      ;;STRING:: "nnext"
                                      (143 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (146 (checkcast (class "clojure.lang.Var")))
                                      (149 (putstatic (fieldCP "const__11" "clojure.core$emit_protocol$fn__5905" (class "clojure.lang.Var"))))
                                      (152 (return))
                                      (endofcode 153))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.AFunction" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "opts_PLUS_sigs" "clojure.core$emit_protocol$fn__5905" (class "java.lang.Object"))))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "iname" "clojure.core$emit_protocol$fn__5905" (class "java.lang.Object"))))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 5) (code_length . 292)
                                   (parsedcode
                                      (0 (iconst_4)) 
                                      (1 (anewarray (class "java.lang.Object"))) 
                                      (4 (dup)) 
                                      (5 (iconst_0)) 
                                      (6 (getstatic (fieldCP "const__0" "clojure.core$emit_protocol$fn__5905" (class "clojure.lang.Keyword")))) 
                                      (9 (aastore)) 
                                      (10 (dup)) 
                                      (11 (iconst_1)) 
                                      (12 (getstatic (fieldCP "const__1" "clojure.core$emit_protocol$fn__5905" (class "clojure.lang.Var")))) 
                                      (15 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (18 (checkcast (class "clojure.lang.IFn"))) 
                                      (21 (getstatic (fieldCP "const__2" "clojure.core$emit_protocol$fn__5905" (class "clojure.lang.AFn")))) 
                                      (24 (aload_0)) 
                                      (25 (getfield (fieldCP "iname" "clojure.core$emit_protocol$fn__5905" (class "java.lang.Object")))) 
                                      (28 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (33 (aastore)) 
                                      (34 (dup)) 
                                      (35 (iconst_2)) 
                                      (36 (getstatic (fieldCP "const__3" "clojure.core$emit_protocol$fn__5905" (class "clojure.lang.Keyword")))) 
                                      (39 (aastore)) 
                                      (40 (dup)) 
                                      (41 (iconst_3)) 
                                      (42 (aload_0)) 
                                      (43 (getfield (fieldCP "iname" "clojure.core$emit_protocol$fn__5905" (class "java.lang.Object")))) 
                                      (46 (aload_0)) 
                                      (47 (aconst_null)) 
                                      (48 (putfield (fieldCP "iname" "clojure.core$emit_protocol$fn__5905" (class "java.lang.Object")))) 
                                      (51 (aastore)) 
                                      (52 (invokestatic (methodCP "mapUniqueKeys" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentMap")))) 
                                      (55 (astore_1)) 
                                      (56 (aload_0)) 
                                      (57 (getfield (fieldCP "opts_PLUS_sigs" "clojure.core$emit_protocol$fn__5905" (class "java.lang.Object")))) 
                                      (60 (aload_0)) 
                                      (61 (aconst_null)) 
                                      (62 (putfield (fieldCP "opts_PLUS_sigs" "clojure.core$emit_protocol$fn__5905" (class "java.lang.Object")))) 
                                      (65 (astore_2)) 
                                      (66 (new (class "clojure.core$emit_protocol$fn__5905$pred__5906__5909"))) ;;at TAG_2
                                      (69 (dup)) 
                                      (70 (invokespecial (methodCP "<init>" "clojure.core$emit_protocol$fn__5905$pred__5906__5909" () void))) 
                                      (73 (astore_3)) 
                                      (74 (getstatic (fieldCP "const__4" "clojure.core$emit_protocol$fn__5905" (class "clojure.lang.Var")))) 
                                      (77 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (80 (checkcast (class "clojure.lang.IFn"))) 
                                      (83 (aload_2)) 
                                      (84 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (89 (astore 4)) 
                                      (91 (aload_3)) 
                                      (92 (checkcast (class "clojure.lang.IFn"))) 
                                      (95 (getstatic (fieldCP "const__5" "clojure.core$emit_protocol$fn__5905" (class "clojure.lang.Var")))) 
                                      (98 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (101 (aload 4)) 
                                      (103 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (108 (dup)) 
                                      (109 (ifnull 174)) ;;to TAG_0
                                      (112 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (115 (if_acmpeq 175)) ;;to TAG_1
                                      (118 (getstatic (fieldCP "const__6" "clojure.core$emit_protocol$fn__5905" (class "clojure.lang.Var")))) 
                                      (121 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (124 (checkcast (class "clojure.lang.IFn"))) 
                                      (127 (aload_1)) 
                                      (128 (getstatic (fieldCP "const__7" "clojure.core$emit_protocol$fn__5905" (class "clojure.lang.Keyword")))) 
                                      (131 (getstatic (fieldCP "const__4" "clojure.core$emit_protocol$fn__5905" (class "clojure.lang.Var")))) 
                                      (134 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (137 (checkcast (class "clojure.lang.IFn"))) 
                                      (140 (aload_2)) 
                                      (141 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (146 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) 
                                      (151 (getstatic (fieldCP "const__8" "clojure.core$emit_protocol$fn__5905" (class "clojure.lang.Var")))) 
                                      (154 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (157 (checkcast (class "clojure.lang.IFn"))) 
                                      (160 (aload_2)) 
                                      (161 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (166 (astore_2)) 
                                      (167 (astore_1)) 
                                      (168 (goto 66))  ;;to TAG_2
                                      (171 (goto 291)) ;;to TAG_3
                                      (174 (pop)) ;;at TAG_0
                                      (175 (aload_3)) ;;at TAG_1
                                      (176 (aconst_null)) 
                                      (177 (astore_3)) 
                                      (178 (checkcast (class "clojure.lang.IFn"))) 
                                      (181 (getstatic (fieldCP "const__9" "clojure.core$emit_protocol$fn__5905" (class "clojure.lang.Var")))) 
                                      (184 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (187 (aload 4)) 
                                      (189 (aconst_null)) 
                                      (190 (astore 4)) 
                                      (192 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (197 (dup)) 
                                      (198 (ifnull 275)) ;;to TAG_4
                                      (201 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (204 (if_acmpeq 276)) ;;to TAG_5
                                      (207 (getstatic (fieldCP "const__6" "clojure.core$emit_protocol$fn__5905" (class "clojure.lang.Var")))) 
                                      (210 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (213 (checkcast (class "clojure.lang.IFn"))) 
                                      (216 (aload_1)) 
                                      (217 (getstatic (fieldCP "const__4" "clojure.core$emit_protocol$fn__5905" (class "clojure.lang.Var")))) 
                                      (220 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (223 (checkcast (class "clojure.lang.IFn"))) 
                                      (226 (aload_2)) 
                                      (227 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (232 (getstatic (fieldCP "const__10" "clojure.core$emit_protocol$fn__5905" (class "clojure.lang.Var")))) 
                                      (235 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (238 (checkcast (class "clojure.lang.IFn"))) 
                                      (241 (aload_2)) 
                                      (242 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (247 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) 
                                      (252 (getstatic (fieldCP "const__11" "clojure.core$emit_protocol$fn__5905" (class "clojure.lang.Var")))) 
                                      (255 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (258 (checkcast (class "clojure.lang.IFn"))) 
                                      (261 (aload_2)) 
                                      (262 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (267 (astore_2)) 
                                      (268 (astore_1)) 
                                      (269 (goto 66))  ;;to TAG_2
                                      (272 (goto 291)) ;;to TAG_3
                                      (275 (pop)) ;;at TAG_4
                                      (276 (iconst_2)) ;;at TAG_5
                                      (277 (anewarray (class "java.lang.Object"))) 
                                      (280 (dup)) 
                                      (281 (iconst_0)) 
                                      (282 (aload_1)) 
                                      (283 (aastore)) 
                                      (284 (dup)) 
                                      (285 (iconst_1)) 
                                      (286 (aload_2)) 
                                      (287 (aastore)) 
                                      (288 (invokestatic (methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector")))) 
                                      (291 (areturn)) ;;at TAG_3
                                      (endofcode 292))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$emit_protocol$fn__5905-class-table*
  (make-static-class-decls 
   *clojure.core$emit_protocol$fn__5905*))

(defconst *package-name-map* 
  ("clojure.core$emit_protocol$fn__5905" . "clojure"))

