; core$emit_deftype_STAR_-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:41 CDT 2014.
;

(defconst *clojure.core$emit_deftype_STAR_*
 (make-class-def
      '(class "clojure.core$emit_deftype_STAR_"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "with-meta")
                        (STRING  "symbol")
                        (STRING  "str")
                        (STRING  "namespace-munge")
                        (STRING  "*ns*")
                        (STRING  "meta")
                        (STRING  "conj")
                        (STRING  "clojure.lang.IType")
                        (STRING  "seq")
                        (STRING  "concat")
                        (STRING  "list")
                        (STRING  "deftype*")
                        (STRING  "implements")
                        (STRING  "."))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__9" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__10" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__11" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__12" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 167)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "with-meta"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$emit_deftype_STAR_" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "symbol"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$emit_deftype_STAR_" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "str"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.core$emit_deftype_STAR_" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 4))        ;;STRING:: "namespace-munge"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.core$emit_deftype_STAR_" (class "clojure.lang.Var"))))
                                      (52 (ldc 0))        ;;STRING:: "clojure.core"
                                      (54 (ldc 5))        ;;STRING:: "*ns*"
                                      (56 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (59 (checkcast (class "clojure.lang.Var")))
                                      (62 (putstatic (fieldCP "const__4" "clojure.core$emit_deftype_STAR_" (class "clojure.lang.Var"))))
                                      (65 (ldc 0))        ;;STRING:: "clojure.core"
                                      (67 (ldc 6))        ;;STRING:: "meta"
                                      (69 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (72 (checkcast (class "clojure.lang.Var")))
                                      (75 (putstatic (fieldCP "const__5" "clojure.core$emit_deftype_STAR_" (class "clojure.lang.Var"))))
                                      (78 (ldc 0))        ;;STRING:: "clojure.core"
                                      (80 (ldc 7))        ;;STRING:: "conj"
                                      (82 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (85 (checkcast (class "clojure.lang.Var")))
                                      (88 (putstatic (fieldCP "const__6" "clojure.core$emit_deftype_STAR_" (class "clojure.lang.Var"))))
                                      (91 (aconst_null))
                                      (92 (ldc 8))        ;;STRING:: "clojure.lang.IType"
                                      (94 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (97 (checkcast (class "clojure.lang.AFn")))
                                      (100 (putstatic (fieldCP "const__7" "clojure.core$emit_deftype_STAR_" (class "clojure.lang.AFn"))))
                                      (103 (ldc 0))       ;;STRING:: "clojure.core"
                                      (105 (ldc 9))       ;;STRING:: "seq"
                                      (107 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (110 (checkcast (class "clojure.lang.Var")))
                                      (113 (putstatic (fieldCP "const__8" "clojure.core$emit_deftype_STAR_" (class "clojure.lang.Var"))))
                                      (116 (ldc 0))       ;;STRING:: "clojure.core"
                                      (118 (ldc 10))      ;;STRING:: "concat"
                                      (120 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (123 (checkcast (class "clojure.lang.Var")))
                                      (126 (putstatic (fieldCP "const__9" "clojure.core$emit_deftype_STAR_" (class "clojure.lang.Var"))))
                                      (129 (ldc 0))       ;;STRING:: "clojure.core"
                                      (131 (ldc 11))      ;;STRING:: "list"
                                      (133 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (136 (checkcast (class "clojure.lang.Var")))
                                      (139 (putstatic (fieldCP "const__10" "clojure.core$emit_deftype_STAR_" (class "clojure.lang.Var"))))
                                      (142 (aconst_null))
                                      (143 (ldc 12))      ;;STRING:: "deftype*"
                                      (145 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (148 (checkcast (class "clojure.lang.AFn")))
                                      (151 (putstatic (fieldCP "const__11" "clojure.core$emit_deftype_STAR_" (class "clojure.lang.AFn"))))
                                      (154 (aconst_null))
                                      (155 (ldc 13))      ;;STRING:: "implements"
                                      (157 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (160 (checkcast (class "clojure.lang.Keyword")))
                                      (163 (putstatic (fieldCP "const__12" "clojure.core$emit_deftype_STAR_" (class "clojure.lang.Keyword"))))
                                      (166 (return))
                                      (endofcode 167))
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
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 10) (max_locals . 8) (code_length . 248)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$emit_deftype_STAR_" (class "clojure.lang.Var"))))
                                      (3 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (6 (checkcast (class "clojure.lang.IFn")))
                                      (9 (getstatic (fieldCP "const__1" "clojure.core$emit_deftype_STAR_" (class "clojure.lang.Var"))))
                                      (12 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (15 (checkcast (class "clojure.lang.IFn")))
                                      (18 (getstatic (fieldCP "const__2" "clojure.core$emit_deftype_STAR_" (class "clojure.lang.Var"))))
                                      (21 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (24 (checkcast (class "clojure.lang.IFn")))
                                      (27 (getstatic (fieldCP "const__3" "clojure.core$emit_deftype_STAR_" (class "clojure.lang.Var"))))
                                      (30 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (33 (checkcast (class "clojure.lang.IFn")))
                                      (36 (getstatic (fieldCP "const__4" "clojure.core$emit_deftype_STAR_" (class "clojure.lang.Var"))))
                                      (39 (invokevirtual
					(methodCP "get" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (42 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (47 (ldc 14))       ;;STRING:: "."
                                      (49 (aload_2))
                                      (50 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4))
                                      (55 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (60 (getstatic (fieldCP "const__5" "clojure.core$emit_deftype_STAR_" (class "clojure.lang.Var"))))
                                      (63 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (66 (checkcast (class "clojure.lang.IFn")))
                                      (69 (aload_2))
                                      (70 (aconst_null))
                                      (71 (astore_2))
                                      (72 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (77 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (82 (astore 6))
                                      (84 (getstatic (fieldCP "const__6" "clojure.core$emit_deftype_STAR_" (class "clojure.lang.Var"))))
                                      (87 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (90 (checkcast (class "clojure.lang.IFn")))
                                      (93 (aload 4))
                                      (95 (aconst_null))
                                      (96 (astore 4))
                                      (98 (getstatic (fieldCP "const__7" "clojure.core$emit_deftype_STAR_" (class "clojure.lang.AFn"))))
                                      (101 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (106 (astore 7))
                                      (108 (getstatic (fieldCP "const__8" "clojure.core$emit_deftype_STAR_" (class "clojure.lang.Var"))))
                                      (111 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (114 (checkcast (class "clojure.lang.IFn")))
                                      (117 (getstatic (fieldCP "const__9" "clojure.core$emit_deftype_STAR_" (class "clojure.lang.Var"))))
                                      (120 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (123 (checkcast (class "clojure.lang.IFn")))
                                      (126 (getstatic (fieldCP "const__10" "clojure.core$emit_deftype_STAR_" (class "clojure.lang.Var"))))
                                      (129 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (132 (checkcast (class "clojure.lang.IFn")))
                                      (135 (getstatic (fieldCP "const__11" "clojure.core$emit_deftype_STAR_" (class "clojure.lang.AFn"))))
                                      (138 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (143 (getstatic (fieldCP "const__10" "clojure.core$emit_deftype_STAR_" (class "clojure.lang.Var"))))
                                      (146 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (149 (checkcast (class "clojure.lang.IFn")))
                                      (152 (aload_1))
                                      (153 (aconst_null))
                                      (154 (astore_1))
                                      (155 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (160 (getstatic (fieldCP "const__10" "clojure.core$emit_deftype_STAR_" (class "clojure.lang.Var"))))
                                      (163 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (166 (checkcast (class "clojure.lang.IFn")))
                                      (169 (aload 6))
                                      (171 (aconst_null))
                                      (172 (astore 6))
                                      (174 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (179 (getstatic (fieldCP "const__10" "clojure.core$emit_deftype_STAR_" (class "clojure.lang.Var"))))
                                      (182 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (185 (checkcast (class "clojure.lang.IFn")))
                                      (188 (aload_3))
                                      (189 (aconst_null))
                                      (190 (astore_3))
                                      (191 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (196 (getstatic (fieldCP "const__10" "clojure.core$emit_deftype_STAR_" (class "clojure.lang.Var"))))
                                      (199 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (202 (checkcast (class "clojure.lang.IFn")))
                                      (205 (getstatic (fieldCP "const__12" "clojure.core$emit_deftype_STAR_" (class "clojure.lang.Keyword"))))
                                      (208 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (213 (getstatic (fieldCP "const__10" "clojure.core$emit_deftype_STAR_" (class "clojure.lang.Var"))))
                                      (216 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (219 (checkcast (class "clojure.lang.IFn")))
                                      (222 (aload 7))
                                      (224 (aconst_null))
                                      (225 (astore 7))
                                      (227 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (232 (aload 5))
                                      (234 (aconst_null))
                                      (235 (astore 5))
                                      (237 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 8))
                                      (242 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (247 (areturn))
                                      (endofcode 248))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$emit_deftype_STAR_-class-table*
  (make-static-class-decls 
   *clojure.core$emit_deftype_STAR_*))

(defconst *package-name-map* 
  ("clojure.core$emit_deftype_STAR_" . "clojure"))

