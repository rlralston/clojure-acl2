; core$condp-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:41 CDT 2014.
;

(defconst *clojure.core$condp*
 (make-class-def
      '(class "clojure.core$condp"
            "clojure.lang.RestFn"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "gensym")
                        (STRING  "seq")
                        (STRING  "concat")
                        (STRING  "list")
                        (STRING  "let")
                        (STRING  "apply")
                        (STRING  "vector")
                        (STRING  "pred__")
                        (STRING  "expr__")
                        (STRING  "res__"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 92)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "gensym"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$condp" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "seq"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$condp" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "concat"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.core$condp" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 4))        ;;STRING:: "list"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.core$condp" (class "clojure.lang.Var"))))
                                      (52 (ldc 0))        ;;STRING:: "clojure.core"
                                      (54 (ldc 5))        ;;STRING:: "let"
                                      (56 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (59 (checkcast (class "clojure.lang.AFn")))
                                      (62 (putstatic (fieldCP "const__4" "clojure.core$condp" (class "clojure.lang.AFn"))))
                                      (65 (ldc 0))        ;;STRING:: "clojure.core"
                                      (67 (ldc 6))        ;;STRING:: "apply"
                                      (69 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (72 (checkcast (class "clojure.lang.Var")))
                                      (75 (putstatic (fieldCP "const__5" "clojure.core$condp" (class "clojure.lang.Var"))))
                                      (78 (ldc 0))        ;;STRING:: "clojure.core"
                                      (80 (ldc 7))        ;;STRING:: "vector"
                                      (82 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (85 (checkcast (class "clojure.lang.Var")))
                                      (88 (putstatic (fieldCP "const__6" "clojure.core$condp" (class "clojure.lang.Var"))))
                                      (91 (return))
                                      (endofcode 92))
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
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 14) (max_locals . 10) (code_length . 281)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$condp" (class "clojure.lang.Var"))))
                                      (3 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (6 (checkcast (class "clojure.lang.IFn")))
                                      (9 (ldc 8))         ;;STRING:: "pred__"
                                      (11 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (16 (astore 6))
                                      (18 (getstatic (fieldCP "const__0" "clojure.core$condp" (class "clojure.lang.Var"))))
                                      (21 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (24 (checkcast (class "clojure.lang.IFn")))
                                      (27 (ldc 9))        ;;STRING:: "expr__"
                                      (29 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (34 (astore 7))
                                      (36 (new (class "clojure.core$condp$emit__5054")))
                                      (39 (dup))
                                      (40 (invokespecial
					(methodCP "<init>" "clojure.core$condp$emit__5054" () void)))
                                      (43 (astore 8))
                                      (45 (getstatic (fieldCP "const__0" "clojure.core$condp" (class "clojure.lang.Var"))))
                                      (48 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (51 (checkcast (class "clojure.lang.IFn")))
                                      (54 (ldc 10))       ;;STRING:: "res__"
                                      (56 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (61 (astore 9))
                                      (63 (getstatic (fieldCP "const__1" "clojure.core$condp" (class "clojure.lang.Var"))))
                                      (66 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (69 (checkcast (class "clojure.lang.IFn")))
                                      (72 (getstatic (fieldCP "const__2" "clojure.core$condp" (class "clojure.lang.Var"))))
                                      (75 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (78 (checkcast (class "clojure.lang.IFn")))
                                      (81 (getstatic (fieldCP "const__3" "clojure.core$condp" (class "clojure.lang.Var"))))
                                      (84 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (87 (checkcast (class "clojure.lang.IFn")))
                                      (90 (getstatic (fieldCP "const__4" "clojure.core$condp" (class "clojure.lang.AFn"))))
                                      (93 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (98 (getstatic (fieldCP "const__3" "clojure.core$condp" (class "clojure.lang.Var"))))
                                      (101 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (104 (checkcast (class "clojure.lang.IFn")))
                                      (107 (getstatic (fieldCP "const__5" "clojure.core$condp" (class "clojure.lang.Var"))))
                                      (110 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (113 (checkcast (class "clojure.lang.IFn")))
                                      (116 (getstatic (fieldCP "const__6" "clojure.core$condp" (class "clojure.lang.Var"))))
                                      (119 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (122 (getstatic (fieldCP "const__1" "clojure.core$condp" (class "clojure.lang.Var"))))
                                      (125 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (128 (checkcast (class "clojure.lang.IFn")))
                                      (131 (getstatic (fieldCP "const__2" "clojure.core$condp" (class "clojure.lang.Var"))))
                                      (134 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (137 (checkcast (class "clojure.lang.IFn")))
                                      (140 (getstatic (fieldCP "const__3" "clojure.core$condp" (class "clojure.lang.Var"))))
                                      (143 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (146 (checkcast (class "clojure.lang.IFn")))
                                      (149 (aload 6))
                                      (151 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (156 (getstatic (fieldCP "const__3" "clojure.core$condp" (class "clojure.lang.Var"))))
                                      (159 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (162 (checkcast (class "clojure.lang.IFn")))
                                      (165 (aload_3))
                                      (166 (aconst_null))
                                      (167 (astore_3))
                                      (168 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (173 (getstatic (fieldCP "const__3" "clojure.core$condp" (class "clojure.lang.Var"))))
                                      (176 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (179 (checkcast (class "clojure.lang.IFn")))
                                      (182 (aload 7))
                                      (184 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (189 (getstatic (fieldCP "const__3" "clojure.core$condp" (class "clojure.lang.Var"))))
                                      (192 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (195 (checkcast (class "clojure.lang.IFn")))
                                      (198 (aload 4))
                                      (200 (aconst_null))
                                      (201 (astore 4))
                                      (203 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (208 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 5))
                                      (213 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (218 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (223 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (228 (getstatic (fieldCP "const__3" "clojure.core$condp" (class "clojure.lang.Var"))))
                                      (231 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (234 (checkcast (class "clojure.lang.IFn")))
                                      (237 (aload 8))
                                      (239 (aconst_null))
                                      (240 (astore 8))
                                      (242 (checkcast (class "clojure.lang.IFn")))
                                      (245 (aload 6))
                                      (247 (aconst_null))
                                      (248 (astore 6))
                                      (250 (aload 7))
                                      (252 (aconst_null))
                                      (253 (astore 7))
                                      (255 (aload 5))
                                      (257 (aconst_null))
                                      (258 (astore 5))
                                      (260 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4))
                                      (265 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (270 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4))
                                      (275 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (280 (areturn))
                                      (endofcode 281))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getRequiredArity"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 2)
                                   (parsedcode
                                      (0 (iconst_4))
                                      (1 (ireturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$condp-class-table*
  (make-static-class-decls 
   *clojure.core$condp*))

(defconst *package-name-map* 
  ("clojure.core$condp" . "clojure"))

