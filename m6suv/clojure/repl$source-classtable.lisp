; repl$source-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:58 CDT 2014.
;

(defconst *clojure.repl$source*
 (make-class-def
      '(class "clojure.repl$source"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "seq")
                        (STRING  "concat")
                        (STRING  "list")
                        (STRING  "println")
                        (STRING  "or")
                        (STRING  "clojure.repl")
                        (STRING  "source-fn")
                        (STRING  "quote")
                        (STRING  "str")
                        (STRING  "Source not found"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 104)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "seq"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.repl$source" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "concat"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.repl$source" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "list"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.repl$source" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 4))        ;;STRING:: "println"
                                      (43 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (46 (checkcast (class "clojure.lang.AFn")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.repl$source" (class "clojure.lang.AFn"))))
                                      (52 (ldc 0))        ;;STRING:: "clojure.core"
                                      (54 (ldc 5))        ;;STRING:: "or"
                                      (56 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (59 (checkcast (class "clojure.lang.AFn")))
                                      (62 (putstatic (fieldCP "const__4" "clojure.repl$source" (class "clojure.lang.AFn"))))
                                      (65 (ldc 6))        ;;STRING:: "clojure.repl"
                                      (67 (ldc 7))        ;;STRING:: "source-fn"
                                      (69 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (72 (checkcast (class "clojure.lang.AFn")))
                                      (75 (putstatic (fieldCP "const__5" "clojure.repl$source" (class "clojure.lang.AFn"))))
                                      (78 (aconst_null))
                                      (79 (ldc 8))        ;;STRING:: "quote"
                                      (81 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (84 (checkcast (class "clojure.lang.AFn")))
                                      (87 (putstatic (fieldCP "const__6" "clojure.repl$source" (class "clojure.lang.AFn"))))
                                      (90 (ldc 0))        ;;STRING:: "clojure.core"
                                      (92 (ldc 9))        ;;STRING:: "str"
                                      (94 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (97 (checkcast (class "clojure.lang.AFn")))
                                      (100 (putstatic (fieldCP "const__7" "clojure.repl$source" (class "clojure.lang.AFn"))))
                                      (103 (return))
                                      (endofcode 104))
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
                                   (max_stack . 18) (max_locals . 4) (code_length . 315)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.repl$source" (class "clojure.lang.Var"))))
                                      (3 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (6 (checkcast (class "clojure.lang.IFn")))
                                      (9 (getstatic (fieldCP "const__1" "clojure.repl$source" (class "clojure.lang.Var"))))
                                      (12 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (15 (checkcast (class "clojure.lang.IFn")))
                                      (18 (getstatic (fieldCP "const__2" "clojure.repl$source" (class "clojure.lang.Var"))))
                                      (21 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (24 (checkcast (class "clojure.lang.IFn")))
                                      (27 (getstatic (fieldCP "const__3" "clojure.repl$source" (class "clojure.lang.AFn"))))
                                      (30 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (35 (getstatic (fieldCP "const__2" "clojure.repl$source" (class "clojure.lang.Var"))))
                                      (38 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (41 (checkcast (class "clojure.lang.IFn")))
                                      (44 (getstatic (fieldCP "const__0" "clojure.repl$source" (class "clojure.lang.Var"))))
                                      (47 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (50 (checkcast (class "clojure.lang.IFn")))
                                      (53 (getstatic (fieldCP "const__1" "clojure.repl$source" (class "clojure.lang.Var"))))
                                      (56 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (59 (checkcast (class "clojure.lang.IFn")))
                                      (62 (getstatic (fieldCP "const__2" "clojure.repl$source" (class "clojure.lang.Var"))))
                                      (65 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (68 (checkcast (class "clojure.lang.IFn")))
                                      (71 (getstatic (fieldCP "const__4" "clojure.repl$source" (class "clojure.lang.AFn"))))
                                      (74 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (79 (getstatic (fieldCP "const__2" "clojure.repl$source" (class "clojure.lang.Var"))))
                                      (82 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (85 (checkcast (class "clojure.lang.IFn")))
                                      (88 (getstatic (fieldCP "const__0" "clojure.repl$source" (class "clojure.lang.Var"))))
                                      (91 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (94 (checkcast (class "clojure.lang.IFn")))
                                      (97 (getstatic (fieldCP "const__1" "clojure.repl$source" (class "clojure.lang.Var"))))
                                      (100 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (103 (checkcast (class "clojure.lang.IFn")))
                                      (106 (getstatic (fieldCP "const__2" "clojure.repl$source" (class "clojure.lang.Var"))))
                                      (109 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (112 (checkcast (class "clojure.lang.IFn")))
                                      (115 (getstatic (fieldCP "const__5" "clojure.repl$source" (class "clojure.lang.AFn"))))
                                      (118 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (123 (getstatic (fieldCP "const__2" "clojure.repl$source" (class "clojure.lang.Var"))))
                                      (126 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (129 (checkcast (class "clojure.lang.IFn")))
                                      (132 (getstatic (fieldCP "const__0" "clojure.repl$source" (class "clojure.lang.Var"))))
                                      (135 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (138 (checkcast (class "clojure.lang.IFn")))
                                      (141 (getstatic (fieldCP "const__1" "clojure.repl$source" (class "clojure.lang.Var"))))
                                      (144 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (147 (checkcast (class "clojure.lang.IFn")))
                                      (150 (getstatic (fieldCP "const__2" "clojure.repl$source" (class "clojure.lang.Var"))))
                                      (153 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (156 (checkcast (class "clojure.lang.IFn")))
                                      (159 (getstatic (fieldCP "const__6" "clojure.repl$source" (class "clojure.lang.AFn"))))
                                      (162 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (167 (getstatic (fieldCP "const__2" "clojure.repl$source" (class "clojure.lang.Var"))))
                                      (170 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (173 (checkcast (class "clojure.lang.IFn")))
                                      (176 (aload_3))
                                      (177 (aconst_null))
                                      (178 (astore_3))
                                      (179 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (184 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (189 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (194 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (199 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (204 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (209 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (214 (getstatic (fieldCP "const__2" "clojure.repl$source" (class "clojure.lang.Var"))))
                                      (217 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (220 (checkcast (class "clojure.lang.IFn")))
                                      (223 (getstatic (fieldCP "const__0" "clojure.repl$source" (class "clojure.lang.Var"))))
                                      (226 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (229 (checkcast (class "clojure.lang.IFn")))
                                      (232 (getstatic (fieldCP "const__1" "clojure.repl$source" (class "clojure.lang.Var"))))
                                      (235 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (238 (checkcast (class "clojure.lang.IFn")))
                                      (241 (getstatic (fieldCP "const__2" "clojure.repl$source" (class "clojure.lang.Var"))))
                                      (244 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (247 (checkcast (class "clojure.lang.IFn")))
                                      (250 (getstatic (fieldCP "const__7" "clojure.repl$source" (class "clojure.lang.AFn"))))
                                      (253 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (258 (getstatic (fieldCP "const__2" "clojure.repl$source" (class "clojure.lang.Var"))))
                                      (261 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (264 (checkcast (class "clojure.lang.IFn")))
                                      (267 (ldc 10))      ;;STRING:: "Source not found"
                                      (269 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (274 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (279 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (284 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (289 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4))
                                      (294 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (299 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (304 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (309 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (314 (areturn))
                                      (endofcode 315))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *repl$source-class-table*
  (make-static-class-decls 
   *clojure.repl$source*))

(defconst *package-name-map* 
  ("clojure.repl$source" . "clojure"))

