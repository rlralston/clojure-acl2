; core$proxy_super-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:45 CDT 2014.
;

(defconst *clojure.core$proxy_super*
 (make-class-def
      '(class "clojure.core$proxy_super"
            "clojure.lang.RestFn"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "seq")
                        (STRING  "concat")
                        (STRING  "list")
                        (STRING  "proxy-call-with-super")
                        (STRING  "fn")
                        (STRING  "apply")
                        (STRING  "vector")
                        (STRING  ".")
                        (STRING  "this")
                        (STRING  "name"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__9" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__10" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 141)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "seq"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$proxy_super" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "concat"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$proxy_super" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "list"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.core$proxy_super" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 4))        ;;STRING:: "proxy-call-with-super"
                                      (43 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (46 (checkcast (class "clojure.lang.AFn")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.core$proxy_super" (class "clojure.lang.AFn"))))
                                      (52 (ldc 0))        ;;STRING:: "clojure.core"
                                      (54 (ldc 5))        ;;STRING:: "fn"
                                      (56 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (59 (checkcast (class "clojure.lang.AFn")))
                                      (62 (putstatic (fieldCP "const__4" "clojure.core$proxy_super" (class "clojure.lang.AFn"))))
                                      (65 (ldc 0))        ;;STRING:: "clojure.core"
                                      (67 (ldc 6))        ;;STRING:: "apply"
                                      (69 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (72 (checkcast (class "clojure.lang.Var")))
                                      (75 (putstatic (fieldCP "const__5" "clojure.core$proxy_super" (class "clojure.lang.Var"))))
                                      (78 (ldc 0))        ;;STRING:: "clojure.core"
                                      (80 (ldc 7))        ;;STRING:: "vector"
                                      (82 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (85 (checkcast (class "clojure.lang.Var")))
                                      (88 (putstatic (fieldCP "const__6" "clojure.core$proxy_super" (class "clojure.lang.Var"))))
                                      (91 (aconst_null))
                                      (92 (ldc 8))        ;;STRING:: "."
                                      (94 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (97 (checkcast (class "clojure.lang.AFn")))
                                      (100 (putstatic (fieldCP "const__7" "clojure.core$proxy_super" (class "clojure.lang.AFn"))))
                                      (103 (aconst_null))
                                      (104 (ldc 9))       ;;STRING:: "this"
                                      (106 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (109 (checkcast (class "clojure.lang.AFn")))
                                      (112 (putstatic (fieldCP "const__8" "clojure.core$proxy_super" (class "clojure.lang.AFn"))))
                                      (115 (aconst_null))
                                      (116 (ldc 9))       ;;STRING:: "this"
                                      (118 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (121 (checkcast (class "clojure.lang.AFn")))
                                      (124 (putstatic (fieldCP "const__9" "clojure.core$proxy_super" (class "clojure.lang.AFn"))))
                                      (127 (ldc 0))       ;;STRING:: "clojure.core"
                                      (129 (ldc 10))      ;;STRING:: "name"
                                      (131 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (134 (checkcast (class "clojure.lang.Var")))
                                      (137 (putstatic (fieldCP "const__10" "clojure.core$proxy_super" (class "clojure.lang.Var"))))
                                      (140 (return))
                                      (endofcode 141))
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
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 16) (max_locals . 5) (code_length . 311)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$proxy_super" (class "clojure.lang.Var"))))
                                      (3 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (6 (checkcast (class "clojure.lang.IFn")))
                                      (9 (getstatic (fieldCP "const__1" "clojure.core$proxy_super" (class "clojure.lang.Var"))))
                                      (12 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (15 (checkcast (class "clojure.lang.IFn")))
                                      (18 (getstatic (fieldCP "const__2" "clojure.core$proxy_super" (class "clojure.lang.Var"))))
                                      (21 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (24 (checkcast (class "clojure.lang.IFn")))
                                      (27 (getstatic (fieldCP "const__3" "clojure.core$proxy_super" (class "clojure.lang.AFn"))))
                                      (30 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (35 (getstatic (fieldCP "const__2" "clojure.core$proxy_super" (class "clojure.lang.Var"))))
                                      (38 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (41 (checkcast (class "clojure.lang.IFn")))
                                      (44 (getstatic (fieldCP "const__0" "clojure.core$proxy_super" (class "clojure.lang.Var"))))
                                      (47 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (50 (checkcast (class "clojure.lang.IFn")))
                                      (53 (getstatic (fieldCP "const__1" "clojure.core$proxy_super" (class "clojure.lang.Var"))))
                                      (56 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (59 (checkcast (class "clojure.lang.IFn")))
                                      (62 (getstatic (fieldCP "const__2" "clojure.core$proxy_super" (class "clojure.lang.Var"))))
                                      (65 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (68 (checkcast (class "clojure.lang.IFn")))
                                      (71 (getstatic (fieldCP "const__4" "clojure.core$proxy_super" (class "clojure.lang.AFn"))))
                                      (74 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (79 (getstatic (fieldCP "const__2" "clojure.core$proxy_super" (class "clojure.lang.Var"))))
                                      (82 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (85 (checkcast (class "clojure.lang.IFn")))
                                      (88 (getstatic (fieldCP "const__5" "clojure.core$proxy_super" (class "clojure.lang.Var"))))
                                      (91 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (94 (checkcast (class "clojure.lang.IFn")))
                                      (97 (getstatic (fieldCP "const__6" "clojure.core$proxy_super" (class "clojure.lang.Var"))))
                                      (100 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (103 (getstatic (fieldCP "const__0" "clojure.core$proxy_super" (class "clojure.lang.Var"))))
                                      (106 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (109 (checkcast (class "clojure.lang.IFn")))
                                      (112 (getstatic (fieldCP "const__1" "clojure.core$proxy_super" (class "clojure.lang.Var"))))
                                      (115 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (118 (checkcast (class "clojure.lang.IFn")))
                                      (121 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1))
                                      (126 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (131 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (136 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (141 (getstatic (fieldCP "const__2" "clojure.core$proxy_super" (class "clojure.lang.Var"))))
                                      (144 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (147 (checkcast (class "clojure.lang.IFn")))
                                      (150 (getstatic (fieldCP "const__0" "clojure.core$proxy_super" (class "clojure.lang.Var"))))
                                      (153 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (156 (checkcast (class "clojure.lang.IFn")))
                                      (159 (getstatic (fieldCP "const__1" "clojure.core$proxy_super" (class "clojure.lang.Var"))))
                                      (162 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (165 (checkcast (class "clojure.lang.IFn")))
                                      (168 (getstatic (fieldCP "const__2" "clojure.core$proxy_super" (class "clojure.lang.Var"))))
                                      (171 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (174 (checkcast (class "clojure.lang.IFn")))
                                      (177 (getstatic (fieldCP "const__7" "clojure.core$proxy_super" (class "clojure.lang.AFn"))))
                                      (180 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (185 (getstatic (fieldCP "const__2" "clojure.core$proxy_super" (class "clojure.lang.Var"))))
                                      (188 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (191 (checkcast (class "clojure.lang.IFn")))
                                      (194 (getstatic (fieldCP "const__8" "clojure.core$proxy_super" (class "clojure.lang.AFn"))))
                                      (197 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (202 (getstatic (fieldCP "const__2" "clojure.core$proxy_super" (class "clojure.lang.Var"))))
                                      (205 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (208 (checkcast (class "clojure.lang.IFn")))
                                      (211 (aload_3))
                                      (212 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (217 (aload 4))
                                      (219 (aconst_null))
                                      (220 (astore 4))
                                      (222 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 5))
                                      (227 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (232 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (237 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4))
                                      (242 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (247 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (252 (getstatic (fieldCP "const__2" "clojure.core$proxy_super" (class "clojure.lang.Var"))))
                                      (255 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (258 (checkcast (class "clojure.lang.IFn")))
                                      (261 (getstatic (fieldCP "const__9" "clojure.core$proxy_super" (class "clojure.lang.AFn"))))
                                      (264 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (269 (getstatic (fieldCP "const__2" "clojure.core$proxy_super" (class "clojure.lang.Var"))))
                                      (272 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (275 (checkcast (class "clojure.lang.IFn")))
                                      (278 (getstatic (fieldCP "const__10" "clojure.core$proxy_super" (class "clojure.lang.Var"))))
                                      (281 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (284 (checkcast (class "clojure.lang.IFn")))
                                      (287 (aload_3))
                                      (288 (aconst_null))
                                      (289 (astore_3))
                                      (290 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (295 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (300 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 5))
                                      (305 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (310 (areturn))
                                      (endofcode 311))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getRequiredArity"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 2)
                                   (parsedcode
                                      (0 (iconst_3))
                                      (1 (ireturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$proxy_super-class-table*
  (make-static-class-decls 
   *clojure.core$proxy_super*))

(defconst *package-name-map* 
  ("clojure.core$proxy_super" . "clojure"))

