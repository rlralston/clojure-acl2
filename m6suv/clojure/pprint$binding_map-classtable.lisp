; pprint$binding_map-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:55 CDT 2014.
;

(defconst *clojure.pprint$binding_map*
 (make-class-def
      '(class "clojure.pprint$binding_map"
            "clojure.lang.RestFn"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "seq")
                        (STRING  "concat")
                        (STRING  "list")
                        (STRING  "do")
                        (STRING  ".")
                        (STRING  "clojure.lang.Var")
                        (STRING  "clojure.pprint")
                        (STRING  "pushThreadBindings")
                        (STRING  "try")
                        (STRING  "finally")
                        (STRING  "popThreadBindings"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__9" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__10" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__11" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 150)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "seq"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.pprint$binding_map" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "concat"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.pprint$binding_map" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "list"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.pprint$binding_map" (class "clojure.lang.Var"))))
                                      (39 (aconst_null))
                                      (40 (ldc 4))        ;;STRING:: "do"
                                      (42 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (45 (checkcast (class "clojure.lang.AFn")))
                                      (48 (putstatic (fieldCP "const__3" "clojure.pprint$binding_map" (class "clojure.lang.AFn"))))
                                      (51 (aconst_null))
                                      (52 (ldc 5))        ;;STRING:: "."
                                      (54 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (57 (checkcast (class "clojure.lang.AFn")))
                                      (60 (putstatic (fieldCP "const__4" "clojure.pprint$binding_map" (class "clojure.lang.AFn"))))
                                      (63 (aconst_null))
                                      (64 (ldc 6))        ;;STRING:: "clojure.lang.Var"
                                      (66 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (69 (checkcast (class "clojure.lang.AFn")))
                                      (72 (putstatic (fieldCP "const__5" "clojure.pprint$binding_map" (class "clojure.lang.AFn"))))
                                      (75 (ldc 7))        ;;STRING:: "clojure.pprint"
                                      (77 (ldc 8))        ;;STRING:: "pushThreadBindings"
                                      (79 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (82 (checkcast (class "clojure.lang.AFn")))
                                      (85 (putstatic (fieldCP "const__6" "clojure.pprint$binding_map" (class "clojure.lang.AFn"))))
                                      (88 (aconst_null))
                                      (89 (ldc 9))        ;;STRING:: "try"
                                      (91 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (94 (checkcast (class "clojure.lang.AFn")))
                                      (97 (putstatic (fieldCP "const__7" "clojure.pprint$binding_map" (class "clojure.lang.AFn"))))
                                      (100 (aconst_null))
                                      (101 (ldc 10))      ;;STRING:: "finally"
                                      (103 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (106 (checkcast (class "clojure.lang.AFn")))
                                      (109 (putstatic (fieldCP "const__8" "clojure.pprint$binding_map" (class "clojure.lang.AFn"))))
                                      (112 (aconst_null))
                                      (113 (ldc 5))       ;;STRING:: "."
                                      (115 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (118 (checkcast (class "clojure.lang.AFn")))
                                      (121 (putstatic (fieldCP "const__9" "clojure.pprint$binding_map" (class "clojure.lang.AFn"))))
                                      (124 (aconst_null))
                                      (125 (ldc 6))       ;;STRING:: "clojure.lang.Var"
                                      (127 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (130 (checkcast (class "clojure.lang.AFn")))
                                      (133 (putstatic (fieldCP "const__10" "clojure.pprint$binding_map" (class "clojure.lang.AFn"))))
                                      (136 (ldc 7))       ;;STRING:: "clojure.pprint"
                                      (138 (ldc 11))      ;;STRING:: "popThreadBindings"
                                      (140 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (143 (checkcast (class "clojure.lang.AFn")))
                                      (146 (putstatic (fieldCP "const__11" "clojure.pprint$binding_map" (class "clojure.lang.AFn"))))
                                      (149 (return))
                                      (endofcode 150))
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
                                   (max_stack . 23) (max_locals . 5) (code_length . 456)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.pprint$binding_map" (class "clojure.lang.Var"))))
                                      (3 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (6 (checkcast (class "clojure.lang.IFn")))
                                      (9 (getstatic (fieldCP "const__1" "clojure.pprint$binding_map" (class "clojure.lang.Var"))))
                                      (12 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (15 (checkcast (class "clojure.lang.IFn")))
                                      (18 (getstatic (fieldCP "const__2" "clojure.pprint$binding_map" (class "clojure.lang.Var"))))
                                      (21 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (24 (checkcast (class "clojure.lang.IFn")))
                                      (27 (getstatic (fieldCP "const__3" "clojure.pprint$binding_map" (class "clojure.lang.AFn"))))
                                      (30 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (35 (getstatic (fieldCP "const__2" "clojure.pprint$binding_map" (class "clojure.lang.Var"))))
                                      (38 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (41 (checkcast (class "clojure.lang.IFn")))
                                      (44 (getstatic (fieldCP "const__0" "clojure.pprint$binding_map" (class "clojure.lang.Var"))))
                                      (47 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (50 (checkcast (class "clojure.lang.IFn")))
                                      (53 (getstatic (fieldCP "const__1" "clojure.pprint$binding_map" (class "clojure.lang.Var"))))
                                      (56 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (59 (checkcast (class "clojure.lang.IFn")))
                                      (62 (getstatic (fieldCP "const__2" "clojure.pprint$binding_map" (class "clojure.lang.Var"))))
                                      (65 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (68 (checkcast (class "clojure.lang.IFn")))
                                      (71 (getstatic (fieldCP "const__4" "clojure.pprint$binding_map" (class "clojure.lang.AFn"))))
                                      (74 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (79 (getstatic (fieldCP "const__2" "clojure.pprint$binding_map" (class "clojure.lang.Var"))))
                                      (82 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (85 (checkcast (class "clojure.lang.IFn")))
                                      (88 (getstatic (fieldCP "const__5" "clojure.pprint$binding_map" (class "clojure.lang.AFn"))))
                                      (91 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (96 (getstatic (fieldCP "const__2" "clojure.pprint$binding_map" (class "clojure.lang.Var"))))
                                      (99 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (102 (checkcast (class "clojure.lang.IFn")))
                                      (105 (getstatic (fieldCP "const__0" "clojure.pprint$binding_map" (class "clojure.lang.Var"))))
                                      (108 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (111 (checkcast (class "clojure.lang.IFn")))
                                      (114 (getstatic (fieldCP "const__1" "clojure.pprint$binding_map" (class "clojure.lang.Var"))))
                                      (117 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (120 (checkcast (class "clojure.lang.IFn")))
                                      (123 (getstatic (fieldCP "const__2" "clojure.pprint$binding_map" (class "clojure.lang.Var"))))
                                      (126 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (129 (checkcast (class "clojure.lang.IFn")))
                                      (132 (getstatic (fieldCP "const__6" "clojure.pprint$binding_map" (class "clojure.lang.AFn"))))
                                      (135 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (140 (getstatic (fieldCP "const__2" "clojure.pprint$binding_map" (class "clojure.lang.Var"))))
                                      (143 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (146 (checkcast (class "clojure.lang.IFn")))
                                      (149 (aload_3))
                                      (150 (aconst_null))
                                      (151 (astore_3))
                                      (152 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (157 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (162 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (167 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (172 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4))
                                      (177 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (182 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (187 (getstatic (fieldCP "const__2" "clojure.pprint$binding_map" (class "clojure.lang.Var"))))
                                      (190 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (193 (checkcast (class "clojure.lang.IFn")))
                                      (196 (getstatic (fieldCP "const__0" "clojure.pprint$binding_map" (class "clojure.lang.Var"))))
                                      (199 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (202 (checkcast (class "clojure.lang.IFn")))
                                      (205 (getstatic (fieldCP "const__1" "clojure.pprint$binding_map" (class "clojure.lang.Var"))))
                                      (208 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (211 (checkcast (class "clojure.lang.IFn")))
                                      (214 (getstatic (fieldCP "const__2" "clojure.pprint$binding_map" (class "clojure.lang.Var"))))
                                      (217 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (220 (checkcast (class "clojure.lang.IFn")))
                                      (223 (getstatic (fieldCP "const__7" "clojure.pprint$binding_map" (class "clojure.lang.AFn"))))
                                      (226 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (231 (aload 4))
                                      (233 (aconst_null))
                                      (234 (astore 4))
                                      (236 (getstatic (fieldCP "const__2" "clojure.pprint$binding_map" (class "clojure.lang.Var"))))
                                      (239 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (242 (checkcast (class "clojure.lang.IFn")))
                                      (245 (getstatic (fieldCP "const__0" "clojure.pprint$binding_map" (class "clojure.lang.Var"))))
                                      (248 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (251 (checkcast (class "clojure.lang.IFn")))
                                      (254 (getstatic (fieldCP "const__1" "clojure.pprint$binding_map" (class "clojure.lang.Var"))))
                                      (257 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (260 (checkcast (class "clojure.lang.IFn")))
                                      (263 (getstatic (fieldCP "const__2" "clojure.pprint$binding_map" (class "clojure.lang.Var"))))
                                      (266 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (269 (checkcast (class "clojure.lang.IFn")))
                                      (272 (getstatic (fieldCP "const__8" "clojure.pprint$binding_map" (class "clojure.lang.AFn"))))
                                      (275 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (280 (getstatic (fieldCP "const__2" "clojure.pprint$binding_map" (class "clojure.lang.Var"))))
                                      (283 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (286 (checkcast (class "clojure.lang.IFn")))
                                      (289 (getstatic (fieldCP "const__0" "clojure.pprint$binding_map" (class "clojure.lang.Var"))))
                                      (292 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (295 (checkcast (class "clojure.lang.IFn")))
                                      (298 (getstatic (fieldCP "const__1" "clojure.pprint$binding_map" (class "clojure.lang.Var"))))
                                      (301 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (304 (checkcast (class "clojure.lang.IFn")))
                                      (307 (getstatic (fieldCP "const__2" "clojure.pprint$binding_map" (class "clojure.lang.Var"))))
                                      (310 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (313 (checkcast (class "clojure.lang.IFn")))
                                      (316 (getstatic (fieldCP "const__9" "clojure.pprint$binding_map" (class "clojure.lang.AFn"))))
                                      (319 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (324 (getstatic (fieldCP "const__2" "clojure.pprint$binding_map" (class "clojure.lang.Var"))))
                                      (327 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (330 (checkcast (class "clojure.lang.IFn")))
                                      (333 (getstatic (fieldCP "const__10" "clojure.pprint$binding_map" (class "clojure.lang.AFn"))))
                                      (336 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (341 (getstatic (fieldCP "const__2" "clojure.pprint$binding_map" (class "clojure.lang.Var"))))
                                      (344 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (347 (checkcast (class "clojure.lang.IFn")))
                                      (350 (getstatic (fieldCP "const__0" "clojure.pprint$binding_map" (class "clojure.lang.Var"))))
                                      (353 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (356 (checkcast (class "clojure.lang.IFn")))
                                      (359 (getstatic (fieldCP "const__1" "clojure.pprint$binding_map" (class "clojure.lang.Var"))))
                                      (362 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (365 (checkcast (class "clojure.lang.IFn")))
                                      (368 (getstatic (fieldCP "const__2" "clojure.pprint$binding_map" (class "clojure.lang.Var"))))
                                      (371 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (374 (checkcast (class "clojure.lang.IFn")))
                                      (377 (getstatic (fieldCP "const__11" "clojure.pprint$binding_map" (class "clojure.lang.AFn"))))
                                      (380 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (385 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (390 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (395 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (400 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4))
                                      (405 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (410 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (415 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (420 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (425 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (430 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4))
                                      (435 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (440 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (445 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4))
                                      (450 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (455 (areturn))
                                      (endofcode 456))
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


(defconst *pprint$binding_map-class-table*
  (make-static-class-decls 
   *clojure.pprint$binding_map*))

(defconst *package-name-map* 
  ("clojure.pprint$binding_map" . "clojure"))
