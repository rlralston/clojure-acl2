; core$maybe_destructured-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:44 CDT 2014.
;

(defconst *clojure.core$maybe_destructured*
 (make-class-def
      '(class "clojure.core$maybe_destructured"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "every?")
                        (STRING  "symbol?")
                        (STRING  "cons")
                        (STRING  "first")
                        (STRING  "next")
                        (STRING  "conj")
                        (STRING  "gensym")
                        (STRING  "seq")
                        (STRING  "concat")
                        (STRING  "list")
                        (STRING  "let")
                        (STRING  "p__"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__9" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__10" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 144)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "every?"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$maybe_destructured" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "symbol?"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$maybe_destructured" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "cons"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.core$maybe_destructured" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 4))        ;;STRING:: "first"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.core$maybe_destructured" (class "clojure.lang.Var"))))
                                      (52 (ldc 0))        ;;STRING:: "clojure.core"
                                      (54 (ldc 5))        ;;STRING:: "next"
                                      (56 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (59 (checkcast (class "clojure.lang.Var")))
                                      (62 (putstatic (fieldCP "const__4" "clojure.core$maybe_destructured" (class "clojure.lang.Var"))))
                                      (65 (ldc 0))        ;;STRING:: "clojure.core"
                                      (67 (ldc 6))        ;;STRING:: "conj"
                                      (69 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (72 (checkcast (class "clojure.lang.Var")))
                                      (75 (putstatic (fieldCP "const__5" "clojure.core$maybe_destructured" (class "clojure.lang.Var"))))
                                      (78 (ldc 0))        ;;STRING:: "clojure.core"
                                      (80 (ldc 7))        ;;STRING:: "gensym"
                                      (82 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (85 (checkcast (class "clojure.lang.Var")))
                                      (88 (putstatic (fieldCP "const__6" "clojure.core$maybe_destructured" (class "clojure.lang.Var"))))
                                      (91 (ldc 0))        ;;STRING:: "clojure.core"
                                      (93 (ldc 8))        ;;STRING:: "seq"
                                      (95 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (98 (checkcast (class "clojure.lang.Var")))
                                      (101 (putstatic (fieldCP "const__7" "clojure.core$maybe_destructured" (class "clojure.lang.Var"))))
                                      (104 (ldc 0))       ;;STRING:: "clojure.core"
                                      (106 (ldc 9))       ;;STRING:: "concat"
                                      (108 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (111 (checkcast (class "clojure.lang.Var")))
                                      (114 (putstatic (fieldCP "const__8" "clojure.core$maybe_destructured" (class "clojure.lang.Var"))))
                                      (117 (ldc 0))       ;;STRING:: "clojure.core"
                                      (119 (ldc 10))      ;;STRING:: "list"
                                      (121 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (124 (checkcast (class "clojure.lang.Var")))
                                      (127 (putstatic (fieldCP "const__9" "clojure.core$maybe_destructured" (class "clojure.lang.Var"))))
                                      (130 (ldc 0))       ;;STRING:: "clojure.core"
                                      (132 (ldc 11))      ;;STRING:: "let"
                                      (134 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (137 (checkcast (class "clojure.lang.AFn")))
                                      (140 (putstatic (fieldCP "const__10" "clojure.core$maybe_destructured" (class "clojure.lang.AFn"))))
                                      (143 (return))
                                      (endofcode 144))
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
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 9) (max_locals . 7) (code_length . 413)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$maybe_destructured" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (getstatic (fieldCP "const__1" "clojure.core$maybe_destructured" (class "clojure.lang.Var")))) 
                                      (12 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (15 (aload_1)) 
                                      (16 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (21 (dup)) 
                                      (22 (ifnull 54)) ;;to TAG_0
                                      (25 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (28 (if_acmpeq 55))  ;;to TAG_1
                                      (31 (getstatic (fieldCP "const__2" "clojure.core$maybe_destructured" (class "clojure.lang.Var")))) 
                                      (34 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (37 (checkcast (class "clojure.lang.IFn"))) 
                                      (40 (aload_1)) 
                                      (41 (aconst_null)) 
                                      (42 (astore_1)) 
                                      (43 (aload_2)) 
                                      (44 (aconst_null)) 
                                      (45 (astore_2)) 
                                      (46 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (51 (goto 412)) ;;to TAG_2
                                      (54 (pop)) ;;at TAG_0
                                      (55 (aload_1)) ;;at TAG_1
                                      (56 (aconst_null)) 
                                      (57 (astore_1)) 
                                      (58 (astore_3)) 
                                      (59 (getstatic (fieldCP "EMPTY" "clojure.lang.PersistentVector" (class "clojure.lang.PersistentVector")))) 
                                      (62 (astore 4)) 
                                      (64 (getstatic (fieldCP "EMPTY" "clojure.lang.PersistentVector" (class "clojure.lang.PersistentVector")))) 
                                      (67 (astore 5)) 
                                      (69 (aload_3)) ;;at TAG_7
                                      (70 (dup)) 
                                      (71 (ifnull 291)) ;;to TAG_3
                                      (74 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (77 (if_acmpeq 292)) ;;to TAG_4
                                      (80 (getstatic (fieldCP "const__1" "clojure.core$maybe_destructured" (class "clojure.lang.Var")))) 
                                      (83 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (86 (checkcast (class "clojure.lang.IFn"))) 
                                      (89 (getstatic (fieldCP "const__3" "clojure.core$maybe_destructured" (class "clojure.lang.Var")))) 
                                      (92 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (95 (checkcast (class "clojure.lang.IFn"))) 
                                      (98 (aload_3)) 
                                      (99 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (104 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (109 (dup)) 
                                      (110 (ifnull 178)) ;;to TAG_5
                                      (113 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (116 (if_acmpeq 179)) ;;to TAG_6
                                      (119 (getstatic (fieldCP "const__4" "clojure.core$maybe_destructured" (class "clojure.lang.Var")))) 
                                      (122 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (125 (checkcast (class "clojure.lang.IFn"))) 
                                      (128 (aload_3)) 
                                      (129 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (134 (getstatic (fieldCP "const__5" "clojure.core$maybe_destructured" (class "clojure.lang.Var")))) 
                                      (137 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (140 (checkcast (class "clojure.lang.IFn"))) 
                                      (143 (aload 4)) 
                                      (145 (getstatic (fieldCP "const__3" "clojure.core$maybe_destructured" (class "clojure.lang.Var")))) 
                                      (148 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (151 (checkcast (class "clojure.lang.IFn"))) 
                                      (154 (aload_3)) 
                                      (155 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (160 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (165 (aload 5)) 
                                      (167 (astore 5)) 
                                      (169 (astore 4)) 
                                      (171 (astore_3)) 
                                      (172 (goto 69)) ;;to TAG_7
                                      (175 (goto 288)) ;;to TAG_8
                                      (178 (pop)) ;;at TAG_5
                                      (179 (getstatic (fieldCP "const__6" "clojure.core$maybe_destructured" (class "clojure.lang.Var")))) ;;at TAG_6
                                      (182 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (185 (checkcast (class "clojure.lang.IFn"))) 
                                      (188 (ldc 12)) ;;STRING:: "p__"
                                      (190 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (195 (astore 6)) 
                                      (197 (getstatic (fieldCP "const__4" "clojure.core$maybe_destructured" (class "clojure.lang.Var")))) 
                                      (200 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (203 (checkcast (class "clojure.lang.IFn"))) 
                                      (206 (aload_3)) 
                                      (207 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (212 (getstatic (fieldCP "const__5" "clojure.core$maybe_destructured" (class "clojure.lang.Var")))) 
                                      (215 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (218 (checkcast (class "clojure.lang.IFn"))) 
                                      (221 (aload 4)) 
                                      (223 (aload 6)) 
                                      (225 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (230 (getstatic (fieldCP "const__5" "clojure.core$maybe_destructured" (class "clojure.lang.Var")))) 
                                      (233 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (236 (checkcast (class "clojure.lang.IFn"))) 
                                      (239 (getstatic (fieldCP "const__5" "clojure.core$maybe_destructured" (class "clojure.lang.Var")))) 
                                      (242 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (245 (checkcast (class "clojure.lang.IFn"))) 
                                      (248 (aload 5)) 
                                      (250 (getstatic (fieldCP "const__3" "clojure.core$maybe_destructured" (class "clojure.lang.Var")))) 
                                      (253 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (256 (checkcast (class "clojure.lang.IFn"))) 
                                      (259 (aload_3)) 
                                      (260 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (265 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (270 (aload 6)) 
                                      (272 (aconst_null)) 
                                      (273 (astore 6)) 
                                      (275 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (280 (astore 5)) 
                                      (282 (astore 4)) 
                                      (284 (astore_3)) 
                                      (285 (goto 69)) ;;to TAG_7
                                      (288 (goto 412)) ;;to TAG_2;;at TAG_8
                                      (291 (pop)) ;;at TAG_3
                                      (292 (getstatic (fieldCP "const__7" "clojure.core$maybe_destructured" (class "clojure.lang.Var")))) ;;at TAG_4
                                      (295 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (298 (checkcast (class "clojure.lang.IFn"))) 
                                      (301 (getstatic (fieldCP "const__8" "clojure.core$maybe_destructured" (class "clojure.lang.Var")))) 
                                      (304 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (307 (checkcast (class "clojure.lang.IFn"))) 
                                      (310 (getstatic (fieldCP "const__9" "clojure.core$maybe_destructured" (class "clojure.lang.Var")))) 
                                      (313 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (316 (checkcast (class "clojure.lang.IFn"))) 
                                      (319 (aload 4)) 
                                      (321 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (326 (getstatic (fieldCP "const__9" "clojure.core$maybe_destructured" (class "clojure.lang.Var")))) 
                                      (329 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (332 (checkcast (class "clojure.lang.IFn"))) 
                                      (335 (getstatic (fieldCP "const__7" "clojure.core$maybe_destructured" (class "clojure.lang.Var")))) 
                                      (338 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (341 (checkcast (class "clojure.lang.IFn"))) 
                                      (344 (getstatic (fieldCP "const__8" "clojure.core$maybe_destructured" (class "clojure.lang.Var")))) 
                                      (347 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (350 (checkcast (class "clojure.lang.IFn"))) 
                                      (353 (getstatic (fieldCP "const__9" "clojure.core$maybe_destructured" (class "clojure.lang.Var")))) 
                                      (356 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (359 (checkcast (class "clojure.lang.IFn"))) 
                                      (362 (getstatic (fieldCP "const__10" "clojure.core$maybe_destructured" (class "clojure.lang.AFn")))) 
                                      (365 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (370 (getstatic (fieldCP "const__9" "clojure.core$maybe_destructured" (class "clojure.lang.Var")))) 
                                      (373 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (376 (checkcast (class "clojure.lang.IFn"))) 
                                      (379 (aload 5)) 
                                      (381 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (386 (aload_2)) 
                                      (387 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) 
                                      (392 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (397 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (402 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (407 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (412 (areturn)) ;;at TAG_2
                                      (endofcode 413))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$maybe_destructured-class-table*
  (make-static-class-decls 
   *clojure.core$maybe_destructured*))

(defconst *package-name-map* 
  ("clojure.core$maybe_destructured" . "clojure"))

