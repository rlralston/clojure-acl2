; repl$re_replace-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:58 CDT 2014.
;

(defconst *clojure.repl$re_replace*
 (make-class-def
      '(class "clojure.repl$re_replace"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "re-matcher")
                        (STRING  "take-while")
                        (STRING  "identity")
                        (STRING  "repeatedly")
                        (STRING  "apply")
                        (STRING  "str")
                        (STRING  "concat")
                        (STRING  "mapcat")
                        (STRING  "cons"))
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
                        (field "const__9" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__10" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__11" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 5) (max_locals . 0) (code_length . 176)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "re-matcher"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.repl$re_replace" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "take-while"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.repl$re_replace" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "identity"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.repl$re_replace" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 4))        ;;STRING:: "repeatedly"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.repl$re_replace" (class "clojure.lang.Var"))))
                                      (52 (ldc 0))        ;;STRING:: "clojure.core"
                                      (54 (ldc 5))        ;;STRING:: "apply"
                                      (56 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (59 (checkcast (class "clojure.lang.Var")))
                                      (62 (putstatic (fieldCP "const__4" "clojure.repl$re_replace" (class "clojure.lang.Var"))))
                                      (65 (ldc 0))        ;;STRING:: "clojure.core"
                                      (67 (ldc 6))        ;;STRING:: "str"
                                      (69 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (72 (checkcast (class "clojure.lang.Var")))
                                      (75 (putstatic (fieldCP "const__5" "clojure.repl$re_replace" (class "clojure.lang.Var"))))
                                      (78 (ldc 0))        ;;STRING:: "clojure.core"
                                      (80 (ldc 7))        ;;STRING:: "concat"
                                      (82 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (85 (checkcast (class "clojure.lang.Var")))
                                      (88 (putstatic (fieldCP "const__6" "clojure.repl$re_replace" (class "clojure.lang.Var"))))
                                      (91 (ldc 0))        ;;STRING:: "clojure.core"
                                      (93 (ldc 8))        ;;STRING:: "mapcat"
                                      (95 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (98 (checkcast (class "clojure.lang.Var")))
                                      (101 (putstatic (fieldCP "const__7" "clojure.repl$re_replace" (class "clojure.lang.Var"))))
                                      (104 (ldc 0))       ;;STRING:: "clojure.core"
                                      (106 (ldc 9))       ;;STRING:: "cons"
                                      (108 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (111 (checkcast (class "clojure.lang.Var")))
                                      (114 (putstatic (fieldCP "const__8" "clojure.repl$re_replace" (class "clojure.lang.Var"))))
                                      (117 (lconst_0))
                                      (118 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (121 (putstatic (fieldCP "const__9" "clojure.repl$re_replace" (class "java.lang.Object"))))
                                      (124 (iconst_3))
                                      (125 (anewarray (class "java.lang.Object")))
                                      (128 (dup))
                                      (129 (iconst_0))
                                      (130 (lconst_0))
                                      (131 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (134 (aastore))
                                      (135 (dup))
                                      (136 (iconst_1))
                                      (137 (lconst_0))
                                      (138 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (141 (aastore))
                                      (142 (dup))
                                      (143 (iconst_2))
                                      (144 (lconst_0))
                                      (145 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (148 (aastore))
                                      (149 (invokestatic
					(methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector"))))
                                      (152 (checkcast (class "clojure.lang.AFn")))
                                      (155 (putstatic (fieldCP "const__10" "clojure.repl$re_replace" (class "clojure.lang.AFn"))))
                                      (158 (iconst_1))
                                      (159 (anewarray (class "java.lang.Object")))
                                      (162 (dup))
                                      (163 (iconst_0))
                                      (164 (aconst_null))
                                      (165 (aastore))
                                      (166 (invokestatic
					(methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector"))))
                                      (169 (checkcast (class "clojure.lang.AFn")))
                                      (172 (putstatic (fieldCP "const__11" "clojure.repl$re_replace" (class "clojure.lang.AFn"))))
                                      (175 (return))
                                      (endofcode 176))
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
                                   (max_stack . 9) (max_locals . 6) (code_length . 171)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.repl$re_replace" (class "clojure.lang.Var"))))
                                      (3 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (6 (checkcast (class "clojure.lang.IFn")))
                                      (9 (aload_1))
                                      (10 (aconst_null))
                                      (11 (astore_1))
                                      (12 (aload_2))
                                      (13 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (18 (astore 4))
                                      (20 (getstatic (fieldCP "const__1" "clojure.repl$re_replace" (class "clojure.lang.Var"))))
                                      (23 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (26 (checkcast (class "clojure.lang.IFn")))
                                      (29 (getstatic (fieldCP "const__2" "clojure.repl$re_replace" (class "clojure.lang.Var"))))
                                      (32 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (35 (getstatic (fieldCP "const__3" "clojure.repl$re_replace" (class "clojure.lang.Var"))))
                                      (38 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (41 (checkcast (class "clojure.lang.IFn")))
                                      (44 (new (class "clojure.repl$re_replace$fn__8775")))
                                      (47 (dup))
                                      (48 (aload 4))
                                      (50 (aconst_null))
                                      (51 (astore 4))
                                      (53 (invokespecial
					(methodCP "<init>" "clojure.repl$re_replace$fn__8775" ((class "java.lang.Object")) void)))
                                      (56 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (61 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (66 (astore 5))
                                      (68 (getstatic (fieldCP "const__4" "clojure.repl$re_replace" (class "clojure.lang.Var"))))
                                      (71 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (74 (checkcast (class "clojure.lang.IFn")))
                                      (77 (getstatic (fieldCP "const__5" "clojure.repl$re_replace" (class "clojure.lang.Var"))))
                                      (80 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (83 (getstatic (fieldCP "const__6" "clojure.repl$re_replace" (class "clojure.lang.Var"))))
                                      (86 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (89 (checkcast (class "clojure.lang.IFn")))
                                      (92 (getstatic (fieldCP "const__7" "clojure.repl$re_replace" (class "clojure.lang.Var"))))
                                      (95 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (98 (checkcast (class "clojure.lang.IFn")))
                                      (101 (new (class "clojure.repl$re_replace$fn__8779")))
                                      (104 (dup))
                                      (105 (aload_2))
                                      (106 (aconst_null))
                                      (107 (astore_2))
                                      (108 (aload_3))
                                      (109 (aconst_null))
                                      (110 (astore_3))
                                      (111 (invokespecial
					(methodCP "<init>" "clojure.repl$re_replace$fn__8779" ((class "java.lang.Object") (class "java.lang.Object")) void)))
                                      (114 (getstatic (fieldCP "const__8" "clojure.repl$re_replace" (class "clojure.lang.Var"))))
                                      (117 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (120 (checkcast (class "clojure.lang.IFn")))
                                      (123 (getstatic (fieldCP "const__10" "clojure.repl$re_replace" (class "clojure.lang.AFn"))))
                                      (126 (aload 5))
                                      (128 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (133 (getstatic (fieldCP "const__6" "clojure.repl$re_replace" (class "clojure.lang.Var"))))
                                      (136 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (139 (checkcast (class "clojure.lang.IFn")))
                                      (142 (aload 5))
                                      (144 (aconst_null))
                                      (145 (astore 5))
                                      (147 (getstatic (fieldCP "const__11" "clojure.repl$re_replace" (class "clojure.lang.AFn"))))
                                      (150 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (155 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4))
                                      (160 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (165 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (170 (areturn))
                                      (endofcode 171))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *repl$re_replace-class-table*
  (make-static-class-decls 
   *clojure.repl$re_replace*))

(defconst *package-name-map* 
  ("clojure.repl$re_replace" . "clojure"))

