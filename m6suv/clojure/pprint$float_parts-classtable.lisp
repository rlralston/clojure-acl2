; pprint$float_parts-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:55 CDT 2014.
;

(defconst *clojure.pprint$float_parts*
 (make-class-def
      '(class "clojure.pprint$float_parts"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.pprint")
                        (STRING  "float-parts-base")
                        (STRING  "clojure.core")
                        (STRING  "nth")
                        (STRING  "rtrim")
                        (STRING  "ltrim")
                        (STRING  "-")
                        (STRING  "count")
                        (STRING  "pos?")
                        (STRING  "=")
                        (STRING  "subs")
                        (STRING  "empty?")
                        (STRING  "0"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__9" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__10" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__11" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__12" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__13" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__14" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 5) (max_locals . 0) (code_length . 186)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.pprint"
                                      (2 (ldc 1))         ;;STRING:: "float-parts-base"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.pprint$float_parts" (class "clojure.lang.Var"))))
                                      (13 (ldc 2))        ;;STRING:: "clojure.core"
                                      (15 (ldc 3))        ;;STRING:: "nth"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.pprint$float_parts" (class "clojure.lang.Var"))))
                                      (26 (lconst_0))
                                      (27 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (30 (putstatic (fieldCP "const__2" "clojure.pprint$float_parts" (class "java.lang.Object"))))
                                      (33 (lconst_1))
                                      (34 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (37 (putstatic (fieldCP "const__3" "clojure.pprint$float_parts" (class "java.lang.Object"))))
                                      (40 (ldc 0))        ;;STRING:: "clojure.pprint"
                                      (42 (ldc 4))        ;;STRING:: "rtrim"
                                      (44 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (47 (checkcast (class "clojure.lang.Var")))
                                      (50 (putstatic (fieldCP "const__4" "clojure.pprint$float_parts" (class "clojure.lang.Var"))))
                                      (53 (bipush 48))
                                      (55 (invokestatic
					(methodCP "valueOf" "java.lang.Character" (char) (class "java.lang.Character"))))
                                      (58 (putstatic (fieldCP "const__5" "clojure.pprint$float_parts" (class "java.lang.Object"))))
                                      (61 (ldc 0))        ;;STRING:: "clojure.pprint"
                                      (63 (ldc 5))        ;;STRING:: "ltrim"
                                      (65 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (68 (checkcast (class "clojure.lang.Var")))
                                      (71 (putstatic (fieldCP "const__6" "clojure.pprint$float_parts" (class "clojure.lang.Var"))))
                                      (74 (ldc 2))        ;;STRING:: "clojure.core"
                                      (76 (ldc 6))        ;;STRING:: "-"
                                      (78 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (81 (checkcast (class "clojure.lang.Var")))
                                      (84 (putstatic (fieldCP "const__7" "clojure.pprint$float_parts" (class "clojure.lang.Var"))))
                                      (87 (ldc 2))        ;;STRING:: "clojure.core"
                                      (89 (ldc 7))        ;;STRING:: "count"
                                      (91 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (94 (checkcast (class "clojure.lang.Var")))
                                      (97 (putstatic (fieldCP "const__8" "clojure.pprint$float_parts" (class "clojure.lang.Var"))))
                                      (100 (ldc 2))       ;;STRING:: "clojure.core"
                                      (102 (ldc 8))       ;;STRING:: "pos?"
                                      (104 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (107 (checkcast (class "clojure.lang.Var")))
                                      (110 (putstatic (fieldCP "const__9" "clojure.pprint$float_parts" (class "clojure.lang.Var"))))
                                      (113 (ldc 2))       ;;STRING:: "clojure.core"
                                      (115 (ldc 9))       ;;STRING:: "="
                                      (117 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (120 (checkcast (class "clojure.lang.Var")))
                                      (123 (putstatic (fieldCP "const__10" "clojure.pprint$float_parts" (class "clojure.lang.Var"))))
                                      (126 (bipush 43))
                                      (128 (invokestatic
					(methodCP "valueOf" "java.lang.Character" (char) (class "java.lang.Character"))))
                                      (131 (putstatic (fieldCP "const__11" "clojure.pprint$float_parts" (class "java.lang.Object"))))
                                      (134 (ldc 2))       ;;STRING:: "clojure.core"
                                      (136 (ldc 10))      ;;STRING:: "subs"
                                      (138 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (141 (checkcast (class "clojure.lang.Var")))
                                      (144 (putstatic (fieldCP "const__12" "clojure.pprint$float_parts" (class "clojure.lang.Var"))))
                                      (147 (ldc 2))       ;;STRING:: "clojure.core"
                                      (149 (ldc 11))      ;;STRING:: "empty?"
                                      (151 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (154 (checkcast (class "clojure.lang.Var")))
                                      (157 (putstatic (fieldCP "const__13" "clojure.pprint$float_parts" (class "clojure.lang.Var"))))
                                      (160 (iconst_2))
                                      (161 (anewarray (class "java.lang.Object")))
                                      (164 (dup))
                                      (165 (iconst_0))
                                      (166 (ldc 12))      ;;STRING:: "0"
                                      (168 (aastore))
                                      (169 (dup))
                                      (170 (iconst_1))
                                      (171 (lconst_0))
                                      (172 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (175 (aastore))
                                      (176 (invokestatic
					(methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector"))))
                                      (179 (checkcast (class "clojure.lang.AFn")))
                                      (182 (putstatic (fieldCP "const__14" "clojure.pprint$float_parts" (class "clojure.lang.AFn"))))
                                      (185 (return))
                                      (endofcode 186))
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
                                   (max_stack . 6) (max_locals . 10) (code_length . 245)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.pprint$float_parts" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (aload_1)) 
                                      (10 (aconst_null)) 
                                      (11 (astore_1)) 
                                      (12 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (17 (astore_2)) 
                                      (18 (aload_2)) 
                                      (19 (lconst_0)) 
                                      (20 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (23 (aconst_null)) 
                                      (24 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (27 (astore_3)) 
                                      (28 (aload_2)) 
                                      (29 (aconst_null)) 
                                      (30 (astore_2)) 
                                      (31 (lconst_1)) 
                                      (32 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (35 (aconst_null)) 
                                      (36 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (39 (astore 4)) 
                                      (41 (getstatic (fieldCP "const__4" "clojure.pprint$float_parts" (class "clojure.lang.Var")))) 
                                      (44 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (47 (checkcast (class "clojure.lang.IFn"))) 
                                      (50 (aload_3)) 
                                      (51 (aconst_null)) 
                                      (52 (astore_3)) 
                                      (53 (getstatic (fieldCP "const__5" "clojure.pprint$float_parts" (class "java.lang.Object")))) 
                                      (56 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (61 (astore 5)) 
                                      (63 (getstatic (fieldCP "const__6" "clojure.pprint$float_parts" (class "clojure.lang.Var")))) 
                                      (66 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (69 (checkcast (class "clojure.lang.IFn"))) 
                                      (72 (aload 5)) 
                                      (74 (getstatic (fieldCP "const__5" "clojure.pprint$float_parts" (class "java.lang.Object")))) 
                                      (77 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (82 (astore 6)) 
                                      (84 (aload 5)) 
                                      (86 (aconst_null)) 
                                      (87 (astore 5)) 
                                      (89 (invokestatic (methodCP "count" "clojure.lang.RT" ((class "java.lang.Object")) int))) 
                                      (92 (i2l)) 
                                      (93 (aload 6)) 
                                      (95 (invokestatic (methodCP "count" "clojure.lang.RT" ((class "java.lang.Object")) int))) 
                                      (98 (i2l)) 
                                      (99 (invokestatic (methodCP "minus" "clojure.lang.Numbers" (long long) long))) 
                                      (102 (lstore 7)) 
                                      (104 (aload 4)) 
                                      (106 (invokestatic (methodCP "count" "clojure.lang.RT" ((class "java.lang.Object")) int))) 
                                      (109 (i2l)) 
                                      (110 (invokestatic (methodCP "isPos" "clojure.lang.Numbers" (long) boolean))) 
                                      (113 (istore 9)) 
                                      (115 (iload 9)) 
                                      (117 (ifeq 139)) ;;to TAG_0
                                      (120 (aload 4)) 
                                      (122 (lconst_0)) 
                                      (123 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (126 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int) (class "java.lang.Object")))) 
                                      (129 (getstatic (fieldCP "const__11" "clojure.pprint$float_parts" (class "java.lang.Object")))) 
                                      (132 (invokestatic (methodCP "equiv" "clojure.lang.Util" ((class "java.lang.Object") (class "java.lang.Object")) boolean))) 
                                      (135 (goto 141)) ;;to TAG_1
                                      (138 (pop)) 
                                      (139 (iload 9)) ;;at TAG_0
                                      (141 (ifeq 170))  ;;to TAG_2;;at TAG_1
                                      (144 (getstatic (fieldCP "const__12" "clojure.pprint$float_parts" (class "clojure.lang.Var")))) 
                                      (147 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (150 (checkcast (class "clojure.lang.IFn"))) 
                                      (153 (aload 4)) 
                                      (155 (aconst_null)) 
                                      (156 (astore 4)) 
                                      (158 (getstatic (fieldCP "const__3" "clojure.pprint$float_parts" (class "java.lang.Object")))) 
                                      (161 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (166 (goto 175)) ;;to TAG_3
                                      (169 (pop)) 
                                      (170 (aload 4)) ;;at TAG_2
                                      (172 (aconst_null)) 
                                      (173 (astore 4)) 
                                      (175 (astore 9)) ;;at TAG_3
                                      (177 (getstatic (fieldCP "const__13" "clojure.pprint$float_parts" (class "clojure.lang.Var")))) 
                                      (180 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (183 (checkcast (class "clojure.lang.IFn"))) 
                                      (186 (aload 6)) 
                                      (188 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (193 (dup)) 
                                      (194 (ifnull 209)) ;;to TAG_4
                                      (197 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (200 (if_acmpeq 210)) ;;to TAG_5
                                      (203 (getstatic (fieldCP "const__14" "clojure.pprint$float_parts" (class "clojure.lang.AFn")))) 
                                      (206 (goto 244)) ;;to TAG_6
                                      (209 (pop)) ;;at TAG_4
                                      (210 (iconst_2)) ;;at TAG_5
                                      (211 (anewarray (class "java.lang.Object"))) 
                                      (214 (dup)) 
                                      (215 (iconst_0)) 
                                      (216 (aload 6)) 
                                      (218 (aconst_null)) 
                                      (219 (astore 6)) 
                                      (221 (aastore)) 
                                      (222 (dup)) 
                                      (223 (iconst_1)) 
                                      (224 (aload 9)) 
                                      (226 (aconst_null)) 
                                      (227 (astore 9)) 
                                      (229 (checkcast (class "java.lang.String"))) 
                                      (232 (invokestatic (methodCP "valueOf" "java.lang.Integer" ((class "java.lang.String")) (class "java.lang.Integer")))) 
                                      (235 (lload 7)) 
                                      (237 (invokestatic (methodCP "minus" "clojure.lang.Numbers" ((class "java.lang.Object") long) (class "java.lang.Number")))) 
                                      (240 (aastore)) 
                                      (241 (invokestatic (methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector")))) 
                                      (244 (areturn)) ;;at TAG_6
                                      (endofcode 245))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *pprint$float_parts-class-table*
  (make-static-class-decls 
   *clojure.pprint$float_parts*))

(defconst *package-name-map* 
  ("clojure.pprint$float_parts" . "clojure"))

