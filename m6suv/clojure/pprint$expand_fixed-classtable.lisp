; pprint$expand_fixed-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:55 CDT 2014.
;

(defconst *clojure.pprint$expand_fixed*
 (make-class-def
      '(class "clojure.pprint$expand_fixed"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "neg?")
                        (STRING  "str")
                        (STRING  "apply")
                        (STRING  "repeat")
                        (STRING  "dec")
                        (STRING  "-")
                        (LONG -1)
                        (STRING  "nth")
                        (STRING  "count")
                        (STRING  "+")
                        (STRING  "inc")
                        (STRING  "<"))
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
                        (field "const__9" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__10" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__11" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__12" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__13" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__14" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 175)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "neg?"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.pprint$expand_fixed" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "str"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.pprint$expand_fixed" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "apply"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.pprint$expand_fixed" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 4))        ;;STRING:: "repeat"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.pprint$expand_fixed" (class "clojure.lang.Var"))))
                                      (52 (ldc 0))        ;;STRING:: "clojure.core"
                                      (54 (ldc 5))        ;;STRING:: "dec"
                                      (56 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (59 (checkcast (class "clojure.lang.Var")))
                                      (62 (putstatic (fieldCP "const__4" "clojure.pprint$expand_fixed" (class "clojure.lang.Var"))))
                                      (65 (ldc 0))        ;;STRING:: "clojure.core"
                                      (67 (ldc 6))        ;;STRING:: "-"
                                      (69 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (72 (checkcast (class "clojure.lang.Var")))
                                      (75 (putstatic (fieldCP "const__5" "clojure.pprint$expand_fixed" (class "clojure.lang.Var"))))
                                      (78 (bipush 48))
                                      (80 (invokestatic
					(methodCP "valueOf" "java.lang.Character" (char) (class "java.lang.Character"))))
                                      (83 (putstatic (fieldCP "const__6" "clojure.pprint$expand_fixed" (class "java.lang.Object"))))
                                      (86 (ldc2_w 7))     ;; LONG:: "-1"
                                      (89 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (92 (putstatic (fieldCP "const__7" "clojure.pprint$expand_fixed" (class "java.lang.Object"))))
                                      (95 (ldc 0))        ;;STRING:: "clojure.core"
                                      (97 (ldc 8))        ;;STRING:: "nth"
                                      (99 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (102 (checkcast (class "clojure.lang.Var")))
                                      (105 (putstatic (fieldCP "const__8" "clojure.pprint$expand_fixed" (class "clojure.lang.Var"))))
                                      (108 (lconst_0))
                                      (109 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (112 (putstatic (fieldCP "const__9" "clojure.pprint$expand_fixed" (class "java.lang.Object"))))
                                      (115 (lconst_1))
                                      (116 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (119 (putstatic (fieldCP "const__10" "clojure.pprint$expand_fixed" (class "java.lang.Object"))))
                                      (122 (ldc 0))       ;;STRING:: "clojure.core"
                                      (124 (ldc 9))       ;;STRING:: "count"
                                      (126 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (129 (checkcast (class "clojure.lang.Var")))
                                      (132 (putstatic (fieldCP "const__11" "clojure.pprint$expand_fixed" (class "clojure.lang.Var"))))
                                      (135 (ldc 0))       ;;STRING:: "clojure.core"
                                      (137 (ldc 10))      ;;STRING:: "+"
                                      (139 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (142 (checkcast (class "clojure.lang.Var")))
                                      (145 (putstatic (fieldCP "const__12" "clojure.pprint$expand_fixed" (class "clojure.lang.Var"))))
                                      (148 (ldc 0))       ;;STRING:: "clojure.core"
                                      (150 (ldc 11))      ;;STRING:: "inc"
                                      (152 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (155 (checkcast (class "clojure.lang.Var")))
                                      (158 (putstatic (fieldCP "const__13" "clojure.pprint$expand_fixed" (class "clojure.lang.Var"))))
                                      (161 (ldc 0))       ;;STRING:: "clojure.core"
                                      (163 (ldc 12))      ;;STRING:: "<"
                                      (165 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (168 (checkcast (class "clojure.lang.Var")))
                                      (171 (putstatic (fieldCP "const__14" "clojure.pprint$expand_fixed" (class "clojure.lang.Var"))))
                                      (174 (return))
                                      (endofcode 175))
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
                                   (max_stack . 9) (max_locals . 9) (code_length . 273)
                                   (parsedcode
                                      (0 (aload_2)) 
                                      (1 (invokestatic (methodCP "isNeg" "clojure.lang.Numbers" ((class "java.lang.Object")) boolean))) 
                                      (4 (ifeq 90)) ;;to TAG_0
                                      (7 (iconst_2)) 
                                      (8 (anewarray (class "java.lang.Object"))) 
                                      (11 (dup)) 
                                      (12 (iconst_0)) 
                                      (13 (getstatic (fieldCP "const__1" "clojure.pprint$expand_fixed" (class "clojure.lang.Var")))) 
                                      (16 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (19 (checkcast (class "clojure.lang.IFn"))) 
                                      (22 (getstatic (fieldCP "const__2" "clojure.pprint$expand_fixed" (class "clojure.lang.Var")))) 
                                      (25 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (28 (checkcast (class "clojure.lang.IFn"))) 
                                      (31 (getstatic (fieldCP "const__1" "clojure.pprint$expand_fixed" (class "clojure.lang.Var")))) 
                                      (34 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (37 (getstatic (fieldCP "const__3" "clojure.pprint$expand_fixed" (class "clojure.lang.Var")))) 
                                      (40 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (43 (checkcast (class "clojure.lang.IFn"))) 
                                      (46 (aload_2)) 
                                      (47 (aconst_null)) 
                                      (48 (astore_2)) 
                                      (49 (invokestatic (methodCP "minus" "clojure.lang.Numbers" ((class "java.lang.Object")) (class "java.lang.Number")))) 
                                      (52 (invokestatic (methodCP "dec" "clojure.lang.Numbers" ((class "java.lang.Object")) (class "java.lang.Number")))) 
                                      (55 (getstatic (fieldCP "const__6" "clojure.pprint$expand_fixed" (class "java.lang.Object")))) 
                                      (58 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (63 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (68 (aload_1)) 
                                      (69 (aconst_null)) 
                                      (70 (astore_1)) 
                                      (71 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (76 (aastore)) 
                                      (77 (dup)) 
                                      (78 (iconst_1)) 
                                      (79 (getstatic (fieldCP "const__7" "clojure.pprint$expand_fixed" (class "java.lang.Object")))) 
                                      (82 (aastore)) 
                                      (83 (invokestatic (methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector")))) 
                                      (86 (goto 109)) ;;to TAG_1
                                      (89 (pop)) 
                                      (90 (iconst_2)) ;;at TAG_0
                                      (91 (anewarray (class "java.lang.Object"))) 
                                      (94 (dup)) 
                                      (95 (iconst_0)) 
                                      (96 (aload_1)) 
                                      (97 (aconst_null)) 
                                      (98 (astore_1)) 
                                      (99 (aastore)) 
                                      (100 (dup)) 
                                      (101 (iconst_1)) 
                                      (102 (aload_2)) 
                                      (103 (aconst_null)) 
                                      (104 (astore_2)) 
                                      (105 (aastore)) 
                                      (106 (invokestatic (methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector")))) 
                                      (109 (astore 4)) ;;at TAG_1
                                      (111 (aload 4)) 
                                      (113 (lconst_0)) 
                                      (114 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (117 (aconst_null)) 
                                      (118 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (121 (astore 5)) 
                                      (123 (aload 4)) 
                                      (125 (aconst_null)) 
                                      (126 (astore 4)) 
                                      (128 (lconst_1)) 
                                      (129 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (132 (aconst_null)) 
                                      (133 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (136 (astore 6)) 
                                      (138 (aload 5)) 
                                      (140 (invokestatic (methodCP "count" "clojure.lang.RT" ((class "java.lang.Object")) int))) 
                                      (143 (istore 7)) 
                                      (145 (aload_3)) 
                                      (146 (dup)) 
                                      (147 (ifnull 174))  ;;to TAG_2
                                      (150 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (153 (if_acmpeq 175)) ;;to TAG_3
                                      (156 (aload 6)) 
                                      (158 (aconst_null)) 
                                      (159 (astore 6)) 
                                      (161 (aload_3)) 
                                      (162 (aconst_null)) 
                                      (163 (astore_3)) 
                                      (164 (invokestatic (methodCP "add" "clojure.lang.Numbers" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Number")))) 
                                      (167 (lconst_1)) 
                                      (168 (invokestatic (methodCP "add" "clojure.lang.Numbers" ((class "java.lang.Object") long) (class "java.lang.Number")))) 
                                      (171 (goto 183)) ;;to TAG_4
                                      (174 (pop)) ;;at TAG_2
                                      (175 (aload 6)) ;;at TAG_3
                                      (177 (aconst_null)) 
                                      (178 (astore 6)) 
                                      (180 (invokestatic (methodCP "inc" "clojure.lang.Numbers" ((class "java.lang.Object")) (class "java.lang.Number")))) 
                                      (183 (astore 8)) ;;at TAG_4
                                      (185 (iload 7)) 
                                      (187 (i2l)) 
                                      (188 (aload 8)) 
                                      (190 (invokestatic (methodCP "lt" "clojure.lang.Numbers" (long (class "java.lang.Object")) boolean))) 
                                      (193 (ifeq 267)) ;;to TAG_5
                                      (196 (getstatic (fieldCP "const__1" "clojure.pprint$expand_fixed" (class "clojure.lang.Var")))) 
                                      (199 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (202 (checkcast (class "clojure.lang.IFn"))) 
                                      (205 (aload 5)) 
                                      (207 (aconst_null)) 
                                      (208 (astore 5)) 
                                      (210 (getstatic (fieldCP "const__2" "clojure.pprint$expand_fixed" (class "clojure.lang.Var")))) 
                                      (213 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (216 (checkcast (class "clojure.lang.IFn"))) 
                                      (219 (getstatic (fieldCP "const__1" "clojure.pprint$expand_fixed" (class "clojure.lang.Var")))) 
                                      (222 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (225 (getstatic (fieldCP "const__3" "clojure.pprint$expand_fixed" (class "clojure.lang.Var")))) 
                                      (228 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (231 (checkcast (class "clojure.lang.IFn"))) 
                                      (234 (aload 8)) 
                                      (236 (aconst_null)) 
                                      (237 (astore 8)) 
                                      (239 (iload 7)) 
                                      (241 (i2l)) 
                                      (242 (invokestatic (methodCP "minus" "clojure.lang.Numbers" ((class "java.lang.Object") long) (class "java.lang.Number")))) 
                                      (245 (getstatic (fieldCP "const__6" "clojure.pprint$expand_fixed" (class "java.lang.Object")))) 
                                      (248 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (253 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (258 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (263 (goto 272)) ;;to TAG_6
                                      (266 (pop)) 
                                      (267 (aload 5)) ;;at TAG_5
                                      (269 (aconst_null)) 
                                      (270 (astore 5)) 
                                      (272 (areturn)) ;;at TAG_6
                                      (endofcode 273))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *pprint$expand_fixed-class-table*
  (make-static-class-decls 
   *clojure.pprint$expand_fixed*))

(defconst *package-name-map* 
  ("clojure.pprint$expand_fixed" . "clojure"))
