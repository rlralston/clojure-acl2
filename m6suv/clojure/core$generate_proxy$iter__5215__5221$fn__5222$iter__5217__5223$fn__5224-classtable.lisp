; core$generate_proxy$iter__5215__5221$fn__5222$iter__5217__5223$fn__5224-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:43 CDT 2014.
;

(defconst *clojure.core$generate_proxy$iter__5215__5221$fn__5222$iter__5217__5223$fn__5224*
 (make-class-def
      '(class "clojure.core$generate_proxy$iter__5215__5221$fn__5222$iter__5217__5223$fn__5224"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "seq")
                        (STRING  "chunked-seq?")
                        (STRING  "chunk-first")
                        (STRING  "int")
                        (STRING  "count")
                        (STRING  "chunk-buffer")
                        (STRING  "chunk-cons")
                        (STRING  "chunk")
                        (STRING  "chunk-rest")
                        (STRING  "first")
                        (STRING  "method-sig")
                        (STRING  "not")
                        (STRING  "cons")
                        (STRING  "rest"))
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
                        (field "const__10" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__11" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__12" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__13" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "s__5218" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "considered" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "iter__5217" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 183)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "seq"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$generate_proxy$iter__5215__5221$fn__5222$iter__5217__5223$fn__5224" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "chunked-seq?"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$generate_proxy$iter__5215__5221$fn__5222$iter__5217__5223$fn__5224" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "chunk-first"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.core$generate_proxy$iter__5215__5221$fn__5222$iter__5217__5223$fn__5224" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 4))        ;;STRING:: "int"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.core$generate_proxy$iter__5215__5221$fn__5222$iter__5217__5223$fn__5224" (class "clojure.lang.Var"))))
                                      (52 (ldc 0))        ;;STRING:: "clojure.core"
                                      (54 (ldc 5))        ;;STRING:: "count"
                                      (56 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (59 (checkcast (class "clojure.lang.Var")))
                                      (62 (putstatic (fieldCP "const__4" "clojure.core$generate_proxy$iter__5215__5221$fn__5222$iter__5217__5223$fn__5224" (class "clojure.lang.Var"))))
                                      (65 (ldc 0))        ;;STRING:: "clojure.core"
                                      (67 (ldc 6))        ;;STRING:: "chunk-buffer"
                                      (69 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (72 (checkcast (class "clojure.lang.Var")))
                                      (75 (putstatic (fieldCP "const__5" "clojure.core$generate_proxy$iter__5215__5221$fn__5222$iter__5217__5223$fn__5224" (class "clojure.lang.Var"))))
                                      (78 (ldc 0))        ;;STRING:: "clojure.core"
                                      (80 (ldc 7))        ;;STRING:: "chunk-cons"
                                      (82 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (85 (checkcast (class "clojure.lang.Var")))
                                      (88 (putstatic (fieldCP "const__6" "clojure.core$generate_proxy$iter__5215__5221$fn__5222$iter__5217__5223$fn__5224" (class "clojure.lang.Var"))))
                                      (91 (ldc 0))        ;;STRING:: "clojure.core"
                                      (93 (ldc 8))        ;;STRING:: "chunk"
                                      (95 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (98 (checkcast (class "clojure.lang.Var")))
                                      (101 (putstatic (fieldCP "const__7" "clojure.core$generate_proxy$iter__5215__5221$fn__5222$iter__5217__5223$fn__5224" (class "clojure.lang.Var"))))
                                      (104 (ldc 0))       ;;STRING:: "clojure.core"
                                      (106 (ldc 9))       ;;STRING:: "chunk-rest"
                                      (108 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (111 (checkcast (class "clojure.lang.Var")))
                                      (114 (putstatic (fieldCP "const__8" "clojure.core$generate_proxy$iter__5215__5221$fn__5222$iter__5217__5223$fn__5224" (class "clojure.lang.Var"))))
                                      (117 (ldc 0))       ;;STRING:: "clojure.core"
                                      (119 (ldc 10))      ;;STRING:: "first"
                                      (121 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (124 (checkcast (class "clojure.lang.Var")))
                                      (127 (putstatic (fieldCP "const__9" "clojure.core$generate_proxy$iter__5215__5221$fn__5222$iter__5217__5223$fn__5224" (class "clojure.lang.Var"))))
                                      (130 (ldc 0))       ;;STRING:: "clojure.core"
                                      (132 (ldc 11))      ;;STRING:: "method-sig"
                                      (134 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (137 (checkcast (class "clojure.lang.Var")))
                                      (140 (putstatic (fieldCP "const__10" "clojure.core$generate_proxy$iter__5215__5221$fn__5222$iter__5217__5223$fn__5224" (class "clojure.lang.Var"))))
                                      (143 (ldc 0))       ;;STRING:: "clojure.core"
                                      (145 (ldc 12))      ;;STRING:: "not"
                                      (147 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (150 (checkcast (class "clojure.lang.Var")))
                                      (153 (putstatic (fieldCP "const__11" "clojure.core$generate_proxy$iter__5215__5221$fn__5222$iter__5217__5223$fn__5224" (class "clojure.lang.Var"))))
                                      (156 (ldc 0))       ;;STRING:: "clojure.core"
                                      (158 (ldc 13))      ;;STRING:: "cons"
                                      (160 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (163 (checkcast (class "clojure.lang.Var")))
                                      (166 (putstatic (fieldCP "const__12" "clojure.core$generate_proxy$iter__5215__5221$fn__5222$iter__5217__5223$fn__5224" (class "clojure.lang.Var"))))
                                      (169 (ldc 0))       ;;STRING:: "clojure.core"
                                      (171 (ldc 14))      ;;STRING:: "rest"
                                      (173 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (176 (checkcast (class "clojure.lang.Var")))
                                      (179 (putstatic (fieldCP "const__13" "clojure.core$generate_proxy$iter__5215__5221$fn__5222$iter__5217__5223$fn__5224" (class "clojure.lang.Var"))))
                                      (182 (return))
                                      (endofcode 183))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 20)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.AFunction" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "s__5218" "clojure.core$generate_proxy$iter__5215__5221$fn__5222$iter__5217__5223$fn__5224" (class "java.lang.Object"))))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "considered" "clojure.core$generate_proxy$iter__5215__5221$fn__5222$iter__5217__5223$fn__5224" (class "java.lang.Object"))))
                                      (14 (aload_0))
                                      (15 (aload_3))
                                      (16 (putfield (fieldCP "iter__5217" "clojure.core$generate_proxy$iter__5215__5221$fn__5222$iter__5217__5223$fn__5224" (class "java.lang.Object"))))
                                      (19 (return))
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 7) (code_length . 426)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "s__5218" "clojure.core$generate_proxy$iter__5215__5221$fn__5222$iter__5217__5223$fn__5224" (class "java.lang.Object")))) 
                                      (4 (aload_0)) 
                                      (5 (aconst_null)) 
                                      (6 (putfield (fieldCP "s__5218" "clojure.core$generate_proxy$iter__5215__5221$fn__5222$iter__5217__5223$fn__5224" (class "java.lang.Object")))) 
                                      (9 (astore_1)) 
                                      (10 (getstatic (fieldCP "const__0" "clojure.core$generate_proxy$iter__5215__5221$fn__5222$iter__5217__5223$fn__5224" (class "clojure.lang.Var")))) ;;at TAG_10
                                      (13 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (16 (checkcast (class "clojure.lang.IFn"))) 
                                      (19 (aload_1)) 
                                      (20 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (25 (astore_2)) 
                                      (26 (aload_2)) 
                                      (27 (dup)) 
                                      (28 (ifnull 423)) ;;to TAG_0
                                      (31 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (34 (if_acmpeq 424))  ;;to TAG_1
                                      (37 (aload_2)) 
                                      (38 (aconst_null)) 
                                      (39 (astore_2)) 
                                      (40 (astore_3)) 
                                      (41 (getstatic (fieldCP "const__1" "clojure.core$generate_proxy$iter__5215__5221$fn__5222$iter__5217__5223$fn__5224" (class "clojure.lang.Var")))) 
                                      (44 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (47 (checkcast (class "clojure.lang.IFn"))) 
                                      (50 (aload_3)) 
                                      (51 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (56 (dup)) 
                                      (57 (ifnull 255)) ;;to TAG_2
                                      (60 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (63 (if_acmpeq 256)) ;;to TAG_3
                                      (66 (getstatic (fieldCP "const__2" "clojure.core$generate_proxy$iter__5215__5221$fn__5222$iter__5217__5223$fn__5224" (class "clojure.lang.Var")))) 
                                      (69 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (72 (checkcast (class "clojure.lang.IFn"))) 
                                      (75 (aload_3)) 
                                      (76 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (81 (astore 4)) 
                                      (83 (aload 4)) 
                                      (85 (invokestatic (methodCP "count" "clojure.lang.RT" ((class "java.lang.Object")) int))) 
                                      (88 (invokestatic (methodCP "intCast" "clojure.lang.RT" (int) int))) 
                                      (91 (istore 5)) 
                                      (93 (getstatic (fieldCP "const__5" "clojure.core$generate_proxy$iter__5215__5221$fn__5222$iter__5217__5223$fn__5224" (class "clojure.lang.Var")))) 
                                      (96 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (99 (checkcast (class "clojure.lang.IFn"))) 
                                      (102 (iload 5)) 
                                      (104 (invokestatic (methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer")))) 
                                      (107 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (112 (astore 6)) 
                                      (114 (new (class "clojure.core$generate_proxy$iter__5215__5221$fn__5222$iter__5217__5223$fn__5224$fn__5225"))) 
                                      (117 (dup)) 
                                      (118 (aload 4)) 
                                      (120 (aconst_null)) 
                                      (121 (astore 4)) 
                                      (123 (iload 5)) 
                                      (125 (aload_0)) 
                                      (126 (getfield (fieldCP "considered" "clojure.core$generate_proxy$iter__5215__5221$fn__5222$iter__5217__5223$fn__5224" (class "java.lang.Object")))) 
                                      (129 (aload 6)) 
                                      (131 (invokespecial (methodCP "<init>" "clojure.core$generate_proxy$iter__5215__5221$fn__5222$iter__5217__5223$fn__5224$fn__5225" ((class "java.lang.Object") int (class "java.lang.Object") (class "java.lang.Object")) void))) 
                                      (134 (checkcast (class "clojure.lang.IFn"))) 
                                      (137 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1)) 
                                      (142 (dup)) 
                                      (143 (ifnull 217)) ;;to TAG_4
                                      (146 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (149 (if_acmpeq 218)) ;;to TAG_5
                                      (152 (getstatic (fieldCP "const__6" "clojure.core$generate_proxy$iter__5215__5221$fn__5222$iter__5217__5223$fn__5224" (class "clojure.lang.Var")))) 
                                      (155 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (158 (checkcast (class "clojure.lang.IFn"))) 
                                      (161 (getstatic (fieldCP "const__7" "clojure.core$generate_proxy$iter__5215__5221$fn__5222$iter__5217__5223$fn__5224" (class "clojure.lang.Var")))) 
                                      (164 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (167 (checkcast (class "clojure.lang.IFn"))) 
                                      (170 (aload 6)) 
                                      (172 (aconst_null)) 
                                      (173 (astore 6)) 
                                      (175 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (180 (aload_0)) 
                                      (181 (getfield (fieldCP "iter__5217" "clojure.core$generate_proxy$iter__5215__5221$fn__5222$iter__5217__5223$fn__5224" (class "java.lang.Object")))) 
                                      (184 (checkcast (class "clojure.lang.IFn"))) 
                                      (187 (getstatic (fieldCP "const__8" "clojure.core$generate_proxy$iter__5215__5221$fn__5222$iter__5217__5223$fn__5224" (class "clojure.lang.Var")))) 
                                      (190 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (193 (checkcast (class "clojure.lang.IFn"))) 
                                      (196 (aload_3)) 
                                      (197 (aconst_null)) 
                                      (198 (astore_3)) 
                                      (199 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (204 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (209 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (214 (goto 252)) ;;to TAG_6
                                      (217 (pop)) ;;at TAG_4
                                      (218 (getstatic (fieldCP "const__6" "clojure.core$generate_proxy$iter__5215__5221$fn__5222$iter__5217__5223$fn__5224" (class "clojure.lang.Var")))) ;;at TAG_5
                                      (221 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (224 (checkcast (class "clojure.lang.IFn"))) 
                                      (227 (getstatic (fieldCP "const__7" "clojure.core$generate_proxy$iter__5215__5221$fn__5222$iter__5217__5223$fn__5224" (class "clojure.lang.Var")))) 
                                      (230 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (233 (checkcast (class "clojure.lang.IFn"))) 
                                      (236 (aload 6)) 
                                      (238 (aconst_null)) 
                                      (239 (astore 6)) 
                                      (241 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (246 (aconst_null)) 
                                      (247 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (252 (goto 420)) ;;to TAG_7;;at TAG_6
                                      (255 (pop)) ;;at TAG_2
                                      (256 (getstatic (fieldCP "const__9" "clojure.core$generate_proxy$iter__5215__5221$fn__5222$iter__5217__5223$fn__5224" (class "clojure.lang.Var")))) ;;at TAG_3
                                      (259 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (262 (checkcast (class "clojure.lang.IFn"))) 
                                      (265 (aload_3)) 
                                      (266 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (271 (astore 4)) 
                                      (273 (getstatic (fieldCP "const__10" "clojure.core$generate_proxy$iter__5215__5221$fn__5222$iter__5217__5223$fn__5224" (class "clojure.lang.Var")))) 
                                      (276 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (279 (checkcast (class "clojure.lang.IFn"))) 
                                      (282 (aload 4)) 
                                      (284 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (289 (astore 5)) 
                                      (291 (getstatic (fieldCP "const__11" "clojure.core$generate_proxy$iter__5215__5221$fn__5222$iter__5217__5223$fn__5224" (class "clojure.lang.Var")))) 
                                      (294 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (297 (checkcast (class "clojure.lang.IFn"))) 
                                      (300 (aload_0)) 
                                      (301 (getfield (fieldCP "considered" "clojure.core$generate_proxy$iter__5215__5221$fn__5222$iter__5217__5223$fn__5224" (class "java.lang.Object")))) 
                                      (304 (checkcast (class "clojure.lang.IFn"))) 
                                      (307 (aload 5)) 
                                      (309 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (314 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (319 (dup)) 
                                      (320 (ifnull 398)) ;;to TAG_8
                                      (323 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (326 (if_acmpeq 399)) ;;to TAG_9
                                      (329 (getstatic (fieldCP "const__12" "clojure.core$generate_proxy$iter__5215__5221$fn__5222$iter__5217__5223$fn__5224" (class "clojure.lang.Var")))) 
                                      (332 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (335 (checkcast (class "clojure.lang.IFn"))) 
                                      (338 (iconst_2)) 
                                      (339 (anewarray (class "java.lang.Object"))) 
                                      (342 (dup)) 
                                      (343 (iconst_0)) 
                                      (344 (aload 5)) 
                                      (346 (aconst_null)) 
                                      (347 (astore 5)) 
                                      (349 (aastore)) 
                                      (350 (dup)) 
                                      (351 (iconst_1)) 
                                      (352 (aload 4)) 
                                      (354 (aconst_null)) 
                                      (355 (astore 4)) 
                                      (357 (aastore)) 
                                      (358 (invokestatic (methodCP "mapUniqueKeys" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentMap")))) 
                                      (361 (aload_0)) 
                                      (362 (getfield (fieldCP "iter__5217" "clojure.core$generate_proxy$iter__5215__5221$fn__5222$iter__5217__5223$fn__5224" (class "java.lang.Object")))) 
                                      (365 (checkcast (class "clojure.lang.IFn"))) 
                                      (368 (getstatic (fieldCP "const__13" "clojure.core$generate_proxy$iter__5215__5221$fn__5222$iter__5217__5223$fn__5224" (class "clojure.lang.Var")))) 
                                      (371 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (374 (checkcast (class "clojure.lang.IFn"))) 
                                      (377 (aload_3)) 
                                      (378 (aconst_null)) 
                                      (379 (astore_3)) 
                                      (380 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (385 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (390 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (395 (goto 420)) ;;to TAG_7
                                      (398 (pop)) ;;at TAG_8
                                      (399 (getstatic (fieldCP "const__13" "clojure.core$generate_proxy$iter__5215__5221$fn__5222$iter__5217__5223$fn__5224" (class "clojure.lang.Var")))) ;;at TAG_9
                                      (402 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (405 (checkcast (class "clojure.lang.IFn"))) 
                                      (408 (aload_3)) 
                                      (409 (aconst_null)) 
                                      (410 (astore_3)) 
                                      (411 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (416 (astore_1)) 
                                      (417 (goto 10)) ;;to TAG_10
                                      (420 (goto 425)) ;;to TAG_11;;at TAG_7
                                      (423 (pop)) ;;at TAG_0
                                      (424 (aconst_null)) ;;at TAG_1
                                      (425 (areturn)) ;;at TAG_11
                                      (endofcode 426))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$generate_proxy$iter__5215__5221$fn__5222$iter__5217__5223$fn__5224-class-table*
  (make-static-class-decls 
   *clojure.core$generate_proxy$iter__5215__5221$fn__5222$iter__5217__5223$fn__5224*))

(defconst *package-name-map* 
  ("clojure.core$generate_proxy$iter__5215__5221$fn__5222$iter__5217__5223$fn__5224" . "clojure"))
