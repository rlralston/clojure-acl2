; pprint$pprint_defn$fn__8336-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:56 CDT 2014.
;

(defconst *clojure.pprint$pprint_defn$fn__8336*
 (make-class-def
      '(class "clojure.pprint$pprint_defn$fn__8336"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.pprint")
                        (STRING  "start-block")
                        (STRING  "clojure.core")
                        (STRING  "*out*")
                        (STRING  "string?")
                        (STRING  "cached-compile")
                        (STRING  "vector?")
                        (STRING  "first")
                        (STRING  "single-defn")
                        (STRING  "else")
                        (STRING  "multi-defn")
                        (STRING  "end-block")
                        (STRING  "pop-thread-bindings")
                        (STRING  "(")
                        (STRING  ")")
                        (STRING  "~w ~1I~@_~w")
                        (STRING  " ~_~w"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__9" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__10" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "defn_sym" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "attr_map" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "defn_name" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "doc_str" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "stuff" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 143)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.pprint"
                                      (2 (ldc 1))         ;;STRING:: "start-block"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.pprint$pprint_defn$fn__8336" (class "clojure.lang.Var"))))
                                      (13 (ldc 2))        ;;STRING:: "clojure.core"
                                      (15 (ldc 3))        ;;STRING:: "*out*"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.pprint$pprint_defn$fn__8336" (class "clojure.lang.Var"))))
                                      (26 (ldc 2))        ;;STRING:: "clojure.core"
                                      (28 (ldc 4))        ;;STRING:: "string?"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.pprint$pprint_defn$fn__8336" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.pprint"
                                      (41 (ldc 5))        ;;STRING:: "cached-compile"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.pprint$pprint_defn$fn__8336" (class "clojure.lang.Var"))))
                                      (52 (ldc 2))        ;;STRING:: "clojure.core"
                                      (54 (ldc 6))        ;;STRING:: "vector?"
                                      (56 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (59 (checkcast (class "clojure.lang.Var")))
                                      (62 (putstatic (fieldCP "const__4" "clojure.pprint$pprint_defn$fn__8336" (class "clojure.lang.Var"))))
                                      (65 (ldc 2))        ;;STRING:: "clojure.core"
                                      (67 (ldc 7))        ;;STRING:: "first"
                                      (69 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (72 (checkcast (class "clojure.lang.Var")))
                                      (75 (putstatic (fieldCP "const__5" "clojure.pprint$pprint_defn$fn__8336" (class "clojure.lang.Var"))))
                                      (78 (ldc 0))        ;;STRING:: "clojure.pprint"
                                      (80 (ldc 8))        ;;STRING:: "single-defn"
                                      (82 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (85 (checkcast (class "clojure.lang.Var")))
                                      (88 (putstatic (fieldCP "const__6" "clojure.pprint$pprint_defn$fn__8336" (class "clojure.lang.Var"))))
                                      (91 (aconst_null))
                                      (92 (ldc 9))        ;;STRING:: "else"
                                      (94 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (97 (checkcast (class "clojure.lang.Keyword")))
                                      (100 (putstatic (fieldCP "const__7" "clojure.pprint$pprint_defn$fn__8336" (class "clojure.lang.Keyword"))))
                                      (103 (ldc 0))       ;;STRING:: "clojure.pprint"
                                      (105 (ldc 10))      ;;STRING:: "multi-defn"
                                      (107 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (110 (checkcast (class "clojure.lang.Var")))
                                      (113 (putstatic (fieldCP "const__8" "clojure.pprint$pprint_defn$fn__8336" (class "clojure.lang.Var"))))
                                      (116 (ldc 0))       ;;STRING:: "clojure.pprint"
                                      (118 (ldc 11))      ;;STRING:: "end-block"
                                      (120 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (123 (checkcast (class "clojure.lang.Var")))
                                      (126 (putstatic (fieldCP "const__9" "clojure.pprint$pprint_defn$fn__8336" (class "clojure.lang.Var"))))
                                      (129 (ldc 2))       ;;STRING:: "clojure.core"
                                      (131 (ldc 12))      ;;STRING:: "pop-thread-bindings"
                                      (133 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (136 (checkcast (class "clojure.lang.Var")))
                                      (139 (putstatic (fieldCP "const__10" "clojure.pprint$pprint_defn$fn__8336" (class "clojure.lang.Var"))))
                                      (142 (return))
                                      (endofcode 143))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 6) (code_length . 32)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.AFunction" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "defn_sym" "clojure.pprint$pprint_defn$fn__8336" (class "java.lang.Object"))))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "attr_map" "clojure.pprint$pprint_defn$fn__8336" (class "java.lang.Object"))))
                                      (14 (aload_0))
                                      (15 (aload_3))
                                      (16 (putfield (fieldCP "defn_name" "clojure.pprint$pprint_defn$fn__8336" (class "java.lang.Object"))))
                                      (19 (aload_0))
                                      (20 (aload 4))
                                      (22 (putfield (fieldCP "doc_str" "clojure.pprint$pprint_defn$fn__8336" (class "java.lang.Object"))))
                                      (25 (aload_0))
                                      (26 (aload 5))
                                      (28 (putfield (fieldCP "stuff" "clojure.pprint$pprint_defn$fn__8336" (class "java.lang.Object"))))
                                      (31 (return))
                                      (endofcode 32))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 5) (code_length . 501)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.pprint$pprint_defn$fn__8336" (class "clojure.lang.Var")))) ;;at TAG_27
                                      (3 (checkcast (class "clojure.lang.IFn"))) 
                                      (6 (getstatic (fieldCP "const__1" "clojure.pprint$pprint_defn$fn__8336" (class "clojure.lang.Var")))) 
                                      (9 (invokevirtual (methodCP "get" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (12 (ldc 13)) ;;STRING:: "("
                                      (14 (aconst_null)) 
                                      (15 (ldc 14)) ;;STRING:: ")"
                                      (17 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 5)) 
                                      (22 (pop)) 
                                      (23 (ldc 15)) ;;STRING:: "~w ~1I~@_~w"
                                      (25 (astore_1)) 
                                      (26 (getstatic (fieldCP "const__2" "clojure.pprint$pprint_defn$fn__8336" (class "clojure.lang.Var")))) 
                                      (29 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (32 (checkcast (class "clojure.lang.IFn"))) 
                                      (35 (aload_1)) 
                                      (36 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (41 (dup)) 
                                      (42 (ifnull 68)) ;;to TAG_0
                                      (45 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (48 (if_acmpeq 69)) ;;to TAG_1
                                      (51 (getstatic (fieldCP "const__3" "clojure.pprint$pprint_defn$fn__8336" (class "clojure.lang.Var")))) 
                                      (54 (checkcast (class "clojure.lang.IFn"))) 
                                      (57 (aload_1)) 
                                      (58 (aconst_null)) 
                                      (59 (astore_1)) 
                                      (60 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (65 (goto 72)) ;;to TAG_2
                                      (68 (pop)) ;;at TAG_0
                                      (69 (aload_1)) ;;at TAG_1
                                      (70 (aconst_null)) 
                                      (71 (astore_1)) 
                                      (72 (astore_2)) ;;at TAG_2
                                      (73 (new (class "clojure.pprint$pprint_defn$fn__8336$fn__8337"))) 
                                      (76 (dup)) 
                                      (77 (aload_2)) 
                                      (78 (aconst_null)) 
                                      (79 (astore_2)) 
                                      (80 (invokespecial (methodCP "<init>" "clojure.pprint$pprint_defn$fn__8336$fn__8337" ((class "java.lang.Object")) void))) 
                                      (83 (checkcast (class "clojure.lang.IFn"))) 
                                      (86 (aload_0)) 
                                      (87 (getfield (fieldCP "defn_sym" "clojure.pprint$pprint_defn$fn__8336" (class "java.lang.Object")))) 
                                      (90 (aload_0)) 
                                      (91 (getfield (fieldCP "defn_name" "clojure.pprint$pprint_defn$fn__8336" (class "java.lang.Object")))) 
                                      (94 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (99 (pop)) 
                                      (100 (aload_0)) 
                                      (101 (getfield (fieldCP "doc_str" "clojure.pprint$pprint_defn$fn__8336" (class "java.lang.Object")))) 
                                      (104 (dup)) 
                                      (105 (ifnull 190)) ;;to TAG_3
                                      (108 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (111 (if_acmpeq 191)) ;;to TAG_4
                                      (114 (ldc 16)) ;;STRING:: " ~_~w"
                                      (116 (astore_1)) 
                                      (117 (getstatic (fieldCP "const__2" "clojure.pprint$pprint_defn$fn__8336" (class "clojure.lang.Var")))) 
                                      (120 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (123 (checkcast (class "clojure.lang.IFn"))) 
                                      (126 (aload_1)) 
                                      (127 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (132 (dup)) 
                                      (133 (ifnull 159)) ;;to TAG_5
                                      (136 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (139 (if_acmpeq 160)) ;;to TAG_6
                                      (142 (getstatic (fieldCP "const__3" "clojure.pprint$pprint_defn$fn__8336" (class "clojure.lang.Var")))) 
                                      (145 (checkcast (class "clojure.lang.IFn"))) 
                                      (148 (aload_1)) 
                                      (149 (aconst_null)) 
                                      (150 (astore_1)) 
                                      (151 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (156 (goto 163)) ;;to TAG_7
                                      (159 (pop)) ;;at TAG_5
                                      (160 (aload_1)) ;;at TAG_6
                                      (161 (aconst_null)) 
                                      (162 (astore_1)) 
                                      (163 (astore_2)) ;;at TAG_7
                                      (164 (new (class "clojure.pprint$pprint_defn$fn__8336$fn__8340"))) 
                                      (167 (dup)) 
                                      (168 (aload_2)) 
                                      (169 (aconst_null)) 
                                      (170 (astore_2)) 
                                      (171 (invokespecial (methodCP "<init>" "clojure.pprint$pprint_defn$fn__8336$fn__8340" ((class "java.lang.Object")) void))) 
                                      (174 (checkcast (class "clojure.lang.IFn"))) 
                                      (177 (aload_0)) 
                                      (178 (getfield (fieldCP "doc_str" "clojure.pprint$pprint_defn$fn__8336" (class "java.lang.Object")))) 
                                      (181 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (186 (pop)) 
                                      (187 (goto 193)) ;;to TAG_8
                                      (190 (pop)) ;;at TAG_3
                                      (191 (aconst_null)) ;;at TAG_4
                                      (192 (pop)) 
                                      (193 (aload_0)) ;;at TAG_8
                                      (194 (getfield (fieldCP "attr_map" "clojure.pprint$pprint_defn$fn__8336" (class "java.lang.Object")))) 
                                      (197 (dup)) 
                                      (198 (ifnull 283)) ;;to TAG_9
                                      (201 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (204 (if_acmpeq 284)) ;;to TAG_10
                                      (207 (ldc 16)) ;;STRING:: " ~_~w"
                                      (209 (astore_1)) 
                                      (210 (getstatic (fieldCP "const__2" "clojure.pprint$pprint_defn$fn__8336" (class "clojure.lang.Var")))) 
                                      (213 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (216 (checkcast (class "clojure.lang.IFn"))) 
                                      (219 (aload_1)) 
                                      (220 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (225 (dup)) 
                                      (226 (ifnull 252)) ;;to TAG_11
                                      (229 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (232 (if_acmpeq 253)) ;;to TAG_12
                                      (235 (getstatic (fieldCP "const__3" "clojure.pprint$pprint_defn$fn__8336" (class "clojure.lang.Var")))) 
                                      (238 (checkcast (class "clojure.lang.IFn"))) 
                                      (241 (aload_1)) 
                                      (242 (aconst_null)) 
                                      (243 (astore_1)) 
                                      (244 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (249 (goto 256))  ;;to TAG_13
                                      (252 (pop)) ;;at TAG_11
                                      (253 (aload_1)) ;;at TAG_12
                                      (254 (aconst_null)) 
                                      (255 (astore_1)) 
                                      (256 (astore_2)) ;;at TAG_13
                                      (257 (new (class "clojure.pprint$pprint_defn$fn__8336$fn__8343"))) 
                                      (260 (dup)) 
                                      (261 (aload_2)) 
                                      (262 (aconst_null)) 
                                      (263 (astore_2)) 
                                      (264 (invokespecial (methodCP "<init>" "clojure.pprint$pprint_defn$fn__8336$fn__8343" ((class "java.lang.Object")) void))) 
                                      (267 (checkcast (class "clojure.lang.IFn"))) 
                                      (270 (aload_0)) 
                                      (271 (getfield (fieldCP "attr_map" "clojure.pprint$pprint_defn$fn__8336" (class "java.lang.Object")))) 
                                      (274 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (279 (pop)) 
                                      (280 (goto 286)) ;;to TAG_14
                                      (283 (pop)) ;;at TAG_9
                                      (284 (aconst_null)) ;;at TAG_10
                                      (285 (pop)) 
                                      (286 (getstatic (fieldCP "const__4" "clojure.pprint$pprint_defn$fn__8336" (class "clojure.lang.Var")))) ;;at TAG_14
                                      (289 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (292 (checkcast (class "clojure.lang.IFn"))) 
                                      (295 (getstatic (fieldCP "const__5" "clojure.pprint$pprint_defn$fn__8336" (class "clojure.lang.Var")))) 
                                      (298 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (301 (checkcast (class "clojure.lang.IFn"))) 
                                      (304 (aload_0)) 
                                      (305 (getfield (fieldCP "stuff" "clojure.pprint$pprint_defn$fn__8336" (class "java.lang.Object")))) 
                                      (308 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (313 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (318 (dup)) 
                                      (319 (ifnull 377)) ;;to TAG_15
                                      (322 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (325 (if_acmpeq 378)) ;;to TAG_16
                                      (328 (getstatic (fieldCP "const__6" "clojure.pprint$pprint_defn$fn__8336" (class "clojure.lang.Var")))) 
                                      (331 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (334 (checkcast (class "clojure.lang.IFn"))) 
                                      (337 (aload_0)) 
                                      (338 (getfield (fieldCP "stuff" "clojure.pprint$pprint_defn$fn__8336" (class "java.lang.Object")))) 
                                      (341 (aload_0)) 
                                      (342 (getfield (fieldCP "doc_str" "clojure.pprint$pprint_defn$fn__8336" (class "java.lang.Object")))) 
                                      (345 (astore_1)) 
                                      (346 (aload_1)) 
                                      (347 (dup)) 
                                      (348 (ifnull 363)) ;;to TAG_17
                                      (351 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (354 (if_acmpeq 364)) ;;to TAG_18
                                      (357 (aload_1)) 
                                      (358 (aconst_null)) 
                                      (359 (astore_1)) 
                                      (360 (goto 368)) ;;to TAG_19
                                      (363 (pop)) ;;at TAG_17
                                      (364 (aload_0)) ;;at TAG_18
                                      (365 (getfield (fieldCP "attr_map" "clojure.pprint$pprint_defn$fn__8336" (class "java.lang.Object")))) 
                                      (368 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) ;;at TAG_19
                                      (373 (pop)) 
                                      (374 (goto 443)) ;;to TAG_20
                                      (377 (pop)) ;;at TAG_15
                                      (378 (getstatic (fieldCP "const__7" "clojure.pprint$pprint_defn$fn__8336" (class "clojure.lang.Keyword")))) ;;at TAG_16
                                      (381 (dup)) 
                                      (382 (ifnull 440)) ;;to TAG_21
                                      (385 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (388 (if_acmpeq 441)) ;;to TAG_22
                                      (391 (getstatic (fieldCP "const__8" "clojure.pprint$pprint_defn$fn__8336" (class "clojure.lang.Var")))) 
                                      (394 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (397 (checkcast (class "clojure.lang.IFn"))) 
                                      (400 (aload_0)) 
                                      (401 (getfield (fieldCP "stuff" "clojure.pprint$pprint_defn$fn__8336" (class "java.lang.Object")))) 
                                      (404 (aload_0)) 
                                      (405 (getfield (fieldCP "doc_str" "clojure.pprint$pprint_defn$fn__8336" (class "java.lang.Object")))) 
                                      (408 (astore_1)) 
                                      (409 (aload_1)) 
                                      (410 (dup)) 
                                      (411 (ifnull 426)) ;;to TAG_23
                                      (414 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (417 (if_acmpeq 427)) ;;to TAG_24
                                      (420 (aload_1)) 
                                      (421 (aconst_null)) 
                                      (422 (astore_1)) 
                                      (423 (goto 431)) ;;to TAG_25
                                      (426 (pop)) ;;at TAG_23
                                      (427 (aload_0)) ;;at TAG_24
                                      (428 (getfield (fieldCP "attr_map" "clojure.pprint$pprint_defn$fn__8336" (class "java.lang.Object")))) 
                                      (431 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) ;;at TAG_25
                                      (436 (pop)) 
                                      (437 (goto 443)) ;;to TAG_20
                                      (440 (pop)) ;;at TAG_21
                                      (441 (aconst_null)) ;;at TAG_22
                                      (442 (pop)) 
                                      (443 (getstatic (fieldCP "const__9" "clojure.pprint$pprint_defn$fn__8336" (class "clojure.lang.Var")))) ;;at TAG_20
                                      (446 (checkcast (class "clojure.lang.IFn"))) 
                                      (449 (getstatic (fieldCP "const__1" "clojure.pprint$pprint_defn$fn__8336" (class "clojure.lang.Var")))) 
                                      (452 (invokevirtual (methodCP "get" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (455 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (460 (astore_3)) 
                                      (461 (getstatic (fieldCP "const__10" "clojure.pprint$pprint_defn$fn__8336" (class "clojure.lang.Var")))) ;;at TAG_28
                                      (464 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (467 (checkcast (class "clojure.lang.IFn"))) 
                                      (470 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1)) 
                                      (475 (pop)) 
                                      (476 (goto 499)) ;;to TAG_26
                                      (479 (astore 4)) ;;at TAG_29
                                      (481 (getstatic (fieldCP "const__10" "clojure.pprint$pprint_defn$fn__8336" (class "clojure.lang.Var")))) 
                                      (484 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (487 (checkcast (class "clojure.lang.IFn"))) 
                                      (490 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1)) 
                                      (495 (pop)) 
                                      (496 (aload 4)) 
                                      (498 (athrow)) 
                                      (499 (aload_3)) ;;at TAG_26
                                      (500 (areturn)) 
                                      (endofcode 501))
                                   (Exceptions 
                                     (handler 0 461  479 (class "java.lang.Throwable")))
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *pprint$pprint_defn$fn__8336-class-table*
  (make-static-class-decls 
   *clojure.pprint$pprint_defn$fn__8336*))

(defconst *package-name-map* 
  ("clojure.pprint$pprint_defn$fn__8336" . "clojure"))
