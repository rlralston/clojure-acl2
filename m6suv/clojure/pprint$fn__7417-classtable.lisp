; pprint$fn__7417-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:55 CDT 2014.
;

(defconst *clojure.pprint$fn__7417*
 (make-class-def
      '(class "clojure.pprint$fn__7417"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "=")
                        (STRING  "type")
                        (STRING  "mandatory")
                        (STRING  "not")
                        (STRING  "fill")
                        (STRING  "deref")
                        (STRING  "done-nl")
                        (STRING  "logical-block")
                        (STRING  "clojure.pprint")
                        (STRING  "emit-nl")
                        (STRING  "trailing-white-space")
                        (STRING  "base")
                        (STRING  "write"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__9" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__10" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "__site__0__" (class "clojure.lang.KeywordLookupSite") (accessflags  *class*  *final*  *static* ) -1)
                        (field "__thunk__0__" (class "clojure.lang.ILookupThunk") (accessflags  *class*  *static* ) -1)
                        (field "__site__1__" (class "clojure.lang.KeywordLookupSite") (accessflags  *class*  *final*  *static* ) -1)
                        (field "__thunk__1__" (class "clojure.lang.ILookupThunk") (accessflags  *class*  *static* ) -1)
                        (field "__site__2__" (class "clojure.lang.KeywordLookupSite") (accessflags  *class*  *final*  *static* ) -1)
                        (field "__thunk__2__" (class "clojure.lang.ILookupThunk") (accessflags  *class*  *static* ) -1)
                        (field "__site__3__" (class "clojure.lang.KeywordLookupSite") (accessflags  *class*  *final*  *static* ) -1)
                        (field "__thunk__3__" (class "clojure.lang.ILookupThunk") (accessflags  *class*  *static* ) -1)
                        (field "__site__4__" (class "clojure.lang.KeywordLookupSite") (accessflags  *class*  *final*  *static* ) -1)
                        (field "__thunk__4__" (class "clojure.lang.ILookupThunk") (accessflags  *class*  *static* ) -1)
                        (field "__site__5__" (class "clojure.lang.KeywordLookupSite") (accessflags  *class*  *final*  *static* ) -1)
                        (field "__thunk__5__" (class "clojure.lang.ILookupThunk") (accessflags  *class*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 0) (code_length . 257)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "="
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.pprint$fn__7417" (class "clojure.lang.Var"))))
                                      (13 (aconst_null))
                                      (14 (ldc 2))        ;;STRING:: "type"
                                      (16 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (19 (checkcast (class "clojure.lang.Keyword")))
                                      (22 (putstatic (fieldCP "const__1" "clojure.pprint$fn__7417" (class "clojure.lang.Keyword"))))
                                      (25 (aconst_null))
                                      (26 (ldc 3))        ;;STRING:: "mandatory"
                                      (28 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (31 (checkcast (class "clojure.lang.Keyword")))
                                      (34 (putstatic (fieldCP "const__2" "clojure.pprint$fn__7417" (class "clojure.lang.Keyword"))))
                                      (37 (ldc 0))        ;;STRING:: "clojure.core"
                                      (39 (ldc 4))        ;;STRING:: "not"
                                      (41 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (44 (checkcast (class "clojure.lang.Var")))
                                      (47 (putstatic (fieldCP "const__3" "clojure.pprint$fn__7417" (class "clojure.lang.Var"))))
                                      (50 (aconst_null))
                                      (51 (ldc 5))        ;;STRING:: "fill"
                                      (53 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (56 (checkcast (class "clojure.lang.Keyword")))
                                      (59 (putstatic (fieldCP "const__4" "clojure.pprint$fn__7417" (class "clojure.lang.Keyword"))))
                                      (62 (ldc 0))        ;;STRING:: "clojure.core"
                                      (64 (ldc 6))        ;;STRING:: "deref"
                                      (66 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (69 (checkcast (class "clojure.lang.Var")))
                                      (72 (putstatic (fieldCP "const__5" "clojure.pprint$fn__7417" (class "clojure.lang.Var"))))
                                      (75 (aconst_null))
                                      (76 (ldc 7))        ;;STRING:: "done-nl"
                                      (78 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (81 (checkcast (class "clojure.lang.Keyword")))
                                      (84 (putstatic (fieldCP "const__6" "clojure.pprint$fn__7417" (class "clojure.lang.Keyword"))))
                                      (87 (aconst_null))
                                      (88 (ldc 8))        ;;STRING:: "logical-block"
                                      (90 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (93 (checkcast (class "clojure.lang.Keyword")))
                                      (96 (putstatic (fieldCP "const__7" "clojure.pprint$fn__7417" (class "clojure.lang.Keyword"))))
                                      (99 (ldc 9))        ;;STRING:: "clojure.pprint"
                                      (101 (ldc 10))      ;;STRING:: "emit-nl"
                                      (103 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (106 (checkcast (class "clojure.lang.Var")))
                                      (109 (putstatic (fieldCP "const__8" "clojure.pprint$fn__7417" (class "clojure.lang.Var"))))
                                      (112 (aconst_null))
                                      (113 (ldc 11))      ;;STRING:: "trailing-white-space"
                                      (115 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (118 (checkcast (class "clojure.lang.Keyword")))
                                      (121 (putstatic (fieldCP "const__9" "clojure.pprint$fn__7417" (class "clojure.lang.Keyword"))))
                                      (124 (aconst_null))
                                      (125 (ldc 12))      ;;STRING:: "base"
                                      (127 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (130 (checkcast (class "clojure.lang.Keyword")))
                                      (133 (putstatic (fieldCP "const__10" "clojure.pprint$fn__7417" (class "clojure.lang.Keyword"))))
                                      (136 (new (class "clojure.lang.KeywordLookupSite")))
                                      (139 (dup))
                                      (140 (aconst_null))
                                      (141 (ldc 2))       ;;STRING:: "type"
                                      (143 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (146 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (149 (dup))
                                      (150 (putstatic (fieldCP "__site__0__" "clojure.pprint$fn__7417" (class "clojure.lang.KeywordLookupSite"))))
                                      (153 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$fn__7417" (class "clojure.lang.ILookupThunk"))))
                                      (156 (new (class "clojure.lang.KeywordLookupSite")))
                                      (159 (dup))
                                      (160 (aconst_null))
                                      (161 (ldc 2))       ;;STRING:: "type"
                                      (163 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (166 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (169 (dup))
                                      (170 (putstatic (fieldCP "__site__1__" "clojure.pprint$fn__7417" (class "clojure.lang.KeywordLookupSite"))))
                                      (173 (putstatic (fieldCP "__thunk__1__" "clojure.pprint$fn__7417" (class "clojure.lang.ILookupThunk"))))
                                      (176 (new (class "clojure.lang.KeywordLookupSite")))
                                      (179 (dup))
                                      (180 (aconst_null))
                                      (181 (ldc 8))       ;;STRING:: "logical-block"
                                      (183 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (186 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (189 (dup))
                                      (190 (putstatic (fieldCP "__site__2__" "clojure.pprint$fn__7417" (class "clojure.lang.KeywordLookupSite"))))
                                      (193 (putstatic (fieldCP "__thunk__2__" "clojure.pprint$fn__7417" (class "clojure.lang.ILookupThunk"))))
                                      (196 (new (class "clojure.lang.KeywordLookupSite")))
                                      (199 (dup))
                                      (200 (aconst_null))
                                      (201 (ldc 7))       ;;STRING:: "done-nl"
                                      (203 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (206 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (209 (dup))
                                      (210 (putstatic (fieldCP "__site__3__" "clojure.pprint$fn__7417" (class "clojure.lang.KeywordLookupSite"))))
                                      (213 (putstatic (fieldCP "__thunk__3__" "clojure.pprint$fn__7417" (class "clojure.lang.ILookupThunk"))))
                                      (216 (new (class "clojure.lang.KeywordLookupSite")))
                                      (219 (dup))
                                      (220 (aconst_null))
                                      (221 (ldc 11))      ;;STRING:: "trailing-white-space"
                                      (223 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (226 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (229 (dup))
                                      (230 (putstatic (fieldCP "__site__4__" "clojure.pprint$fn__7417" (class "clojure.lang.KeywordLookupSite"))))
                                      (233 (putstatic (fieldCP "__thunk__4__" "clojure.pprint$fn__7417" (class "clojure.lang.ILookupThunk"))))
                                      (236 (new (class "clojure.lang.KeywordLookupSite")))
                                      (239 (dup))
                                      (240 (aconst_null))
                                      (241 (ldc 12))      ;;STRING:: "base"
                                      (243 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (246 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (249 (dup))
                                      (250 (putstatic (fieldCP "__site__5__" "clojure.pprint$fn__7417" (class "clojure.lang.KeywordLookupSite"))))
                                      (253 (putstatic (fieldCP "__thunk__5__" "clojure.pprint$fn__7417" (class "clojure.lang.ILookupThunk"))))
                                      (256 (return))
                                      (endofcode 257))
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
                                   (max_stack . 7) (max_locals . 5) (code_length . 489)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "__thunk__0__" "clojure.pprint$fn__7417" (class "clojure.lang.ILookupThunk")))) 
                                      (3 (dup)) 
                                      (4 (aload_2)) 
                                      (5 (dup_x2)) 
                                      (6 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (11 (dup_x2)) 
                                      (12 (if_acmpeq 19)) ;;to TAG_0
                                      (15 (pop)) 
                                      (16 (goto 41)) ;;to TAG_1
                                      (19 (swap)) ;;at TAG_0
                                      (20 (pop)) 
                                      (21 (dup)) 
                                      (22 (getstatic (fieldCP "__site__0__" "clojure.pprint$fn__7417" (class "clojure.lang.KeywordLookupSite")))) 
                                      (25 (swap)) 
                                      (26 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (31 (dup)) 
                                      (32 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$fn__7417" (class "clojure.lang.ILookupThunk")))) 
                                      (35 (swap)) 
                                      (36 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (41 (getstatic (fieldCP "const__2" "clojure.pprint$fn__7417" (class "clojure.lang.Keyword")))) ;;at TAG_1
                                      (44 (invokestatic (methodCP "equiv" "clojure.lang.Util" ((class "java.lang.Object") (class "java.lang.Object")) boolean))) 
                                      (47 (istore_3)) 
                                      (48 (iload_3)) 
                                      (49 (ifeq 69)) ;;to TAG_2
                                      (52 (iload_3)) 
                                      (53 (ifeq 62)) ;;to TAG_3
                                      (56 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (59 (goto 65)) ;;to TAG_4
                                      (62 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) ;;at TAG_3
                                      (65 (goto 260)) ;;to TAG_5;;at TAG_4
                                      (68 (pop)) 
                                      (69 (getstatic (fieldCP "const__3" "clojure.pprint$fn__7417" (class "clojure.lang.Var")))) ;;at TAG_2
                                      (72 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (75 (checkcast (class "clojure.lang.IFn"))) 
                                      (78 (getstatic (fieldCP "__thunk__1__" "clojure.pprint$fn__7417" (class "clojure.lang.ILookupThunk")))) 
                                      (81 (dup)) 
                                      (82 (aload_2)) 
                                      (83 (dup_x2)) 
                                      (84 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (89 (dup_x2)) 
                                      (90 (if_acmpeq 97)) ;;to TAG_6
                                      (93 (pop)) 
                                      (94 (goto 119)) ;;to TAG_7
                                      (97 (swap)) ;;at TAG_6
                                      (98 (pop)) 
                                      (99 (dup)) 
                                      (100 (getstatic (fieldCP "__site__1__" "clojure.pprint$fn__7417" (class "clojure.lang.KeywordLookupSite")))) 
                                      (103 (swap)) 
                                      (104 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (109 (dup)) 
                                      (110 (putstatic (fieldCP "__thunk__1__" "clojure.pprint$fn__7417" (class "clojure.lang.ILookupThunk")))) 
                                      (113 (swap)) 
                                      (114 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (119 (getstatic (fieldCP "const__4" "clojure.pprint$fn__7417" (class "clojure.lang.Keyword")))) ;;at TAG_7
                                      (122 (invokestatic (methodCP "equiv" "clojure.lang.Util" ((class "java.lang.Object") (class "java.lang.Object")) boolean))) 
                                      (125 (ifeq 134)) ;;to TAG_8
                                      (128 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (131 (goto 137)) ;;to TAG_9
                                      (134 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) ;;at TAG_8
                                      (137 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) ;;at TAG_9
                                      (142 (astore 4)) 
                                      (144 (aload 4)) 
                                      (146 (dup)) 
                                      (147 (ifnull 254)) ;;to TAG_10
                                      (150 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (153 (if_acmpeq 255)) ;;to TAG_11
                                      (156 (getstatic (fieldCP "const__5" "clojure.pprint$fn__7417" (class "clojure.lang.Var")))) 
                                      (159 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (162 (checkcast (class "clojure.lang.IFn"))) 
                                      (165 (getstatic (fieldCP "__thunk__3__" "clojure.pprint$fn__7417" (class "clojure.lang.ILookupThunk")))) 
                                      (168 (dup)) 
                                      (169 (getstatic (fieldCP "__thunk__2__" "clojure.pprint$fn__7417" (class "clojure.lang.ILookupThunk")))) 
                                      (172 (dup)) 
                                      (173 (aload_2)) 
                                      (174 (dup_x2)) 
                                      (175 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (180 (dup_x2)) 
                                      (181 (if_acmpeq 188)) ;;to TAG_12
                                      (184 (pop)) 
                                      (185 (goto 210))  ;;to TAG_13
                                      (188 (swap)) ;;at TAG_12
                                      (189 (pop)) 
                                      (190 (dup)) 
                                      (191 (getstatic (fieldCP "__site__2__" "clojure.pprint$fn__7417" (class "clojure.lang.KeywordLookupSite")))) 
                                      (194 (swap)) 
                                      (195 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (200 (dup)) 
                                      (201 (putstatic (fieldCP "__thunk__2__" "clojure.pprint$fn__7417" (class "clojure.lang.ILookupThunk")))) 
                                      (204 (swap)) 
                                      (205 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (210 (dup_x2)) ;;at TAG_13
                                      (211 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (216 (dup_x2)) 
                                      (217 (if_acmpeq 224)) ;;to TAG_14
                                      (220 (pop)) 
                                      (221 (goto 246)) ;;to TAG_15
                                      (224 (swap)) ;;at TAG_14
                                      (225 (pop)) 
                                      (226 (dup)) 
                                      (227 (getstatic (fieldCP "__site__3__" "clojure.pprint$fn__7417" (class "clojure.lang.KeywordLookupSite")))) 
                                      (230 (swap)) 
                                      (231 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (236 (dup)) 
                                      (237 (putstatic (fieldCP "__thunk__3__" "clojure.pprint$fn__7417" (class "clojure.lang.ILookupThunk")))) 
                                      (240 (swap)) 
                                      (241 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (246 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) ;;at TAG_15
                                      (251 (goto 260)) ;;to TAG_5
                                      (254 (pop)) ;;at TAG_10
                                      (255 (aload 4)) ;;at TAG_11
                                      (257 (aconst_null)) 
                                      (258 (astore 4)) 
                                      (260 (dup)) ;;at TAG_5
                                      (261 (ifnull 292)) ;;to TAG_16
                                      (264 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (267 (if_acmpeq 293)) ;;to TAG_17
                                      (270 (getstatic (fieldCP "const__8" "clojure.pprint$fn__7417" (class "clojure.lang.Var")))) 
                                      (273 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (276 (checkcast (class "clojure.lang.IFn"))) 
                                      (279 (aload_1)) 
                                      (280 (aload_2)) 
                                      (281 (aconst_null)) 
                                      (282 (astore_2)) 
                                      (283 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (288 (pop)) 
                                      (289 (goto 472)) ;;to TAG_18
                                      (292 (pop)) ;;at TAG_16
                                      (293 (getstatic (fieldCP "__thunk__4__" "clojure.pprint$fn__7417" (class "clojure.lang.ILookupThunk")))) ;;at TAG_17
                                      (296 (dup)) 
                                      (297 (getstatic (fieldCP "const__5" "clojure.pprint$fn__7417" (class "clojure.lang.Var")))) 
                                      (300 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (303 (checkcast (class "clojure.lang.IFn"))) 
                                      (306 (getstatic (fieldCP "const__5" "clojure.pprint$fn__7417" (class "clojure.lang.Var")))) 
                                      (309 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (312 (checkcast (class "clojure.lang.IFn"))) 
                                      (315 (aload_1)) 
                                      (316 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (321 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (326 (dup_x2)) 
                                      (327 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (332 (dup_x2)) 
                                      (333 (if_acmpeq 340)) ;;to TAG_19
                                      (336 (pop)) 
                                      (337 (goto 362)) ;;to TAG_20
                                      (340 (swap)) ;;at TAG_19
                                      (341 (pop)) 
                                      (342 (dup)) 
                                      (343 (getstatic (fieldCP "__site__4__" "clojure.pprint$fn__7417" (class "clojure.lang.KeywordLookupSite")))) 
                                      (346 (swap)) 
                                      (347 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (352 (dup)) 
                                      (353 (putstatic (fieldCP "__thunk__4__" "clojure.pprint$fn__7417" (class "clojure.lang.ILookupThunk")))) 
                                      (356 (swap)) 
                                      (357 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (362 (astore_3)) ;;at TAG_20
                                      (363 (aload_3)) 
                                      (364 (dup)) 
                                      (365 (ifnull 469)) ;;to TAG_21
                                      (368 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (371 (if_acmpeq 470)) ;;to TAG_22
                                      (374 (aload_3)) 
                                      (375 (aconst_null)) 
                                      (376 (astore_3)) 
                                      (377 (astore 4)) 
                                      (379 (getstatic (fieldCP "__thunk__5__" "clojure.pprint$fn__7417" (class "clojure.lang.ILookupThunk")))) 
                                      (382 (dup)) 
                                      (383 (getstatic (fieldCP "const__5" "clojure.pprint$fn__7417" (class "clojure.lang.Var")))) 
                                      (386 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (389 (checkcast (class "clojure.lang.IFn"))) 
                                      (392 (getstatic (fieldCP "const__5" "clojure.pprint$fn__7417" (class "clojure.lang.Var")))) 
                                      (395 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (398 (checkcast (class "clojure.lang.IFn"))) 
                                      (401 (aload_1)) 
                                      (402 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (407 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (412 (dup_x2)) 
                                      (413 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (418 (dup_x2)) 
                                      (419 (if_acmpeq 426)) ;;to TAG_23
                                      (422 (pop)) 
                                      (423 (goto 448)) ;;to TAG_24
                                      (426 (swap)) ;;at TAG_23
                                      (427 (pop)) 
                                      (428 (dup)) 
                                      (429 (getstatic (fieldCP "__site__5__" "clojure.pprint$fn__7417" (class "clojure.lang.KeywordLookupSite")))) 
                                      (432 (swap)) 
                                      (433 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (438 (dup)) 
                                      (439 (putstatic (fieldCP "__thunk__5__" "clojure.pprint$fn__7417" (class "clojure.lang.ILookupThunk")))) 
                                      (442 (swap)) 
                                      (443 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (448 (ldc 13)) ;;at TAG_24;;STRING:: "write"
                                      (450 (iconst_1)) 
                                      (451 (anewarray (class "java.lang.Object"))) 
                                      (454 (dup)) 
                                      (455 (iconst_0)) 
                                      (456 (aload 4)) 
                                      (458 (aconst_null)) 
                                      (459 (astore 4)) 
                                      (461 (aastore)) 
                                      (462 (invokestatic (methodCP "invokeInstanceMethod" "clojure.lang.Reflector" ((class "java.lang.Object") (class "java.lang.String") (array (class "java.lang.Object"))) (class "java.lang.Object")))) 
                                      (465 (pop)) 
                                      (466 (goto 472)) ;;to TAG_18
                                      (469 (pop)) ;;at TAG_21
                                      (470 (aconst_null)) ;;at TAG_22
                                      (471 (pop)) 
                                      (472 (new (class "clojure.pprint$fn__7417$fn__7418"))) ;;at TAG_18
                                      (475 (dup)) 
                                      (476 (aload_1)) 
                                      (477 (aconst_null)) 
                                      (478 (astore_1)) 
                                      (479 (invokespecial (methodCP "<init>" "clojure.pprint$fn__7417$fn__7418" ((class "java.lang.Object")) void))) 
                                      (482 (checkcast (class "java.util.concurrent.Callable"))) 
                                      (485 (invokestatic (methodCP "runInTransaction" "clojure.lang.LockingTransaction" ((class "java.util.concurrent.Callable")) (class "java.lang.Object")))) 
                                      (488 (areturn)) 
                                      (endofcode 489))
                                   (Exceptions )
                                   (StackMap )))
                        (method "swapThunk"
                              (parameters int (class "clojure.lang.ILookupThunk"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 3) (code_length . 83)
                                   (parsedcode
                                      (0 (iload_1)) 
                                      (1 (tableswitch (tableswitchinfo 82 (0 . 5) (40 47 54 61 68 75))))  ;;to TAG_2;;to TAG_3;;to TAG_4;;to TAG_5;;to TAG_6;;to TAG_0;;to TAG_1
                                      (40 (aload_2)) ;;at TAG_1
                                      (41 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$fn__7417" (class "clojure.lang.ILookupThunk")))) 
                                      (44 (goto 82)) ;;to TAG_0
                                      (47 (aload_2)) ;;at TAG_2
                                      (48 (putstatic (fieldCP "__thunk__1__" "clojure.pprint$fn__7417" (class "clojure.lang.ILookupThunk")))) 
                                      (51 (goto 82)) ;;to TAG_0
                                      (54 (aload_2)) ;;at TAG_3
                                      (55 (putstatic (fieldCP "__thunk__2__" "clojure.pprint$fn__7417" (class "clojure.lang.ILookupThunk")))) 
                                      (58 (goto 82)) ;;to TAG_0
                                      (61 (aload_2)) ;;at TAG_4
                                      (62 (putstatic (fieldCP "__thunk__3__" "clojure.pprint$fn__7417" (class "clojure.lang.ILookupThunk")))) 
                                      (65 (goto 82)) ;;to TAG_0
                                      (68 (aload_2)) ;;at TAG_5
                                      (69 (putstatic (fieldCP "__thunk__4__" "clojure.pprint$fn__7417" (class "clojure.lang.ILookupThunk")))) 
                                      (72 (goto 82)) ;;to TAG_0
                                      (75 (aload_2)) ;;at TAG_6
                                      (76 (putstatic (fieldCP "__thunk__5__" "clojure.pprint$fn__7417" (class "clojure.lang.ILookupThunk")))) 
                                      (79 (goto 82)) ;;to TAG_0
                                      (82 (return)) ;;at TAG_0
                                      (endofcode 83))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *pprint$fn__7417-class-table*
  (make-static-class-decls 
   *clojure.pprint$fn__7417*))

(defconst *package-name-map* 
  ("clojure.pprint$fn__7417" . "clojure"))

