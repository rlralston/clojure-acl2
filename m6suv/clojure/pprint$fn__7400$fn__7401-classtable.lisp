; pprint$fn__7400$fn__7401-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:55 CDT 2014.
;

(defconst *clojure.pprint$fn__7400$fn__7401*
 (make-class-def
      '(class "clojure.pprint$fn__7400$fn__7401"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "prefix")
                        (STRING  "base")
                        (STRING  "clojure.core")
                        (STRING  "deref")
                        (STRING  "clojure.pprint")
                        (STRING  "get-column")
                        (STRING  "ref-set")
                        (STRING  "start-col")
                        (STRING  "indent")
                        (STRING  "write"))
            (fields
                        (field "const__0" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
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
                        (field "this" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "lb" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 0) (code_length . 188)
                                   (parsedcode
                                      (0 (aconst_null))
                                      (1 (ldc 0))         ;;STRING:: "prefix"
                                      (3 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (6 (checkcast (class "clojure.lang.Keyword")))
                                      (9 (putstatic (fieldCP "const__0" "clojure.pprint$fn__7400$fn__7401" (class "clojure.lang.Keyword"))))
                                      (12 (aconst_null))
                                      (13 (ldc 1))        ;;STRING:: "base"
                                      (15 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (18 (checkcast (class "clojure.lang.Keyword")))
                                      (21 (putstatic (fieldCP "const__1" "clojure.pprint$fn__7400$fn__7401" (class "clojure.lang.Keyword"))))
                                      (24 (ldc 2))        ;;STRING:: "clojure.core"
                                      (26 (ldc 3))        ;;STRING:: "deref"
                                      (28 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (31 (checkcast (class "clojure.lang.Var")))
                                      (34 (putstatic (fieldCP "const__2" "clojure.pprint$fn__7400$fn__7401" (class "clojure.lang.Var"))))
                                      (37 (ldc 4))        ;;STRING:: "clojure.pprint"
                                      (39 (ldc 5))        ;;STRING:: "get-column"
                                      (41 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (44 (checkcast (class "clojure.lang.Var")))
                                      (47 (putstatic (fieldCP "const__3" "clojure.pprint$fn__7400$fn__7401" (class "clojure.lang.Var"))))
                                      (50 (ldc 2))        ;;STRING:: "clojure.core"
                                      (52 (ldc 6))        ;;STRING:: "ref-set"
                                      (54 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (57 (checkcast (class "clojure.lang.Var")))
                                      (60 (putstatic (fieldCP "const__4" "clojure.pprint$fn__7400$fn__7401" (class "clojure.lang.Var"))))
                                      (63 (aconst_null))
                                      (64 (ldc 7))        ;;STRING:: "start-col"
                                      (66 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (69 (checkcast (class "clojure.lang.Keyword")))
                                      (72 (putstatic (fieldCP "const__5" "clojure.pprint$fn__7400$fn__7401" (class "clojure.lang.Keyword"))))
                                      (75 (aconst_null))
                                      (76 (ldc 8))        ;;STRING:: "indent"
                                      (78 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (81 (checkcast (class "clojure.lang.Keyword")))
                                      (84 (putstatic (fieldCP "const__6" "clojure.pprint$fn__7400$fn__7401" (class "clojure.lang.Keyword"))))
                                      (87 (new (class "clojure.lang.KeywordLookupSite")))
                                      (90 (dup))
                                      (91 (aconst_null))
                                      (92 (ldc 0))        ;;STRING:: "prefix"
                                      (94 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (97 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (100 (dup))
                                      (101 (putstatic (fieldCP "__site__0__" "clojure.pprint$fn__7400$fn__7401" (class "clojure.lang.KeywordLookupSite"))))
                                      (104 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$fn__7400$fn__7401" (class "clojure.lang.ILookupThunk"))))
                                      (107 (new (class "clojure.lang.KeywordLookupSite")))
                                      (110 (dup))
                                      (111 (aconst_null))
                                      (112 (ldc 1))       ;;STRING:: "base"
                                      (114 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (117 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (120 (dup))
                                      (121 (putstatic (fieldCP "__site__1__" "clojure.pprint$fn__7400$fn__7401" (class "clojure.lang.KeywordLookupSite"))))
                                      (124 (putstatic (fieldCP "__thunk__1__" "clojure.pprint$fn__7400$fn__7401" (class "clojure.lang.ILookupThunk"))))
                                      (127 (new (class "clojure.lang.KeywordLookupSite")))
                                      (130 (dup))
                                      (131 (aconst_null))
                                      (132 (ldc 1))       ;;STRING:: "base"
                                      (134 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (137 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (140 (dup))
                                      (141 (putstatic (fieldCP "__site__2__" "clojure.pprint$fn__7400$fn__7401" (class "clojure.lang.KeywordLookupSite"))))
                                      (144 (putstatic (fieldCP "__thunk__2__" "clojure.pprint$fn__7400$fn__7401" (class "clojure.lang.ILookupThunk"))))
                                      (147 (new (class "clojure.lang.KeywordLookupSite")))
                                      (150 (dup))
                                      (151 (aconst_null))
                                      (152 (ldc 7))       ;;STRING:: "start-col"
                                      (154 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (157 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (160 (dup))
                                      (161 (putstatic (fieldCP "__site__3__" "clojure.pprint$fn__7400$fn__7401" (class "clojure.lang.KeywordLookupSite"))))
                                      (164 (putstatic (fieldCP "__thunk__3__" "clojure.pprint$fn__7400$fn__7401" (class "clojure.lang.ILookupThunk"))))
                                      (167 (new (class "clojure.lang.KeywordLookupSite")))
                                      (170 (dup))
                                      (171 (aconst_null))
                                      (172 (ldc 8))       ;;STRING:: "indent"
                                      (174 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (177 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (180 (dup))
                                      (181 (putstatic (fieldCP "__site__4__" "clojure.pprint$fn__7400$fn__7401" (class "clojure.lang.KeywordLookupSite"))))
                                      (184 (putstatic (fieldCP "__thunk__4__" "clojure.pprint$fn__7400$fn__7401" (class "clojure.lang.ILookupThunk"))))
                                      (187 (return))
                                      (endofcode 188))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.AFunction" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "this" "clojure.pprint$fn__7400$fn__7401" (class "java.lang.Object"))))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "lb" "clojure.pprint$fn__7400$fn__7401" (class "java.lang.Object"))))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 7) (max_locals . 3) (code_length . 363)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "__thunk__0__" "clojure.pprint$fn__7400$fn__7401" (class "clojure.lang.ILookupThunk")))) 
                                      (3 (dup)) 
                                      (4 (aload_0)) 
                                      (5 (getfield (fieldCP "lb" "clojure.pprint$fn__7400$fn__7401" (class "java.lang.Object")))) 
                                      (8 (dup_x2)) 
                                      (9 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (14 (dup_x2)) 
                                      (15 (if_acmpeq 22)) ;;to TAG_0
                                      (18 (pop)) 
                                      (19 (goto 44))  ;;to TAG_1
                                      (22 (swap)) ;;at TAG_0
                                      (23 (pop)) 
                                      (24 (dup)) 
                                      (25 (getstatic (fieldCP "__site__0__" "clojure.pprint$fn__7400$fn__7401" (class "clojure.lang.KeywordLookupSite")))) 
                                      (28 (swap)) 
                                      (29 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (34 (dup)) 
                                      (35 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$fn__7400$fn__7401" (class "clojure.lang.ILookupThunk")))) 
                                      (38 (swap)) 
                                      (39 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (44 (astore_1)) ;;at TAG_1
                                      (45 (aload_1)) 
                                      (46 (dup)) 
                                      (47 (ifnull 151)) ;;to TAG_2
                                      (50 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (53 (if_acmpeq 152)) ;;to TAG_3
                                      (56 (aload_1)) 
                                      (57 (aconst_null)) 
                                      (58 (astore_1)) 
                                      (59 (astore_2)) 
                                      (60 (getstatic (fieldCP "__thunk__1__" "clojure.pprint$fn__7400$fn__7401" (class "clojure.lang.ILookupThunk")))) 
                                      (63 (dup)) 
                                      (64 (getstatic (fieldCP "const__2" "clojure.pprint$fn__7400$fn__7401" (class "clojure.lang.Var")))) 
                                      (67 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (70 (checkcast (class "clojure.lang.IFn"))) 
                                      (73 (getstatic (fieldCP "const__2" "clojure.pprint$fn__7400$fn__7401" (class "clojure.lang.Var")))) 
                                      (76 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (79 (checkcast (class "clojure.lang.IFn"))) 
                                      (82 (aload_0)) 
                                      (83 (getfield (fieldCP "this" "clojure.pprint$fn__7400$fn__7401" (class "java.lang.Object")))) 
                                      (86 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (91 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (96 (dup_x2)) 
                                      (97 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (102 (dup_x2)) 
                                      (103 (if_acmpeq 110)) ;;to TAG_4
                                      (106 (pop)) 
                                      (107 (goto 132)) ;;to TAG_5
                                      (110 (swap)) ;;at TAG_4
                                      (111 (pop)) 
                                      (112 (dup)) 
                                      (113 (getstatic (fieldCP "__site__1__" "clojure.pprint$fn__7400$fn__7401" (class "clojure.lang.KeywordLookupSite")))) 
                                      (116 (swap)) 
                                      (117 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (122 (dup)) 
                                      (123 (putstatic (fieldCP "__thunk__1__" "clojure.pprint$fn__7400$fn__7401" (class "clojure.lang.ILookupThunk")))) 
                                      (126 (swap)) 
                                      (127 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (132 (ldc 9)) ;;at TAG_5;;STRING:: "write"
                                      (134 (iconst_1)) 
                                      (135 (anewarray (class "java.lang.Object"))) 
                                      (138 (dup)) 
                                      (139 (iconst_0)) 
                                      (140 (aload_2)) 
                                      (141 (aconst_null)) 
                                      (142 (astore_2)) 
                                      (143 (aastore)) 
                                      (144 (invokestatic (methodCP "invokeInstanceMethod" "clojure.lang.Reflector" ((class "java.lang.Object") (class "java.lang.String") (array (class "java.lang.Object"))) (class "java.lang.Object")))) 
                                      (147 (pop)) 
                                      (148 (goto 154)) ;;to TAG_6
                                      (151 (pop)) ;;at TAG_2
                                      (152 (aconst_null)) ;;at TAG_3
                                      (153 (pop)) 
                                      (154 (getstatic (fieldCP "const__3" "clojure.pprint$fn__7400$fn__7401" (class "clojure.lang.Var")))) ;;at TAG_6
                                      (157 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (160 (checkcast (class "clojure.lang.IFn"))) 
                                      (163 (getstatic (fieldCP "__thunk__2__" "clojure.pprint$fn__7400$fn__7401" (class "clojure.lang.ILookupThunk")))) 
                                      (166 (dup)) 
                                      (167 (getstatic (fieldCP "const__2" "clojure.pprint$fn__7400$fn__7401" (class "clojure.lang.Var")))) 
                                      (170 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (173 (checkcast (class "clojure.lang.IFn"))) 
                                      (176 (getstatic (fieldCP "const__2" "clojure.pprint$fn__7400$fn__7401" (class "clojure.lang.Var")))) 
                                      (179 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (182 (checkcast (class "clojure.lang.IFn"))) 
                                      (185 (aload_0)) 
                                      (186 (getfield (fieldCP "this" "clojure.pprint$fn__7400$fn__7401" (class "java.lang.Object")))) 
                                      (189 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (194 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (199 (dup_x2)) 
                                      (200 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (205 (dup_x2)) 
                                      (206 (if_acmpeq 213)) ;;to TAG_7
                                      (209 (pop)) 
                                      (210 (goto 235)) ;;to TAG_8
                                      (213 (swap)) ;;at TAG_7
                                      (214 (pop)) 
                                      (215 (dup)) 
                                      (216 (getstatic (fieldCP "__site__2__" "clojure.pprint$fn__7400$fn__7401" (class "clojure.lang.KeywordLookupSite")))) 
                                      (219 (swap)) 
                                      (220 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (225 (dup)) 
                                      (226 (putstatic (fieldCP "__thunk__2__" "clojure.pprint$fn__7400$fn__7401" (class "clojure.lang.ILookupThunk")))) 
                                      (229 (swap)) 
                                      (230 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (235 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) ;;at TAG_8
                                      (240 (astore_1)) 
                                      (241 (getstatic (fieldCP "const__4" "clojure.pprint$fn__7400$fn__7401" (class "clojure.lang.Var")))) 
                                      (244 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (247 (checkcast (class "clojure.lang.IFn"))) 
                                      (250 (getstatic (fieldCP "__thunk__3__" "clojure.pprint$fn__7400$fn__7401" (class "clojure.lang.ILookupThunk")))) 
                                      (253 (dup)) 
                                      (254 (aload_0)) 
                                      (255 (getfield (fieldCP "lb" "clojure.pprint$fn__7400$fn__7401" (class "java.lang.Object")))) 
                                      (258 (dup_x2)) 
                                      (259 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (264 (dup_x2)) 
                                      (265 (if_acmpeq 272)) ;;to TAG_9
                                      (268 (pop)) 
                                      (269 (goto 294)) ;;to TAG_10
                                      (272 (swap)) ;;at TAG_9
                                      (273 (pop)) 
                                      (274 (dup)) 
                                      (275 (getstatic (fieldCP "__site__3__" "clojure.pprint$fn__7400$fn__7401" (class "clojure.lang.KeywordLookupSite")))) 
                                      (278 (swap)) 
                                      (279 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (284 (dup)) 
                                      (285 (putstatic (fieldCP "__thunk__3__" "clojure.pprint$fn__7400$fn__7401" (class "clojure.lang.ILookupThunk")))) 
                                      (288 (swap)) 
                                      (289 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (294 (aload_1)) ;;at TAG_10
                                      (295 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (300 (pop)) 
                                      (301 (getstatic (fieldCP "const__4" "clojure.pprint$fn__7400$fn__7401" (class "clojure.lang.Var")))) 
                                      (304 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (307 (checkcast (class "clojure.lang.IFn"))) 
                                      (310 (getstatic (fieldCP "__thunk__4__" "clojure.pprint$fn__7400$fn__7401" (class "clojure.lang.ILookupThunk")))) 
                                      (313 (dup)) 
                                      (314 (aload_0)) 
                                      (315 (getfield (fieldCP "lb" "clojure.pprint$fn__7400$fn__7401" (class "java.lang.Object")))) 
                                      (318 (dup_x2)) 
                                      (319 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (324 (dup_x2)) 
                                      (325 (if_acmpeq 332)) ;;to TAG_11
                                      (328 (pop)) 
                                      (329 (goto 354)) ;;to TAG_12
                                      (332 (swap)) ;;at TAG_11
                                      (333 (pop)) 
                                      (334 (dup)) 
                                      (335 (getstatic (fieldCP "__site__4__" "clojure.pprint$fn__7400$fn__7401" (class "clojure.lang.KeywordLookupSite")))) 
                                      (338 (swap)) 
                                      (339 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (344 (dup)) 
                                      (345 (putstatic (fieldCP "__thunk__4__" "clojure.pprint$fn__7400$fn__7401" (class "clojure.lang.ILookupThunk")))) 
                                      (348 (swap)) 
                                      (349 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (354 (aload_1)) ;;at TAG_12
                                      (355 (aconst_null)) 
                                      (356 (astore_1)) 
                                      (357 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (362 (areturn)) 
                                      (endofcode 363))
                                   (Exceptions )
                                   (StackMap )))
                        (method "swapThunk"
                              (parameters int (class "clojure.lang.ILookupThunk"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 3) (code_length . 72)
                                   (parsedcode
                                      (0 (iload_1)) 
                                      (1 (tableswitch (tableswitchinfo 71 (0 . 4) (36 43 50 57 64))))  ;;to TAG_2;;to TAG_3;;to TAG_4;;to TAG_5;;to TAG_0;;to TAG_1
                                      (36 (aload_2)) ;;at TAG_1
                                      (37 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$fn__7400$fn__7401" (class "clojure.lang.ILookupThunk")))) 
                                      (40 (goto 71)) ;;to TAG_0
                                      (43 (aload_2)) ;;at TAG_2
                                      (44 (putstatic (fieldCP "__thunk__1__" "clojure.pprint$fn__7400$fn__7401" (class "clojure.lang.ILookupThunk")))) 
                                      (47 (goto 71)) ;;to TAG_0
                                      (50 (aload_2)) ;;at TAG_3
                                      (51 (putstatic (fieldCP "__thunk__2__" "clojure.pprint$fn__7400$fn__7401" (class "clojure.lang.ILookupThunk")))) 
                                      (54 (goto 71)) ;;to TAG_0
                                      (57 (aload_2)) ;;at TAG_4
                                      (58 (putstatic (fieldCP "__thunk__3__" "clojure.pprint$fn__7400$fn__7401" (class "clojure.lang.ILookupThunk")))) 
                                      (61 (goto 71)) ;;to TAG_0
                                      (64 (aload_2)) ;;at TAG_5
                                      (65 (putstatic (fieldCP "__thunk__4__" "clojure.pprint$fn__7400$fn__7401" (class "clojure.lang.ILookupThunk")))) 
                                      (68 (goto 71)) ;;to TAG_0
                                      (71 (return)) ;;at TAG_0
                                      (endofcode 72))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *pprint$fn__7400$fn__7401-class-table*
  (make-static-class-decls 
   *clojure.pprint$fn__7400$fn__7401*))

(defconst *package-name-map* 
  ("clojure.pprint$fn__7400$fn__7401" . "clojure"))

