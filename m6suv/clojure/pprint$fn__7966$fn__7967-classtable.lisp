; pprint$fn__7966$fn__7967-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:55 CDT 2014.
;

(defconst *clojure.pprint$fn__7966$fn__7967*
 (make-class-def
      '(class "clojure.pprint$fn__7966$fn__7967"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "colon")
                        (STRING  "clojure.pprint")
                        (STRING  "relative-reposition")
                        (LONG -1)
                        (STRING  "at")
                        (STRING  "y")
                        (STRING  "ies")
                        (STRING  "")
                        (STRING  "s")
                        (STRING  "next-arg")
                        (STRING  "clojure.core")
                        (STRING  "nth")
                        (STRING  "print")
                        (STRING  "=")
                        (STRING  "first")
                        (STRING  "second"))
            (fields
                        (field "const__0" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__9" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__10" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__11" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__12" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__13" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "__site__0__" (class "clojure.lang.KeywordLookupSite") (accessflags  *class*  *final*  *static* ) -1)
                        (field "__thunk__0__" (class "clojure.lang.ILookupThunk") (accessflags  *class*  *static* ) -1)
                        (field "__site__1__" (class "clojure.lang.KeywordLookupSite") (accessflags  *class*  *final*  *static* ) -1)
                        (field "__thunk__1__" (class "clojure.lang.ILookupThunk") (accessflags  *class*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 0) (code_length . 225)
                                   (parsedcode
                                      (0 (aconst_null))
                                      (1 (ldc 0))         ;;STRING:: "colon"
                                      (3 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (6 (checkcast (class "clojure.lang.Keyword")))
                                      (9 (putstatic (fieldCP "const__0" "clojure.pprint$fn__7966$fn__7967" (class "clojure.lang.Keyword"))))
                                      (12 (ldc 1))        ;;STRING:: "clojure.pprint"
                                      (14 (ldc 2))        ;;STRING:: "relative-reposition"
                                      (16 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (19 (checkcast (class "clojure.lang.Var")))
                                      (22 (putstatic (fieldCP "const__1" "clojure.pprint$fn__7966$fn__7967" (class "clojure.lang.Var"))))
                                      (25 (ldc2_w 3))     ;; LONG:: "-1"
                                      (28 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (31 (putstatic (fieldCP "const__2" "clojure.pprint$fn__7966$fn__7967" (class "java.lang.Object"))))
                                      (34 (aconst_null))
                                      (35 (ldc 4))        ;;STRING:: "at"
                                      (37 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (40 (checkcast (class "clojure.lang.Keyword")))
                                      (43 (putstatic (fieldCP "const__3" "clojure.pprint$fn__7966$fn__7967" (class "clojure.lang.Keyword"))))
                                      (46 (iconst_2))
                                      (47 (anewarray (class "java.lang.Object")))
                                      (50 (dup))
                                      (51 (iconst_0))
                                      (52 (ldc 5))        ;;STRING:: "y"
                                      (54 (aastore))
                                      (55 (dup))
                                      (56 (iconst_1))
                                      (57 (ldc 6))        ;;STRING:: "ies"
                                      (59 (aastore))
                                      (60 (invokestatic
					(methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector"))))
                                      (63 (checkcast (class "clojure.lang.AFn")))
                                      (66 (putstatic (fieldCP "const__4" "clojure.pprint$fn__7966$fn__7967" (class "clojure.lang.AFn"))))
                                      (69 (iconst_2))
                                      (70 (anewarray (class "java.lang.Object")))
                                      (73 (dup))
                                      (74 (iconst_0))
                                      (75 (ldc 7))        ;;STRING:: ""
                                      (77 (aastore))
                                      (78 (dup))
                                      (79 (iconst_1))
                                      (80 (ldc 8))        ;;STRING:: "s"
                                      (82 (aastore))
                                      (83 (invokestatic
					(methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector"))))
                                      (86 (checkcast (class "clojure.lang.AFn")))
                                      (89 (putstatic (fieldCP "const__5" "clojure.pprint$fn__7966$fn__7967" (class "clojure.lang.AFn"))))
                                      (92 (ldc 1))        ;;STRING:: "clojure.pprint"
                                      (94 (ldc 9))        ;;STRING:: "next-arg"
                                      (96 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (99 (checkcast (class "clojure.lang.Var")))
                                      (102 (putstatic (fieldCP "const__6" "clojure.pprint$fn__7966$fn__7967" (class "clojure.lang.Var"))))
                                      (105 (ldc 10))      ;;STRING:: "clojure.core"
                                      (107 (ldc 11))      ;;STRING:: "nth"
                                      (109 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (112 (checkcast (class "clojure.lang.Var")))
                                      (115 (putstatic (fieldCP "const__7" "clojure.pprint$fn__7966$fn__7967" (class "clojure.lang.Var"))))
                                      (118 (lconst_0))
                                      (119 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (122 (putstatic (fieldCP "const__8" "clojure.pprint$fn__7966$fn__7967" (class "java.lang.Object"))))
                                      (125 (lconst_1))
                                      (126 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (129 (putstatic (fieldCP "const__9" "clojure.pprint$fn__7966$fn__7967" (class "java.lang.Object"))))
                                      (132 (ldc 10))      ;;STRING:: "clojure.core"
                                      (134 (ldc 12))      ;;STRING:: "print"
                                      (136 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (139 (checkcast (class "clojure.lang.Var")))
                                      (142 (putstatic (fieldCP "const__10" "clojure.pprint$fn__7966$fn__7967" (class "clojure.lang.Var"))))
                                      (145 (ldc 10))      ;;STRING:: "clojure.core"
                                      (147 (ldc 13))      ;;STRING:: "="
                                      (149 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (152 (checkcast (class "clojure.lang.Var")))
                                      (155 (putstatic (fieldCP "const__11" "clojure.pprint$fn__7966$fn__7967" (class "clojure.lang.Var"))))
                                      (158 (ldc 10))      ;;STRING:: "clojure.core"
                                      (160 (ldc 14))      ;;STRING:: "first"
                                      (162 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (165 (checkcast (class "clojure.lang.Var")))
                                      (168 (putstatic (fieldCP "const__12" "clojure.pprint$fn__7966$fn__7967" (class "clojure.lang.Var"))))
                                      (171 (ldc 10))      ;;STRING:: "clojure.core"
                                      (173 (ldc 15))      ;;STRING:: "second"
                                      (175 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (178 (checkcast (class "clojure.lang.Var")))
                                      (181 (putstatic (fieldCP "const__13" "clojure.pprint$fn__7966$fn__7967" (class "clojure.lang.Var"))))
                                      (184 (new (class "clojure.lang.KeywordLookupSite")))
                                      (187 (dup))
                                      (188 (aconst_null))
                                      (189 (ldc 0))       ;;STRING:: "colon"
                                      (191 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (194 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (197 (dup))
                                      (198 (putstatic (fieldCP "__site__0__" "clojure.pprint$fn__7966$fn__7967" (class "clojure.lang.KeywordLookupSite"))))
                                      (201 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$fn__7966$fn__7967" (class "clojure.lang.ILookupThunk"))))
                                      (204 (new (class "clojure.lang.KeywordLookupSite")))
                                      (207 (dup))
                                      (208 (aconst_null))
                                      (209 (ldc 4))       ;;STRING:: "at"
                                      (211 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (214 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (217 (dup))
                                      (218 (putstatic (fieldCP "__site__1__" "clojure.pprint$fn__7966$fn__7967" (class "clojure.lang.KeywordLookupSite"))))
                                      (221 (putstatic (fieldCP "__thunk__1__" "clojure.pprint$fn__7966$fn__7967" (class "clojure.lang.ILookupThunk"))))
                                      (224 (return))
                                      (endofcode 225))
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
                                   (max_stack . 4) (max_locals . 9) (code_length . 268)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "__thunk__0__" "clojure.pprint$fn__7966$fn__7967" (class "clojure.lang.ILookupThunk")))) 
                                      (3 (dup)) 
                                      (4 (aload_1)) 
                                      (5 (dup_x2)) 
                                      (6 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (11 (dup_x2)) 
                                      (12 (if_acmpeq 19)) ;;to TAG_0
                                      (15 (pop)) 
                                      (16 (goto 41))  ;;to TAG_1
                                      (19 (swap)) ;;at TAG_0
                                      (20 (pop)) 
                                      (21 (dup)) 
                                      (22 (getstatic (fieldCP "__site__0__" "clojure.pprint$fn__7966$fn__7967" (class "clojure.lang.KeywordLookupSite")))) 
                                      (25 (swap)) 
                                      (26 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (31 (dup)) 
                                      (32 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$fn__7966$fn__7967" (class "clojure.lang.ILookupThunk")))) 
                                      (35 (swap)) 
                                      (36 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (41 (dup)) ;;at TAG_1
                                      (42 (ifnull 74)) ;;to TAG_2
                                      (45 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (48 (if_acmpeq 75)) ;;to TAG_3
                                      (51 (getstatic (fieldCP "const__1" "clojure.pprint$fn__7966$fn__7967" (class "clojure.lang.Var")))) 
                                      (54 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (57 (checkcast (class "clojure.lang.IFn"))) 
                                      (60 (aload_2)) 
                                      (61 (aconst_null)) 
                                      (62 (astore_2)) 
                                      (63 (getstatic (fieldCP "const__2" "clojure.pprint$fn__7966$fn__7967" (class "java.lang.Object")))) 
                                      (66 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (71 (goto 78)) ;;to TAG_4
                                      (74 (pop)) ;;at TAG_2
                                      (75 (aload_2)) ;;at TAG_3
                                      (76 (aconst_null)) 
                                      (77 (astore_2)) 
                                      (78 (astore 4)) ;;at TAG_4
                                      (80 (getstatic (fieldCP "__thunk__1__" "clojure.pprint$fn__7966$fn__7967" (class "clojure.lang.ILookupThunk")))) 
                                      (83 (dup)) 
                                      (84 (aload_1)) 
                                      (85 (aconst_null)) 
                                      (86 (astore_1)) 
                                      (87 (dup_x2)) 
                                      (88 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (93 (dup_x2)) 
                                      (94 (if_acmpeq 101)) ;;to TAG_5
                                      (97 (pop)) 
                                      (98 (goto 123)) ;;to TAG_6
                                      (101 (swap)) ;;at TAG_5
                                      (102 (pop)) 
                                      (103 (dup)) 
                                      (104 (getstatic (fieldCP "__site__1__" "clojure.pprint$fn__7966$fn__7967" (class "clojure.lang.KeywordLookupSite")))) 
                                      (107 (swap)) 
                                      (108 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (113 (dup)) 
                                      (114 (putstatic (fieldCP "__thunk__1__" "clojure.pprint$fn__7966$fn__7967" (class "clojure.lang.ILookupThunk")))) 
                                      (117 (swap)) 
                                      (118 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (123 (dup)) ;;at TAG_6
                                      (124 (ifnull 139)) ;;to TAG_7
                                      (127 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (130 (if_acmpeq 140)) ;;to TAG_8
                                      (133 (getstatic (fieldCP "const__4" "clojure.pprint$fn__7966$fn__7967" (class "clojure.lang.AFn")))) 
                                      (136 (goto 143)) ;;to TAG_9
                                      (139 (pop)) ;;at TAG_7
                                      (140 (getstatic (fieldCP "const__5" "clojure.pprint$fn__7966$fn__7967" (class "clojure.lang.AFn")))) ;;at TAG_8
                                      (143 (astore 5)) ;;at TAG_9
                                      (145 (getstatic (fieldCP "const__6" "clojure.pprint$fn__7966$fn__7967" (class "clojure.lang.Var")))) 
                                      (148 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (151 (checkcast (class "clojure.lang.IFn"))) 
                                      (154 (aload 4)) 
                                      (156 (aconst_null)) 
                                      (157 (astore 4)) 
                                      (159 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (164 (astore 6)) 
                                      (166 (aload 6)) 
                                      (168 (lconst_0)) 
                                      (169 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (172 (aconst_null)) 
                                      (173 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (176 (astore 7)) 
                                      (178 (aload 6)) 
                                      (180 (aconst_null)) 
                                      (181 (astore 6)) 
                                      (183 (lconst_1)) 
                                      (184 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (187 (aconst_null)) 
                                      (188 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (191 (astore 8)) 
                                      (193 (getstatic (fieldCP "const__10" "clojure.pprint$fn__7966$fn__7967" (class "clojure.lang.Var")))) 
                                      (196 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (199 (checkcast (class "clojure.lang.IFn"))) 
                                      (202 (aload 7)) 
                                      (204 (aconst_null)) 
                                      (205 (astore 7)) 
                                      (207 (lconst_1)) 
                                      (208 (invokestatic (methodCP "equiv" "clojure.lang.Util" ((class "java.lang.Object") long) boolean))) 
                                      (211 (ifeq 237)) ;;to TAG_10
                                      (214 (getstatic (fieldCP "const__12" "clojure.pprint$fn__7966$fn__7967" (class "clojure.lang.Var")))) 
                                      (217 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (220 (checkcast (class "clojure.lang.IFn"))) 
                                      (223 (aload 5)) 
                                      (225 (aconst_null)) 
                                      (226 (astore 5)) 
                                      (228 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (233 (goto 256)) ;;to TAG_11
                                      (236 (pop)) 
                                      (237 (getstatic (fieldCP "const__13" "clojure.pprint$fn__7966$fn__7967" (class "clojure.lang.Var")))) ;;at TAG_10
                                      (240 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (243 (checkcast (class "clojure.lang.IFn"))) 
                                      (246 (aload 5)) 
                                      (248 (aconst_null)) 
                                      (249 (astore 5)) 
                                      (251 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (256 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) ;;at TAG_11
                                      (261 (pop)) 
                                      (262 (aload 8)) 
                                      (264 (aconst_null)) 
                                      (265 (astore 8)) 
                                      (267 (areturn)) 
                                      (endofcode 268))
                                   (Exceptions )
                                   (StackMap )))
                        (method "swapThunk"
                              (parameters int (class "clojure.lang.ILookupThunk"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 3) (code_length . 39)
                                   (parsedcode
                                      (0 (iload_1)) 
                                      (1 (tableswitch (tableswitchinfo 38 (0 . 1) (24 31))))  ;;to TAG_2;;to TAG_0;;to TAG_1
                                      (24 (aload_2)) ;;at TAG_1
                                      (25 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$fn__7966$fn__7967" (class "clojure.lang.ILookupThunk")))) 
                                      (28 (goto 38)) ;;to TAG_0
                                      (31 (aload_2)) ;;at TAG_2
                                      (32 (putstatic (fieldCP "__thunk__1__" "clojure.pprint$fn__7966$fn__7967" (class "clojure.lang.ILookupThunk")))) 
                                      (35 (goto 38)) ;;to TAG_0
                                      (38 (return)) ;;at TAG_0
                                      (endofcode 39))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *pprint$fn__7966$fn__7967-class-table*
  (make-static-class-decls 
   *clojure.pprint$fn__7966$fn__7967*))

(defconst *package-name-map* 
  ("clojure.pprint$fn__7966$fn__7967" . "clojure"))

