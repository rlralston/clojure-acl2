; pprint$fn__8023-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:55 CDT 2014.
;

(defconst *clojure.pprint$fn__8023*
 (make-class-def
      '(class "clojure.pprint$fn__8023"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "at")
                        (STRING  "colon")
                        (STRING  "clojure.pprint")
                        (STRING  "upcase-writer")
                        (STRING  "capitalize-word-writer")
                        (STRING  "init-cap-writer")
                        (STRING  "else")
                        (STRING  "downcase-writer"))
            (fields
                        (field "const__0" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "__site__0__" (class "clojure.lang.KeywordLookupSite") (accessflags  *class*  *final*  *static* ) -1)
                        (field "__thunk__0__" (class "clojure.lang.ILookupThunk") (accessflags  *class*  *static* ) -1)
                        (field "__site__1__" (class "clojure.lang.KeywordLookupSite") (accessflags  *class*  *final*  *static* ) -1)
                        (field "__thunk__1__" (class "clojure.lang.ILookupThunk") (accessflags  *class*  *static* ) -1)
                        (field "__site__2__" (class "clojure.lang.KeywordLookupSite") (accessflags  *class*  *final*  *static* ) -1)
                        (field "__thunk__2__" (class "clojure.lang.ILookupThunk") (accessflags  *class*  *static* ) -1)
                        (field "__site__3__" (class "clojure.lang.KeywordLookupSite") (accessflags  *class*  *final*  *static* ) -1)
                        (field "__thunk__3__" (class "clojure.lang.ILookupThunk") (accessflags  *class*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 0) (code_length . 169)
                                   (parsedcode
                                      (0 (aconst_null))
                                      (1 (ldc 0))         ;;STRING:: "at"
                                      (3 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (6 (checkcast (class "clojure.lang.Keyword")))
                                      (9 (putstatic (fieldCP "const__0" "clojure.pprint$fn__8023" (class "clojure.lang.Keyword"))))
                                      (12 (aconst_null))
                                      (13 (ldc 1))        ;;STRING:: "colon"
                                      (15 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (18 (checkcast (class "clojure.lang.Keyword")))
                                      (21 (putstatic (fieldCP "const__1" "clojure.pprint$fn__8023" (class "clojure.lang.Keyword"))))
                                      (24 (ldc 2))        ;;STRING:: "clojure.pprint"
                                      (26 (ldc 3))        ;;STRING:: "upcase-writer"
                                      (28 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (31 (checkcast (class "clojure.lang.Var")))
                                      (34 (putstatic (fieldCP "const__2" "clojure.pprint$fn__8023" (class "clojure.lang.Var"))))
                                      (37 (ldc 2))        ;;STRING:: "clojure.pprint"
                                      (39 (ldc 4))        ;;STRING:: "capitalize-word-writer"
                                      (41 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (44 (checkcast (class "clojure.lang.Var")))
                                      (47 (putstatic (fieldCP "const__3" "clojure.pprint$fn__8023" (class "clojure.lang.Var"))))
                                      (50 (ldc 2))        ;;STRING:: "clojure.pprint"
                                      (52 (ldc 5))        ;;STRING:: "init-cap-writer"
                                      (54 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (57 (checkcast (class "clojure.lang.Var")))
                                      (60 (putstatic (fieldCP "const__4" "clojure.pprint$fn__8023" (class "clojure.lang.Var"))))
                                      (63 (aconst_null))
                                      (64 (ldc 6))        ;;STRING:: "else"
                                      (66 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (69 (checkcast (class "clojure.lang.Keyword")))
                                      (72 (putstatic (fieldCP "const__5" "clojure.pprint$fn__8023" (class "clojure.lang.Keyword"))))
                                      (75 (ldc 2))        ;;STRING:: "clojure.pprint"
                                      (77 (ldc 7))        ;;STRING:: "downcase-writer"
                                      (79 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (82 (checkcast (class "clojure.lang.Var")))
                                      (85 (putstatic (fieldCP "const__6" "clojure.pprint$fn__8023" (class "clojure.lang.Var"))))
                                      (88 (new (class "clojure.lang.KeywordLookupSite")))
                                      (91 (dup))
                                      (92 (aconst_null))
                                      (93 (ldc 0))        ;;STRING:: "at"
                                      (95 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (98 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (101 (dup))
                                      (102 (putstatic (fieldCP "__site__0__" "clojure.pprint$fn__8023" (class "clojure.lang.KeywordLookupSite"))))
                                      (105 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$fn__8023" (class "clojure.lang.ILookupThunk"))))
                                      (108 (new (class "clojure.lang.KeywordLookupSite")))
                                      (111 (dup))
                                      (112 (aconst_null))
                                      (113 (ldc 1))       ;;STRING:: "colon"
                                      (115 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (118 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (121 (dup))
                                      (122 (putstatic (fieldCP "__site__1__" "clojure.pprint$fn__8023" (class "clojure.lang.KeywordLookupSite"))))
                                      (125 (putstatic (fieldCP "__thunk__1__" "clojure.pprint$fn__8023" (class "clojure.lang.ILookupThunk"))))
                                      (128 (new (class "clojure.lang.KeywordLookupSite")))
                                      (131 (dup))
                                      (132 (aconst_null))
                                      (133 (ldc 1))       ;;STRING:: "colon"
                                      (135 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (138 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (141 (dup))
                                      (142 (putstatic (fieldCP "__site__2__" "clojure.pprint$fn__8023" (class "clojure.lang.KeywordLookupSite"))))
                                      (145 (putstatic (fieldCP "__thunk__2__" "clojure.pprint$fn__8023" (class "clojure.lang.ILookupThunk"))))
                                      (148 (new (class "clojure.lang.KeywordLookupSite")))
                                      (151 (dup))
                                      (152 (aconst_null))
                                      (153 (ldc 0))       ;;STRING:: "at"
                                      (155 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (158 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (161 (dup))
                                      (162 (putstatic (fieldCP "__site__3__" "clojure.pprint$fn__8023" (class "clojure.lang.KeywordLookupSite"))))
                                      (165 (putstatic (fieldCP "__thunk__3__" "clojure.pprint$fn__8023" (class "clojure.lang.ILookupThunk"))))
                                      (168 (return))
                                      (endofcode 169))
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
                                   (max_stack . 4) (max_locals . 4) (code_length . 281)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "__thunk__0__" "clojure.pprint$fn__8023" (class "clojure.lang.ILookupThunk")))) 
                                      (3 (dup)) 
                                      (4 (aload_1)) 
                                      (5 (dup_x2)) 
                                      (6 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (11 (dup_x2)) 
                                      (12 (if_acmpeq 19)) ;;to TAG_0
                                      (15 (pop)) 
                                      (16 (goto 41)) ;;to TAG_1
                                      (19 (swap)) ;;at TAG_0
                                      (20 (pop)) 
                                      (21 (dup)) 
                                      (22 (getstatic (fieldCP "__site__0__" "clojure.pprint$fn__8023" (class "clojure.lang.KeywordLookupSite")))) 
                                      (25 (swap)) 
                                      (26 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (31 (dup)) 
                                      (32 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$fn__8023" (class "clojure.lang.ILookupThunk")))) 
                                      (35 (swap)) 
                                      (36 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (41 (astore_3)) ;;at TAG_1
                                      (42 (aload_3)) 
                                      (43 (dup)) 
                                      (44 (ifnull 97)) ;;to TAG_2
                                      (47 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (50 (if_acmpeq 98)) ;;to TAG_3
                                      (53 (getstatic (fieldCP "__thunk__1__" "clojure.pprint$fn__8023" (class "clojure.lang.ILookupThunk")))) 
                                      (56 (dup)) 
                                      (57 (aload_1)) 
                                      (58 (dup_x2)) 
                                      (59 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (64 (dup_x2)) 
                                      (65 (if_acmpeq 72)) ;;to TAG_4
                                      (68 (pop)) 
                                      (69 (goto 94)) ;;to TAG_5
                                      (72 (swap)) ;;at TAG_4
                                      (73 (pop)) 
                                      (74 (dup)) 
                                      (75 (getstatic (fieldCP "__site__1__" "clojure.pprint$fn__8023" (class "clojure.lang.KeywordLookupSite")))) 
                                      (78 (swap)) 
                                      (79 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (84 (dup)) 
                                      (85 (putstatic (fieldCP "__thunk__1__" "clojure.pprint$fn__8023" (class "clojure.lang.ILookupThunk")))) 
                                      (88 (swap)) 
                                      (89 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (94 (goto 101)) ;;to TAG_6;;at TAG_5
                                      (97 (pop)) ;;at TAG_2
                                      (98 (aload_3)) ;;at TAG_3
                                      (99 (aconst_null)) 
                                      (100 (astore_3)) 
                                      (101 (dup)) ;;at TAG_6
                                      (102 (ifnull 120)) ;;to TAG_7
                                      (105 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (108 (if_acmpeq 121)) ;;to TAG_8
                                      (111 (getstatic (fieldCP "const__2" "clojure.pprint$fn__8023" (class "clojure.lang.Var")))) 
                                      (114 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (117 (goto 269)) ;;to TAG_9
                                      (120 (pop)) ;;at TAG_7
                                      (121 (getstatic (fieldCP "__thunk__2__" "clojure.pprint$fn__8023" (class "clojure.lang.ILookupThunk")))) ;;at TAG_8
                                      (124 (dup)) 
                                      (125 (aload_1)) 
                                      (126 (dup_x2)) 
                                      (127 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (132 (dup_x2)) 
                                      (133 (if_acmpeq 140)) ;;to TAG_10
                                      (136 (pop)) 
                                      (137 (goto 162)) ;;to TAG_11
                                      (140 (swap)) ;;at TAG_10
                                      (141 (pop)) 
                                      (142 (dup)) 
                                      (143 (getstatic (fieldCP "__site__2__" "clojure.pprint$fn__8023" (class "clojure.lang.KeywordLookupSite")))) 
                                      (146 (swap)) 
                                      (147 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (152 (dup)) 
                                      (153 (putstatic (fieldCP "__thunk__2__" "clojure.pprint$fn__8023" (class "clojure.lang.ILookupThunk")))) 
                                      (156 (swap)) 
                                      (157 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (162 (dup)) ;;at TAG_11
                                      (163 (ifnull 181)) ;;to TAG_12
                                      (166 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (169 (if_acmpeq 182))  ;;to TAG_13
                                      (172 (getstatic (fieldCP "const__3" "clojure.pprint$fn__8023" (class "clojure.lang.Var")))) 
                                      (175 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (178 (goto 269)) ;;to TAG_9
                                      (181 (pop)) ;;at TAG_12
                                      (182 (getstatic (fieldCP "__thunk__3__" "clojure.pprint$fn__8023" (class "clojure.lang.ILookupThunk")))) ;;at TAG_13
                                      (185 (dup)) 
                                      (186 (aload_1)) 
                                      (187 (aconst_null)) 
                                      (188 (astore_1)) 
                                      (189 (dup_x2)) 
                                      (190 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (195 (dup_x2)) 
                                      (196 (if_acmpeq 203)) ;;to TAG_14
                                      (199 (pop)) 
                                      (200 (goto 225)) ;;to TAG_15
                                      (203 (swap)) ;;at TAG_14
                                      (204 (pop)) 
                                      (205 (dup)) 
                                      (206 (getstatic (fieldCP "__site__3__" "clojure.pprint$fn__8023" (class "clojure.lang.KeywordLookupSite")))) 
                                      (209 (swap)) 
                                      (210 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (215 (dup)) 
                                      (216 (putstatic (fieldCP "__thunk__3__" "clojure.pprint$fn__8023" (class "clojure.lang.ILookupThunk")))) 
                                      (219 (swap)) 
                                      (220 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (225 (dup)) ;;at TAG_15
                                      (226 (ifnull 244)) ;;to TAG_16
                                      (229 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (232 (if_acmpeq 245)) ;;to TAG_17
                                      (235 (getstatic (fieldCP "const__4" "clojure.pprint$fn__8023" (class "clojure.lang.Var")))) 
                                      (238 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (241 (goto 269)) ;;to TAG_9
                                      (244 (pop)) ;;at TAG_16
                                      (245 (getstatic (fieldCP "const__5" "clojure.pprint$fn__8023" (class "clojure.lang.Keyword")))) ;;at TAG_17
                                      (248 (dup)) 
                                      (249 (ifnull 267)) ;;to TAG_18
                                      (252 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (255 (if_acmpeq 268)) ;;to TAG_19
                                      (258 (getstatic (fieldCP "const__6" "clojure.pprint$fn__8023" (class "clojure.lang.Var")))) 
                                      (261 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (264 (goto 269)) ;;to TAG_9
                                      (267 (pop)) ;;at TAG_18
                                      (268 (aconst_null)) ;;at TAG_19
                                      (269 (astore_3)) ;;at TAG_9
                                      (270 (new (class "clojure.pprint$fn__8023$fn__8024"))) 
                                      (273 (dup)) 
                                      (274 (aload_3)) 
                                      (275 (aconst_null)) 
                                      (276 (astore_3)) 
                                      (277 (invokespecial (methodCP "<init>" "clojure.pprint$fn__8023$fn__8024" ((class "java.lang.Object")) void))) 
                                      (280 (areturn)) 
                                      (endofcode 281))
                                   (Exceptions )
                                   (StackMap )))
                        (method "swapThunk"
                              (parameters int (class "clojure.lang.ILookupThunk"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 3) (code_length . 61)
                                   (parsedcode
                                      (0 (iload_1)) 
                                      (1 (tableswitch (tableswitchinfo 60 (0 . 3) (32 39 46 53))))  ;;to TAG_2;;to TAG_3;;to TAG_4;;to TAG_0;;to TAG_1
                                      (32 (aload_2)) ;;at TAG_1
                                      (33 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$fn__8023" (class "clojure.lang.ILookupThunk")))) 
                                      (36 (goto 60)) ;;to TAG_0
                                      (39 (aload_2)) ;;at TAG_2
                                      (40 (putstatic (fieldCP "__thunk__1__" "clojure.pprint$fn__8023" (class "clojure.lang.ILookupThunk")))) 
                                      (43 (goto 60)) ;;to TAG_0
                                      (46 (aload_2)) ;;at TAG_3
                                      (47 (putstatic (fieldCP "__thunk__2__" "clojure.pprint$fn__8023" (class "clojure.lang.ILookupThunk")))) 
                                      (50 (goto 60)) ;;to TAG_0
                                      (53 (aload_2)) ;;at TAG_4
                                      (54 (putstatic (fieldCP "__thunk__3__" "clojure.pprint$fn__8023" (class "clojure.lang.ILookupThunk")))) 
                                      (57 (goto 60)) ;;to TAG_0
                                      (60 (return)) ;;at TAG_0
                                      (endofcode 61))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *pprint$fn__8023-class-table*
  (make-static-class-decls 
   *clojure.pprint$fn__8023*))

(defconst *package-name-map* 
  ("clojure.pprint$fn__8023" . "clojure"))

