; pprint$absolute_reposition-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:54 CDT 2014.
;

(defconst *clojure.pprint$absolute_reposition*
 (make-class-def
      '(class "clojure.pprint$absolute_reposition"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  ">=")
                        (STRING  "pos")
                        (STRING  "clojure.pprint")
                        (STRING  "relative-reposition")
                        (STRING  "-")
                        (STRING  "struct")
                        (STRING  "arg-navigator")
                        (STRING  "seq")
                        (STRING  "drop"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
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
                                   (max_stack . 4) (max_locals . 0) (code_length . 183)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: ">="
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.pprint$absolute_reposition" (class "clojure.lang.Var"))))
                                      (13 (aconst_null))
                                      (14 (ldc 2))        ;;STRING:: "pos"
                                      (16 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (19 (checkcast (class "clojure.lang.Keyword")))
                                      (22 (putstatic (fieldCP "const__1" "clojure.pprint$absolute_reposition" (class "clojure.lang.Keyword"))))
                                      (25 (ldc 3))        ;;STRING:: "clojure.pprint"
                                      (27 (ldc 4))        ;;STRING:: "relative-reposition"
                                      (29 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (32 (checkcast (class "clojure.lang.Var")))
                                      (35 (putstatic (fieldCP "const__2" "clojure.pprint$absolute_reposition" (class "clojure.lang.Var"))))
                                      (38 (ldc 0))        ;;STRING:: "clojure.core"
                                      (40 (ldc 5))        ;;STRING:: "-"
                                      (42 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (45 (checkcast (class "clojure.lang.Var")))
                                      (48 (putstatic (fieldCP "const__3" "clojure.pprint$absolute_reposition" (class "clojure.lang.Var"))))
                                      (51 (ldc 0))        ;;STRING:: "clojure.core"
                                      (53 (ldc 6))        ;;STRING:: "struct"
                                      (55 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (58 (checkcast (class "clojure.lang.Var")))
                                      (61 (putstatic (fieldCP "const__4" "clojure.pprint$absolute_reposition" (class "clojure.lang.Var"))))
                                      (64 (ldc 3))        ;;STRING:: "clojure.pprint"
                                      (66 (ldc 7))        ;;STRING:: "arg-navigator"
                                      (68 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (71 (checkcast (class "clojure.lang.Var")))
                                      (74 (putstatic (fieldCP "const__5" "clojure.pprint$absolute_reposition" (class "clojure.lang.Var"))))
                                      (77 (aconst_null))
                                      (78 (ldc 8))        ;;STRING:: "seq"
                                      (80 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (83 (checkcast (class "clojure.lang.Keyword")))
                                      (86 (putstatic (fieldCP "const__6" "clojure.pprint$absolute_reposition" (class "clojure.lang.Keyword"))))
                                      (89 (ldc 0))        ;;STRING:: "clojure.core"
                                      (91 (ldc 9))        ;;STRING:: "drop"
                                      (93 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (96 (checkcast (class "clojure.lang.Var")))
                                      (99 (putstatic (fieldCP "const__7" "clojure.pprint$absolute_reposition" (class "clojure.lang.Var"))))
                                      (102 (new (class "clojure.lang.KeywordLookupSite")))
                                      (105 (dup))
                                      (106 (aconst_null))
                                      (107 (ldc 2))       ;;STRING:: "pos"
                                      (109 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (112 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (115 (dup))
                                      (116 (putstatic (fieldCP "__site__0__" "clojure.pprint$absolute_reposition" (class "clojure.lang.KeywordLookupSite"))))
                                      (119 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$absolute_reposition" (class "clojure.lang.ILookupThunk"))))
                                      (122 (new (class "clojure.lang.KeywordLookupSite")))
                                      (125 (dup))
                                      (126 (aconst_null))
                                      (127 (ldc 2))       ;;STRING:: "pos"
                                      (129 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (132 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (135 (dup))
                                      (136 (putstatic (fieldCP "__site__1__" "clojure.pprint$absolute_reposition" (class "clojure.lang.KeywordLookupSite"))))
                                      (139 (putstatic (fieldCP "__thunk__1__" "clojure.pprint$absolute_reposition" (class "clojure.lang.ILookupThunk"))))
                                      (142 (new (class "clojure.lang.KeywordLookupSite")))
                                      (145 (dup))
                                      (146 (aconst_null))
                                      (147 (ldc 8))       ;;STRING:: "seq"
                                      (149 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (152 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (155 (dup))
                                      (156 (putstatic (fieldCP "__site__2__" "clojure.pprint$absolute_reposition" (class "clojure.lang.KeywordLookupSite"))))
                                      (159 (putstatic (fieldCP "__thunk__2__" "clojure.pprint$absolute_reposition" (class "clojure.lang.ILookupThunk"))))
                                      (162 (new (class "clojure.lang.KeywordLookupSite")))
                                      (165 (dup))
                                      (166 (aconst_null))
                                      (167 (ldc 8))       ;;STRING:: "seq"
                                      (169 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (172 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (175 (dup))
                                      (176 (putstatic (fieldCP "__site__3__" "clojure.pprint$absolute_reposition" (class "clojure.lang.KeywordLookupSite"))))
                                      (179 (putstatic (fieldCP "__thunk__3__" "clojure.pprint$absolute_reposition" (class "clojure.lang.ILookupThunk"))))
                                      (182 (return))
                                      (endofcode 183))
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
                                   (max_stack . 9) (max_locals . 3) (code_length . 239)
                                   (parsedcode
                                      (0 (aload_2)) 
                                      (1 (getstatic (fieldCP "__thunk__0__" "clojure.pprint$absolute_reposition" (class "clojure.lang.ILookupThunk")))) 
                                      (4 (dup)) 
                                      (5 (aload_1)) 
                                      (6 (dup_x2)) 
                                      (7 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (12 (dup_x2)) 
                                      (13 (if_acmpeq 20)) ;;to TAG_0
                                      (16 (pop)) 
                                      (17 (goto 42))  ;;to TAG_1
                                      (20 (swap)) ;;at TAG_0
                                      (21 (pop)) 
                                      (22 (dup)) 
                                      (23 (getstatic (fieldCP "__site__0__" "clojure.pprint$absolute_reposition" (class "clojure.lang.KeywordLookupSite")))) 
                                      (26 (swap)) 
                                      (27 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (32 (dup)) 
                                      (33 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$absolute_reposition" (class "clojure.lang.ILookupThunk")))) 
                                      (36 (swap)) 
                                      (37 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (42 (invokestatic (methodCP "gte" "clojure.lang.Numbers" ((class "java.lang.Object") (class "java.lang.Object")) boolean))) ;;at TAG_1
                                      (45 (ifeq 116)) ;;to TAG_2
                                      (48 (getstatic (fieldCP "const__2" "clojure.pprint$absolute_reposition" (class "clojure.lang.Var")))) 
                                      (51 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (54 (checkcast (class "clojure.lang.IFn"))) 
                                      (57 (aload_1)) 
                                      (58 (getstatic (fieldCP "__thunk__1__" "clojure.pprint$absolute_reposition" (class "clojure.lang.ILookupThunk")))) 
                                      (61 (dup)) 
                                      (62 (aload_1)) 
                                      (63 (aconst_null)) 
                                      (64 (astore_1)) 
                                      (65 (dup_x2)) 
                                      (66 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (71 (dup_x2)) 
                                      (72 (if_acmpeq 79)) ;;to TAG_3
                                      (75 (pop)) 
                                      (76 (goto 101)) ;;to TAG_4
                                      (79 (swap)) ;;at TAG_3
                                      (80 (pop)) 
                                      (81 (dup)) 
                                      (82 (getstatic (fieldCP "__site__1__" "clojure.pprint$absolute_reposition" (class "clojure.lang.KeywordLookupSite")))) 
                                      (85 (swap)) 
                                      (86 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (91 (dup)) 
                                      (92 (putstatic (fieldCP "__thunk__1__" "clojure.pprint$absolute_reposition" (class "clojure.lang.ILookupThunk")))) 
                                      (95 (swap)) 
                                      (96 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (101 (aload_2)) ;;at TAG_4
                                      (102 (aconst_null)) 
                                      (103 (astore_2)) 
                                      (104 (invokestatic (methodCP "minus" "clojure.lang.Numbers" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Number")))) 
                                      (107 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (112 (goto 238)) ;;to TAG_5
                                      (115 (pop)) 
                                      (116 (getstatic (fieldCP "const__4" "clojure.pprint$absolute_reposition" (class "clojure.lang.Var")))) ;;at TAG_2
                                      (119 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (122 (checkcast (class "clojure.lang.IFn"))) 
                                      (125 (getstatic (fieldCP "const__5" "clojure.pprint$absolute_reposition" (class "clojure.lang.Var")))) 
                                      (128 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (131 (getstatic (fieldCP "__thunk__2__" "clojure.pprint$absolute_reposition" (class "clojure.lang.ILookupThunk")))) 
                                      (134 (dup)) 
                                      (135 (aload_1)) 
                                      (136 (dup_x2)) 
                                      (137 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (142 (dup_x2)) 
                                      (143 (if_acmpeq 150)) ;;to TAG_6
                                      (146 (pop)) 
                                      (147 (goto 172)) ;;to TAG_7
                                      (150 (swap)) ;;at TAG_6
                                      (151 (pop)) 
                                      (152 (dup)) 
                                      (153 (getstatic (fieldCP "__site__2__" "clojure.pprint$absolute_reposition" (class "clojure.lang.KeywordLookupSite")))) 
                                      (156 (swap)) 
                                      (157 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (162 (dup)) 
                                      (163 (putstatic (fieldCP "__thunk__2__" "clojure.pprint$absolute_reposition" (class "clojure.lang.ILookupThunk")))) 
                                      (166 (swap)) 
                                      (167 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (172 (getstatic (fieldCP "const__7" "clojure.pprint$absolute_reposition" (class "clojure.lang.Var")))) ;;at TAG_7
                                      (175 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (178 (checkcast (class "clojure.lang.IFn"))) 
                                      (181 (aload_2)) 
                                      (182 (getstatic (fieldCP "__thunk__3__" "clojure.pprint$absolute_reposition" (class "clojure.lang.ILookupThunk")))) 
                                      (185 (dup)) 
                                      (186 (aload_1)) 
                                      (187 (aconst_null)) 
                                      (188 (astore_1)) 
                                      (189 (dup_x2)) 
                                      (190 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (195 (dup_x2)) 
                                      (196 (if_acmpeq 203)) ;;to TAG_8
                                      (199 (pop)) 
                                      (200 (goto 225)) ;;to TAG_9
                                      (203 (swap)) ;;at TAG_8
                                      (204 (pop)) 
                                      (205 (dup)) 
                                      (206 (getstatic (fieldCP "__site__3__" "clojure.pprint$absolute_reposition" (class "clojure.lang.KeywordLookupSite")))) 
                                      (209 (swap)) 
                                      (210 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (215 (dup)) 
                                      (216 (putstatic (fieldCP "__thunk__3__" "clojure.pprint$absolute_reposition" (class "clojure.lang.ILookupThunk")))) 
                                      (219 (swap)) 
                                      (220 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (225 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) ;;at TAG_9
                                      (230 (aload_2)) 
                                      (231 (aconst_null)) 
                                      (232 (astore_2)) 
                                      (233 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 5)) 
                                      (238 (areturn)) ;;at TAG_5
                                      (endofcode 239))
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
                                      (33 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$absolute_reposition" (class "clojure.lang.ILookupThunk")))) 
                                      (36 (goto 60)) ;;to TAG_0
                                      (39 (aload_2)) ;;at TAG_2
                                      (40 (putstatic (fieldCP "__thunk__1__" "clojure.pprint$absolute_reposition" (class "clojure.lang.ILookupThunk")))) 
                                      (43 (goto 60)) ;;to TAG_0
                                      (46 (aload_2)) ;;at TAG_3
                                      (47 (putstatic (fieldCP "__thunk__2__" "clojure.pprint$absolute_reposition" (class "clojure.lang.ILookupThunk")))) 
                                      (50 (goto 60)) ;;to TAG_0
                                      (53 (aload_2)) ;;at TAG_4
                                      (54 (putstatic (fieldCP "__thunk__3__" "clojure.pprint$absolute_reposition" (class "clojure.lang.ILookupThunk")))) 
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


(defconst *pprint$absolute_reposition-class-table*
  (make-static-class-decls 
   *clojure.pprint$absolute_reposition*))

(defconst *package-name-map* 
  ("clojure.pprint$absolute_reposition" . "clojure"))

