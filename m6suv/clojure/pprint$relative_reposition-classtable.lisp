; pprint$relative_reposition-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:57 CDT 2014.
;

(defconst *clojure.pprint$relative_reposition*
 (make-class-def
      '(class "clojure.pprint$relative_reposition"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "+")
                        (STRING  "pos")
                        (STRING  "neg?")
                        (STRING  "clojure.pprint")
                        (STRING  "absolute-reposition")
                        (STRING  "struct")
                        (STRING  "arg-navigator")
                        (STRING  "seq")
                        (STRING  "drop")
                        (STRING  "rest"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "__site__0__" (class "clojure.lang.KeywordLookupSite") (accessflags  *class*  *final*  *static* ) -1)
                        (field "__thunk__0__" (class "clojure.lang.ILookupThunk") (accessflags  *class*  *static* ) -1)
                        (field "__site__1__" (class "clojure.lang.KeywordLookupSite") (accessflags  *class*  *final*  *static* ) -1)
                        (field "__thunk__1__" (class "clojure.lang.ILookupThunk") (accessflags  *class*  *static* ) -1)
                        (field "__site__2__" (class "clojure.lang.KeywordLookupSite") (accessflags  *class*  *final*  *static* ) -1)
                        (field "__thunk__2__" (class "clojure.lang.ILookupThunk") (accessflags  *class*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 0) (code_length . 175)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "+"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.pprint$relative_reposition" (class "clojure.lang.Var"))))
                                      (13 (aconst_null))
                                      (14 (ldc 2))        ;;STRING:: "pos"
                                      (16 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (19 (checkcast (class "clojure.lang.Keyword")))
                                      (22 (putstatic (fieldCP "const__1" "clojure.pprint$relative_reposition" (class "clojure.lang.Keyword"))))
                                      (25 (ldc 0))        ;;STRING:: "clojure.core"
                                      (27 (ldc 3))        ;;STRING:: "neg?"
                                      (29 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (32 (checkcast (class "clojure.lang.Var")))
                                      (35 (putstatic (fieldCP "const__2" "clojure.pprint$relative_reposition" (class "clojure.lang.Var"))))
                                      (38 (ldc 4))        ;;STRING:: "clojure.pprint"
                                      (40 (ldc 5))        ;;STRING:: "absolute-reposition"
                                      (42 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (45 (checkcast (class "clojure.lang.Var")))
                                      (48 (putstatic (fieldCP "const__3" "clojure.pprint$relative_reposition" (class "clojure.lang.Var"))))
                                      (51 (ldc 0))        ;;STRING:: "clojure.core"
                                      (53 (ldc 6))        ;;STRING:: "struct"
                                      (55 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (58 (checkcast (class "clojure.lang.Var")))
                                      (61 (putstatic (fieldCP "const__4" "clojure.pprint$relative_reposition" (class "clojure.lang.Var"))))
                                      (64 (ldc 4))        ;;STRING:: "clojure.pprint"
                                      (66 (ldc 7))        ;;STRING:: "arg-navigator"
                                      (68 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (71 (checkcast (class "clojure.lang.Var")))
                                      (74 (putstatic (fieldCP "const__5" "clojure.pprint$relative_reposition" (class "clojure.lang.Var"))))
                                      (77 (aconst_null))
                                      (78 (ldc 8))        ;;STRING:: "seq"
                                      (80 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (83 (checkcast (class "clojure.lang.Keyword")))
                                      (86 (putstatic (fieldCP "const__6" "clojure.pprint$relative_reposition" (class "clojure.lang.Keyword"))))
                                      (89 (ldc 0))        ;;STRING:: "clojure.core"
                                      (91 (ldc 9))        ;;STRING:: "drop"
                                      (93 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (96 (checkcast (class "clojure.lang.Var")))
                                      (99 (putstatic (fieldCP "const__7" "clojure.pprint$relative_reposition" (class "clojure.lang.Var"))))
                                      (102 (aconst_null))
                                      (103 (ldc 10))      ;;STRING:: "rest"
                                      (105 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (108 (checkcast (class "clojure.lang.Keyword")))
                                      (111 (putstatic (fieldCP "const__8" "clojure.pprint$relative_reposition" (class "clojure.lang.Keyword"))))
                                      (114 (new (class "clojure.lang.KeywordLookupSite")))
                                      (117 (dup))
                                      (118 (aconst_null))
                                      (119 (ldc 2))       ;;STRING:: "pos"
                                      (121 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (124 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (127 (dup))
                                      (128 (putstatic (fieldCP "__site__0__" "clojure.pprint$relative_reposition" (class "clojure.lang.KeywordLookupSite"))))
                                      (131 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$relative_reposition" (class "clojure.lang.ILookupThunk"))))
                                      (134 (new (class "clojure.lang.KeywordLookupSite")))
                                      (137 (dup))
                                      (138 (aconst_null))
                                      (139 (ldc 8))       ;;STRING:: "seq"
                                      (141 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (144 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (147 (dup))
                                      (148 (putstatic (fieldCP "__site__1__" "clojure.pprint$relative_reposition" (class "clojure.lang.KeywordLookupSite"))))
                                      (151 (putstatic (fieldCP "__thunk__1__" "clojure.pprint$relative_reposition" (class "clojure.lang.ILookupThunk"))))
                                      (154 (new (class "clojure.lang.KeywordLookupSite")))
                                      (157 (dup))
                                      (158 (aconst_null))
                                      (159 (ldc 10))      ;;STRING:: "rest"
                                      (161 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (164 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (167 (dup))
                                      (168 (putstatic (fieldCP "__site__2__" "clojure.pprint$relative_reposition" (class "clojure.lang.KeywordLookupSite"))))
                                      (171 (putstatic (fieldCP "__thunk__2__" "clojure.pprint$relative_reposition" (class "clojure.lang.ILookupThunk"))))
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
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 9) (max_locals . 4) (code_length . 202)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "__thunk__0__" "clojure.pprint$relative_reposition" (class "clojure.lang.ILookupThunk")))) 
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
                                      (22 (getstatic (fieldCP "__site__0__" "clojure.pprint$relative_reposition" (class "clojure.lang.KeywordLookupSite")))) 
                                      (25 (swap)) 
                                      (26 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (31 (dup)) 
                                      (32 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$relative_reposition" (class "clojure.lang.ILookupThunk")))) 
                                      (35 (swap)) 
                                      (36 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (41 (aload_2)) ;;at TAG_1
                                      (42 (invokestatic (methodCP "add" "clojure.lang.Numbers" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Number")))) 
                                      (45 (astore_3)) 
                                      (46 (aload_2)) 
                                      (47 (invokestatic (methodCP "isNeg" "clojure.lang.Numbers" ((class "java.lang.Object")) boolean))) 
                                      (50 (ifeq 77))  ;;to TAG_2
                                      (53 (getstatic (fieldCP "const__3" "clojure.pprint$relative_reposition" (class "clojure.lang.Var")))) 
                                      (56 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (59 (checkcast (class "clojure.lang.IFn"))) 
                                      (62 (aload_1)) 
                                      (63 (aconst_null)) 
                                      (64 (astore_1)) 
                                      (65 (aload_3)) 
                                      (66 (aconst_null)) 
                                      (67 (astore_3)) 
                                      (68 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (73 (goto 201)) ;;to TAG_3
                                      (76 (pop)) 
                                      (77 (getstatic (fieldCP "const__4" "clojure.pprint$relative_reposition" (class "clojure.lang.Var")))) ;;at TAG_2
                                      (80 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (83 (checkcast (class "clojure.lang.IFn"))) 
                                      (86 (getstatic (fieldCP "const__5" "clojure.pprint$relative_reposition" (class "clojure.lang.Var")))) 
                                      (89 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (92 (getstatic (fieldCP "__thunk__1__" "clojure.pprint$relative_reposition" (class "clojure.lang.ILookupThunk")))) 
                                      (95 (dup)) 
                                      (96 (aload_1)) 
                                      (97 (dup_x2)) 
                                      (98 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (103 (dup_x2)) 
                                      (104 (if_acmpeq 111)) ;;to TAG_4
                                      (107 (pop)) 
                                      (108 (goto 133)) ;;to TAG_5
                                      (111 (swap)) ;;at TAG_4
                                      (112 (pop)) 
                                      (113 (dup)) 
                                      (114 (getstatic (fieldCP "__site__1__" "clojure.pprint$relative_reposition" (class "clojure.lang.KeywordLookupSite")))) 
                                      (117 (swap)) 
                                      (118 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (123 (dup)) 
                                      (124 (putstatic (fieldCP "__thunk__1__" "clojure.pprint$relative_reposition" (class "clojure.lang.ILookupThunk")))) 
                                      (127 (swap)) 
                                      (128 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (133 (getstatic (fieldCP "const__7" "clojure.pprint$relative_reposition" (class "clojure.lang.Var")))) ;;at TAG_5
                                      (136 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (139 (checkcast (class "clojure.lang.IFn"))) 
                                      (142 (aload_2)) 
                                      (143 (aconst_null)) 
                                      (144 (astore_2)) 
                                      (145 (getstatic (fieldCP "__thunk__2__" "clojure.pprint$relative_reposition" (class "clojure.lang.ILookupThunk")))) 
                                      (148 (dup)) 
                                      (149 (aload_1)) 
                                      (150 (aconst_null)) 
                                      (151 (astore_1)) 
                                      (152 (dup_x2)) 
                                      (153 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (158 (dup_x2)) 
                                      (159 (if_acmpeq 166)) ;;to TAG_6
                                      (162 (pop)) 
                                      (163 (goto 188)) ;;to TAG_7
                                      (166 (swap)) ;;at TAG_6
                                      (167 (pop)) 
                                      (168 (dup)) 
                                      (169 (getstatic (fieldCP "__site__2__" "clojure.pprint$relative_reposition" (class "clojure.lang.KeywordLookupSite")))) 
                                      (172 (swap)) 
                                      (173 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (178 (dup)) 
                                      (179 (putstatic (fieldCP "__thunk__2__" "clojure.pprint$relative_reposition" (class "clojure.lang.ILookupThunk")))) 
                                      (182 (swap)) 
                                      (183 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (188 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) ;;at TAG_7
                                      (193 (aload_3)) 
                                      (194 (aconst_null)) 
                                      (195 (astore_3)) 
                                      (196 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 5)) 
                                      (201 (areturn)) ;;at TAG_3
                                      (endofcode 202))
                                   (Exceptions )
                                   (StackMap )))
                        (method "swapThunk"
                              (parameters int (class "clojure.lang.ILookupThunk"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 3) (code_length . 50)
                                   (parsedcode
                                      (0 (iload_1)) 
                                      (1 (tableswitch (tableswitchinfo 49 (0 . 2) (28 35 42))))  ;;to TAG_2;;to TAG_3;;to TAG_0;;to TAG_1
                                      (28 (aload_2)) ;;at TAG_1
                                      (29 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$relative_reposition" (class "clojure.lang.ILookupThunk")))) 
                                      (32 (goto 49)) ;;to TAG_0
                                      (35 (aload_2)) ;;at TAG_2
                                      (36 (putstatic (fieldCP "__thunk__1__" "clojure.pprint$relative_reposition" (class "clojure.lang.ILookupThunk")))) 
                                      (39 (goto 49)) ;;to TAG_0
                                      (42 (aload_2)) ;;at TAG_3
                                      (43 (putstatic (fieldCP "__thunk__2__" "clojure.pprint$relative_reposition" (class "clojure.lang.ILookupThunk")))) 
                                      (46 (goto 49)) ;;to TAG_0
                                      (49 (return)) ;;at TAG_0
                                      (endofcode 50))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *pprint$relative_reposition-class-table*
  (make-static-class-decls 
   *clojure.pprint$relative_reposition*))

(defconst *package-name-map* 
  ("clojure.pprint$relative_reposition" . "clojure"))

