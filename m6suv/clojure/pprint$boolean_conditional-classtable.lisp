; pprint$boolean_conditional-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:55 CDT 2014.
;

(defconst *clojure.pprint$boolean_conditional*
 (make-class-def
      '(class "clojure.pprint$boolean_conditional"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.pprint")
                        (STRING  "next-arg")
                        (STRING  "clojure.core")
                        (STRING  "nth")
                        (STRING  "clauses")
                        (STRING  "second")
                        (STRING  "first")
                        (STRING  "execute-sub-format")
                        (STRING  "base-args"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
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
                                   (max_stack . 4) (max_locals . 0) (code_length . 144)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.pprint"
                                      (2 (ldc 1))         ;;STRING:: "next-arg"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.pprint$boolean_conditional" (class "clojure.lang.Var"))))
                                      (13 (ldc 2))        ;;STRING:: "clojure.core"
                                      (15 (ldc 3))        ;;STRING:: "nth"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.pprint$boolean_conditional" (class "clojure.lang.Var"))))
                                      (26 (lconst_0))
                                      (27 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (30 (putstatic (fieldCP "const__2" "clojure.pprint$boolean_conditional" (class "java.lang.Object"))))
                                      (33 (lconst_1))
                                      (34 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (37 (putstatic (fieldCP "const__3" "clojure.pprint$boolean_conditional" (class "java.lang.Object"))))
                                      (40 (aconst_null))
                                      (41 (ldc 4))        ;;STRING:: "clauses"
                                      (43 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (46 (checkcast (class "clojure.lang.Keyword")))
                                      (49 (putstatic (fieldCP "const__4" "clojure.pprint$boolean_conditional" (class "clojure.lang.Keyword"))))
                                      (52 (ldc 2))        ;;STRING:: "clojure.core"
                                      (54 (ldc 5))        ;;STRING:: "second"
                                      (56 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (59 (checkcast (class "clojure.lang.Var")))
                                      (62 (putstatic (fieldCP "const__5" "clojure.pprint$boolean_conditional" (class "clojure.lang.Var"))))
                                      (65 (ldc 2))        ;;STRING:: "clojure.core"
                                      (67 (ldc 6))        ;;STRING:: "first"
                                      (69 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (72 (checkcast (class "clojure.lang.Var")))
                                      (75 (putstatic (fieldCP "const__6" "clojure.pprint$boolean_conditional" (class "clojure.lang.Var"))))
                                      (78 (ldc 0))        ;;STRING:: "clojure.pprint"
                                      (80 (ldc 7))        ;;STRING:: "execute-sub-format"
                                      (82 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (85 (checkcast (class "clojure.lang.Var")))
                                      (88 (putstatic (fieldCP "const__7" "clojure.pprint$boolean_conditional" (class "clojure.lang.Var"))))
                                      (91 (aconst_null))
                                      (92 (ldc 8))        ;;STRING:: "base-args"
                                      (94 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (97 (checkcast (class "clojure.lang.Keyword")))
                                      (100 (putstatic (fieldCP "const__8" "clojure.pprint$boolean_conditional" (class "clojure.lang.Keyword"))))
                                      (103 (new (class "clojure.lang.KeywordLookupSite")))
                                      (106 (dup))
                                      (107 (aconst_null))
                                      (108 (ldc 4))       ;;STRING:: "clauses"
                                      (110 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (113 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (116 (dup))
                                      (117 (putstatic (fieldCP "__site__0__" "clojure.pprint$boolean_conditional" (class "clojure.lang.KeywordLookupSite"))))
                                      (120 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$boolean_conditional" (class "clojure.lang.ILookupThunk"))))
                                      (123 (new (class "clojure.lang.KeywordLookupSite")))
                                      (126 (dup))
                                      (127 (aconst_null))
                                      (128 (ldc 8))       ;;STRING:: "base-args"
                                      (130 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (133 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (136 (dup))
                                      (137 (putstatic (fieldCP "__site__1__" "clojure.pprint$boolean_conditional" (class "clojure.lang.KeywordLookupSite"))))
                                      (140 (putstatic (fieldCP "__thunk__1__" "clojure.pprint$boolean_conditional" (class "clojure.lang.ILookupThunk"))))
                                      (143 (return))
                                      (endofcode 144))
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
                                   (max_stack . 7) (max_locals . 9) (code_length . 237)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.pprint$boolean_conditional" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (aload_2)) 
                                      (10 (aconst_null)) 
                                      (11 (astore_2)) 
                                      (12 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (17 (astore 4)) 
                                      (19 (aload 4)) 
                                      (21 (lconst_0)) 
                                      (22 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (25 (aconst_null)) 
                                      (26 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (29 (astore 5)) 
                                      (31 (aload 4)) 
                                      (33 (aconst_null)) 
                                      (34 (astore 4)) 
                                      (36 (lconst_1)) 
                                      (37 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (40 (aconst_null)) 
                                      (41 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (44 (astore 6)) 
                                      (46 (getstatic (fieldCP "__thunk__0__" "clojure.pprint$boolean_conditional" (class "clojure.lang.ILookupThunk")))) 
                                      (49 (dup)) 
                                      (50 (aload_1)) 
                                      (51 (dup_x2)) 
                                      (52 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (57 (dup_x2)) 
                                      (58 (if_acmpeq 65)) ;;to TAG_0
                                      (61 (pop)) 
                                      (62 (goto 87))  ;;to TAG_1
                                      (65 (swap)) ;;at TAG_0
                                      (66 (pop)) 
                                      (67 (dup)) 
                                      (68 (getstatic (fieldCP "__site__0__" "clojure.pprint$boolean_conditional" (class "clojure.lang.KeywordLookupSite")))) 
                                      (71 (swap)) 
                                      (72 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (77 (dup)) 
                                      (78 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$boolean_conditional" (class "clojure.lang.ILookupThunk")))) 
                                      (81 (swap)) 
                                      (82 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (87 (astore 7)) ;;at TAG_1
                                      (89 (aload 5)) 
                                      (91 (aconst_null)) 
                                      (92 (astore 5)) 
                                      (94 (dup)) 
                                      (95 (ifnull 126)) ;;to TAG_2
                                      (98 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (101 (if_acmpeq 127)) ;;to TAG_3
                                      (104 (getstatic (fieldCP "const__5" "clojure.pprint$boolean_conditional" (class "clojure.lang.Var")))) 
                                      (107 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (110 (checkcast (class "clojure.lang.IFn"))) 
                                      (113 (aload 7)) 
                                      (115 (aconst_null)) 
                                      (116 (astore 7)) 
                                      (118 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (123 (goto 146)) ;;to TAG_4
                                      (126 (pop)) ;;at TAG_2
                                      (127 (getstatic (fieldCP "const__6" "clojure.pprint$boolean_conditional" (class "clojure.lang.Var")))) ;;at TAG_3
                                      (130 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (133 (checkcast (class "clojure.lang.IFn"))) 
                                      (136 (aload 7)) 
                                      (138 (aconst_null)) 
                                      (139 (astore 7)) 
                                      (141 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (146 (astore 8)) ;;at TAG_4
                                      (148 (aload 8)) 
                                      (150 (dup)) 
                                      (151 (ifnull 230)) ;;to TAG_5
                                      (154 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (157 (if_acmpeq 231)) ;;to TAG_6
                                      (160 (getstatic (fieldCP "const__7" "clojure.pprint$boolean_conditional" (class "clojure.lang.Var")))) 
                                      (163 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (166 (checkcast (class "clojure.lang.IFn"))) 
                                      (169 (aload 8)) 
                                      (171 (aconst_null)) 
                                      (172 (astore 8)) 
                                      (174 (aload 6)) 
                                      (176 (aconst_null)) 
                                      (177 (astore 6)) 
                                      (179 (getstatic (fieldCP "__thunk__1__" "clojure.pprint$boolean_conditional" (class "clojure.lang.ILookupThunk")))) 
                                      (182 (dup)) 
                                      (183 (aload_1)) 
                                      (184 (aconst_null)) 
                                      (185 (astore_1)) 
                                      (186 (dup_x2)) 
                                      (187 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (192 (dup_x2)) 
                                      (193 (if_acmpeq 200)) ;;to TAG_7
                                      (196 (pop)) 
                                      (197 (goto 222)) ;;to TAG_8
                                      (200 (swap)) ;;at TAG_7
                                      (201 (pop)) 
                                      (202 (dup)) 
                                      (203 (getstatic (fieldCP "__site__1__" "clojure.pprint$boolean_conditional" (class "clojure.lang.KeywordLookupSite")))) 
                                      (206 (swap)) 
                                      (207 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (212 (dup)) 
                                      (213 (putstatic (fieldCP "__thunk__1__" "clojure.pprint$boolean_conditional" (class "clojure.lang.ILookupThunk")))) 
                                      (216 (swap)) 
                                      (217 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (222 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) ;;at TAG_8
                                      (227 (goto 236)) ;;to TAG_9
                                      (230 (pop)) ;;at TAG_5
                                      (231 (aload 6)) ;;at TAG_6
                                      (233 (aconst_null)) 
                                      (234 (astore 6)) 
                                      (236 (areturn)) ;;at TAG_9
                                      (endofcode 237))
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
                                      (25 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$boolean_conditional" (class "clojure.lang.ILookupThunk")))) 
                                      (28 (goto 38)) ;;to TAG_0
                                      (31 (aload_2)) ;;at TAG_2
                                      (32 (putstatic (fieldCP "__thunk__1__" "clojure.pprint$boolean_conditional" (class "clojure.lang.ILookupThunk")))) 
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


(defconst *pprint$boolean_conditional-class-table*
  (make-static-class-decls 
   *clojure.pprint$boolean_conditional*))

(defconst *package-name-map* 
  ("clojure.pprint$boolean_conditional" . "clojure"))

