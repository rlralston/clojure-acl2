; core$print_meta-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:45 CDT 2014.
;

(defconst *clojure.core$print_meta*
 (make-class-def
      '(class "clojure.core$print_meta"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "meta")
                        (STRING  "pos?")
                        (STRING  "count")
                        (STRING  "*print-dup*")
                        (STRING  "*print-meta*")
                        (STRING  "*print-readably*")
                        (STRING  "=")
                        (STRING  "tag")
                        (STRING  "pr-on")
                        (STRING  "^")
                        (STRING  " "))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__9" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
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
                                   (max_stack . 4) (max_locals . 0) (code_length . 164)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "meta"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$print_meta" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "pos?"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$print_meta" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "count"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.core$print_meta" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 4))        ;;STRING:: "*print-dup*"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.core$print_meta" (class "clojure.lang.Var"))))
                                      (52 (ldc 0))        ;;STRING:: "clojure.core"
                                      (54 (ldc 5))        ;;STRING:: "*print-meta*"
                                      (56 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (59 (checkcast (class "clojure.lang.Var")))
                                      (62 (putstatic (fieldCP "const__4" "clojure.core$print_meta" (class "clojure.lang.Var"))))
                                      (65 (ldc 0))        ;;STRING:: "clojure.core"
                                      (67 (ldc 6))        ;;STRING:: "*print-readably*"
                                      (69 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (72 (checkcast (class "clojure.lang.Var")))
                                      (75 (putstatic (fieldCP "const__5" "clojure.core$print_meta" (class "clojure.lang.Var"))))
                                      (78 (ldc 0))        ;;STRING:: "clojure.core"
                                      (80 (ldc 7))        ;;STRING:: "="
                                      (82 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (85 (checkcast (class "clojure.lang.Var")))
                                      (88 (putstatic (fieldCP "const__6" "clojure.core$print_meta" (class "clojure.lang.Var"))))
                                      (91 (lconst_1))
                                      (92 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (95 (putstatic (fieldCP "const__7" "clojure.core$print_meta" (class "java.lang.Object"))))
                                      (98 (aconst_null))
                                      (99 (ldc 8))        ;;STRING:: "tag"
                                      (101 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (104 (checkcast (class "clojure.lang.Keyword")))
                                      (107 (putstatic (fieldCP "const__8" "clojure.core$print_meta" (class "clojure.lang.Keyword"))))
                                      (110 (ldc 0))       ;;STRING:: "clojure.core"
                                      (112 (ldc 9))       ;;STRING:: "pr-on"
                                      (114 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (117 (checkcast (class "clojure.lang.Var")))
                                      (120 (putstatic (fieldCP "const__9" "clojure.core$print_meta" (class "clojure.lang.Var"))))
                                      (123 (new (class "clojure.lang.KeywordLookupSite")))
                                      (126 (dup))
                                      (127 (aconst_null))
                                      (128 (ldc 8))       ;;STRING:: "tag"
                                      (130 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (133 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (136 (dup))
                                      (137 (putstatic (fieldCP "__site__0__" "clojure.core$print_meta" (class "clojure.lang.KeywordLookupSite"))))
                                      (140 (putstatic (fieldCP "__thunk__0__" "clojure.core$print_meta" (class "clojure.lang.ILookupThunk"))))
                                      (143 (new (class "clojure.lang.KeywordLookupSite")))
                                      (146 (dup))
                                      (147 (aconst_null))
                                      (148 (ldc 8))       ;;STRING:: "tag"
                                      (150 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (153 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (156 (dup))
                                      (157 (putstatic (fieldCP "__site__1__" "clojure.core$print_meta" (class "clojure.lang.KeywordLookupSite"))))
                                      (160 (putstatic (fieldCP "__thunk__1__" "clojure.core$print_meta" (class "clojure.lang.ILookupThunk"))))
                                      (163 (return))
                                      (endofcode 164))
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
                                   (max_stack . 5) (max_locals . 8) (code_length . 355)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$print_meta" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (aload_1)) 
                                      (10 (aconst_null)) 
                                      (11 (astore_1)) 
                                      (12 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (17 (astore_3)) 
                                      (18 (aload_3)) 
                                      (19 (dup)) 
                                      (20 (ifnull 352)) ;;to TAG_0
                                      (23 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (26 (if_acmpeq 353)) ;;to TAG_1
                                      (29 (aload_3)) 
                                      (30 (aconst_null)) 
                                      (31 (astore_3)) 
                                      (32 (astore 4)) 
                                      (34 (aload 4)) 
                                      (36 (invokestatic (methodCP "count" "clojure.lang.RT" ((class "java.lang.Object")) int))) 
                                      (39 (i2l)) 
                                      (40 (invokestatic (methodCP "isPos" "clojure.lang.Numbers" (long) boolean))) 
                                      (43 (istore 5)) 
                                      (45 (iload 5)) 
                                      (47 (ifeq 118)) ;;to TAG_2
                                      (50 (getstatic (fieldCP "const__3" "clojure.core$print_meta" (class "clojure.lang.Var")))) 
                                      (53 (invokevirtual (methodCP "get" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (56 (astore 6)) 
                                      (58 (aload 6)) 
                                      (60 (dup)) 
                                      (61 (ifnull 78)) ;;to TAG_3
                                      (64 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (67 (if_acmpeq 79)) ;;to TAG_4
                                      (70 (aload 6)) 
                                      (72 (aconst_null)) 
                                      (73 (astore 6)) 
                                      (75 (goto 114)) ;;to TAG_5
                                      (78 (pop)) ;;at TAG_3
                                      (79 (getstatic (fieldCP "const__4" "clojure.core$print_meta" (class "clojure.lang.Var")))) ;;at TAG_4
                                      (82 (invokevirtual (methodCP "get" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (85 (astore 7)) 
                                      (87 (aload 7)) 
                                      (89 (dup)) 
                                      (90 (ifnull 108)) ;;to TAG_6
                                      (93 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (96 (if_acmpeq 109)) ;;to TAG_7
                                      (99 (getstatic (fieldCP "const__5" "clojure.core$print_meta" (class "clojure.lang.Var")))) 
                                      (102 (invokevirtual (methodCP "get" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (105 (goto 114)) ;;to TAG_5
                                      (108 (pop)) ;;at TAG_6
                                      (109 (aload 7)) ;;at TAG_7
                                      (111 (aconst_null)) 
                                      (112 (astore 7)) 
                                      (114 (goto 132)) ;;to TAG_8;;at TAG_5
                                      (117 (pop)) 
                                      (118 (iload 5)) ;;at TAG_2
                                      (120 (ifeq 129)) ;;to TAG_9
                                      (123 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (126 (goto 132)) ;;to TAG_8
                                      (129 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) ;;at TAG_9
                                      (132 (dup)) ;;at TAG_8
                                      (133 (ifnull 347)) ;;to TAG_10
                                      (136 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (139 (if_acmpeq 348)) ;;to TAG_11
                                      (142 (aload_2)) 
                                      (143 (checkcast (class "java.io.Writer"))) 
                                      (146 (ldc 10)) ;;STRING:: "^"
                                      (148 (checkcast (class "java.lang.String"))) 
                                      (151 (invokevirtual (methodCP "write" "java.io.Writer" ((class "java.lang.String")) void))) 
                                      (154 (aconst_null)) 
                                      (155 (pop)) 
                                      (156 (aload 4)) 
                                      (158 (invokestatic (methodCP "count" "clojure.lang.RT" ((class "java.lang.Object")) int))) 
                                      (161 (i2l)) 
                                      (162 (lconst_1)) 
                                      (163 (invokestatic (methodCP "equiv" "clojure.lang.Util" (long long) boolean))) 
                                      (166 (istore 5)) 
                                      (168 (iload 5)) 
                                      (170 (ifeq 219)) ;;to TAG_12
                                      (173 (getstatic (fieldCP "__thunk__0__" "clojure.core$print_meta" (class "clojure.lang.ILookupThunk")))) 
                                      (176 (dup)) 
                                      (177 (aload 4)) 
                                      (179 (dup_x2)) 
                                      (180 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (185 (dup_x2)) 
                                      (186 (if_acmpeq 193))  ;;to TAG_13
                                      (189 (pop)) 
                                      (190 (goto 215)) ;;to TAG_14
                                      (193 (swap)) ;;at TAG_13
                                      (194 (pop)) 
                                      (195 (dup)) 
                                      (196 (getstatic (fieldCP "__site__0__" "clojure.core$print_meta" (class "clojure.lang.KeywordLookupSite")))) 
                                      (199 (swap)) 
                                      (200 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (205 (dup)) 
                                      (206 (putstatic (fieldCP "__thunk__0__" "clojure.core$print_meta" (class "clojure.lang.ILookupThunk")))) 
                                      (209 (swap)) 
                                      (210 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (215 (goto 233)) ;;to TAG_15;;at TAG_14
                                      (218 (pop)) 
                                      (219 (iload 5)) ;;at TAG_12
                                      (221 (ifeq 230)) ;;to TAG_16
                                      (224 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (227 (goto 233)) ;;to TAG_15
                                      (230 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) ;;at TAG_16
                                      (233 (dup)) ;;at TAG_15
                                      (234 (ifnull 307)) ;;to TAG_17
                                      (237 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (240 (if_acmpeq 308)) ;;to TAG_18
                                      (243 (getstatic (fieldCP "const__9" "clojure.core$print_meta" (class "clojure.lang.Var")))) 
                                      (246 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (249 (checkcast (class "clojure.lang.IFn"))) 
                                      (252 (getstatic (fieldCP "__thunk__1__" "clojure.core$print_meta" (class "clojure.lang.ILookupThunk")))) 
                                      (255 (dup)) 
                                      (256 (aload 4)) 
                                      (258 (aconst_null)) 
                                      (259 (astore 4)) 
                                      (261 (dup_x2)) 
                                      (262 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (267 (dup_x2)) 
                                      (268 (if_acmpeq 275)) ;;to TAG_19
                                      (271 (pop)) 
                                      (272 (goto 297)) ;;to TAG_20
                                      (275 (swap)) ;;at TAG_19
                                      (276 (pop)) 
                                      (277 (dup)) 
                                      (278 (getstatic (fieldCP "__site__1__" "clojure.core$print_meta" (class "clojure.lang.KeywordLookupSite")))) 
                                      (281 (swap)) 
                                      (282 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (287 (dup)) 
                                      (288 (putstatic (fieldCP "__thunk__1__" "clojure.core$print_meta" (class "clojure.lang.ILookupThunk")))) 
                                      (291 (swap)) 
                                      (292 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (297 (aload_2)) ;;at TAG_20
                                      (298 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (303 (pop)) 
                                      (304 (goto 329)) ;;to TAG_21
                                      (307 (pop)) ;;at TAG_17
                                      (308 (getstatic (fieldCP "const__9" "clojure.core$print_meta" (class "clojure.lang.Var")))) ;;at TAG_18
                                      (311 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (314 (checkcast (class "clojure.lang.IFn"))) 
                                      (317 (aload 4)) 
                                      (319 (aconst_null)) 
                                      (320 (astore 4)) 
                                      (322 (aload_2)) 
                                      (323 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (328 (pop)) 
                                      (329 (aload_2)) ;;at TAG_21
                                      (330 (aconst_null)) 
                                      (331 (astore_2)) 
                                      (332 (checkcast (class "java.io.Writer"))) 
                                      (335 (ldc 11)) ;;STRING:: " "
                                      (337 (checkcast (class "java.lang.String"))) 
                                      (340 (invokevirtual (methodCP "write" "java.io.Writer" ((class "java.lang.String")) void))) 
                                      (343 (aconst_null)) 
                                      (344 (goto 349)) ;;to TAG_22
                                      (347 (pop)) ;;at TAG_10
                                      (348 (aconst_null)) ;;at TAG_11
                                      (349 (goto 354)) ;;to TAG_23;;at TAG_22
                                      (352 (pop)) ;;at TAG_0
                                      (353 (aconst_null)) ;;at TAG_1
                                      (354 (areturn)) ;;at TAG_23
                                      (endofcode 355))
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
                                      (25 (putstatic (fieldCP "__thunk__0__" "clojure.core$print_meta" (class "clojure.lang.ILookupThunk")))) 
                                      (28 (goto 38)) ;;to TAG_0
                                      (31 (aload_2)) ;;at TAG_2
                                      (32 (putstatic (fieldCP "__thunk__1__" "clojure.core$print_meta" (class "clojure.lang.ILookupThunk")))) 
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


(defconst *core$print_meta-class-table*
  (make-static-class-decls 
   *clojure.core$print_meta*))

(defconst *package-name-map* 
  ("clojure.core$print_meta" . "clojure"))
