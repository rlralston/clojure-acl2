; core$emit_protocol$fn__5921-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:42 CDT 2014.
;

(defconst *clojure.core$emit_protocol$fn__5921*
 (make-class-def
      '(class "clojure.core$emit_protocol$fn__5921"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "keyword")
                        (STRING  "name")
                        (STRING  "on"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
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
                                   (max_stack . 4) (max_locals . 0) (code_length . 98)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "keyword"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$emit_protocol$fn__5921" (class "clojure.lang.Var"))))
                                      (13 (aconst_null))
                                      (14 (ldc 2))        ;;STRING:: "name"
                                      (16 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (19 (checkcast (class "clojure.lang.Keyword")))
                                      (22 (putstatic (fieldCP "const__1" "clojure.core$emit_protocol$fn__5921" (class "clojure.lang.Keyword"))))
                                      (25 (aconst_null))
                                      (26 (ldc 3))        ;;STRING:: "on"
                                      (28 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (31 (checkcast (class "clojure.lang.Keyword")))
                                      (34 (putstatic (fieldCP "const__2" "clojure.core$emit_protocol$fn__5921" (class "clojure.lang.Keyword"))))
                                      (37 (new (class "clojure.lang.KeywordLookupSite")))
                                      (40 (dup))
                                      (41 (aconst_null))
                                      (42 (ldc 2))        ;;STRING:: "name"
                                      (44 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (47 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (50 (dup))
                                      (51 (putstatic (fieldCP "__site__0__" "clojure.core$emit_protocol$fn__5921" (class "clojure.lang.KeywordLookupSite"))))
                                      (54 (putstatic (fieldCP "__thunk__0__" "clojure.core$emit_protocol$fn__5921" (class "clojure.lang.ILookupThunk"))))
                                      (57 (new (class "clojure.lang.KeywordLookupSite")))
                                      (60 (dup))
                                      (61 (aconst_null))
                                      (62 (ldc 3))        ;;STRING:: "on"
                                      (64 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (67 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (70 (dup))
                                      (71 (putstatic (fieldCP "__site__1__" "clojure.core$emit_protocol$fn__5921" (class "clojure.lang.KeywordLookupSite"))))
                                      (74 (putstatic (fieldCP "__thunk__1__" "clojure.core$emit_protocol$fn__5921" (class "clojure.lang.ILookupThunk"))))
                                      (77 (new (class "clojure.lang.KeywordLookupSite")))
                                      (80 (dup))
                                      (81 (aconst_null))
                                      (82 (ldc 2))        ;;STRING:: "name"
                                      (84 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (87 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (90 (dup))
                                      (91 (putstatic (fieldCP "__site__2__" "clojure.core$emit_protocol$fn__5921" (class "clojure.lang.KeywordLookupSite"))))
                                      (94 (putstatic (fieldCP "__thunk__2__" "clojure.core$emit_protocol$fn__5921" (class "clojure.lang.ILookupThunk"))))
                                      (97 (return))
                                      (endofcode 98))
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
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 8) (max_locals . 3) (code_length . 186)
                                   (parsedcode
                                      (0 (iconst_2)) 
                                      (1 (anewarray (class "java.lang.Object"))) 
                                      (4 (dup)) 
                                      (5 (iconst_0)) 
                                      (6 (getstatic (fieldCP "const__0" "clojure.core$emit_protocol$fn__5921" (class "clojure.lang.Var")))) 
                                      (9 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (12 (checkcast (class "clojure.lang.IFn"))) 
                                      (15 (getstatic (fieldCP "__thunk__0__" "clojure.core$emit_protocol$fn__5921" (class "clojure.lang.ILookupThunk")))) 
                                      (18 (dup)) 
                                      (19 (aload_1)) 
                                      (20 (dup_x2)) 
                                      (21 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (26 (dup_x2)) 
                                      (27 (if_acmpeq 34)) ;;to TAG_0
                                      (30 (pop)) 
                                      (31 (goto 56)) ;;to TAG_1
                                      (34 (swap)) ;;at TAG_0
                                      (35 (pop)) 
                                      (36 (dup)) 
                                      (37 (getstatic (fieldCP "__site__0__" "clojure.core$emit_protocol$fn__5921" (class "clojure.lang.KeywordLookupSite")))) 
                                      (40 (swap)) 
                                      (41 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (46 (dup)) 
                                      (47 (putstatic (fieldCP "__thunk__0__" "clojure.core$emit_protocol$fn__5921" (class "clojure.lang.ILookupThunk")))) 
                                      (50 (swap)) 
                                      (51 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (56 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) ;;at TAG_1
                                      (61 (aastore)) 
                                      (62 (dup)) 
                                      (63 (iconst_1)) 
                                      (64 (getstatic (fieldCP "const__0" "clojure.core$emit_protocol$fn__5921" (class "clojure.lang.Var")))) 
                                      (67 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (70 (checkcast (class "clojure.lang.IFn"))) 
                                      (73 (getstatic (fieldCP "__thunk__1__" "clojure.core$emit_protocol$fn__5921" (class "clojure.lang.ILookupThunk")))) 
                                      (76 (dup)) 
                                      (77 (aload_1)) 
                                      (78 (dup_x2)) 
                                      (79 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (84 (dup_x2)) 
                                      (85 (if_acmpeq 92))  ;;to TAG_2
                                      (88 (pop)) 
                                      (89 (goto 114)) ;;to TAG_3
                                      (92 (swap)) ;;at TAG_2
                                      (93 (pop)) 
                                      (94 (dup)) 
                                      (95 (getstatic (fieldCP "__site__1__" "clojure.core$emit_protocol$fn__5921" (class "clojure.lang.KeywordLookupSite")))) 
                                      (98 (swap)) 
                                      (99 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (104 (dup)) 
                                      (105 (putstatic (fieldCP "__thunk__1__" "clojure.core$emit_protocol$fn__5921" (class "clojure.lang.ILookupThunk")))) 
                                      (108 (swap)) 
                                      (109 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (114 (astore_2)) ;;at TAG_3
                                      (115 (aload_2)) 
                                      (116 (dup)) 
                                      (117 (ifnull 132)) ;;to TAG_4
                                      (120 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (123 (if_acmpeq 133)) ;;to TAG_5
                                      (126 (aload_2)) 
                                      (127 (aconst_null)) 
                                      (128 (astore_2)) 
                                      (129 (goto 176)) ;;to TAG_6
                                      (132 (pop)) ;;at TAG_4
                                      (133 (getstatic (fieldCP "__thunk__2__" "clojure.core$emit_protocol$fn__5921" (class "clojure.lang.ILookupThunk")))) ;;at TAG_5
                                      (136 (dup)) 
                                      (137 (aload_1)) 
                                      (138 (aconst_null)) 
                                      (139 (astore_1)) 
                                      (140 (dup_x2)) 
                                      (141 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (146 (dup_x2)) 
                                      (147 (if_acmpeq 154)) ;;to TAG_7
                                      (150 (pop)) 
                                      (151 (goto 176)) ;;to TAG_6
                                      (154 (swap)) ;;at TAG_7
                                      (155 (pop)) 
                                      (156 (dup)) 
                                      (157 (getstatic (fieldCP "__site__2__" "clojure.core$emit_protocol$fn__5921" (class "clojure.lang.KeywordLookupSite")))) 
                                      (160 (swap)) 
                                      (161 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (166 (dup)) 
                                      (167 (putstatic (fieldCP "__thunk__2__" "clojure.core$emit_protocol$fn__5921" (class "clojure.lang.ILookupThunk")))) 
                                      (170 (swap)) 
                                      (171 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (176 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) ;;at TAG_6
                                      (181 (aastore)) 
                                      (182 (invokestatic (methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector")))) 
                                      (185 (areturn)) 
                                      (endofcode 186))
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
                                      (29 (putstatic (fieldCP "__thunk__0__" "clojure.core$emit_protocol$fn__5921" (class "clojure.lang.ILookupThunk")))) 
                                      (32 (goto 49)) ;;to TAG_0
                                      (35 (aload_2)) ;;at TAG_2
                                      (36 (putstatic (fieldCP "__thunk__1__" "clojure.core$emit_protocol$fn__5921" (class "clojure.lang.ILookupThunk")))) 
                                      (39 (goto 49)) ;;to TAG_0
                                      (42 (aload_2)) ;;at TAG_3
                                      (43 (putstatic (fieldCP "__thunk__2__" "clojure.core$emit_protocol$fn__5921" (class "clojure.lang.ILookupThunk")))) 
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


(defconst *core$emit_protocol$fn__5921-class-table*
  (make-static-class-decls 
   *clojure.core$emit_protocol$fn__5921*))

(defconst *package-name-map* 
  ("clojure.core$emit_protocol$fn__5921" . "clojure"))
