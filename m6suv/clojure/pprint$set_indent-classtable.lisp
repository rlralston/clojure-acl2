; pprint$set_indent-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:57 CDT 2014.
;

(defconst *clojure.pprint$set_indent*
 (make-class-def
      '(class "clojure.pprint$set_indent"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "colon")
                        (STRING  "current")
                        (STRING  "block")
                        (STRING  "clojure.pprint")
                        (STRING  "pprint-indent")
                        (STRING  "n"))
            (fields
                        (field "const__0" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
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
                                   (max_stack . 4) (max_locals . 0) (code_length . 102)
                                   (parsedcode
                                      (0 (aconst_null))
                                      (1 (ldc 0))         ;;STRING:: "colon"
                                      (3 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (6 (checkcast (class "clojure.lang.Keyword")))
                                      (9 (putstatic (fieldCP "const__0" "clojure.pprint$set_indent" (class "clojure.lang.Keyword"))))
                                      (12 (aconst_null))
                                      (13 (ldc 1))        ;;STRING:: "current"
                                      (15 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (18 (checkcast (class "clojure.lang.Keyword")))
                                      (21 (putstatic (fieldCP "const__1" "clojure.pprint$set_indent" (class "clojure.lang.Keyword"))))
                                      (24 (aconst_null))
                                      (25 (ldc 2))        ;;STRING:: "block"
                                      (27 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (30 (checkcast (class "clojure.lang.Keyword")))
                                      (33 (putstatic (fieldCP "const__2" "clojure.pprint$set_indent" (class "clojure.lang.Keyword"))))
                                      (36 (ldc 3))        ;;STRING:: "clojure.pprint"
                                      (38 (ldc 4))        ;;STRING:: "pprint-indent"
                                      (40 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (43 (checkcast (class "clojure.lang.Var")))
                                      (46 (putstatic (fieldCP "const__3" "clojure.pprint$set_indent" (class "clojure.lang.Var"))))
                                      (49 (aconst_null))
                                      (50 (ldc 5))        ;;STRING:: "n"
                                      (52 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (55 (checkcast (class "clojure.lang.Keyword")))
                                      (58 (putstatic (fieldCP "const__4" "clojure.pprint$set_indent" (class "clojure.lang.Keyword"))))
                                      (61 (new (class "clojure.lang.KeywordLookupSite")))
                                      (64 (dup))
                                      (65 (aconst_null))
                                      (66 (ldc 0))        ;;STRING:: "colon"
                                      (68 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (71 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (74 (dup))
                                      (75 (putstatic (fieldCP "__site__0__" "clojure.pprint$set_indent" (class "clojure.lang.KeywordLookupSite"))))
                                      (78 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$set_indent" (class "clojure.lang.ILookupThunk"))))
                                      (81 (new (class "clojure.lang.KeywordLookupSite")))
                                      (84 (dup))
                                      (85 (aconst_null))
                                      (86 (ldc 5))        ;;STRING:: "n"
                                      (88 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (91 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (94 (dup))
                                      (95 (putstatic (fieldCP "__site__1__" "clojure.pprint$set_indent" (class "clojure.lang.KeywordLookupSite"))))
                                      (98 (putstatic (fieldCP "__thunk__1__" "clojure.pprint$set_indent" (class "clojure.lang.ILookupThunk"))))
                                      (101 (return))
                                      (endofcode 102))
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
                                   (max_stack . 6) (max_locals . 5) (code_length . 130)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "__thunk__0__" "clojure.pprint$set_indent" (class "clojure.lang.ILookupThunk")))) 
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
                                      (22 (getstatic (fieldCP "__site__0__" "clojure.pprint$set_indent" (class "clojure.lang.KeywordLookupSite")))) 
                                      (25 (swap)) 
                                      (26 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (31 (dup)) 
                                      (32 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$set_indent" (class "clojure.lang.ILookupThunk")))) 
                                      (35 (swap)) 
                                      (36 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (41 (dup)) ;;at TAG_1
                                      (42 (ifnull 57))  ;;to TAG_2
                                      (45 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (48 (if_acmpeq 58)) ;;to TAG_3
                                      (51 (getstatic (fieldCP "const__1" "clojure.pprint$set_indent" (class "clojure.lang.Keyword")))) 
                                      (54 (goto 61)) ;;to TAG_4
                                      (57 (pop)) ;;at TAG_2
                                      (58 (getstatic (fieldCP "const__2" "clojure.pprint$set_indent" (class "clojure.lang.Keyword")))) ;;at TAG_3
                                      (61 (astore 4)) ;;at TAG_4
                                      (63 (getstatic (fieldCP "const__3" "clojure.pprint$set_indent" (class "clojure.lang.Var")))) 
                                      (66 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (69 (checkcast (class "clojure.lang.IFn"))) 
                                      (72 (aload 4)) 
                                      (74 (aconst_null)) 
                                      (75 (astore 4)) 
                                      (77 (getstatic (fieldCP "__thunk__1__" "clojure.pprint$set_indent" (class "clojure.lang.ILookupThunk")))) 
                                      (80 (dup)) 
                                      (81 (aload_1)) 
                                      (82 (aconst_null)) 
                                      (83 (astore_1)) 
                                      (84 (dup_x2)) 
                                      (85 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (90 (dup_x2)) 
                                      (91 (if_acmpeq 98)) ;;to TAG_5
                                      (94 (pop)) 
                                      (95 (goto 120)) ;;to TAG_6
                                      (98 (swap)) ;;at TAG_5
                                      (99 (pop)) 
                                      (100 (dup)) 
                                      (101 (getstatic (fieldCP "__site__1__" "clojure.pprint$set_indent" (class "clojure.lang.KeywordLookupSite")))) 
                                      (104 (swap)) 
                                      (105 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (110 (dup)) 
                                      (111 (putstatic (fieldCP "__thunk__1__" "clojure.pprint$set_indent" (class "clojure.lang.ILookupThunk")))) 
                                      (114 (swap)) 
                                      (115 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (120 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) ;;at TAG_6
                                      (125 (pop)) 
                                      (126 (aload_2)) 
                                      (127 (aconst_null)) 
                                      (128 (astore_2)) 
                                      (129 (areturn)) 
                                      (endofcode 130))
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
                                      (25 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$set_indent" (class "clojure.lang.ILookupThunk")))) 
                                      (28 (goto 38)) ;;to TAG_0
                                      (31 (aload_2)) ;;at TAG_2
                                      (32 (putstatic (fieldCP "__thunk__1__" "clojure.pprint$set_indent" (class "clojure.lang.ILookupThunk")))) 
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


(defconst *pprint$set_indent-class-table*
  (make-static-class-decls 
   *clojure.pprint$set_indent*))

(defconst *package-name-map* 
  ("clojure.pprint$set_indent" . "clojure"))

