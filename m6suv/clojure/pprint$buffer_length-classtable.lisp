; pprint$buffer_length-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:55 CDT 2014.
;

(defconst *clojure.pprint$buffer_length*
 (make-class-def
      '(class "clojure.pprint$buffer_length"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "seq")
                        (STRING  "-")
                        (STRING  "end-pos")
                        (STRING  "last")
                        (STRING  "start-pos")
                        (STRING  "first"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
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
                                   (max_stack . 4) (max_locals . 0) (code_length . 124)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "seq"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.pprint$buffer_length" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "-"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.pprint$buffer_length" (class "clojure.lang.Var"))))
                                      (26 (aconst_null))
                                      (27 (ldc 3))        ;;STRING:: "end-pos"
                                      (29 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (32 (checkcast (class "clojure.lang.Keyword")))
                                      (35 (putstatic (fieldCP "const__2" "clojure.pprint$buffer_length" (class "clojure.lang.Keyword"))))
                                      (38 (ldc 0))        ;;STRING:: "clojure.core"
                                      (40 (ldc 4))        ;;STRING:: "last"
                                      (42 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (45 (checkcast (class "clojure.lang.Var")))
                                      (48 (putstatic (fieldCP "const__3" "clojure.pprint$buffer_length" (class "clojure.lang.Var"))))
                                      (51 (aconst_null))
                                      (52 (ldc 5))        ;;STRING:: "start-pos"
                                      (54 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (57 (checkcast (class "clojure.lang.Keyword")))
                                      (60 (putstatic (fieldCP "const__4" "clojure.pprint$buffer_length" (class "clojure.lang.Keyword"))))
                                      (63 (ldc 0))        ;;STRING:: "clojure.core"
                                      (65 (ldc 6))        ;;STRING:: "first"
                                      (67 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (70 (checkcast (class "clojure.lang.Var")))
                                      (73 (putstatic (fieldCP "const__5" "clojure.pprint$buffer_length" (class "clojure.lang.Var"))))
                                      (76 (lconst_0))
                                      (77 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (80 (putstatic (fieldCP "const__6" "clojure.pprint$buffer_length" (class "java.lang.Object"))))
                                      (83 (new (class "clojure.lang.KeywordLookupSite")))
                                      (86 (dup))
                                      (87 (aconst_null))
                                      (88 (ldc 3))        ;;STRING:: "end-pos"
                                      (90 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (93 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (96 (dup))
                                      (97 (putstatic (fieldCP "__site__0__" "clojure.pprint$buffer_length" (class "clojure.lang.KeywordLookupSite"))))
                                      (100 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$buffer_length" (class "clojure.lang.ILookupThunk"))))
                                      (103 (new (class "clojure.lang.KeywordLookupSite")))
                                      (106 (dup))
                                      (107 (aconst_null))
                                      (108 (ldc 5))       ;;STRING:: "start-pos"
                                      (110 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (113 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (116 (dup))
                                      (117 (putstatic (fieldCP "__site__1__" "clojure.pprint$buffer_length" (class "clojure.lang.KeywordLookupSite"))))
                                      (120 (putstatic (fieldCP "__thunk__1__" "clojure.pprint$buffer_length" (class "clojure.lang.ILookupThunk"))))
                                      (123 (return))
                                      (endofcode 124))
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
                                   (max_stack . 6) (max_locals . 3) (code_length . 152)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.pprint$buffer_length" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (aload_1)) 
                                      (10 (aconst_null)) 
                                      (11 (astore_1)) 
                                      (12 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (17 (astore_2)) 
                                      (18 (aload_2)) 
                                      (19 (dup)) 
                                      (20 (ifnull 147)) ;;to TAG_0
                                      (23 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (26 (if_acmpeq 148)) ;;to TAG_1
                                      (29 (getstatic (fieldCP "__thunk__0__" "clojure.pprint$buffer_length" (class "clojure.lang.ILookupThunk")))) 
                                      (32 (dup)) 
                                      (33 (getstatic (fieldCP "const__3" "clojure.pprint$buffer_length" (class "clojure.lang.Var")))) 
                                      (36 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (39 (checkcast (class "clojure.lang.IFn"))) 
                                      (42 (aload_2)) 
                                      (43 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (48 (dup_x2)) 
                                      (49 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (54 (dup_x2)) 
                                      (55 (if_acmpeq 62))  ;;to TAG_2
                                      (58 (pop)) 
                                      (59 (goto 84)) ;;to TAG_3
                                      (62 (swap)) ;;at TAG_2
                                      (63 (pop)) 
                                      (64 (dup)) 
                                      (65 (getstatic (fieldCP "__site__0__" "clojure.pprint$buffer_length" (class "clojure.lang.KeywordLookupSite")))) 
                                      (68 (swap)) 
                                      (69 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (74 (dup)) 
                                      (75 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$buffer_length" (class "clojure.lang.ILookupThunk")))) 
                                      (78 (swap)) 
                                      (79 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (84 (getstatic (fieldCP "__thunk__1__" "clojure.pprint$buffer_length" (class "clojure.lang.ILookupThunk")))) ;;at TAG_3
                                      (87 (dup)) 
                                      (88 (getstatic (fieldCP "const__5" "clojure.pprint$buffer_length" (class "clojure.lang.Var")))) 
                                      (91 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (94 (checkcast (class "clojure.lang.IFn"))) 
                                      (97 (aload_2)) 
                                      (98 (aconst_null)) 
                                      (99 (astore_2)) 
                                      (100 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (105 (dup_x2)) 
                                      (106 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (111 (dup_x2)) 
                                      (112 (if_acmpeq 119)) ;;to TAG_4
                                      (115 (pop)) 
                                      (116 (goto 141)) ;;to TAG_5
                                      (119 (swap)) ;;at TAG_4
                                      (120 (pop)) 
                                      (121 (dup)) 
                                      (122 (getstatic (fieldCP "__site__1__" "clojure.pprint$buffer_length" (class "clojure.lang.KeywordLookupSite")))) 
                                      (125 (swap)) 
                                      (126 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (131 (dup)) 
                                      (132 (putstatic (fieldCP "__thunk__1__" "clojure.pprint$buffer_length" (class "clojure.lang.ILookupThunk")))) 
                                      (135 (swap)) 
                                      (136 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (141 (invokestatic (methodCP "minus" "clojure.lang.Numbers" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Number")))) ;;at TAG_5
                                      (144 (goto 151)) ;;to TAG_6
                                      (147 (pop)) ;;at TAG_0
                                      (148 (getstatic (fieldCP "const__6" "clojure.pprint$buffer_length" (class "java.lang.Object")))) ;;at TAG_1
                                      (151 (areturn)) ;;at TAG_6
                                      (endofcode 152))
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
                                      (25 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$buffer_length" (class "clojure.lang.ILookupThunk")))) 
                                      (28 (goto 38)) ;;to TAG_0
                                      (31 (aload_2)) ;;at TAG_2
                                      (32 (putstatic (fieldCP "__thunk__1__" "clojure.pprint$buffer_length" (class "clojure.lang.ILookupThunk")))) 
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


(defconst *pprint$buffer_length-class-table*
  (make-static-class-decls 
   *clojure.pprint$buffer_length*))

(defconst *package-name-map* 
  ("clojure.pprint$buffer_length" . "clojure"))

