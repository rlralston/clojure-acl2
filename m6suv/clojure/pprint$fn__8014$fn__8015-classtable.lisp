; pprint$fn__8014$fn__8015-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:55 CDT 2014.
;

(defconst *clojure.pprint$fn__8014$fn__8015*
 (make-class-def
      '(class "clojure.pprint$fn__8014$fn__8015"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.pprint")
                        (STRING  "get-format-arg")
                        (STRING  "clojure.core")
                        (STRING  "nth")
                        (STRING  "execute-sub-format")
                        (STRING  "base-args"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "__site__0__" (class "clojure.lang.KeywordLookupSite") (accessflags  *class*  *final*  *static* ) -1)
                        (field "__thunk__0__" (class "clojure.lang.ILookupThunk") (accessflags  *class*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 0) (code_length . 86)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.pprint"
                                      (2 (ldc 1))         ;;STRING:: "get-format-arg"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.pprint$fn__8014$fn__8015" (class "clojure.lang.Var"))))
                                      (13 (ldc 2))        ;;STRING:: "clojure.core"
                                      (15 (ldc 3))        ;;STRING:: "nth"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.pprint$fn__8014$fn__8015" (class "clojure.lang.Var"))))
                                      (26 (lconst_0))
                                      (27 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (30 (putstatic (fieldCP "const__2" "clojure.pprint$fn__8014$fn__8015" (class "java.lang.Object"))))
                                      (33 (lconst_1))
                                      (34 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (37 (putstatic (fieldCP "const__3" "clojure.pprint$fn__8014$fn__8015" (class "java.lang.Object"))))
                                      (40 (ldc 0))        ;;STRING:: "clojure.pprint"
                                      (42 (ldc 4))        ;;STRING:: "execute-sub-format"
                                      (44 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (47 (checkcast (class "clojure.lang.Var")))
                                      (50 (putstatic (fieldCP "const__4" "clojure.pprint$fn__8014$fn__8015" (class "clojure.lang.Var"))))
                                      (53 (aconst_null))
                                      (54 (ldc 5))        ;;STRING:: "base-args"
                                      (56 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (59 (checkcast (class "clojure.lang.Keyword")))
                                      (62 (putstatic (fieldCP "const__5" "clojure.pprint$fn__8014$fn__8015" (class "clojure.lang.Keyword"))))
                                      (65 (new (class "clojure.lang.KeywordLookupSite")))
                                      (68 (dup))
                                      (69 (aconst_null))
                                      (70 (ldc 5))        ;;STRING:: "base-args"
                                      (72 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (75 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (78 (dup))
                                      (79 (putstatic (fieldCP "__site__0__" "clojure.pprint$fn__8014$fn__8015" (class "clojure.lang.KeywordLookupSite"))))
                                      (82 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$fn__8014$fn__8015" (class "clojure.lang.ILookupThunk"))))
                                      (85 (return))
                                      (endofcode 86))
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
                                   (max_stack . 7) (max_locals . 7) (code_length . 114)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.pprint$fn__8014$fn__8015" (class "clojure.lang.Var")))) 
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
                                      (46 (getstatic (fieldCP "const__4" "clojure.pprint$fn__8014$fn__8015" (class "clojure.lang.Var")))) 
                                      (49 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (52 (checkcast (class "clojure.lang.IFn"))) 
                                      (55 (aload 5)) 
                                      (57 (aconst_null)) 
                                      (58 (astore 5)) 
                                      (60 (aload 6)) 
                                      (62 (aconst_null)) 
                                      (63 (astore 6)) 
                                      (65 (getstatic (fieldCP "__thunk__0__" "clojure.pprint$fn__8014$fn__8015" (class "clojure.lang.ILookupThunk")))) 
                                      (68 (dup)) 
                                      (69 (aload_1)) 
                                      (70 (aconst_null)) 
                                      (71 (astore_1)) 
                                      (72 (dup_x2)) 
                                      (73 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (78 (dup_x2)) 
                                      (79 (if_acmpeq 86))  ;;to TAG_0
                                      (82 (pop)) 
                                      (83 (goto 108)) ;;to TAG_1
                                      (86 (swap)) ;;at TAG_0
                                      (87 (pop)) 
                                      (88 (dup)) 
                                      (89 (getstatic (fieldCP "__site__0__" "clojure.pprint$fn__8014$fn__8015" (class "clojure.lang.KeywordLookupSite")))) 
                                      (92 (swap)) 
                                      (93 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (98 (dup)) 
                                      (99 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$fn__8014$fn__8015" (class "clojure.lang.ILookupThunk")))) 
                                      (102 (swap)) 
                                      (103 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (108 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) ;;at TAG_1
                                      (113 (areturn)) 
                                      (endofcode 114))
                                   (Exceptions )
                                   (StackMap )))
                        (method "swapThunk"
                              (parameters int (class "clojure.lang.ILookupThunk"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 3) (code_length . 28)
                                   (parsedcode
                                      (0 (iload_1)) 
                                      (1 (tableswitch (tableswitchinfo 27 (0 . 0) (20))))  ;;to TAG_0;;to TAG_1
                                      (20 (aload_2)) ;;at TAG_1
                                      (21 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$fn__8014$fn__8015" (class "clojure.lang.ILookupThunk")))) 
                                      (24 (goto 27))  ;;to TAG_0
                                      (27 (return)) ;;at TAG_0
                                      (endofcode 28))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *pprint$fn__8014$fn__8015-class-table*
  (make-static-class-decls 
   *clojure.pprint$fn__8014$fn__8015*))

(defconst *package-name-map* 
  ("clojure.pprint$fn__8014$fn__8015" . "clojure"))
