; pprint$linear_nl_QMARK_-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:56 CDT 2014.
;

(defconst *clojure.pprint$linear_nl_QMARK_*
 (make-class-def
      '(class "clojure.pprint$linear_nl_QMARK_"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "deref")
                        (STRING  "done-nl")
                        (STRING  "not")
                        (STRING  "clojure.pprint")
                        (STRING  "tokens-fit?"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "__site__0__" (class "clojure.lang.KeywordLookupSite") (accessflags  *class*  *final*  *static* ) -1)
                        (field "__thunk__0__" (class "clojure.lang.ILookupThunk") (accessflags  *class*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 0) (code_length . 72)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "deref"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.pprint$linear_nl_QMARK_" (class "clojure.lang.Var"))))
                                      (13 (aconst_null))
                                      (14 (ldc 2))        ;;STRING:: "done-nl"
                                      (16 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (19 (checkcast (class "clojure.lang.Keyword")))
                                      (22 (putstatic (fieldCP "const__1" "clojure.pprint$linear_nl_QMARK_" (class "clojure.lang.Keyword"))))
                                      (25 (ldc 0))        ;;STRING:: "clojure.core"
                                      (27 (ldc 3))        ;;STRING:: "not"
                                      (29 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (32 (checkcast (class "clojure.lang.Var")))
                                      (35 (putstatic (fieldCP "const__2" "clojure.pprint$linear_nl_QMARK_" (class "clojure.lang.Var"))))
                                      (38 (ldc 4))        ;;STRING:: "clojure.pprint"
                                      (40 (ldc 5))        ;;STRING:: "tokens-fit?"
                                      (42 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (45 (checkcast (class "clojure.lang.Var")))
                                      (48 (putstatic (fieldCP "const__3" "clojure.pprint$linear_nl_QMARK_" (class "clojure.lang.Var"))))
                                      (51 (new (class "clojure.lang.KeywordLookupSite")))
                                      (54 (dup))
                                      (55 (aconst_null))
                                      (56 (ldc 2))        ;;STRING:: "done-nl"
                                      (58 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (61 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (64 (dup))
                                      (65 (putstatic (fieldCP "__site__0__" "clojure.pprint$linear_nl_QMARK_" (class "clojure.lang.KeywordLookupSite"))))
                                      (68 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$linear_nl_QMARK_" (class "clojure.lang.ILookupThunk"))))
                                      (71 (return))
                                      (endofcode 72))
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
                                   (max_stack . 5) (max_locals . 5) (code_length . 115)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.pprint$linear_nl_QMARK_" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (getstatic (fieldCP "__thunk__0__" "clojure.pprint$linear_nl_QMARK_" (class "clojure.lang.ILookupThunk")))) 
                                      (12 (dup)) 
                                      (13 (aload_2)) 
                                      (14 (aconst_null)) 
                                      (15 (astore_2)) 
                                      (16 (dup_x2)) 
                                      (17 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (22 (dup_x2)) 
                                      (23 (if_acmpeq 30)) ;;to TAG_0
                                      (26 (pop)) 
                                      (27 (goto 52)) ;;to TAG_1
                                      (30 (swap)) ;;at TAG_0
                                      (31 (pop)) 
                                      (32 (dup)) 
                                      (33 (getstatic (fieldCP "__site__0__" "clojure.pprint$linear_nl_QMARK_" (class "clojure.lang.KeywordLookupSite")))) 
                                      (36 (swap)) 
                                      (37 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (42 (dup)) 
                                      (43 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$linear_nl_QMARK_" (class "clojure.lang.ILookupThunk")))) 
                                      (46 (swap)) 
                                      (47 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (52 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) ;;at TAG_1
                                      (57 (astore 4)) 
                                      (59 (aload 4)) 
                                      (61 (dup)) 
                                      (62 (ifnull 79))  ;;to TAG_2
                                      (65 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (68 (if_acmpeq 80)) ;;to TAG_3
                                      (71 (aload 4)) 
                                      (73 (aconst_null)) 
                                      (74 (astore 4)) 
                                      (76 (goto 114)) ;;to TAG_4
                                      (79 (pop)) ;;at TAG_2
                                      (80 (getstatic (fieldCP "const__2" "clojure.pprint$linear_nl_QMARK_" (class "clojure.lang.Var")))) ;;at TAG_3
                                      (83 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (86 (checkcast (class "clojure.lang.IFn"))) 
                                      (89 (getstatic (fieldCP "const__3" "clojure.pprint$linear_nl_QMARK_" (class "clojure.lang.Var")))) 
                                      (92 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (95 (checkcast (class "clojure.lang.IFn"))) 
                                      (98 (aload_1)) 
                                      (99 (aconst_null)) 
                                      (100 (astore_1)) 
                                      (101 (aload_3)) 
                                      (102 (aconst_null)) 
                                      (103 (astore_3)) 
                                      (104 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (109 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (114 (areturn)) ;;at TAG_4
                                      (endofcode 115))
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
                                      (21 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$linear_nl_QMARK_" (class "clojure.lang.ILookupThunk")))) 
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


(defconst *pprint$linear_nl_QMARK_-class-table*
  (make-static-class-decls 
   *clojure.pprint$linear_nl_QMARK_*))

(defconst *package-name-map* 
  ("clojure.pprint$linear_nl_QMARK_" . "clojure"))
