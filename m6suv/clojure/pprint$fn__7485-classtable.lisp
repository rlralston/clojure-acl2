; pprint$fn__7485-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:55 CDT 2014.
;

(defconst *clojure.pprint$fn__7485*
 (make-class-def
      '(class "clojure.pprint$fn__7485"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "str")
                        (STRING  "data")
                        (STRING  "trailing-white-space"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
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
                                   (max_stack . 4) (max_locals . 0) (code_length . 86)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "str"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.pprint$fn__7485" (class "clojure.lang.Var"))))
                                      (13 (bipush 34))
                                      (15 (invokestatic
					(methodCP "valueOf" "java.lang.Character" (char) (class "java.lang.Character"))))
                                      (18 (putstatic (fieldCP "const__1" "clojure.pprint$fn__7485" (class "java.lang.Object"))))
                                      (21 (aconst_null))
                                      (22 (ldc 2))        ;;STRING:: "data"
                                      (24 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (27 (checkcast (class "clojure.lang.Keyword")))
                                      (30 (putstatic (fieldCP "const__2" "clojure.pprint$fn__7485" (class "clojure.lang.Keyword"))))
                                      (33 (aconst_null))
                                      (34 (ldc 3))        ;;STRING:: "trailing-white-space"
                                      (36 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (39 (checkcast (class "clojure.lang.Keyword")))
                                      (42 (putstatic (fieldCP "const__3" "clojure.pprint$fn__7485" (class "clojure.lang.Keyword"))))
                                      (45 (new (class "clojure.lang.KeywordLookupSite")))
                                      (48 (dup))
                                      (49 (aconst_null))
                                      (50 (ldc 2))        ;;STRING:: "data"
                                      (52 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (55 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (58 (dup))
                                      (59 (putstatic (fieldCP "__site__0__" "clojure.pprint$fn__7485" (class "clojure.lang.KeywordLookupSite"))))
                                      (62 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$fn__7485" (class "clojure.lang.ILookupThunk"))))
                                      (65 (new (class "clojure.lang.KeywordLookupSite")))
                                      (68 (dup))
                                      (69 (aconst_null))
                                      (70 (ldc 3))        ;;STRING:: "trailing-white-space"
                                      (72 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (75 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (78 (dup))
                                      (79 (putstatic (fieldCP "__site__1__" "clojure.pprint$fn__7485" (class "clojure.lang.KeywordLookupSite"))))
                                      (82 (putstatic (fieldCP "__thunk__1__" "clojure.pprint$fn__7485" (class "clojure.lang.ILookupThunk"))))
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
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 7) (max_locals . 2) (code_length . 105)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.pprint$fn__7485" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (getstatic (fieldCP "const__1" "clojure.pprint$fn__7485" (class "java.lang.Object")))) 
                                      (12 (getstatic (fieldCP "__thunk__0__" "clojure.pprint$fn__7485" (class "clojure.lang.ILookupThunk")))) 
                                      (15 (dup)) 
                                      (16 (aload_1)) 
                                      (17 (dup_x2)) 
                                      (18 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (23 (dup_x2)) 
                                      (24 (if_acmpeq 31)) ;;to TAG_0
                                      (27 (pop)) 
                                      (28 (goto 53)) ;;to TAG_1
                                      (31 (swap)) ;;at TAG_0
                                      (32 (pop)) 
                                      (33 (dup)) 
                                      (34 (getstatic (fieldCP "__site__0__" "clojure.pprint$fn__7485" (class "clojure.lang.KeywordLookupSite")))) 
                                      (37 (swap)) 
                                      (38 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (43 (dup)) 
                                      (44 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$fn__7485" (class "clojure.lang.ILookupThunk")))) 
                                      (47 (swap)) 
                                      (48 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (53 (getstatic (fieldCP "__thunk__1__" "clojure.pprint$fn__7485" (class "clojure.lang.ILookupThunk")))) ;;at TAG_1
                                      (56 (dup)) 
                                      (57 (aload_1)) 
                                      (58 (aconst_null)) 
                                      (59 (astore_1)) 
                                      (60 (dup_x2)) 
                                      (61 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (66 (dup_x2)) 
                                      (67 (if_acmpeq 74))  ;;to TAG_2
                                      (70 (pop)) 
                                      (71 (goto 96)) ;;to TAG_3
                                      (74 (swap)) ;;at TAG_2
                                      (75 (pop)) 
                                      (76 (dup)) 
                                      (77 (getstatic (fieldCP "__site__1__" "clojure.pprint$fn__7485" (class "clojure.lang.KeywordLookupSite")))) 
                                      (80 (swap)) 
                                      (81 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (86 (dup)) 
                                      (87 (putstatic (fieldCP "__thunk__1__" "clojure.pprint$fn__7485" (class "clojure.lang.ILookupThunk")))) 
                                      (90 (swap)) 
                                      (91 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (96 (getstatic (fieldCP "const__1" "clojure.pprint$fn__7485" (class "java.lang.Object")))) ;;at TAG_3
                                      (99 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 5)) 
                                      (104 (areturn)) 
                                      (endofcode 105))
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
                                      (25 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$fn__7485" (class "clojure.lang.ILookupThunk")))) 
                                      (28 (goto 38)) ;;to TAG_0
                                      (31 (aload_2)) ;;at TAG_2
                                      (32 (putstatic (fieldCP "__thunk__1__" "clojure.pprint$fn__7485" (class "clojure.lang.ILookupThunk")))) 
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


(defconst *pprint$fn__7485-class-table*
  (make-static-class-decls 
   *clojure.pprint$fn__7485*))

(defconst *package-name-map* 
  ("clojure.pprint$fn__7485" . "clojure"))

