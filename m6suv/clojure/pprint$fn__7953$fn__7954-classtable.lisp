; pprint$fn__7953$fn__7954-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:55 CDT 2014.
;

(defconst *clojure.pprint$fn__7953$fn__7954*
 (make-class-def
      '(class "clojure.pprint$fn__7953$fn__7954"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.pprint")
                        (STRING  "format-integer")
                        (STRING  "base"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "__site__0__" (class "clojure.lang.KeywordLookupSite") (accessflags  *class*  *final*  *static* ) -1)
                        (field "__thunk__0__" (class "clojure.lang.ILookupThunk") (accessflags  *class*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 0) (code_length . 46)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.pprint"
                                      (2 (ldc 1))         ;;STRING:: "format-integer"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.pprint$fn__7953$fn__7954" (class "clojure.lang.Var"))))
                                      (13 (aconst_null))
                                      (14 (ldc 2))        ;;STRING:: "base"
                                      (16 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (19 (checkcast (class "clojure.lang.Keyword")))
                                      (22 (putstatic (fieldCP "const__1" "clojure.pprint$fn__7953$fn__7954" (class "clojure.lang.Keyword"))))
                                      (25 (new (class "clojure.lang.KeywordLookupSite")))
                                      (28 (dup))
                                      (29 (aconst_null))
                                      (30 (ldc 2))        ;;STRING:: "base"
                                      (32 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (35 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (38 (dup))
                                      (39 (putstatic (fieldCP "__site__0__" "clojure.pprint$fn__7953$fn__7954" (class "clojure.lang.KeywordLookupSite"))))
                                      (42 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$fn__7953$fn__7954" (class "clojure.lang.ILookupThunk"))))
                                      (45 (return))
                                      (endofcode 46))
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
                                   (max_stack . 6) (max_locals . 4) (code_length . 65)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.pprint$fn__7953$fn__7954" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (getstatic (fieldCP "__thunk__0__" "clojure.pprint$fn__7953$fn__7954" (class "clojure.lang.ILookupThunk")))) 
                                      (12 (dup)) 
                                      (13 (aload_1)) 
                                      (14 (dup_x2)) 
                                      (15 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (20 (dup_x2)) 
                                      (21 (if_acmpeq 28))  ;;to TAG_0
                                      (24 (pop)) 
                                      (25 (goto 50)) ;;to TAG_1
                                      (28 (swap)) ;;at TAG_0
                                      (29 (pop)) 
                                      (30 (dup)) 
                                      (31 (getstatic (fieldCP "__site__0__" "clojure.pprint$fn__7953$fn__7954" (class "clojure.lang.KeywordLookupSite")))) 
                                      (34 (swap)) 
                                      (35 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (40 (dup)) 
                                      (41 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$fn__7953$fn__7954" (class "clojure.lang.ILookupThunk")))) 
                                      (44 (swap)) 
                                      (45 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (50 (aload_1)) ;;at TAG_1
                                      (51 (aconst_null)) 
                                      (52 (astore_1)) 
                                      (53 (aload_2)) 
                                      (54 (aconst_null)) 
                                      (55 (astore_2)) 
                                      (56 (aload_3)) 
                                      (57 (aconst_null)) 
                                      (58 (astore_3)) 
                                      (59 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 5)) 
                                      (64 (areturn)) 
                                      (endofcode 65))
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
                                      (21 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$fn__7953$fn__7954" (class "clojure.lang.ILookupThunk")))) 
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


(defconst *pprint$fn__7953$fn__7954-class-table*
  (make-static-class-decls 
   *clojure.pprint$fn__7953$fn__7954*))

(defconst *package-name-map* 
  ("clojure.pprint$fn__7953$fn__7954" . "clojure"))
