; pprint$get_miser_width-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:56 CDT 2014.
;

(defconst *clojure.pprint$get_miser_width*
 (make-class-def
      '(class "clojure.pprint$get_miser_width"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "miser-width")
                        (STRING  "clojure.core")
                        (STRING  "deref"))
            (fields
                        (field "const__0" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
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
                                      (0 (aconst_null))
                                      (1 (ldc 0))         ;;STRING:: "miser-width"
                                      (3 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (6 (checkcast (class "clojure.lang.Keyword")))
                                      (9 (putstatic (fieldCP "const__0" "clojure.pprint$get_miser_width" (class "clojure.lang.Keyword"))))
                                      (12 (ldc 1))        ;;STRING:: "clojure.core"
                                      (14 (ldc 2))        ;;STRING:: "deref"
                                      (16 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (19 (checkcast (class "clojure.lang.Var")))
                                      (22 (putstatic (fieldCP "const__1" "clojure.pprint$get_miser_width" (class "clojure.lang.Var"))))
                                      (25 (new (class "clojure.lang.KeywordLookupSite")))
                                      (28 (dup))
                                      (29 (aconst_null))
                                      (30 (ldc 0))        ;;STRING:: "miser-width"
                                      (32 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (35 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (38 (dup))
                                      (39 (putstatic (fieldCP "__site__0__" "clojure.pprint$get_miser_width" (class "clojure.lang.KeywordLookupSite"))))
                                      (42 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$get_miser_width" (class "clojure.lang.ILookupThunk"))))
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
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 2) (code_length . 72)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "__thunk__0__" "clojure.pprint$get_miser_width" (class "clojure.lang.ILookupThunk")))) 
                                      (3 (dup)) 
                                      (4 (getstatic (fieldCP "const__1" "clojure.pprint$get_miser_width" (class "clojure.lang.Var")))) 
                                      (7 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (10 (checkcast (class "clojure.lang.IFn"))) 
                                      (13 (getstatic (fieldCP "const__1" "clojure.pprint$get_miser_width" (class "clojure.lang.Var")))) 
                                      (16 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (19 (checkcast (class "clojure.lang.IFn"))) 
                                      (22 (aload_1)) 
                                      (23 (aconst_null)) 
                                      (24 (astore_1)) 
                                      (25 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (30 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (35 (dup_x2)) 
                                      (36 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (41 (dup_x2)) 
                                      (42 (if_acmpeq 49))  ;;to TAG_0
                                      (45 (pop)) 
                                      (46 (goto 71)) ;;to TAG_1
                                      (49 (swap)) ;;at TAG_0
                                      (50 (pop)) 
                                      (51 (dup)) 
                                      (52 (getstatic (fieldCP "__site__0__" "clojure.pprint$get_miser_width" (class "clojure.lang.KeywordLookupSite")))) 
                                      (55 (swap)) 
                                      (56 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (61 (dup)) 
                                      (62 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$get_miser_width" (class "clojure.lang.ILookupThunk")))) 
                                      (65 (swap)) 
                                      (66 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (71 (areturn)) ;;at TAG_1
                                      (endofcode 72))
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
                                      (21 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$get_miser_width" (class "clojure.lang.ILookupThunk")))) 
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


(defconst *pprint$get_miser_width-class-table*
  (make-static-class-decls 
   *clojure.pprint$get_miser_width*))

(defconst *package-name-map* 
  ("clojure.pprint$get_miser_width" . "clojure"))
