; zip$make_node-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:59 CDT 2014.
;

(defconst *clojure.zip$make_node*
 (make-class-def
      '(class "clojure.zip$make_node"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "zip")
                        (STRING  "make-node")
                        (STRING  "clojure.core")
                        (STRING  "meta"))
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
                                   (max_stack . 4) (max_locals . 0) (code_length . 48)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "zip"
                                      (2 (ldc 1))         ;;STRING:: "make-node"
                                      (4 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (7 (checkcast (class "clojure.lang.Keyword")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.zip$make_node" (class "clojure.lang.Keyword"))))
                                      (13 (ldc 2))        ;;STRING:: "clojure.core"
                                      (15 (ldc 3))        ;;STRING:: "meta"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.zip$make_node" (class "clojure.lang.Var"))))
                                      (26 (new (class "clojure.lang.KeywordLookupSite")))
                                      (29 (dup))
                                      (30 (ldc 0))        ;;STRING:: "zip"
                                      (32 (ldc 1))        ;;STRING:: "make-node"
                                      (34 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (37 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (40 (dup))
                                      (41 (putstatic (fieldCP "__site__0__" "clojure.zip$make_node" (class "clojure.lang.KeywordLookupSite"))))
                                      (44 (putstatic (fieldCP "__thunk__0__" "clojure.zip$make_node" (class "clojure.lang.ILookupThunk"))))
                                      (47 (return))
                                      (endofcode 48))
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
                                   (max_stack . 5) (max_locals . 4) (code_length . 72)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "__thunk__0__" "clojure.zip$make_node" (class "clojure.lang.ILookupThunk")))) 
                                      (3 (dup)) 
                                      (4 (getstatic (fieldCP "const__1" "clojure.zip$make_node" (class "clojure.lang.Var")))) 
                                      (7 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (10 (checkcast (class "clojure.lang.IFn"))) 
                                      (13 (aload_1)) 
                                      (14 (aconst_null)) 
                                      (15 (astore_1)) 
                                      (16 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (21 (dup_x2)) 
                                      (22 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (27 (dup_x2)) 
                                      (28 (if_acmpeq 35))  ;;to TAG_0
                                      (31 (pop)) 
                                      (32 (goto 57)) ;;to TAG_1
                                      (35 (swap)) ;;at TAG_0
                                      (36 (pop)) 
                                      (37 (dup)) 
                                      (38 (getstatic (fieldCP "__site__0__" "clojure.zip$make_node" (class "clojure.lang.KeywordLookupSite")))) 
                                      (41 (swap)) 
                                      (42 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (47 (dup)) 
                                      (48 (putstatic (fieldCP "__thunk__0__" "clojure.zip$make_node" (class "clojure.lang.ILookupThunk")))) 
                                      (51 (swap)) 
                                      (52 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (57 (checkcast (class "clojure.lang.IFn"))) ;;at TAG_1
                                      (60 (aload_2)) 
                                      (61 (aconst_null)) 
                                      (62 (astore_2)) 
                                      (63 (aload_3)) 
                                      (64 (aconst_null)) 
                                      (65 (astore_3)) 
                                      (66 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (71 (areturn)) 
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
                                      (21 (putstatic (fieldCP "__thunk__0__" "clojure.zip$make_node" (class "clojure.lang.ILookupThunk")))) 
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


(defconst *zip$make_node-class-table*
  (make-static-class-decls 
   *clojure.zip$make_node*))

(defconst *package-name-map* 
  ("clojure.zip$make_node" . "clojure"))
