; pprint$pretty_writer$fn__7521-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:56 CDT 2014.
;

(defconst *clojure.pprint$pretty_writer$fn__7521*
 (make-class-def
      '(class "clojure.pprint$pretty_writer$fn__7521"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "=")
                        (STRING  "mode")
                        (STRING  "deref")
                        (STRING  "buffering")
                        (STRING  "clojure.pprint")
                        (STRING  "write-white-space"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "__site__0__" (class "clojure.lang.KeywordLookupSite") (accessflags  *class*  *final*  *static* ) -1)
                        (field "__thunk__0__" (class "clojure.lang.ILookupThunk") (accessflags  *class*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 0) (code_length . 84)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "="
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.pprint$pretty_writer$fn__7521" (class "clojure.lang.Var"))))
                                      (13 (aconst_null))
                                      (14 (ldc 2))        ;;STRING:: "mode"
                                      (16 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (19 (checkcast (class "clojure.lang.Keyword")))
                                      (22 (putstatic (fieldCP "const__1" "clojure.pprint$pretty_writer$fn__7521" (class "clojure.lang.Keyword"))))
                                      (25 (ldc 0))        ;;STRING:: "clojure.core"
                                      (27 (ldc 3))        ;;STRING:: "deref"
                                      (29 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (32 (checkcast (class "clojure.lang.Var")))
                                      (35 (putstatic (fieldCP "const__2" "clojure.pprint$pretty_writer$fn__7521" (class "clojure.lang.Var"))))
                                      (38 (aconst_null))
                                      (39 (ldc 4))        ;;STRING:: "buffering"
                                      (41 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (44 (checkcast (class "clojure.lang.Keyword")))
                                      (47 (putstatic (fieldCP "const__3" "clojure.pprint$pretty_writer$fn__7521" (class "clojure.lang.Keyword"))))
                                      (50 (ldc 5))        ;;STRING:: "clojure.pprint"
                                      (52 (ldc 6))        ;;STRING:: "write-white-space"
                                      (54 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (57 (checkcast (class "clojure.lang.Var")))
                                      (60 (putstatic (fieldCP "const__4" "clojure.pprint$pretty_writer$fn__7521" (class "clojure.lang.Var"))))
                                      (63 (new (class "clojure.lang.KeywordLookupSite")))
                                      (66 (dup))
                                      (67 (aconst_null))
                                      (68 (ldc 2))        ;;STRING:: "mode"
                                      (70 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (73 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (76 (dup))
                                      (77 (putstatic (fieldCP "__site__0__" "clojure.pprint$pretty_writer$fn__7521" (class "clojure.lang.KeywordLookupSite"))))
                                      (80 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$pretty_writer$fn__7521" (class "clojure.lang.ILookupThunk"))))
                                      (83 (return))
                                      (endofcode 84))
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
                                   (max_stack . 5) (max_locals . 2) (code_length . 116)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "__thunk__0__" "clojure.pprint$pretty_writer$fn__7521" (class "clojure.lang.ILookupThunk")))) 
                                      (3 (dup)) 
                                      (4 (getstatic (fieldCP "const__2" "clojure.pprint$pretty_writer$fn__7521" (class "clojure.lang.Var")))) 
                                      (7 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (10 (checkcast (class "clojure.lang.IFn"))) 
                                      (13 (getstatic (fieldCP "const__2" "clojure.pprint$pretty_writer$fn__7521" (class "clojure.lang.Var")))) 
                                      (16 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (19 (checkcast (class "clojure.lang.IFn"))) 
                                      (22 (aload_1)) 
                                      (23 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (28 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (33 (dup_x2)) 
                                      (34 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (39 (dup_x2)) 
                                      (40 (if_acmpeq 47)) ;;to TAG_0
                                      (43 (pop)) 
                                      (44 (goto 69)) ;;to TAG_1
                                      (47 (swap)) ;;at TAG_0
                                      (48 (pop)) 
                                      (49 (dup)) 
                                      (50 (getstatic (fieldCP "__site__0__" "clojure.pprint$pretty_writer$fn__7521" (class "clojure.lang.KeywordLookupSite")))) 
                                      (53 (swap)) 
                                      (54 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (59 (dup)) 
                                      (60 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$pretty_writer$fn__7521" (class "clojure.lang.ILookupThunk")))) 
                                      (63 (swap)) 
                                      (64 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (69 (getstatic (fieldCP "const__3" "clojure.pprint$pretty_writer$fn__7521" (class "clojure.lang.Keyword")))) ;;at TAG_1
                                      (72 (invokestatic (methodCP "equiv" "clojure.lang.Util" ((class "java.lang.Object") (class "java.lang.Object")) boolean))) 
                                      (75 (ifeq 98))  ;;to TAG_2
                                      (78 (new (class "clojure.pprint$pretty_writer$fn__7521$fn__7522"))) 
                                      (81 (dup)) 
                                      (82 (aload_1)) 
                                      (83 (aconst_null)) 
                                      (84 (astore_1)) 
                                      (85 (invokespecial (methodCP "<init>" "clojure.pprint$pretty_writer$fn__7521$fn__7522" ((class "java.lang.Object")) void))) 
                                      (88 (checkcast (class "java.util.concurrent.Callable"))) 
                                      (91 (invokestatic (methodCP "runInTransaction" "clojure.lang.LockingTransaction" ((class "java.util.concurrent.Callable")) (class "java.lang.Object")))) 
                                      (94 (goto 115)) ;;to TAG_3
                                      (97 (pop)) 
                                      (98 (getstatic (fieldCP "const__4" "clojure.pprint$pretty_writer$fn__7521" (class "clojure.lang.Var")))) ;;at TAG_2
                                      (101 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (104 (checkcast (class "clojure.lang.IFn"))) 
                                      (107 (aload_1)) 
                                      (108 (aconst_null)) 
                                      (109 (astore_1)) 
                                      (110 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (115 (areturn)) ;;at TAG_3
                                      (endofcode 116))
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
                                      (21 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$pretty_writer$fn__7521" (class "clojure.lang.ILookupThunk")))) 
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


(defconst *pprint$pretty_writer$fn__7521-class-table*
  (make-static-class-decls 
   *clojure.pprint$pretty_writer$fn__7521*))

(defconst *package-name-map* 
  ("clojure.pprint$pretty_writer$fn__7521" . "clojure"))

