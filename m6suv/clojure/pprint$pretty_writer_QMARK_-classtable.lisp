; pprint$pretty_writer_QMARK_-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:57 CDT 2014.
;

(defconst *clojure.pprint$pretty_writer_QMARK_*
 (make-class-def
      '(class "clojure.pprint$pretty_writer_QMARK_"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "instance?")
                        (STRING  "pretty-writer")
                        (STRING  "deref"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "__site__0__" (class "clojure.lang.KeywordLookupSite") (accessflags  *class*  *final*  *static* ) -1)
                        (field "__thunk__0__" (class "clojure.lang.ILookupThunk") (accessflags  *class*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 0) (code_length . 59)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "instance?"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.pprint$pretty_writer_QMARK_" (class "clojure.lang.Var"))))
                                      (13 (aconst_null))
                                      (14 (ldc 2))        ;;STRING:: "pretty-writer"
                                      (16 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (19 (checkcast (class "clojure.lang.Keyword")))
                                      (22 (putstatic (fieldCP "const__1" "clojure.pprint$pretty_writer_QMARK_" (class "clojure.lang.Keyword"))))
                                      (25 (ldc 0))        ;;STRING:: "clojure.core"
                                      (27 (ldc 3))        ;;STRING:: "deref"
                                      (29 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (32 (checkcast (class "clojure.lang.Var")))
                                      (35 (putstatic (fieldCP "const__2" "clojure.pprint$pretty_writer_QMARK_" (class "clojure.lang.Var"))))
                                      (38 (new (class "clojure.lang.KeywordLookupSite")))
                                      (41 (dup))
                                      (42 (aconst_null))
                                      (43 (ldc 2))        ;;STRING:: "pretty-writer"
                                      (45 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (48 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (51 (dup))
                                      (52 (putstatic (fieldCP "__site__0__" "clojure.pprint$pretty_writer_QMARK_" (class "clojure.lang.KeywordLookupSite"))))
                                      (55 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$pretty_writer_QMARK_" (class "clojure.lang.ILookupThunk"))))
                                      (58 (return))
                                      (endofcode 59))
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
                                   (max_stack . 6) (max_locals . 3) (code_length . 98)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (instanceof (class "clojure.lang.IDeref"))) 
                                      (4 (istore_2)) 
                                      (5 (iload_2)) 
                                      (6 (ifeq 84)) ;;to TAG_0
                                      (9 (getstatic (fieldCP "__thunk__0__" "clojure.pprint$pretty_writer_QMARK_" (class "clojure.lang.ILookupThunk")))) 
                                      (12 (dup)) 
                                      (13 (getstatic (fieldCP "const__2" "clojure.pprint$pretty_writer_QMARK_" (class "clojure.lang.Var")))) 
                                      (16 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (19 (checkcast (class "clojure.lang.IFn"))) 
                                      (22 (getstatic (fieldCP "const__2" "clojure.pprint$pretty_writer_QMARK_" (class "clojure.lang.Var")))) 
                                      (25 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (28 (checkcast (class "clojure.lang.IFn"))) 
                                      (31 (aload_1)) 
                                      (32 (aconst_null)) 
                                      (33 (astore_1)) 
                                      (34 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (39 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (44 (dup_x2)) 
                                      (45 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (50 (dup_x2)) 
                                      (51 (if_acmpeq 58)) ;;to TAG_1
                                      (54 (pop)) 
                                      (55 (goto 80))  ;;to TAG_2
                                      (58 (swap)) ;;at TAG_1
                                      (59 (pop)) 
                                      (60 (dup)) 
                                      (61 (getstatic (fieldCP "__site__0__" "clojure.pprint$pretty_writer_QMARK_" (class "clojure.lang.KeywordLookupSite")))) 
                                      (64 (swap)) 
                                      (65 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (70 (dup)) 
                                      (71 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$pretty_writer_QMARK_" (class "clojure.lang.ILookupThunk")))) 
                                      (74 (swap)) 
                                      (75 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (80 (goto 97)) ;;to TAG_3;;at TAG_2
                                      (83 (pop)) 
                                      (84 (iload_2)) ;;at TAG_0
                                      (85 (ifeq 94)) ;;to TAG_4
                                      (88 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (91 (goto 97)) ;;to TAG_3
                                      (94 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) ;;at TAG_4
                                      (97 (areturn)) ;;at TAG_3
                                      (endofcode 98))
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
                                      (21 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$pretty_writer_QMARK_" (class "clojure.lang.ILookupThunk")))) 
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


(defconst *pprint$pretty_writer_QMARK_-class-table*
  (make-static-class-decls 
   *clojure.pprint$pretty_writer_QMARK_*))

(defconst *package-name-map* 
  ("clojure.pprint$pretty_writer_QMARK_" . "clojure"))

