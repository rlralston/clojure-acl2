; zip$rights-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:59 CDT 2014.
;

(defconst *clojure.zip$rights*
 (make-class-def
      '(class "clojure.zip$rights"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "r"))
            (fields
                        (field "const__0" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "__site__0__" (class "clojure.lang.KeywordLookupSite") (accessflags  *class*  *final*  *static* ) -1)
                        (field "__thunk__0__" (class "clojure.lang.ILookupThunk") (accessflags  *class*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 0) (code_length . 40)
                                   (parsedcode
                                      (0 (aconst_null))
                                      (1 (ldc 0))         ;;STRING:: "r"
                                      (3 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (6 (checkcast (class "clojure.lang.Keyword")))
                                      (9 (putstatic (fieldCP "const__0" "clojure.zip$rights" (class "clojure.lang.Keyword"))))
                                      (12 (lconst_1))
                                      (13 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (16 (putstatic (fieldCP "const__1" "clojure.zip$rights" (class "java.lang.Object"))))
                                      (19 (new (class "clojure.lang.KeywordLookupSite")))
                                      (22 (dup))
                                      (23 (aconst_null))
                                      (24 (ldc 0))        ;;STRING:: "r"
                                      (26 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (29 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (32 (dup))
                                      (33 (putstatic (fieldCP "__site__0__" "clojure.zip$rights" (class "clojure.lang.KeywordLookupSite"))))
                                      (36 (putstatic (fieldCP "__thunk__0__" "clojure.zip$rights" (class "clojure.lang.ILookupThunk"))))
                                      (39 (return))
                                      (endofcode 40))
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
                                   (max_stack . 4) (max_locals . 2) (code_length . 55)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "__thunk__0__" "clojure.zip$rights" (class "clojure.lang.ILookupThunk")))) 
                                      (3 (dup)) 
                                      (4 (aload_1)) 
                                      (5 (aconst_null)) 
                                      (6 (astore_1)) 
                                      (7 (checkcast (class "clojure.lang.IFn"))) 
                                      (10 (getstatic (fieldCP "const__1" "clojure.zip$rights" (class "java.lang.Object")))) 
                                      (13 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (18 (dup_x2)) 
                                      (19 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (24 (dup_x2)) 
                                      (25 (if_acmpeq 32))  ;;to TAG_0
                                      (28 (pop)) 
                                      (29 (goto 54)) ;;to TAG_1
                                      (32 (swap)) ;;at TAG_0
                                      (33 (pop)) 
                                      (34 (dup)) 
                                      (35 (getstatic (fieldCP "__site__0__" "clojure.zip$rights" (class "clojure.lang.KeywordLookupSite")))) 
                                      (38 (swap)) 
                                      (39 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (44 (dup)) 
                                      (45 (putstatic (fieldCP "__thunk__0__" "clojure.zip$rights" (class "clojure.lang.ILookupThunk")))) 
                                      (48 (swap)) 
                                      (49 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (54 (areturn)) ;;at TAG_1
                                      (endofcode 55))
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
                                      (21 (putstatic (fieldCP "__thunk__0__" "clojure.zip$rights" (class "clojure.lang.ILookupThunk")))) 
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


(defconst *zip$rights-class-table*
  (make-static-class-decls 
   *clojure.zip$rights*))

(defconst *package-name-map* 
  ("clojure.zip$rights" . "clojure"))
