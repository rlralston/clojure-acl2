; pprint$start_block_t_QMARK_-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:57 CDT 2014.
;

(defconst *clojure.pprint$start_block_t_QMARK_*
 (make-class-def
      '(class "clojure.pprint$start_block_t_QMARK_"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "=")
                        (STRING  "type-tag")
                        (STRING  "start-block-t"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "__site__0__" (class "clojure.lang.KeywordLookupSite") (accessflags  *class*  *final*  *static* ) -1)
                        (field "__thunk__0__" (class "clojure.lang.ILookupThunk") (accessflags  *class*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 0) (code_length . 58)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "="
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.pprint$start_block_t_QMARK_" (class "clojure.lang.Var"))))
                                      (13 (aconst_null))
                                      (14 (ldc 2))        ;;STRING:: "type-tag"
                                      (16 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (19 (checkcast (class "clojure.lang.Keyword")))
                                      (22 (putstatic (fieldCP "const__1" "clojure.pprint$start_block_t_QMARK_" (class "clojure.lang.Keyword"))))
                                      (25 (aconst_null))
                                      (26 (ldc 3))        ;;STRING:: "start-block-t"
                                      (28 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (31 (checkcast (class "clojure.lang.Keyword")))
                                      (34 (putstatic (fieldCP "const__2" "clojure.pprint$start_block_t_QMARK_" (class "clojure.lang.Keyword"))))
                                      (37 (new (class "clojure.lang.KeywordLookupSite")))
                                      (40 (dup))
                                      (41 (aconst_null))
                                      (42 (ldc 2))        ;;STRING:: "type-tag"
                                      (44 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (47 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (50 (dup))
                                      (51 (putstatic (fieldCP "__site__0__" "clojure.pprint$start_block_t_QMARK_" (class "clojure.lang.KeywordLookupSite"))))
                                      (54 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$start_block_t_QMARK_" (class "clojure.lang.ILookupThunk"))))
                                      (57 (return))
                                      (endofcode 58))
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
                                   (max_stack . 4) (max_locals . 2) (code_length . 62)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "__thunk__0__" "clojure.pprint$start_block_t_QMARK_" (class "clojure.lang.ILookupThunk")))) 
                                      (3 (dup)) 
                                      (4 (aload_1)) 
                                      (5 (aconst_null)) 
                                      (6 (astore_1)) 
                                      (7 (dup_x2)) 
                                      (8 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (13 (dup_x2)) 
                                      (14 (if_acmpeq 21)) ;;to TAG_0
                                      (17 (pop)) 
                                      (18 (goto 43)) ;;to TAG_1
                                      (21 (swap)) ;;at TAG_0
                                      (22 (pop)) 
                                      (23 (dup)) 
                                      (24 (getstatic (fieldCP "__site__0__" "clojure.pprint$start_block_t_QMARK_" (class "clojure.lang.KeywordLookupSite")))) 
                                      (27 (swap)) 
                                      (28 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (33 (dup)) 
                                      (34 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$start_block_t_QMARK_" (class "clojure.lang.ILookupThunk")))) 
                                      (37 (swap)) 
                                      (38 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (43 (getstatic (fieldCP "const__2" "clojure.pprint$start_block_t_QMARK_" (class "clojure.lang.Keyword")))) ;;at TAG_1
                                      (46 (invokestatic (methodCP "equiv" "clojure.lang.Util" ((class "java.lang.Object") (class "java.lang.Object")) boolean))) 
                                      (49 (ifeq 58))  ;;to TAG_2
                                      (52 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (55 (goto 61)) ;;to TAG_3
                                      (58 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) ;;at TAG_2
                                      (61 (areturn)) ;;at TAG_3
                                      (endofcode 62))
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
                                      (21 (putstatic (fieldCP "__thunk__0__" "clojure.pprint$start_block_t_QMARK_" (class "clojure.lang.ILookupThunk")))) 
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


(defconst *pprint$start_block_t_QMARK_-class-table*
  (make-static-class-decls 
   *clojure.pprint$start_block_t_QMARK_*))

(defconst *package-name-map* 
  ("clojure.pprint$start_block_t_QMARK_" . "clojure"))

