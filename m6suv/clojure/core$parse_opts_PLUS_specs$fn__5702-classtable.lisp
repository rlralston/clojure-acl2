; core$parse_opts_PLUS_specs$fn__5702-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:44 CDT 2014.
;

(defconst *clojure.core$parse_opts_PLUS_specs$fn__5702*
 (make-class-def
      '(class "clojure.core$parse_opts_PLUS_specs$fn__5702"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "var?")
                        (STRING  "resolve")
                        (STRING  "on")
                        (STRING  "deref"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
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
                                      (2 (ldc 1))         ;;STRING:: "var?"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$parse_opts_PLUS_specs$fn__5702" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "resolve"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$parse_opts_PLUS_specs$fn__5702" (class "clojure.lang.Var"))))
                                      (26 (aconst_null))
                                      (27 (ldc 3))        ;;STRING:: "on"
                                      (29 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (32 (checkcast (class "clojure.lang.Keyword")))
                                      (35 (putstatic (fieldCP "const__2" "clojure.core$parse_opts_PLUS_specs$fn__5702" (class "clojure.lang.Keyword"))))
                                      (38 (ldc 0))        ;;STRING:: "clojure.core"
                                      (40 (ldc 4))        ;;STRING:: "deref"
                                      (42 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (45 (checkcast (class "clojure.lang.Var")))
                                      (48 (putstatic (fieldCP "const__3" "clojure.core$parse_opts_PLUS_specs$fn__5702" (class "clojure.lang.Var"))))
                                      (51 (new (class "clojure.lang.KeywordLookupSite")))
                                      (54 (dup))
                                      (55 (aconst_null))
                                      (56 (ldc 3))        ;;STRING:: "on"
                                      (58 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (61 (invokespecial
					(methodCP "<init>" "clojure.lang.KeywordLookupSite" ((class "clojure.lang.Keyword")) void)))
                                      (64 (dup))
                                      (65 (putstatic (fieldCP "__site__0__" "clojure.core$parse_opts_PLUS_specs$fn__5702" (class "clojure.lang.KeywordLookupSite"))))
                                      (68 (putstatic (fieldCP "__thunk__0__" "clojure.core$parse_opts_PLUS_specs$fn__5702" (class "clojure.lang.ILookupThunk"))))
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
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 2) (code_length . 118)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$parse_opts_PLUS_specs$fn__5702" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (getstatic (fieldCP "const__1" "clojure.core$parse_opts_PLUS_specs$fn__5702" (class "clojure.lang.Var")))) 
                                      (12 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (15 (checkcast (class "clojure.lang.IFn"))) 
                                      (18 (aload_1)) 
                                      (19 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (24 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (29 (dup)) 
                                      (30 (ifnull 113)) ;;to TAG_0
                                      (33 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (36 (if_acmpeq 114)) ;;to TAG_1
                                      (39 (getstatic (fieldCP "__thunk__0__" "clojure.core$parse_opts_PLUS_specs$fn__5702" (class "clojure.lang.ILookupThunk")))) 
                                      (42 (dup)) 
                                      (43 (getstatic (fieldCP "const__3" "clojure.core$parse_opts_PLUS_specs$fn__5702" (class "clojure.lang.Var")))) 
                                      (46 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (49 (checkcast (class "clojure.lang.IFn"))) 
                                      (52 (getstatic (fieldCP "const__1" "clojure.core$parse_opts_PLUS_specs$fn__5702" (class "clojure.lang.Var")))) 
                                      (55 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (58 (checkcast (class "clojure.lang.IFn"))) 
                                      (61 (aload_1)) 
                                      (62 (aconst_null)) 
                                      (63 (astore_1)) 
                                      (64 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (69 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (74 (dup_x2)) 
                                      (75 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (80 (dup_x2)) 
                                      (81 (if_acmpeq 88))  ;;to TAG_2
                                      (84 (pop)) 
                                      (85 (goto 110)) ;;to TAG_3
                                      (88 (swap)) ;;at TAG_2
                                      (89 (pop)) 
                                      (90 (dup)) 
                                      (91 (getstatic (fieldCP "__site__0__" "clojure.core$parse_opts_PLUS_specs$fn__5702" (class "clojure.lang.KeywordLookupSite")))) 
                                      (94 (swap)) 
                                      (95 (invokeinterface (methodCP "fault" "clojure.lang.ILookupSite" ((class "java.lang.Object")) (class "clojure.lang.ILookupThunk")) 2)) 
                                      (100 (dup)) 
                                      (101 (putstatic (fieldCP "__thunk__0__" "clojure.core$parse_opts_PLUS_specs$fn__5702" (class "clojure.lang.ILookupThunk")))) 
                                      (104 (swap)) 
                                      (105 (invokeinterface (methodCP "get" "clojure.lang.ILookupThunk" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (110 (goto 117)) ;;to TAG_4;;at TAG_3
                                      (113 (pop)) ;;at TAG_0
                                      (114 (aload_1)) ;;at TAG_1
                                      (115 (aconst_null)) 
                                      (116 (astore_1)) 
                                      (117 (areturn)) ;;at TAG_4
                                      (endofcode 118))
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
                                      (21 (putstatic (fieldCP "__thunk__0__" "clojure.core$parse_opts_PLUS_specs$fn__5702" (class "clojure.lang.ILookupThunk")))) 
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


(defconst *core$parse_opts_PLUS_specs$fn__5702-class-table*
  (make-static-class-decls 
   *clojure.core$parse_opts_PLUS_specs$fn__5702*))

(defconst *package-name-map* 
  ("clojure.core$parse_opts_PLUS_specs$fn__5702" . "clojure"))

