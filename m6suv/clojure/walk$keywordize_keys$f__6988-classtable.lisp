; walk$keywordize_keys$f__6988-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:59 CDT 2014.
;

(defconst *clojure.walk$keywordize_keys$f__6988*
 (make-class-def
      '(class "clojure.walk$keywordize_keys$f__6988"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "nth")
                        (STRING  "string?")
                        (STRING  "keyword"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 54)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "nth"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.walk$keywordize_keys$f__6988" (class "clojure.lang.Var"))))
                                      (13 (lconst_0))
                                      (14 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (17 (putstatic (fieldCP "const__1" "clojure.walk$keywordize_keys$f__6988" (class "java.lang.Object"))))
                                      (20 (lconst_1))
                                      (21 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (24 (putstatic (fieldCP "const__2" "clojure.walk$keywordize_keys$f__6988" (class "java.lang.Object"))))
                                      (27 (ldc 0))        ;;STRING:: "clojure.core"
                                      (29 (ldc 2))        ;;STRING:: "string?"
                                      (31 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (34 (checkcast (class "clojure.lang.Var")))
                                      (37 (putstatic (fieldCP "const__3" "clojure.walk$keywordize_keys$f__6988" (class "clojure.lang.Var"))))
                                      (40 (ldc 0))        ;;STRING:: "clojure.core"
                                      (42 (ldc 3))        ;;STRING:: "keyword"
                                      (44 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (47 (checkcast (class "clojure.lang.Var")))
                                      (50 (putstatic (fieldCP "const__4" "clojure.walk$keywordize_keys$f__6988" (class "clojure.lang.Var"))))
                                      (53 (return))
                                      (endofcode 54))
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
                                   (max_stack . 6) (max_locals . 5) (code_length . 113)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (aconst_null)) 
                                      (2 (astore_1)) 
                                      (3 (astore_2)) 
                                      (4 (aload_2)) 
                                      (5 (lconst_0)) 
                                      (6 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (9 (aconst_null)) 
                                      (10 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (13 (astore_3)) 
                                      (14 (aload_2)) 
                                      (15 (aconst_null)) 
                                      (16 (astore_2)) 
                                      (17 (lconst_1)) 
                                      (18 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (21 (aconst_null)) 
                                      (22 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (25 (astore 4)) 
                                      (27 (getstatic (fieldCP "const__3" "clojure.walk$keywordize_keys$f__6988" (class "clojure.lang.Var")))) 
                                      (30 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (33 (checkcast (class "clojure.lang.IFn"))) 
                                      (36 (aload_3)) 
                                      (37 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (42 (dup)) 
                                      (43 (ifnull 90)) ;;to TAG_0
                                      (46 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (49 (if_acmpeq 91)) ;;to TAG_1
                                      (52 (iconst_2)) 
                                      (53 (anewarray (class "java.lang.Object"))) 
                                      (56 (dup)) 
                                      (57 (iconst_0)) 
                                      (58 (getstatic (fieldCP "const__4" "clojure.walk$keywordize_keys$f__6988" (class "clojure.lang.Var")))) 
                                      (61 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (64 (checkcast (class "clojure.lang.IFn"))) 
                                      (67 (aload_3)) 
                                      (68 (aconst_null)) 
                                      (69 (astore_3)) 
                                      (70 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (75 (aastore)) 
                                      (76 (dup)) 
                                      (77 (iconst_1)) 
                                      (78 (aload 4)) 
                                      (80 (aconst_null)) 
                                      (81 (astore 4)) 
                                      (83 (aastore)) 
                                      (84 (invokestatic (methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector")))) 
                                      (87 (goto 112))  ;;to TAG_2
                                      (90 (pop)) ;;at TAG_0
                                      (91 (iconst_2)) ;;at TAG_1
                                      (92 (anewarray (class "java.lang.Object"))) 
                                      (95 (dup)) 
                                      (96 (iconst_0)) 
                                      (97 (aload_3)) 
                                      (98 (aconst_null)) 
                                      (99 (astore_3)) 
                                      (100 (aastore)) 
                                      (101 (dup)) 
                                      (102 (iconst_1)) 
                                      (103 (aload 4)) 
                                      (105 (aconst_null)) 
                                      (106 (astore 4)) 
                                      (108 (aastore)) 
                                      (109 (invokestatic (methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector")))) 
                                      (112 (areturn)) ;;at TAG_2
                                      (endofcode 113))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *walk$keywordize_keys$f__6988-class-table*
  (make-static-class-decls 
   *clojure.walk$keywordize_keys$f__6988*))

(defconst *package-name-map* 
  ("clojure.walk$keywordize_keys$f__6988" . "clojure"))

