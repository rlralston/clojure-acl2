; main$repl_read-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:53 CDT 2014.
;

(defconst *clojure.main$repl_read*
 (make-class-def
      '(class "clojure.main$repl_read"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "line-start")
                        (STRING  "stream-end")
                        (STRING  "clojure.main")
                        (STRING  "skip-whitespace")
                        (STRING  "clojure.core")
                        (STRING  "*in*")
                        (STRING  "read")
                        (STRING  "skip-if-eol"))
            (fields
                        (field "const__0" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 77)
                                   (parsedcode
                                      (0 (aconst_null))
                                      (1 (ldc 0))         ;;STRING:: "line-start"
                                      (3 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (6 (checkcast (class "clojure.lang.Keyword")))
                                      (9 (putstatic (fieldCP "const__0" "clojure.main$repl_read" (class "clojure.lang.Keyword"))))
                                      (12 (aconst_null))
                                      (13 (ldc 1))        ;;STRING:: "stream-end"
                                      (15 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (18 (checkcast (class "clojure.lang.Keyword")))
                                      (21 (putstatic (fieldCP "const__1" "clojure.main$repl_read" (class "clojure.lang.Keyword"))))
                                      (24 (ldc 2))        ;;STRING:: "clojure.main"
                                      (26 (ldc 3))        ;;STRING:: "skip-whitespace"
                                      (28 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (31 (checkcast (class "clojure.lang.Var")))
                                      (34 (putstatic (fieldCP "const__2" "clojure.main$repl_read" (class "clojure.lang.Var"))))
                                      (37 (ldc 4))        ;;STRING:: "clojure.core"
                                      (39 (ldc 5))        ;;STRING:: "*in*"
                                      (41 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (44 (checkcast (class "clojure.lang.Var")))
                                      (47 (putstatic (fieldCP "const__3" "clojure.main$repl_read" (class "clojure.lang.Var"))))
                                      (50 (ldc 4))        ;;STRING:: "clojure.core"
                                      (52 (ldc 6))        ;;STRING:: "read"
                                      (54 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (57 (checkcast (class "clojure.lang.Var")))
                                      (60 (putstatic (fieldCP "const__4" "clojure.main$repl_read" (class "clojure.lang.Var"))))
                                      (63 (ldc 2))        ;;STRING:: "clojure.main"
                                      (65 (ldc 7))        ;;STRING:: "skip-if-eol"
                                      (67 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (70 (checkcast (class "clojure.lang.Var")))
                                      (73 (putstatic (fieldCP "const__5" "clojure.main$repl_read" (class "clojure.lang.Var"))))
                                      (76 (return))
                                      (endofcode 77))
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
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 5) (code_length . 121)
                                   (parsedcode
                                      (0 (iconst_4)) 
                                      (1 (anewarray (class "java.lang.Object"))) 
                                      (4 (dup)) 
                                      (5 (iconst_0)) 
                                      (6 (getstatic (fieldCP "const__0" "clojure.main$repl_read" (class "clojure.lang.Keyword")))) 
                                      (9 (aastore)) 
                                      (10 (dup)) 
                                      (11 (iconst_1)) 
                                      (12 (aload_1)) 
                                      (13 (aconst_null)) 
                                      (14 (astore_1)) 
                                      (15 (aastore)) 
                                      (16 (dup)) 
                                      (17 (iconst_2)) 
                                      (18 (getstatic (fieldCP "const__1" "clojure.main$repl_read" (class "clojure.lang.Keyword")))) 
                                      (21 (aastore)) 
                                      (22 (dup)) 
                                      (23 (iconst_3)) 
                                      (24 (aload_2)) 
                                      (25 (aconst_null)) 
                                      (26 (astore_2)) 
                                      (27 (aastore)) 
                                      (28 (invokestatic (methodCP "mapUniqueKeys" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentMap")))) 
                                      (31 (checkcast (class "clojure.lang.IFn"))) 
                                      (34 (getstatic (fieldCP "const__2" "clojure.main$repl_read" (class "clojure.lang.Var")))) 
                                      (37 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (40 (checkcast (class "clojure.lang.IFn"))) 
                                      (43 (getstatic (fieldCP "const__3" "clojure.main$repl_read" (class "clojure.lang.Var")))) 
                                      (46 (invokevirtual (methodCP "get" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (49 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (54 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (59 (astore_3)) 
                                      (60 (aload_3)) 
                                      (61 (dup)) 
                                      (62 (ifnull 77)) ;;to TAG_0
                                      (65 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (68 (if_acmpeq 78)) ;;to TAG_1
                                      (71 (aload_3)) 
                                      (72 (aconst_null)) 
                                      (73 (astore_3)) 
                                      (74 (goto 120))  ;;to TAG_2
                                      (77 (pop)) ;;at TAG_0
                                      (78 (getstatic (fieldCP "const__4" "clojure.main$repl_read" (class "clojure.lang.Var")))) ;;at TAG_1
                                      (81 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (84 (checkcast (class "clojure.lang.IFn"))) 
                                      (87 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1)) 
                                      (92 (astore 4)) 
                                      (94 (getstatic (fieldCP "const__5" "clojure.main$repl_read" (class "clojure.lang.Var")))) 
                                      (97 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (100 (checkcast (class "clojure.lang.IFn"))) 
                                      (103 (getstatic (fieldCP "const__3" "clojure.main$repl_read" (class "clojure.lang.Var")))) 
                                      (106 (invokevirtual (methodCP "get" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (109 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (114 (pop)) 
                                      (115 (aload 4)) 
                                      (117 (aconst_null)) 
                                      (118 (astore 4)) 
                                      (120 (areturn)) ;;at TAG_2
                                      (endofcode 121))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *main$repl_read-class-table*
  (make-static-class-decls 
   *clojure.main$repl_read*))

(defconst *package-name-map* 
  ("clojure.main$repl_read" . "clojure"))

