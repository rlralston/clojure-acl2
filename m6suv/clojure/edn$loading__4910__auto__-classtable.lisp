; edn$loading__4910__auto__-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:48 CDT 2014.
;

(defconst *clojure.edn$loading__4910__auto__*
 (make-class-def
      '(class "clojure.edn$loading__4910__auto__"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "refer")
                        (STRING  "exclude")
                        (STRING  "read")
                        (STRING  "read-string"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 5) (max_locals . 0) (code_length . 69)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "refer"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.edn$loading__4910__auto__" (class "clojure.lang.Var"))))
                                      (13 (aconst_null))
                                      (14 (ldc 0))        ;;STRING:: "clojure.core"
                                      (16 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (19 (checkcast (class "clojure.lang.AFn")))
                                      (22 (putstatic (fieldCP "const__1" "clojure.edn$loading__4910__auto__" (class "clojure.lang.AFn"))))
                                      (25 (aconst_null))
                                      (26 (ldc 2))        ;;STRING:: "exclude"
                                      (28 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (31 (checkcast (class "clojure.lang.Keyword")))
                                      (34 (putstatic (fieldCP "const__2" "clojure.edn$loading__4910__auto__" (class "clojure.lang.Keyword"))))
                                      (37 (iconst_2))
                                      (38 (anewarray (class "java.lang.Object")))
                                      (41 (dup))
                                      (42 (iconst_0))
                                      (43 (aconst_null))
                                      (44 (ldc 3))        ;;STRING:: "read"
                                      (46 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (49 (aastore))
                                      (50 (dup))
                                      (51 (iconst_1))
                                      (52 (aconst_null))
                                      (53 (ldc 4))        ;;STRING:: "read-string"
                                      (55 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (58 (aastore))
                                      (59 (invokestatic
					(methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector"))))
                                      (62 (checkcast (class "clojure.lang.AFn")))
                                      (65 (putstatic (fieldCP "const__3" "clojure.edn$loading__4910__auto__" (class "clojure.lang.AFn"))))
                                      (68 (return))
                                      (endofcode 69))
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
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 70)
                                   (parsedcode
                                      (0 (iconst_2)) 
                                      (1 (anewarray (class "java.lang.Object"))) 
                                      (4 (dup)) 
                                      (5 (iconst_0)) 
                                      (6 (getstatic (fieldCP "LOADER" "clojure.lang.Compiler" (class "clojure.lang.Var")))) 
                                      (9 (aastore)) 
                                      (10 (dup)) 
                                      (11 (iconst_1)) 
                                      (12 (aload_0)) 
                                      (13 (invokevirtual (methodCP "getClass" "java.lang.Object" () (class "java.lang.Class")))) 
                                      (16 (checkcast (class "java.lang.Class"))) 
                                      (19 (invokevirtual (methodCP "getClassLoader" "java.lang.Class" () (class "java.lang.ClassLoader")))) 
                                      (22 (aastore)) 
                                      (23 (invokestatic (methodCP "mapUniqueKeys" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentMap")))) 
                                      (26 (checkcast (class "clojure.lang.Associative"))) 
                                      (29 (invokestatic (methodCP "pushThreadBindings" "clojure.lang.Var" ((class "clojure.lang.Associative")) void))) 
                                      (32 (getstatic (fieldCP "const__0" "clojure.edn$loading__4910__auto__" (class "clojure.lang.Var")))) ;;at TAG_1
                                      (35 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (38 (checkcast (class "clojure.lang.IFn"))) 
                                      (41 (getstatic (fieldCP "const__1" "clojure.edn$loading__4910__auto__" (class "clojure.lang.AFn")))) 
                                      (44 (getstatic (fieldCP "const__2" "clojure.edn$loading__4910__auto__" (class "clojure.lang.Keyword")))) 
                                      (47 (getstatic (fieldCP "const__3" "clojure.edn$loading__4910__auto__" (class "clojure.lang.AFn")))) 
                                      (50 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) 
                                      (55 (astore_1)) 
                                      (56 (invokestatic (methodCP "popThreadBindings" "clojure.lang.Var" () void))) ;;at TAG_2
                                      (59 (goto 68)) ;;to TAG_0
                                      (62 (astore_2)) ;;at TAG_3
                                      (63 (invokestatic (methodCP "popThreadBindings" "clojure.lang.Var" () void))) 
                                      (66 (aload_2)) 
                                      (67 (athrow)) 
                                      (68 (aload_1)) ;;at TAG_0
                                      (69 (areturn)) 
                                      (endofcode 70))
                                   (Exceptions 
                                     (handler 32 56  62 (class "java.lang.Throwable")))
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *edn$loading__4910__auto__-class-table*
  (make-static-class-decls 
   *clojure.edn$loading__4910__auto__*))

(defconst *package-name-map* 
  ("clojure.edn$loading__4910__auto__" . "clojure"))

