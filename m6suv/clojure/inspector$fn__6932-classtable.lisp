; inspector$fn__6932-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:49 CDT 2014.
;

(defconst *clojure.inspector$fn__6932*
 (make-class-def
      '(class "clojure.inspector$fn__6932"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "vector?")
                        (STRING  "vec")
                        (STRING  "nrows")
                        (STRING  "count")
                        (STRING  "get-value")
                        (STRING  "get-label"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 76)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "vector?"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.inspector$fn__6932" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "vec"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.inspector$fn__6932" (class "clojure.lang.Var"))))
                                      (26 (aconst_null))
                                      (27 (ldc 3))        ;;STRING:: "nrows"
                                      (29 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (32 (checkcast (class "clojure.lang.Keyword")))
                                      (35 (putstatic (fieldCP "const__2" "clojure.inspector$fn__6932" (class "clojure.lang.Keyword"))))
                                      (38 (ldc 0))        ;;STRING:: "clojure.core"
                                      (40 (ldc 4))        ;;STRING:: "count"
                                      (42 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (45 (checkcast (class "clojure.lang.Var")))
                                      (48 (putstatic (fieldCP "const__3" "clojure.inspector$fn__6932" (class "clojure.lang.Var"))))
                                      (51 (aconst_null))
                                      (52 (ldc 5))        ;;STRING:: "get-value"
                                      (54 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (57 (checkcast (class "clojure.lang.Keyword")))
                                      (60 (putstatic (fieldCP "const__4" "clojure.inspector$fn__6932" (class "clojure.lang.Keyword"))))
                                      (63 (aconst_null))
                                      (64 (ldc 6))        ;;STRING:: "get-label"
                                      (66 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (69 (checkcast (class "clojure.lang.Keyword")))
                                      (72 (putstatic (fieldCP "const__5" "clojure.inspector$fn__6932" (class "clojure.lang.Keyword"))))
                                      (75 (return))
                                      (endofcode 76))
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
                                   (max_stack . 7) (max_locals . 3) (code_length . 110)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.inspector$fn__6932" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (aload_1)) 
                                      (10 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (15 (dup)) 
                                      (16 (ifnull 31)) ;;to TAG_0
                                      (19 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (22 (if_acmpeq 32)) ;;to TAG_1
                                      (25 (aload_1)) 
                                      (26 (aconst_null)) 
                                      (27 (astore_1)) 
                                      (28 (goto 49))  ;;to TAG_2
                                      (31 (pop)) ;;at TAG_0
                                      (32 (getstatic (fieldCP "const__1" "clojure.inspector$fn__6932" (class "clojure.lang.Var")))) ;;at TAG_1
                                      (35 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (38 (checkcast (class "clojure.lang.IFn"))) 
                                      (41 (aload_1)) 
                                      (42 (aconst_null)) 
                                      (43 (astore_1)) 
                                      (44 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (49 (astore_2)) ;;at TAG_2
                                      (50 (bipush 6)) 
                                      (52 (anewarray (class "java.lang.Object"))) 
                                      (55 (dup)) 
                                      (56 (iconst_0)) 
                                      (57 (getstatic (fieldCP "const__2" "clojure.inspector$fn__6932" (class "clojure.lang.Keyword")))) 
                                      (60 (aastore)) 
                                      (61 (dup)) 
                                      (62 (iconst_1)) 
                                      (63 (aload_2)) 
                                      (64 (invokestatic (methodCP "count" "clojure.lang.RT" ((class "java.lang.Object")) int))) 
                                      (67 (invokestatic (methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer")))) 
                                      (70 (aastore)) 
                                      (71 (dup)) 
                                      (72 (iconst_2)) 
                                      (73 (getstatic (fieldCP "const__4" "clojure.inspector$fn__6932" (class "clojure.lang.Keyword")))) 
                                      (76 (aastore)) 
                                      (77 (dup)) 
                                      (78 (iconst_3)) 
                                      (79 (new (class "clojure.inspector$fn__6932$fn__6933"))) 
                                      (82 (dup)) 
                                      (83 (aload_2)) 
                                      (84 (aconst_null)) 
                                      (85 (astore_2)) 
                                      (86 (invokespecial (methodCP "<init>" "clojure.inspector$fn__6932$fn__6933" ((class "java.lang.Object")) void))) 
                                      (89 (aastore)) 
                                      (90 (dup)) 
                                      (91 (iconst_4)) 
                                      (92 (getstatic (fieldCP "const__5" "clojure.inspector$fn__6932" (class "clojure.lang.Keyword")))) 
                                      (95 (aastore)) 
                                      (96 (dup)) 
                                      (97 (iconst_5)) 
                                      (98 (new (class "clojure.inspector$fn__6932$fn__6935"))) 
                                      (101 (dup)) 
                                      (102 (invokespecial (methodCP "<init>" "clojure.inspector$fn__6932$fn__6935" () void))) 
                                      (105 (aastore)) 
                                      (106 (invokestatic (methodCP "mapUniqueKeys" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentMap")))) 
                                      (109 (areturn)) 
                                      (endofcode 110))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *inspector$fn__6932-class-table*
  (make-static-class-decls 
   *clojure.inspector$fn__6932*))

(defconst *package-name-map* 
  ("clojure.inspector$fn__6932" . "clojure"))

