; pprint$pprint_let-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:56 CDT 2014.
;

(defconst *clojure.pprint$pprint_let*
 (make-class-def
      '(class "clojure.pprint$pprint_let"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "first")
                        (STRING  "clojure.pprint")
                        (STRING  "level-exceeded")
                        (STRING  "*out*")
                        (STRING  "push-thread-bindings")
                        (STRING  "*current-level*")
                        (STRING  "inc")
                        (STRING  "var-get")
                        (STRING  "*current-length*")
                        (STRING  "#"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 112)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "first"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.pprint$pprint_let" (class "clojure.lang.Var"))))
                                      (13 (ldc 2))        ;;STRING:: "clojure.pprint"
                                      (15 (ldc 3))        ;;STRING:: "level-exceeded"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.pprint$pprint_let" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 4))        ;;STRING:: "*out*"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.pprint$pprint_let" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 5))        ;;STRING:: "push-thread-bindings"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.pprint$pprint_let" (class "clojure.lang.Var"))))
                                      (52 (ldc 2))        ;;STRING:: "clojure.pprint"
                                      (54 (ldc 6))        ;;STRING:: "*current-level*"
                                      (56 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (59 (checkcast (class "clojure.lang.Var")))
                                      (62 (putstatic (fieldCP "const__4" "clojure.pprint$pprint_let" (class "clojure.lang.Var"))))
                                      (65 (ldc 0))        ;;STRING:: "clojure.core"
                                      (67 (ldc 7))        ;;STRING:: "inc"
                                      (69 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (72 (checkcast (class "clojure.lang.Var")))
                                      (75 (putstatic (fieldCP "const__5" "clojure.pprint$pprint_let" (class "clojure.lang.Var"))))
                                      (78 (ldc 0))        ;;STRING:: "clojure.core"
                                      (80 (ldc 8))        ;;STRING:: "var-get"
                                      (82 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (85 (checkcast (class "clojure.lang.Var")))
                                      (88 (putstatic (fieldCP "const__6" "clojure.pprint$pprint_let" (class "clojure.lang.Var"))))
                                      (91 (ldc 2))        ;;STRING:: "clojure.pprint"
                                      (93 (ldc 9))        ;;STRING:: "*current-length*"
                                      (95 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (98 (checkcast (class "clojure.lang.Var")))
                                      (101 (putstatic (fieldCP "const__7" "clojure.pprint$pprint_let" (class "clojure.lang.Var"))))
                                      (104 (lconst_0))
                                      (105 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (108 (putstatic (fieldCP "const__8" "clojure.pprint$pprint_let" (class "java.lang.Object"))))
                                      (111 (return))
                                      (endofcode 112))
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
                                   (max_stack . 6) (max_locals . 3) (code_length . 147)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.pprint$pprint_let" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (aload_1)) 
                                      (10 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (15 (astore_2)) 
                                      (16 (getstatic (fieldCP "const__1" "clojure.pprint$pprint_let" (class "clojure.lang.Var")))) 
                                      (19 (checkcast (class "clojure.lang.IFn"))) 
                                      (22 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1)) 
                                      (27 (dup)) 
                                      (28 (ifnull 59)) ;;to TAG_0
                                      (31 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (34 (if_acmpeq 60)) ;;to TAG_1
                                      (37 (getstatic (fieldCP "const__2" "clojure.pprint$pprint_let" (class "clojure.lang.Var")))) 
                                      (40 (invokevirtual (methodCP "get" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (43 (checkcast (class "java.io.Writer"))) 
                                      (46 (ldc 10)) ;;STRING:: "#"
                                      (48 (checkcast (class "java.lang.String"))) 
                                      (51 (invokevirtual (methodCP "write" "java.io.Writer" ((class "java.lang.String")) void))) 
                                      (54 (aconst_null)) 
                                      (55 (pop)) 
                                      (56 (goto 145))  ;;to TAG_2
                                      (59 (pop)) ;;at TAG_0
                                      (60 (getstatic (fieldCP "const__3" "clojure.pprint$pprint_let" (class "clojure.lang.Var")))) ;;at TAG_1
                                      (63 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (66 (checkcast (class "clojure.lang.IFn"))) 
                                      (69 (iconst_4)) 
                                      (70 (anewarray (class "java.lang.Object"))) 
                                      (73 (dup)) 
                                      (74 (iconst_0)) 
                                      (75 (getstatic (fieldCP "const__4" "clojure.pprint$pprint_let" (class "clojure.lang.Var")))) 
                                      (78 (aastore)) 
                                      (79 (dup)) 
                                      (80 (iconst_1)) 
                                      (81 (getstatic (fieldCP "const__6" "clojure.pprint$pprint_let" (class "clojure.lang.Var")))) 
                                      (84 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (87 (checkcast (class "clojure.lang.IFn"))) 
                                      (90 (getstatic (fieldCP "const__4" "clojure.pprint$pprint_let" (class "clojure.lang.Var")))) 
                                      (93 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (98 (invokestatic (methodCP "inc" "clojure.lang.Numbers" ((class "java.lang.Object")) (class "java.lang.Number")))) 
                                      (101 (aastore)) 
                                      (102 (dup)) 
                                      (103 (iconst_2)) 
                                      (104 (getstatic (fieldCP "const__7" "clojure.pprint$pprint_let" (class "clojure.lang.Var")))) 
                                      (107 (aastore)) 
                                      (108 (dup)) 
                                      (109 (iconst_3)) 
                                      (110 (getstatic (fieldCP "const__8" "clojure.pprint$pprint_let" (class "java.lang.Object")))) 
                                      (113 (aastore)) 
                                      (114 (invokestatic (methodCP "map" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentMap")))) 
                                      (117 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (122 (pop)) 
                                      (123 (new (class "clojure.pprint$pprint_let$fn__8363"))) 
                                      (126 (dup)) 
                                      (127 (aload_1)) 
                                      (128 (aconst_null)) 
                                      (129 (astore_1)) 
                                      (130 (aload_2)) 
                                      (131 (aconst_null)) 
                                      (132 (astore_2)) 
                                      (133 (invokespecial (methodCP "<init>" "clojure.pprint$pprint_let$fn__8363" ((class "java.lang.Object") (class "java.lang.Object")) void))) 
                                      (136 (checkcast (class "clojure.lang.IFn"))) 
                                      (139 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1)) 
                                      (144 (pop)) 
                                      (145 (aconst_null)) ;;at TAG_2
                                      (146 (areturn)) 
                                      (endofcode 147))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *pprint$pprint_let-class-table*
  (make-static-class-decls 
   *clojure.pprint$pprint_let*))

(defconst *package-name-map* 
  ("clojure.pprint$pprint_let" . "clojure"))
