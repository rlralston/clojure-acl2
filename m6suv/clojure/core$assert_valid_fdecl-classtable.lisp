; core$assert_valid_fdecl-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:40 CDT 2014.
;

(defconst *clojure.core$assert_valid_fdecl*
 (make-class-def
      '(class "clojure.core$assert_valid_fdecl"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "empty?")
                        (STRING  "map")
                        (STRING  "seq")
                        (STRING  "remove")
                        (STRING  "str")
                        (STRING  "first")
                        (STRING  "Parameter declaration missing")
                        (STRING  "Parameter declaration ")
                        (STRING  " should be a vector"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
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
                                   (max_stack . 2) (max_locals . 0) (code_length . 79)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "empty?"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$assert_valid_fdecl" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "map"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$assert_valid_fdecl" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "seq"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.core$assert_valid_fdecl" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 4))        ;;STRING:: "remove"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.core$assert_valid_fdecl" (class "clojure.lang.Var"))))
                                      (52 (ldc 0))        ;;STRING:: "clojure.core"
                                      (54 (ldc 5))        ;;STRING:: "str"
                                      (56 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (59 (checkcast (class "clojure.lang.Var")))
                                      (62 (putstatic (fieldCP "const__4" "clojure.core$assert_valid_fdecl" (class "clojure.lang.Var"))))
                                      (65 (ldc 0))        ;;STRING:: "clojure.core"
                                      (67 (ldc 6))        ;;STRING:: "first"
                                      (69 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (72 (checkcast (class "clojure.lang.Var")))
                                      (75 (putstatic (fieldCP "const__5" "clojure.core$assert_valid_fdecl" (class "clojure.lang.Var"))))
                                      (78 (return))
                                      (endofcode 79))
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
                                   (max_stack . 7) (max_locals . 4) (code_length . 178)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$assert_valid_fdecl" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (aload_1)) 
                                      (10 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (15 (dup)) 
                                      (16 (ifnull 44)) ;;to TAG_0
                                      (19 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (22 (if_acmpeq 45)) ;;to TAG_1
                                      (25 (new (class "java.lang.IllegalArgumentException"))) 
                                      (28 (dup)) 
                                      (29 (ldc 7)) ;;STRING:: "Parameter declaration missing"
                                      (31 (checkcast (class "java.lang.String"))) 
                                      (34 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (37 (checkcast (class "java.lang.Throwable"))) 
                                      (40 (athrow)) 
                                      (41 (goto 47))  ;;to TAG_2
                                      (44 (pop)) ;;at TAG_0
                                      (45 (aconst_null)) ;;at TAG_1
                                      (46 (pop)) 
                                      (47 (getstatic (fieldCP "const__1" "clojure.core$assert_valid_fdecl" (class "clojure.lang.Var")))) ;;at TAG_2
                                      (50 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (53 (checkcast (class "clojure.lang.IFn"))) 
                                      (56 (new (class "clojure.core$assert_valid_fdecl$fn__6491"))) 
                                      (59 (dup)) 
                                      (60 (aload_1)) 
                                      (61 (invokespecial (methodCP "<init>" "clojure.core$assert_valid_fdecl$fn__6491" ((class "java.lang.Object")) void))) 
                                      (64 (aload_1)) 
                                      (65 (aconst_null)) 
                                      (66 (astore_1)) 
                                      (67 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (72 (astore_2)) 
                                      (73 (getstatic (fieldCP "const__2" "clojure.core$assert_valid_fdecl" (class "clojure.lang.Var")))) 
                                      (76 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (79 (checkcast (class "clojure.lang.IFn"))) 
                                      (82 (getstatic (fieldCP "const__3" "clojure.core$assert_valid_fdecl" (class "clojure.lang.Var")))) 
                                      (85 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (88 (checkcast (class "clojure.lang.IFn"))) 
                                      (91 (new (class "clojure.core$assert_valid_fdecl$fn__6493"))) 
                                      (94 (dup)) 
                                      (95 (invokespecial (methodCP "<init>" "clojure.core$assert_valid_fdecl$fn__6493" () void))) 
                                      (98 (aload_2)) 
                                      (99 (aconst_null)) 
                                      (100 (astore_2)) 
                                      (101 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (106 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (111 (astore_3)) 
                                      (112 (aload_3)) 
                                      (113 (dup)) 
                                      (114 (ifnull 175)) ;;to TAG_3
                                      (117 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (120 (if_acmpeq 176)) ;;to TAG_4
                                      (123 (new (class "java.lang.IllegalArgumentException"))) 
                                      (126 (dup)) 
                                      (127 (getstatic (fieldCP "const__4" "clojure.core$assert_valid_fdecl" (class "clojure.lang.Var")))) 
                                      (130 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (133 (checkcast (class "clojure.lang.IFn"))) 
                                      (136 (ldc 8)) ;;STRING:: "Parameter declaration "
                                      (138 (getstatic (fieldCP "const__5" "clojure.core$assert_valid_fdecl" (class "clojure.lang.Var")))) 
                                      (141 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (144 (checkcast (class "clojure.lang.IFn"))) 
                                      (147 (aload_3)) 
                                      (148 (aconst_null)) 
                                      (149 (astore_3)) 
                                      (150 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (155 (ldc 9)) ;;STRING:: " should be a vector"
                                      (157 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) 
                                      (162 (checkcast (class "java.lang.String"))) 
                                      (165 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (168 (checkcast (class "java.lang.Throwable"))) 
                                      (171 (athrow)) 
                                      (172 (goto 177)) ;;to TAG_5
                                      (175 (pop)) ;;at TAG_3
                                      (176 (aconst_null)) ;;at TAG_4
                                      (177 (areturn)) ;;at TAG_5
                                      (endofcode 178))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$assert_valid_fdecl-class-table*
  (make-static-class-decls 
   *clojure.core$assert_valid_fdecl*))

(defconst *package-name-map* 
  ("clojure.core$assert_valid_fdecl" . "clojure"))

