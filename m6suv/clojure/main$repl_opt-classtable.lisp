; main$repl_opt-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:53 CDT 2014.
;

(defconst *clojure.main$repl_opt*
 (make-class-def
      '(class "clojure.main$repl_opt"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "nth")
                        (STRING  "nthnext")
                        (STRING  "some")
                        (STRING  "println")
                        (STRING  "clojure-version")
                        (STRING  "clojure.main")
                        (STRING  "repl")
                        (STRING  "init")
                        (STRING  "prn")
                        (STRING  "Clojure"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__9" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 118)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "nth"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.main$repl_opt" (class "clojure.lang.Var"))))
                                      (13 (lconst_0))
                                      (14 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (17 (putstatic (fieldCP "const__1" "clojure.main$repl_opt" (class "java.lang.Object"))))
                                      (20 (ldc 0))        ;;STRING:: "clojure.core"
                                      (22 (ldc 2))        ;;STRING:: "nthnext"
                                      (24 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (27 (checkcast (class "clojure.lang.Var")))
                                      (30 (putstatic (fieldCP "const__2" "clojure.main$repl_opt" (class "clojure.lang.Var"))))
                                      (33 (lconst_1))
                                      (34 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (37 (putstatic (fieldCP "const__3" "clojure.main$repl_opt" (class "java.lang.Object"))))
                                      (40 (ldc 0))        ;;STRING:: "clojure.core"
                                      (42 (ldc 3))        ;;STRING:: "some"
                                      (44 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (47 (checkcast (class "clojure.lang.Var")))
                                      (50 (putstatic (fieldCP "const__4" "clojure.main$repl_opt" (class "clojure.lang.Var"))))
                                      (53 (ldc 0))        ;;STRING:: "clojure.core"
                                      (55 (ldc 4))        ;;STRING:: "println"
                                      (57 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (60 (checkcast (class "clojure.lang.Var")))
                                      (63 (putstatic (fieldCP "const__5" "clojure.main$repl_opt" (class "clojure.lang.Var"))))
                                      (66 (ldc 0))        ;;STRING:: "clojure.core"
                                      (68 (ldc 5))        ;;STRING:: "clojure-version"
                                      (70 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (73 (checkcast (class "clojure.lang.Var")))
                                      (76 (putstatic (fieldCP "const__6" "clojure.main$repl_opt" (class "clojure.lang.Var"))))
                                      (79 (ldc 6))        ;;STRING:: "clojure.main"
                                      (81 (ldc 7))        ;;STRING:: "repl"
                                      (83 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (86 (checkcast (class "clojure.lang.Var")))
                                      (89 (putstatic (fieldCP "const__7" "clojure.main$repl_opt" (class "clojure.lang.Var"))))
                                      (92 (aconst_null))
                                      (93 (ldc 8))        ;;STRING:: "init"
                                      (95 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (98 (checkcast (class "clojure.lang.Keyword")))
                                      (101 (putstatic (fieldCP "const__8" "clojure.main$repl_opt" (class "clojure.lang.Keyword"))))
                                      (104 (ldc 0))       ;;STRING:: "clojure.core"
                                      (106 (ldc 9))       ;;STRING:: "prn"
                                      (108 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (111 (checkcast (class "clojure.lang.Var")))
                                      (114 (putstatic (fieldCP "const__9" "clojure.main$repl_opt" (class "clojure.lang.Var"))))
                                      (117 (return))
                                      (endofcode 118))
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
                                   (max_stack . 7) (max_locals . 6) (code_length . 163)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (aconst_null)) 
                                      (2 (astore_1)) 
                                      (3 (astore_3)) 
                                      (4 (aload_3)) 
                                      (5 (lconst_0)) 
                                      (6 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (9 (aconst_null)) 
                                      (10 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (13 (astore 4)) 
                                      (15 (getstatic (fieldCP "const__2" "clojure.main$repl_opt" (class "clojure.lang.Var")))) 
                                      (18 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (21 (checkcast (class "clojure.lang.IFn"))) 
                                      (24 (aload_3)) 
                                      (25 (aconst_null)) 
                                      (26 (astore_3)) 
                                      (27 (getstatic (fieldCP "const__3" "clojure.main$repl_opt" (class "java.lang.Object")))) 
                                      (30 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (35 (astore 5)) 
                                      (37 (getstatic (fieldCP "const__4" "clojure.main$repl_opt" (class "clojure.lang.Var")))) 
                                      (40 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (43 (checkcast (class "clojure.lang.IFn"))) 
                                      (46 (new (class "clojure.main$repl_opt$fn__6624"))) 
                                      (49 (dup)) 
                                      (50 (invokespecial (methodCP "<init>" "clojure.main$repl_opt$fn__6624" () void))) 
                                      (53 (aload_2)) 
                                      (54 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (59 (dup)) 
                                      (60 (ifnull 74)) ;;to TAG_0
                                      (63 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (66 (if_acmpeq 75)) ;;to TAG_1
                                      (69 (aconst_null)) 
                                      (70 (pop)) 
                                      (71 (goto 106))  ;;to TAG_2
                                      (74 (pop)) ;;at TAG_0
                                      (75 (getstatic (fieldCP "const__5" "clojure.main$repl_opt" (class "clojure.lang.Var")))) ;;at TAG_1
                                      (78 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (81 (checkcast (class "clojure.lang.IFn"))) 
                                      (84 (ldc 10)) ;;STRING:: "Clojure"
                                      (86 (getstatic (fieldCP "const__6" "clojure.main$repl_opt" (class "clojure.lang.Var")))) 
                                      (89 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (92 (checkcast (class "clojure.lang.IFn"))) 
                                      (95 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1)) 
                                      (100 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (105 (pop)) 
                                      (106 (getstatic (fieldCP "const__7" "clojure.main$repl_opt" (class "clojure.lang.Var")))) ;;at TAG_2
                                      (109 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (112 (checkcast (class "clojure.lang.IFn"))) 
                                      (115 (getstatic (fieldCP "const__8" "clojure.main$repl_opt" (class "clojure.lang.Keyword")))) 
                                      (118 (new (class "clojure.main$repl_opt$fn__6626"))) 
                                      (121 (dup)) 
                                      (122 (aload_2)) 
                                      (123 (aconst_null)) 
                                      (124 (astore_2)) 
                                      (125 (aload 5)) 
                                      (127 (aconst_null)) 
                                      (128 (astore 5)) 
                                      (130 (invokespecial (methodCP "<init>" "clojure.main$repl_opt$fn__6626" ((class "java.lang.Object") (class "java.lang.Object")) void))) 
                                      (133 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (138 (pop)) 
                                      (139 (getstatic (fieldCP "const__9" "clojure.main$repl_opt" (class "clojure.lang.Var")))) 
                                      (142 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (145 (checkcast (class "clojure.lang.IFn"))) 
                                      (148 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1)) 
                                      (153 (pop)) 
                                      (154 (lconst_0)) 
                                      (155 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (158 (invokestatic (methodCP "exit" "java.lang.System" (int) void))) 
                                      (161 (aconst_null)) 
                                      (162 (areturn)) 
                                      (endofcode 163))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *main$repl_opt-class-table*
  (make-static-class-decls 
   *clojure.main$repl_opt*))

(defconst *package-name-map* 
  ("clojure.main$repl_opt" . "clojure"))
