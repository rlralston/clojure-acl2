; core$concat$fn__3923-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:41 CDT 2014.
;

(defconst *clojure.core$concat$fn__3923*
 (make-class-def
      '(class "clojure.core$concat$fn__3923"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "seq")
                        (STRING  "chunked-seq?")
                        (STRING  "chunk-cons")
                        (STRING  "chunk-first")
                        (STRING  "concat")
                        (STRING  "chunk-rest")
                        (STRING  "cons")
                        (STRING  "first")
                        (STRING  "rest"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "y" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "x" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 118)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "seq"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$concat$fn__3923" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "chunked-seq?"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$concat$fn__3923" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "chunk-cons"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.core$concat$fn__3923" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 4))        ;;STRING:: "chunk-first"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.core$concat$fn__3923" (class "clojure.lang.Var"))))
                                      (52 (ldc 0))        ;;STRING:: "clojure.core"
                                      (54 (ldc 5))        ;;STRING:: "concat"
                                      (56 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (59 (checkcast (class "clojure.lang.Var")))
                                      (62 (putstatic (fieldCP "const__4" "clojure.core$concat$fn__3923" (class "clojure.lang.Var"))))
                                      (65 (ldc 0))        ;;STRING:: "clojure.core"
                                      (67 (ldc 6))        ;;STRING:: "chunk-rest"
                                      (69 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (72 (checkcast (class "clojure.lang.Var")))
                                      (75 (putstatic (fieldCP "const__5" "clojure.core$concat$fn__3923" (class "clojure.lang.Var"))))
                                      (78 (ldc 0))        ;;STRING:: "clojure.core"
                                      (80 (ldc 7))        ;;STRING:: "cons"
                                      (82 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (85 (checkcast (class "clojure.lang.Var")))
                                      (88 (putstatic (fieldCP "const__6" "clojure.core$concat$fn__3923" (class "clojure.lang.Var"))))
                                      (91 (ldc 0))        ;;STRING:: "clojure.core"
                                      (93 (ldc 8))        ;;STRING:: "first"
                                      (95 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (98 (checkcast (class "clojure.lang.Var")))
                                      (101 (putstatic (fieldCP "const__7" "clojure.core$concat$fn__3923" (class "clojure.lang.Var"))))
                                      (104 (ldc 0))       ;;STRING:: "clojure.core"
                                      (106 (ldc 9))       ;;STRING:: "rest"
                                      (108 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (111 (checkcast (class "clojure.lang.Var")))
                                      (114 (putstatic (fieldCP "const__8" "clojure.core$concat$fn__3923" (class "clojure.lang.Var"))))
                                      (117 (return))
                                      (endofcode 118))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.AFunction" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "y" "clojure.core$concat$fn__3923" (class "java.lang.Object"))))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "x" "clojure.core$concat$fn__3923" (class "java.lang.Object"))))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 7) (max_locals . 2) (code_length . 216)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$concat$fn__3923" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (aload_0)) 
                                      (10 (getfield (fieldCP "x" "clojure.core$concat$fn__3923" (class "java.lang.Object")))) 
                                      (13 (aload_0)) 
                                      (14 (aconst_null)) 
                                      (15 (putfield (fieldCP "x" "clojure.core$concat$fn__3923" (class "java.lang.Object")))) 
                                      (18 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (23 (astore_1)) 
                                      (24 (aload_1)) 
                                      (25 (dup)) 
                                      (26 (ifnull 205)) ;;to TAG_0
                                      (29 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (32 (if_acmpeq 206)) ;;to TAG_1
                                      (35 (getstatic (fieldCP "const__1" "clojure.core$concat$fn__3923" (class "clojure.lang.Var")))) 
                                      (38 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (41 (checkcast (class "clojure.lang.IFn"))) 
                                      (44 (aload_1)) 
                                      (45 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (50 (dup)) 
                                      (51 (ifnull 132))  ;;to TAG_2
                                      (54 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (57 (if_acmpeq 133)) ;;to TAG_3
                                      (60 (getstatic (fieldCP "const__2" "clojure.core$concat$fn__3923" (class "clojure.lang.Var")))) 
                                      (63 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (66 (checkcast (class "clojure.lang.IFn"))) 
                                      (69 (getstatic (fieldCP "const__3" "clojure.core$concat$fn__3923" (class "clojure.lang.Var")))) 
                                      (72 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (75 (checkcast (class "clojure.lang.IFn"))) 
                                      (78 (aload_1)) 
                                      (79 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (84 (getstatic (fieldCP "const__4" "clojure.core$concat$fn__3923" (class "clojure.lang.Var")))) 
                                      (87 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (90 (checkcast (class "clojure.lang.IFn"))) 
                                      (93 (getstatic (fieldCP "const__5" "clojure.core$concat$fn__3923" (class "clojure.lang.Var")))) 
                                      (96 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (99 (checkcast (class "clojure.lang.IFn"))) 
                                      (102 (aload_1)) 
                                      (103 (aconst_null)) 
                                      (104 (astore_1)) 
                                      (105 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (110 (aload_0)) 
                                      (111 (getfield (fieldCP "y" "clojure.core$concat$fn__3923" (class "java.lang.Object")))) 
                                      (114 (aload_0)) 
                                      (115 (aconst_null)) 
                                      (116 (putfield (fieldCP "y" "clojure.core$concat$fn__3923" (class "java.lang.Object")))) 
                                      (119 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (124 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (129 (goto 202)) ;;to TAG_4
                                      (132 (pop)) ;;at TAG_2
                                      (133 (getstatic (fieldCP "const__6" "clojure.core$concat$fn__3923" (class "clojure.lang.Var")))) ;;at TAG_3
                                      (136 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (139 (checkcast (class "clojure.lang.IFn"))) 
                                      (142 (getstatic (fieldCP "const__7" "clojure.core$concat$fn__3923" (class "clojure.lang.Var")))) 
                                      (145 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (148 (checkcast (class "clojure.lang.IFn"))) 
                                      (151 (aload_1)) 
                                      (152 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (157 (getstatic (fieldCP "const__4" "clojure.core$concat$fn__3923" (class "clojure.lang.Var")))) 
                                      (160 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (163 (checkcast (class "clojure.lang.IFn"))) 
                                      (166 (getstatic (fieldCP "const__8" "clojure.core$concat$fn__3923" (class "clojure.lang.Var")))) 
                                      (169 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (172 (checkcast (class "clojure.lang.IFn"))) 
                                      (175 (aload_1)) 
                                      (176 (aconst_null)) 
                                      (177 (astore_1)) 
                                      (178 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (183 (aload_0)) 
                                      (184 (getfield (fieldCP "y" "clojure.core$concat$fn__3923" (class "java.lang.Object")))) 
                                      (187 (aload_0)) 
                                      (188 (aconst_null)) 
                                      (189 (putfield (fieldCP "y" "clojure.core$concat$fn__3923" (class "java.lang.Object")))) 
                                      (192 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (197 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (202 (goto 215)) ;;to TAG_5;;at TAG_4
                                      (205 (pop)) ;;at TAG_0
                                      (206 (aload_0)) ;;at TAG_1
                                      (207 (getfield (fieldCP "y" "clojure.core$concat$fn__3923" (class "java.lang.Object")))) 
                                      (210 (aload_0)) 
                                      (211 (aconst_null)) 
                                      (212 (putfield (fieldCP "y" "clojure.core$concat$fn__3923" (class "java.lang.Object")))) 
                                      (215 (areturn)) ;;at TAG_5
                                      (endofcode 216))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$concat$fn__3923-class-table*
  (make-static-class-decls 
   *clojure.core$concat$fn__3923*))

(defconst *package-name-map* 
  ("clojure.core$concat$fn__3923" . "clojure"))

