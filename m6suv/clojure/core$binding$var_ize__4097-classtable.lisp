; core$binding$var_ize__4097-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:40 CDT 2014.
;

(defconst *clojure.core$binding$var_ize__4097*
 (make-class-def
      '(class "clojure.core$binding$var_ize__4097"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "seq")
                        (STRING  "conj")
                        (STRING  "concat")
                        (STRING  "list")
                        (STRING  "var")
                        (STRING  "first")
                        (STRING  "second")
                        (STRING  "next"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 104)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "seq"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$binding$var_ize__4097" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "conj"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$binding$var_ize__4097" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "concat"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.core$binding$var_ize__4097" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 4))        ;;STRING:: "list"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.core$binding$var_ize__4097" (class "clojure.lang.Var"))))
                                      (52 (aconst_null))
                                      (53 (ldc 5))        ;;STRING:: "var"
                                      (55 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (58 (checkcast (class "clojure.lang.AFn")))
                                      (61 (putstatic (fieldCP "const__4" "clojure.core$binding$var_ize__4097" (class "clojure.lang.AFn"))))
                                      (64 (ldc 0))        ;;STRING:: "clojure.core"
                                      (66 (ldc 6))        ;;STRING:: "first"
                                      (68 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (71 (checkcast (class "clojure.lang.Var")))
                                      (74 (putstatic (fieldCP "const__5" "clojure.core$binding$var_ize__4097" (class "clojure.lang.Var"))))
                                      (77 (ldc 0))        ;;STRING:: "clojure.core"
                                      (79 (ldc 7))        ;;STRING:: "second"
                                      (81 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (84 (checkcast (class "clojure.lang.Var")))
                                      (87 (putstatic (fieldCP "const__6" "clojure.core$binding$var_ize__4097" (class "clojure.lang.Var"))))
                                      (90 (ldc 0))        ;;STRING:: "clojure.core"
                                      (92 (ldc 8))        ;;STRING:: "next"
                                      (94 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (97 (checkcast (class "clojure.lang.Var")))
                                      (100 (putstatic (fieldCP "const__7" "clojure.core$binding$var_ize__4097" (class "clojure.lang.Var"))))
                                      (103 (return))
                                      (endofcode 104))
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
                                   (max_stack . 9) (max_locals . 4) (code_length . 205)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "EMPTY" "clojure.lang.PersistentVector" (class "clojure.lang.PersistentVector")))) 
                                      (3 (astore_2)) 
                                      (4 (getstatic (fieldCP "const__0" "clojure.core$binding$var_ize__4097" (class "clojure.lang.Var")))) 
                                      (7 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (10 (checkcast (class "clojure.lang.IFn"))) 
                                      (13 (aload_1)) 
                                      (14 (aconst_null)) 
                                      (15 (astore_1)) 
                                      (16 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (21 (astore_3)) 
                                      (22 (aload_3)) ;;at TAG_2
                                      (23 (dup)) 
                                      (24 (ifnull 188)) ;;to TAG_0
                                      (27 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (30 (if_acmpeq 189)) ;;to TAG_1
                                      (33 (getstatic (fieldCP "const__1" "clojure.core$binding$var_ize__4097" (class "clojure.lang.Var")))) 
                                      (36 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (39 (checkcast (class "clojure.lang.IFn"))) 
                                      (42 (getstatic (fieldCP "const__1" "clojure.core$binding$var_ize__4097" (class "clojure.lang.Var")))) 
                                      (45 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (48 (checkcast (class "clojure.lang.IFn"))) 
                                      (51 (aload_2)) 
                                      (52 (getstatic (fieldCP "const__0" "clojure.core$binding$var_ize__4097" (class "clojure.lang.Var")))) 
                                      (55 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (58 (checkcast (class "clojure.lang.IFn"))) 
                                      (61 (getstatic (fieldCP "const__2" "clojure.core$binding$var_ize__4097" (class "clojure.lang.Var")))) 
                                      (64 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (67 (checkcast (class "clojure.lang.IFn"))) 
                                      (70 (getstatic (fieldCP "const__3" "clojure.core$binding$var_ize__4097" (class "clojure.lang.Var")))) 
                                      (73 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (76 (checkcast (class "clojure.lang.IFn"))) 
                                      (79 (getstatic (fieldCP "const__4" "clojure.core$binding$var_ize__4097" (class "clojure.lang.AFn")))) 
                                      (82 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (87 (getstatic (fieldCP "const__3" "clojure.core$binding$var_ize__4097" (class "clojure.lang.Var")))) 
                                      (90 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (93 (checkcast (class "clojure.lang.IFn"))) 
                                      (96 (getstatic (fieldCP "const__5" "clojure.core$binding$var_ize__4097" (class "clojure.lang.Var")))) 
                                      (99 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (102 (checkcast (class "clojure.lang.IFn"))) 
                                      (105 (aload_3)) 
                                      (106 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (111 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (116 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (121 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (126 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (131 (getstatic (fieldCP "const__6" "clojure.core$binding$var_ize__4097" (class "clojure.lang.Var")))) 
                                      (134 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (137 (checkcast (class "clojure.lang.IFn"))) 
                                      (140 (aload_3)) 
                                      (141 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (146 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (151 (getstatic (fieldCP "const__7" "clojure.core$binding$var_ize__4097" (class "clojure.lang.Var")))) 
                                      (154 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (157 (checkcast (class "clojure.lang.IFn"))) 
                                      (160 (getstatic (fieldCP "const__7" "clojure.core$binding$var_ize__4097" (class "clojure.lang.Var")))) 
                                      (163 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (166 (checkcast (class "clojure.lang.IFn"))) 
                                      (169 (aload_3)) 
                                      (170 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (175 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (180 (astore_3)) 
                                      (181 (astore_2)) 
                                      (182 (goto 22))  ;;to TAG_2
                                      (185 (goto 204)) ;;to TAG_3
                                      (188 (pop)) ;;at TAG_0
                                      (189 (getstatic (fieldCP "const__0" "clojure.core$binding$var_ize__4097" (class "clojure.lang.Var")))) ;;at TAG_1
                                      (192 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (195 (checkcast (class "clojure.lang.IFn"))) 
                                      (198 (aload_2)) 
                                      (199 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (204 (areturn)) ;;at TAG_3
                                      (endofcode 205))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$binding$var_ize__4097-class-table*
  (make-static-class-decls 
   *clojure.core$binding$var_ize__4097*))

(defconst *package-name-map* 
  ("clojure.core$binding$var_ize__4097" . "clojure"))
