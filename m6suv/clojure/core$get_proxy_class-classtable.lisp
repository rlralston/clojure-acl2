; core$get_proxy_class-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:44 CDT 2014.
;

(defconst *clojure.core$get_proxy_class*
 (make-class-def
      '(class "clojure.core$get_proxy_class"
            "clojure.lang.RestFn"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "get-super-and-interfaces")
                        (STRING  "nth")
                        (STRING  "proxy-name")
                        (STRING  "generate-proxy")
                        (STRING  "deref"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 80)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "get-super-and-interfaces"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$get_proxy_class" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "nth"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$get_proxy_class" (class "clojure.lang.Var"))))
                                      (26 (lconst_0))
                                      (27 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (30 (putstatic (fieldCP "const__2" "clojure.core$get_proxy_class" (class "java.lang.Object"))))
                                      (33 (lconst_1))
                                      (34 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (37 (putstatic (fieldCP "const__3" "clojure.core$get_proxy_class" (class "java.lang.Object"))))
                                      (40 (ldc 0))        ;;STRING:: "clojure.core"
                                      (42 (ldc 3))        ;;STRING:: "proxy-name"
                                      (44 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (47 (checkcast (class "clojure.lang.Var")))
                                      (50 (putstatic (fieldCP "const__4" "clojure.core$get_proxy_class" (class "clojure.lang.Var"))))
                                      (53 (ldc 0))        ;;STRING:: "clojure.core"
                                      (55 (ldc 4))        ;;STRING:: "generate-proxy"
                                      (57 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (60 (checkcast (class "clojure.lang.Var")))
                                      (63 (putstatic (fieldCP "const__5" "clojure.core$get_proxy_class" (class "clojure.lang.Var"))))
                                      (66 (ldc 0))        ;;STRING:: "clojure.core"
                                      (68 (ldc 5))        ;;STRING:: "deref"
                                      (70 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (73 (checkcast (class "clojure.lang.Var")))
                                      (76 (putstatic (fieldCP "const__6" "clojure.core$get_proxy_class" (class "clojure.lang.Var"))))
                                      (79 (return))
                                      (endofcode 80))
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
					(methodCP "<init>" "clojure.lang.RestFn" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "doInvoke"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 8) (max_locals . 10) (code_length . 198)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$get_proxy_class" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (aload_1)) 
                                      (10 (aconst_null)) 
                                      (11 (astore_1)) 
                                      (12 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (17 (astore_2)) 
                                      (18 (aload_2)) 
                                      (19 (lconst_0)) 
                                      (20 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (23 (aconst_null)) 
                                      (24 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (27 (astore_3)) 
                                      (28 (aload_2)) 
                                      (29 (aconst_null)) 
                                      (30 (astore_2)) 
                                      (31 (lconst_1)) 
                                      (32 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (35 (aconst_null)) 
                                      (36 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (39 (astore 4)) 
                                      (41 (getstatic (fieldCP "const__4" "clojure.core$get_proxy_class" (class "clojure.lang.Var")))) 
                                      (44 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (47 (checkcast (class "clojure.lang.IFn"))) 
                                      (50 (aload_3)) 
                                      (51 (aload 4)) 
                                      (53 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (58 (astore 5)) 
                                      (60 (aload 5)) 
                                      (62 (checkcast (class "java.lang.String"))) 
                                      (65 (invokestatic (methodCP "loadClassForName" "clojure.lang.RT" ((class "java.lang.String")) (class "java.lang.Class")))) 
                                      (68 (astore 6)) 
                                      (70 (aload 6)) 
                                      (72 (dup)) 
                                      (73 (ifnull 90)) ;;to TAG_0
                                      (76 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (79 (if_acmpeq 91)) ;;to TAG_1
                                      (82 (aload 6)) 
                                      (84 (aconst_null)) 
                                      (85 (astore 6)) 
                                      (87 (goto 197))  ;;to TAG_2
                                      (90 (pop)) ;;at TAG_0
                                      (91 (getstatic (fieldCP "const__5" "clojure.core$get_proxy_class" (class "clojure.lang.Var")))) ;;at TAG_1
                                      (94 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (97 (checkcast (class "clojure.lang.IFn"))) 
                                      (100 (aload_3)) 
                                      (101 (aload 4)) 
                                      (103 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (108 (astore 7)) 
                                      (110 (aload 7)) 
                                      (112 (lconst_0)) 
                                      (113 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (116 (aconst_null)) 
                                      (117 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (120 (astore 8)) 
                                      (122 (aload 7)) 
                                      (124 (aconst_null)) 
                                      (125 (astore 7)) 
                                      (127 (lconst_1)) 
                                      (128 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (131 (aconst_null)) 
                                      (132 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (135 (astore 9)) 
                                      (137 (getstatic (fieldCP "const__6" "clojure.core$get_proxy_class" (class "clojure.lang.Var")))) 
                                      (140 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (143 (checkcast (class "clojure.lang.IFn"))) 
                                      (146 (getstatic (fieldCP "LOADER" "clojure.lang.Compiler" (class "clojure.lang.Var")))) 
                                      (149 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (154 (checkcast (class "clojure.lang.DynamicClassLoader"))) 
                                      (157 (aload 5)) 
                                      (159 (aconst_null)) 
                                      (160 (astore 5)) 
                                      (162 (checkcast (class "java.lang.String"))) 
                                      (165 (aload 9)) 
                                      (167 (aconst_null)) 
                                      (168 (astore 9)) 
                                      (170 (checkcast (array byte))) 
                                      (173 (iconst_2)) 
                                      (174 (anewarray (class "java.lang.Object"))) 
                                      (177 (dup)) 
                                      (178 (iconst_0)) 
                                      (179 (aload_3)) 
                                      (180 (aconst_null)) 
                                      (181 (astore_3)) 
                                      (182 (aastore)) 
                                      (183 (dup)) 
                                      (184 (iconst_1)) 
                                      (185 (aload 4)) 
                                      (187 (aconst_null)) 
                                      (188 (astore 4)) 
                                      (190 (aastore)) 
                                      (191 (invokestatic (methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector")))) 
                                      (194 (invokevirtual (methodCP "defineClass" "clojure.lang.DynamicClassLoader" ((class "java.lang.String") (array byte) (class "java.lang.Object")) (class "java.lang.Class")))) 
                                      (197 (areturn)) ;;at TAG_2
                                      (endofcode 198))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getRequiredArity"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 2)
                                   (parsedcode
                                      (0 (iconst_0))
                                      (1 (ireturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$get_proxy_class-class-table*
  (make-static-class-decls 
   *clojure.core$get_proxy_class*))

(defconst *package-name-map* 
  ("clojure.core$get_proxy_class" . "clojure"))

