; core$generate_proxy$iter__5215__5221$fn__5222-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:43 CDT 2014.
;

(defconst *clojure.core$generate_proxy$iter__5215__5221$fn__5222*
 (make-class-def
      '(class "clojure.core$generate_proxy$iter__5215__5221$fn__5222"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "seq")
                        (STRING  "first")
                        (STRING  "concat")
                        (STRING  "rest"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "iter__5215" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "s__5216" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "considered" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 53)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "seq"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$generate_proxy$iter__5215__5221$fn__5222" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "first"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$generate_proxy$iter__5215__5221$fn__5222" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "concat"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.core$generate_proxy$iter__5215__5221$fn__5222" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 4))        ;;STRING:: "rest"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.core$generate_proxy$iter__5215__5221$fn__5222" (class "clojure.lang.Var"))))
                                      (52 (return))
                                      (endofcode 53))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 20)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.AFunction" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "iter__5215" "clojure.core$generate_proxy$iter__5215__5221$fn__5222" (class "java.lang.Object"))))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "s__5216" "clojure.core$generate_proxy$iter__5215__5221$fn__5222" (class "java.lang.Object"))))
                                      (14 (aload_0))
                                      (15 (aload_3))
                                      (16 (putfield (fieldCP "considered" "clojure.core$generate_proxy$iter__5215__5221$fn__5222" (class "java.lang.Object"))))
                                      (19 (return))
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 7) (code_length . 200)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "s__5216" "clojure.core$generate_proxy$iter__5215__5221$fn__5222" (class "java.lang.Object")))) 
                                      (4 (aload_0)) 
                                      (5 (aconst_null)) 
                                      (6 (putfield (fieldCP "s__5216" "clojure.core$generate_proxy$iter__5215__5221$fn__5222" (class "java.lang.Object")))) 
                                      (9 (astore_1)) 
                                      (10 (getstatic (fieldCP "const__0" "clojure.core$generate_proxy$iter__5215__5221$fn__5222" (class "clojure.lang.Var")))) ;;at TAG_5
                                      (13 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (16 (checkcast (class "clojure.lang.IFn"))) 
                                      (19 (aload_1)) 
                                      (20 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (25 (astore_2)) 
                                      (26 (aload_2)) 
                                      (27 (dup)) 
                                      (28 (ifnull 197)) ;;to TAG_0
                                      (31 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (34 (if_acmpeq 198)) ;;to TAG_1
                                      (37 (aload_2)) 
                                      (38 (aconst_null)) 
                                      (39 (astore_2)) 
                                      (40 (astore_3)) 
                                      (41 (getstatic (fieldCP "const__1" "clojure.core$generate_proxy$iter__5215__5221$fn__5222" (class "clojure.lang.Var")))) 
                                      (44 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (47 (checkcast (class "clojure.lang.IFn"))) 
                                      (50 (aload_3)) 
                                      (51 (aconst_null)) 
                                      (52 (astore_3)) 
                                      (53 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (58 (astore 4)) 
                                      (60 (new (class "clojure.core$generate_proxy$iter__5215__5221$fn__5222$iter__5217__5223"))) 
                                      (63 (dup)) 
                                      (64 (aload_0)) 
                                      (65 (getfield (fieldCP "considered" "clojure.core$generate_proxy$iter__5215__5221$fn__5222" (class "java.lang.Object")))) 
                                      (68 (invokespecial (methodCP "<init>" "clojure.core$generate_proxy$iter__5215__5221$fn__5222$iter__5217__5223" ((class "java.lang.Object")) void))) 
                                      (71 (astore 5)) 
                                      (73 (getstatic (fieldCP "const__0" "clojure.core$generate_proxy$iter__5215__5221$fn__5222" (class "clojure.lang.Var")))) 
                                      (76 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (79 (checkcast (class "clojure.lang.IFn"))) 
                                      (82 (aload 5)) 
                                      (84 (aconst_null)) 
                                      (85 (astore 5)) 
                                      (87 (checkcast (class "clojure.lang.IFn"))) 
                                      (90 (aload 4)) 
                                      (92 (aconst_null)) 
                                      (93 (astore 4)) 
                                      (95 (checkcast (class "java.lang.Class"))) 
                                      (98 (invokevirtual (methodCP "getMethods" "java.lang.Class" () (array (class "java.lang.reflect.Method"))))) 
                                      (101 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (106 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (111 (astore 6)) 
                                      (113 (aload 6)) 
                                      (115 (dup)) 
                                      (116 (ifnull 174))  ;;to TAG_2
                                      (119 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (122 (if_acmpeq 175)) ;;to TAG_3
                                      (125 (getstatic (fieldCP "const__2" "clojure.core$generate_proxy$iter__5215__5221$fn__5222" (class "clojure.lang.Var")))) 
                                      (128 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (131 (checkcast (class "clojure.lang.IFn"))) 
                                      (134 (aload 6)) 
                                      (136 (aconst_null)) 
                                      (137 (astore 6)) 
                                      (139 (aload_0)) 
                                      (140 (getfield (fieldCP "iter__5215" "clojure.core$generate_proxy$iter__5215__5221$fn__5222" (class "java.lang.Object")))) 
                                      (143 (checkcast (class "clojure.lang.IFn"))) 
                                      (146 (getstatic (fieldCP "const__3" "clojure.core$generate_proxy$iter__5215__5221$fn__5222" (class "clojure.lang.Var")))) 
                                      (149 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (152 (checkcast (class "clojure.lang.IFn"))) 
                                      (155 (aload_1)) 
                                      (156 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (161 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (166 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (171 (goto 194)) ;;to TAG_4
                                      (174 (pop)) ;;at TAG_2
                                      (175 (getstatic (fieldCP "const__3" "clojure.core$generate_proxy$iter__5215__5221$fn__5222" (class "clojure.lang.Var")))) ;;at TAG_3
                                      (178 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (181 (checkcast (class "clojure.lang.IFn"))) 
                                      (184 (aload_1)) 
                                      (185 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (190 (astore_1)) 
                                      (191 (goto 10)) ;;to TAG_5
                                      (194 (goto 199)) ;;to TAG_6;;at TAG_4
                                      (197 (pop)) ;;at TAG_0
                                      (198 (aconst_null)) ;;at TAG_1
                                      (199 (areturn)) ;;at TAG_6
                                      (endofcode 200))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$generate_proxy$iter__5215__5221$fn__5222-class-table*
  (make-static-class-decls 
   *clojure.core$generate_proxy$iter__5215__5221$fn__5222*))

(defconst *package-name-map* 
  ("clojure.core$generate_proxy$iter__5215__5221$fn__5222" . "clojure"))

