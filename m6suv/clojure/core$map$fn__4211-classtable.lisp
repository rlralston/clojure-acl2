; core$map$fn__4211-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:44 CDT 2014.
;

(defconst *clojure.core$map$fn__4211*
 (make-class-def
      '(class "clojure.core$map$fn__4211"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "seq")
                        (STRING  "cons")
                        (STRING  "first")
                        (STRING  "map")
                        (STRING  "rest"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "f" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "c2" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "c1" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 66)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "seq"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$map$fn__4211" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "cons"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$map$fn__4211" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "first"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.core$map$fn__4211" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 4))        ;;STRING:: "map"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.core$map$fn__4211" (class "clojure.lang.Var"))))
                                      (52 (ldc 0))        ;;STRING:: "clojure.core"
                                      (54 (ldc 5))        ;;STRING:: "rest"
                                      (56 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (59 (checkcast (class "clojure.lang.Var")))
                                      (62 (putstatic (fieldCP "const__4" "clojure.core$map$fn__4211" (class "clojure.lang.Var"))))
                                      (65 (return))
                                      (endofcode 66))
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
                                      (6 (putfield (fieldCP "f" "clojure.core$map$fn__4211" (class "java.lang.Object"))))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "c2" "clojure.core$map$fn__4211" (class "java.lang.Object"))))
                                      (14 (aload_0))
                                      (15 (aload_3))
                                      (16 (putfield (fieldCP "c1" "clojure.core$map$fn__4211" (class "java.lang.Object"))))
                                      (19 (return))
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 8) (max_locals . 4) (code_length . 198)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$map$fn__4211" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (aload_0)) 
                                      (10 (getfield (fieldCP "c1" "clojure.core$map$fn__4211" (class "java.lang.Object")))) 
                                      (13 (aload_0)) 
                                      (14 (aconst_null)) 
                                      (15 (putfield (fieldCP "c1" "clojure.core$map$fn__4211" (class "java.lang.Object")))) 
                                      (18 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (23 (astore_1)) 
                                      (24 (getstatic (fieldCP "const__0" "clojure.core$map$fn__4211" (class "clojure.lang.Var")))) 
                                      (27 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (30 (checkcast (class "clojure.lang.IFn"))) 
                                      (33 (aload_0)) 
                                      (34 (getfield (fieldCP "c2" "clojure.core$map$fn__4211" (class "java.lang.Object")))) 
                                      (37 (aload_0)) 
                                      (38 (aconst_null)) 
                                      (39 (putfield (fieldCP "c2" "clojure.core$map$fn__4211" (class "java.lang.Object")))) 
                                      (42 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (47 (astore_2)) 
                                      (48 (aload_1)) 
                                      (49 (astore_3)) 
                                      (50 (aload_3)) 
                                      (51 (dup)) 
                                      (52 (ifnull 65)) ;;to TAG_0
                                      (55 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (58 (if_acmpeq 66)) ;;to TAG_1
                                      (61 (aload_2)) 
                                      (62 (goto 69))  ;;to TAG_2
                                      (65 (pop)) ;;at TAG_0
                                      (66 (aload_3)) ;;at TAG_1
                                      (67 (aconst_null)) 
                                      (68 (astore_3)) 
                                      (69 (dup)) ;;at TAG_2
                                      (70 (ifnull 195)) ;;to TAG_3
                                      (73 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (76 (if_acmpeq 196)) ;;to TAG_4
                                      (79 (getstatic (fieldCP "const__1" "clojure.core$map$fn__4211" (class "clojure.lang.Var")))) 
                                      (82 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (85 (checkcast (class "clojure.lang.IFn"))) 
                                      (88 (aload_0)) 
                                      (89 (getfield (fieldCP "f" "clojure.core$map$fn__4211" (class "java.lang.Object")))) 
                                      (92 (checkcast (class "clojure.lang.IFn"))) 
                                      (95 (getstatic (fieldCP "const__2" "clojure.core$map$fn__4211" (class "clojure.lang.Var")))) 
                                      (98 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (101 (checkcast (class "clojure.lang.IFn"))) 
                                      (104 (aload_1)) 
                                      (105 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (110 (getstatic (fieldCP "const__2" "clojure.core$map$fn__4211" (class "clojure.lang.Var")))) 
                                      (113 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (116 (checkcast (class "clojure.lang.IFn"))) 
                                      (119 (aload_2)) 
                                      (120 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (125 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (130 (getstatic (fieldCP "const__3" "clojure.core$map$fn__4211" (class "clojure.lang.Var")))) 
                                      (133 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (136 (checkcast (class "clojure.lang.IFn"))) 
                                      (139 (aload_0)) 
                                      (140 (getfield (fieldCP "f" "clojure.core$map$fn__4211" (class "java.lang.Object")))) 
                                      (143 (aload_0)) 
                                      (144 (aconst_null)) 
                                      (145 (putfield (fieldCP "f" "clojure.core$map$fn__4211" (class "java.lang.Object")))) 
                                      (148 (getstatic (fieldCP "const__4" "clojure.core$map$fn__4211" (class "clojure.lang.Var")))) 
                                      (151 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (154 (checkcast (class "clojure.lang.IFn"))) 
                                      (157 (aload_1)) 
                                      (158 (aconst_null)) 
                                      (159 (astore_1)) 
                                      (160 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (165 (getstatic (fieldCP "const__4" "clojure.core$map$fn__4211" (class "clojure.lang.Var")))) 
                                      (168 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (171 (checkcast (class "clojure.lang.IFn"))) 
                                      (174 (aload_2)) 
                                      (175 (aconst_null)) 
                                      (176 (astore_2)) 
                                      (177 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (182 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) 
                                      (187 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (192 (goto 197)) ;;to TAG_5
                                      (195 (pop)) ;;at TAG_3
                                      (196 (aconst_null)) ;;at TAG_4
                                      (197 (areturn)) ;;at TAG_5
                                      (endofcode 198))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$map$fn__4211-class-table*
  (make-static-class-decls 
   *clojure.core$map$fn__4211*))

(defconst *package-name-map* 
  ("clojure.core$map$fn__4211" . "clojure"))

