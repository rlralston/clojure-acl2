; core$interleave$fn__4528-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:44 CDT 2014.
;

(defconst *clojure.core$interleave$fn__4528*
 (make-class-def
      '(class "clojure.core$interleave$fn__4528"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "map")
                        (STRING  "seq")
                        (STRING  "conj")
                        (STRING  "every?")
                        (STRING  "identity")
                        (STRING  "concat")
                        (STRING  "first")
                        (STRING  "apply")
                        (STRING  "interleave")
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
                        (field "const__9" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "colls" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "c1" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "c2" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 131)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "map"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$interleave$fn__4528" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "seq"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$interleave$fn__4528" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "conj"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.core$interleave$fn__4528" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 4))        ;;STRING:: "every?"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.core$interleave$fn__4528" (class "clojure.lang.Var"))))
                                      (52 (ldc 0))        ;;STRING:: "clojure.core"
                                      (54 (ldc 5))        ;;STRING:: "identity"
                                      (56 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (59 (checkcast (class "clojure.lang.Var")))
                                      (62 (putstatic (fieldCP "const__4" "clojure.core$interleave$fn__4528" (class "clojure.lang.Var"))))
                                      (65 (ldc 0))        ;;STRING:: "clojure.core"
                                      (67 (ldc 6))        ;;STRING:: "concat"
                                      (69 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (72 (checkcast (class "clojure.lang.Var")))
                                      (75 (putstatic (fieldCP "const__5" "clojure.core$interleave$fn__4528" (class "clojure.lang.Var"))))
                                      (78 (ldc 0))        ;;STRING:: "clojure.core"
                                      (80 (ldc 7))        ;;STRING:: "first"
                                      (82 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (85 (checkcast (class "clojure.lang.Var")))
                                      (88 (putstatic (fieldCP "const__6" "clojure.core$interleave$fn__4528" (class "clojure.lang.Var"))))
                                      (91 (ldc 0))        ;;STRING:: "clojure.core"
                                      (93 (ldc 8))        ;;STRING:: "apply"
                                      (95 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (98 (checkcast (class "clojure.lang.Var")))
                                      (101 (putstatic (fieldCP "const__7" "clojure.core$interleave$fn__4528" (class "clojure.lang.Var"))))
                                      (104 (ldc 0))       ;;STRING:: "clojure.core"
                                      (106 (ldc 9))       ;;STRING:: "interleave"
                                      (108 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (111 (checkcast (class "clojure.lang.Var")))
                                      (114 (putstatic (fieldCP "const__8" "clojure.core$interleave$fn__4528" (class "clojure.lang.Var"))))
                                      (117 (ldc 0))       ;;STRING:: "clojure.core"
                                      (119 (ldc 10))      ;;STRING:: "rest"
                                      (121 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (124 (checkcast (class "clojure.lang.Var")))
                                      (127 (putstatic (fieldCP "const__9" "clojure.core$interleave$fn__4528" (class "clojure.lang.Var"))))
                                      (130 (return))
                                      (endofcode 131))
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
                                      (6 (putfield (fieldCP "colls" "clojure.core$interleave$fn__4528" (class "java.lang.Object"))))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "c1" "clojure.core$interleave$fn__4528" (class "java.lang.Object"))))
                                      (14 (aload_0))
                                      (15 (aload_3))
                                      (16 (putfield (fieldCP "c2" "clojure.core$interleave$fn__4528" (class "java.lang.Object"))))
                                      (19 (return))
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 8) (max_locals . 2) (code_length . 177)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$interleave$fn__4528" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (getstatic (fieldCP "const__1" "clojure.core$interleave$fn__4528" (class "clojure.lang.Var")))) 
                                      (12 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (15 (getstatic (fieldCP "const__2" "clojure.core$interleave$fn__4528" (class "clojure.lang.Var")))) 
                                      (18 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (21 (checkcast (class "clojure.lang.IFn"))) 
                                      (24 (aload_0)) 
                                      (25 (getfield (fieldCP "colls" "clojure.core$interleave$fn__4528" (class "java.lang.Object")))) 
                                      (28 (aload_0)) 
                                      (29 (aconst_null)) 
                                      (30 (putfield (fieldCP "colls" "clojure.core$interleave$fn__4528" (class "java.lang.Object")))) 
                                      (33 (aload_0)) 
                                      (34 (getfield (fieldCP "c2" "clojure.core$interleave$fn__4528" (class "java.lang.Object")))) 
                                      (37 (aload_0)) 
                                      (38 (aconst_null)) 
                                      (39 (putfield (fieldCP "c2" "clojure.core$interleave$fn__4528" (class "java.lang.Object")))) 
                                      (42 (aload_0)) 
                                      (43 (getfield (fieldCP "c1" "clojure.core$interleave$fn__4528" (class "java.lang.Object")))) 
                                      (46 (aload_0)) 
                                      (47 (aconst_null)) 
                                      (48 (putfield (fieldCP "c1" "clojure.core$interleave$fn__4528" (class "java.lang.Object")))) 
                                      (51 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) 
                                      (56 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (61 (astore_1)) 
                                      (62 (getstatic (fieldCP "const__3" "clojure.core$interleave$fn__4528" (class "clojure.lang.Var")))) 
                                      (65 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (68 (checkcast (class "clojure.lang.IFn"))) 
                                      (71 (getstatic (fieldCP "const__4" "clojure.core$interleave$fn__4528" (class "clojure.lang.Var")))) 
                                      (74 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (77 (aload_1)) 
                                      (78 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (83 (dup)) 
                                      (84 (ifnull 174)) ;;to TAG_0
                                      (87 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (90 (if_acmpeq 175)) ;;to TAG_1
                                      (93 (getstatic (fieldCP "const__5" "clojure.core$interleave$fn__4528" (class "clojure.lang.Var")))) 
                                      (96 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (99 (checkcast (class "clojure.lang.IFn"))) 
                                      (102 (getstatic (fieldCP "const__0" "clojure.core$interleave$fn__4528" (class "clojure.lang.Var")))) 
                                      (105 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (108 (checkcast (class "clojure.lang.IFn"))) 
                                      (111 (getstatic (fieldCP "const__6" "clojure.core$interleave$fn__4528" (class "clojure.lang.Var")))) 
                                      (114 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (117 (aload_1)) 
                                      (118 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (123 (getstatic (fieldCP "const__7" "clojure.core$interleave$fn__4528" (class "clojure.lang.Var")))) 
                                      (126 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (129 (checkcast (class "clojure.lang.IFn"))) 
                                      (132 (getstatic (fieldCP "const__8" "clojure.core$interleave$fn__4528" (class "clojure.lang.Var")))) 
                                      (135 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (138 (getstatic (fieldCP "const__0" "clojure.core$interleave$fn__4528" (class "clojure.lang.Var")))) 
                                      (141 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (144 (checkcast (class "clojure.lang.IFn"))) 
                                      (147 (getstatic (fieldCP "const__9" "clojure.core$interleave$fn__4528" (class "clojure.lang.Var")))) 
                                      (150 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (153 (aload_1)) 
                                      (154 (aconst_null)) 
                                      (155 (astore_1)) 
                                      (156 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (161 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (166 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (171 (goto 176))  ;;to TAG_2
                                      (174 (pop)) ;;at TAG_0
                                      (175 (aconst_null)) ;;at TAG_1
                                      (176 (areturn)) ;;at TAG_2
                                      (endofcode 177))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$interleave$fn__4528-class-table*
  (make-static-class-decls 
   *clojure.core$interleave$fn__4528*))

(defconst *package-name-map* 
  ("clojure.core$interleave$fn__4528" . "clojure"))

