; core$emit_protocol$fn__5912$fn__5914-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:42 CDT 2014.
;

(defconst *clojure.core$emit_protocol$fn__5912$fn__5914*
 (make-class-def
      '(class "clojure.core$emit_protocol$fn__5912$fn__5914"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "rest")
                        (STRING  "vector?")
                        (STRING  "first")
                        (STRING  "conj")
                        (STRING  "next")
                        (STRING  "seq"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "s" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 79)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "rest"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$emit_protocol$fn__5912$fn__5914" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "vector?"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$emit_protocol$fn__5912$fn__5914" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "first"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.core$emit_protocol$fn__5912$fn__5914" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 4))        ;;STRING:: "conj"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.core$emit_protocol$fn__5912$fn__5914" (class "clojure.lang.Var"))))
                                      (52 (ldc 0))        ;;STRING:: "clojure.core"
                                      (54 (ldc 5))        ;;STRING:: "next"
                                      (56 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (59 (checkcast (class "clojure.lang.Var")))
                                      (62 (putstatic (fieldCP "const__4" "clojure.core$emit_protocol$fn__5912$fn__5914" (class "clojure.lang.Var"))))
                                      (65 (ldc 0))        ;;STRING:: "clojure.core"
                                      (67 (ldc 6))        ;;STRING:: "seq"
                                      (69 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (72 (checkcast (class "clojure.lang.Var")))
                                      (75 (putstatic (fieldCP "const__5" "clojure.core$emit_protocol$fn__5912$fn__5914" (class "clojure.lang.Var"))))
                                      (78 (return))
                                      (endofcode 79))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.AFunction" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "s" "clojure.core$emit_protocol$fn__5912$fn__5914" (class "java.lang.Object"))))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 3) (code_length . 165)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "EMPTY" "clojure.lang.PersistentVector" (class "clojure.lang.PersistentVector")))) 
                                      (3 (astore_1)) 
                                      (4 (getstatic (fieldCP "const__0" "clojure.core$emit_protocol$fn__5912$fn__5914" (class "clojure.lang.Var")))) 
                                      (7 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (10 (checkcast (class "clojure.lang.IFn"))) 
                                      (13 (aload_0)) 
                                      (14 (getfield (fieldCP "s" "clojure.core$emit_protocol$fn__5912$fn__5914" (class "java.lang.Object")))) 
                                      (17 (aload_0)) 
                                      (18 (aconst_null)) 
                                      (19 (putfield (fieldCP "s" "clojure.core$emit_protocol$fn__5912$fn__5914" (class "java.lang.Object")))) 
                                      (22 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (27 (astore_2)) 
                                      (28 (getstatic (fieldCP "const__1" "clojure.core$emit_protocol$fn__5912$fn__5914" (class "clojure.lang.Var")))) ;;at TAG_2
                                      (31 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (34 (checkcast (class "clojure.lang.IFn"))) 
                                      (37 (getstatic (fieldCP "const__2" "clojure.core$emit_protocol$fn__5912$fn__5914" (class "clojure.lang.Var")))) 
                                      (40 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (43 (checkcast (class "clojure.lang.IFn"))) 
                                      (46 (aload_2)) 
                                      (47 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (52 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (57 (dup)) 
                                      (58 (ifnull 120)) ;;to TAG_0
                                      (61 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (64 (if_acmpeq 121)) ;;to TAG_1
                                      (67 (getstatic (fieldCP "const__3" "clojure.core$emit_protocol$fn__5912$fn__5914" (class "clojure.lang.Var")))) 
                                      (70 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (73 (checkcast (class "clojure.lang.IFn"))) 
                                      (76 (aload_1)) 
                                      (77 (getstatic (fieldCP "const__2" "clojure.core$emit_protocol$fn__5912$fn__5914" (class "clojure.lang.Var")))) 
                                      (80 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (83 (checkcast (class "clojure.lang.IFn"))) 
                                      (86 (aload_2)) 
                                      (87 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (92 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (97 (getstatic (fieldCP "const__4" "clojure.core$emit_protocol$fn__5912$fn__5914" (class "clojure.lang.Var")))) 
                                      (100 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (103 (checkcast (class "clojure.lang.IFn"))) 
                                      (106 (aload_2)) 
                                      (107 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (112 (astore_2)) 
                                      (113 (astore_1)) 
                                      (114 (goto 28))  ;;to TAG_2
                                      (117 (goto 164)) ;;to TAG_3
                                      (120 (pop)) ;;at TAG_0
                                      (121 (iconst_2)) ;;at TAG_1
                                      (122 (anewarray (class "java.lang.Object"))) 
                                      (125 (dup)) 
                                      (126 (iconst_0)) 
                                      (127 (getstatic (fieldCP "const__5" "clojure.core$emit_protocol$fn__5912$fn__5914" (class "clojure.lang.Var")))) 
                                      (130 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (133 (checkcast (class "clojure.lang.IFn"))) 
                                      (136 (aload_1)) 
                                      (137 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (142 (aastore)) 
                                      (143 (dup)) 
                                      (144 (iconst_1)) 
                                      (145 (getstatic (fieldCP "const__2" "clojure.core$emit_protocol$fn__5912$fn__5914" (class "clojure.lang.Var")))) 
                                      (148 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (151 (checkcast (class "clojure.lang.IFn"))) 
                                      (154 (aload_2)) 
                                      (155 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (160 (aastore)) 
                                      (161 (invokestatic (methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector")))) 
                                      (164 (areturn)) ;;at TAG_3
                                      (endofcode 165))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$emit_protocol$fn__5912$fn__5914-class-table*
  (make-static-class-decls 
   *clojure.core$emit_protocol$fn__5912$fn__5914*))

(defconst *package-name-map* 
  ("clojure.core$emit_protocol$fn__5912$fn__5914" . "clojure"))

