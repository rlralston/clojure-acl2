; core$defmacro$fn__3880-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:41 CDT 2014.
;

(defconst *clojure.core$defmacro$fn__3880*
 (make-class-def
      '(class "clojure.core$defmacro$fn__3880"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "list")
                        (STRING  "first")
                        (STRING  "string?")
                        (STRING  "cons")
                        (STRING  "next")
                        (STRING  "map?"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "name" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "args" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 79)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "list"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$defmacro$fn__3880" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "first"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$defmacro$fn__3880" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "string?"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.core$defmacro$fn__3880" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 4))        ;;STRING:: "cons"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.core$defmacro$fn__3880" (class "clojure.lang.Var"))))
                                      (52 (ldc 0))        ;;STRING:: "clojure.core"
                                      (54 (ldc 5))        ;;STRING:: "next"
                                      (56 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (59 (checkcast (class "clojure.lang.Var")))
                                      (62 (putstatic (fieldCP "const__4" "clojure.core$defmacro$fn__3880" (class "clojure.lang.Var"))))
                                      (65 (ldc 0))        ;;STRING:: "clojure.core"
                                      (67 (ldc 6))        ;;STRING:: "map?"
                                      (69 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (72 (checkcast (class "clojure.lang.Var")))
                                      (75 (putstatic (fieldCP "const__5" "clojure.core$defmacro$fn__3880" (class "clojure.lang.Var"))))
                                      (78 (return))
                                      (endofcode 79))
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
                                      (6 (putfield (fieldCP "name" "clojure.core$defmacro$fn__3880" (class "java.lang.Object"))))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "args" "clojure.core$defmacro$fn__3880" (class "java.lang.Object"))))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 4) (code_length . 186)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$defmacro$fn__3880" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (aload_0)) 
                                      (10 (getfield (fieldCP "name" "clojure.core$defmacro$fn__3880" (class "java.lang.Object")))) 
                                      (13 (aload_0)) 
                                      (14 (aconst_null)) 
                                      (15 (putfield (fieldCP "name" "clojure.core$defmacro$fn__3880" (class "java.lang.Object")))) 
                                      (18 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (23 (astore_1)) 
                                      (24 (aload_0)) 
                                      (25 (getfield (fieldCP "args" "clojure.core$defmacro$fn__3880" (class "java.lang.Object")))) 
                                      (28 (aload_0)) 
                                      (29 (aconst_null)) 
                                      (30 (putfield (fieldCP "args" "clojure.core$defmacro$fn__3880" (class "java.lang.Object")))) 
                                      (33 (astore_2)) 
                                      (34 (getstatic (fieldCP "const__1" "clojure.core$defmacro$fn__3880" (class "clojure.lang.Var")))) ;;at TAG_2
                                      (37 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (40 (checkcast (class "clojure.lang.IFn"))) 
                                      (43 (aload_2)) 
                                      (44 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (49 (astore_3)) 
                                      (50 (getstatic (fieldCP "const__2" "clojure.core$defmacro$fn__3880" (class "clojure.lang.Var")))) 
                                      (53 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (56 (checkcast (class "clojure.lang.IFn"))) 
                                      (59 (aload_3)) 
                                      (60 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (65 (dup)) 
                                      (66 (ifnull 116)) ;;to TAG_0
                                      (69 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (72 (if_acmpeq 117)) ;;to TAG_1
                                      (75 (getstatic (fieldCP "const__3" "clojure.core$defmacro$fn__3880" (class "clojure.lang.Var")))) 
                                      (78 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (81 (checkcast (class "clojure.lang.IFn"))) 
                                      (84 (aload_3)) 
                                      (85 (aconst_null)) 
                                      (86 (astore_3)) 
                                      (87 (aload_1)) 
                                      (88 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (93 (getstatic (fieldCP "const__4" "clojure.core$defmacro$fn__3880" (class "clojure.lang.Var")))) 
                                      (96 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (99 (checkcast (class "clojure.lang.IFn"))) 
                                      (102 (aload_2)) 
                                      (103 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (108 (astore_2)) 
                                      (109 (astore_1)) 
                                      (110 (goto 34))  ;;to TAG_2
                                      (113 (goto 185)) ;;to TAG_3
                                      (116 (pop)) ;;at TAG_0
                                      (117 (getstatic (fieldCP "const__5" "clojure.core$defmacro$fn__3880" (class "clojure.lang.Var")))) ;;at TAG_1
                                      (120 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (123 (checkcast (class "clojure.lang.IFn"))) 
                                      (126 (aload_3)) 
                                      (127 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (132 (dup)) 
                                      (133 (ifnull 183)) ;;to TAG_4
                                      (136 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (139 (if_acmpeq 184)) ;;to TAG_5
                                      (142 (getstatic (fieldCP "const__3" "clojure.core$defmacro$fn__3880" (class "clojure.lang.Var")))) 
                                      (145 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (148 (checkcast (class "clojure.lang.IFn"))) 
                                      (151 (aload_3)) 
                                      (152 (aconst_null)) 
                                      (153 (astore_3)) 
                                      (154 (aload_1)) 
                                      (155 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (160 (getstatic (fieldCP "const__4" "clojure.core$defmacro$fn__3880" (class "clojure.lang.Var")))) 
                                      (163 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (166 (checkcast (class "clojure.lang.IFn"))) 
                                      (169 (aload_2)) 
                                      (170 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (175 (astore_2)) 
                                      (176 (astore_1)) 
                                      (177 (goto 34))  ;;to TAG_2
                                      (180 (goto 185)) ;;to TAG_3
                                      (183 (pop)) ;;at TAG_4
                                      (184 (aload_1)) ;;at TAG_5
                                      (185 (areturn)) ;;at TAG_3
                                      (endofcode 186))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$defmacro$fn__3880-class-table*
  (make-static-class-decls 
   *clojure.core$defmacro$fn__3880*))

(defconst *package-name-map* 
  ("clojure.core$defmacro$fn__3880" . "clojure"))

