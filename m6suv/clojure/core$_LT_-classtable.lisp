; core$_LT_-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:46 CDT 2014.
;

(defconst *clojure.core$_LT_*
 (make-class-def
      '(class "clojure.core$_LT_"
            "clojure.lang.RestFn"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "<")
                        (STRING  "next")
                        (STRING  "first"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 40)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "<"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$_LT_" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "next"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$_LT_" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "first"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.core$_LT_" (class "clojure.lang.Var"))))
                                      (39 (return))
                                      (endofcode 40))
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
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 4) (code_length . 123)
                                   (parsedcode
                                      (0 (aload_1)) ;;at TAG_3
                                      (1 (aconst_null)) 
                                      (2 (astore_1)) 
                                      (3 (aload_2)) 
                                      (4 (invokestatic (methodCP "lt" "clojure.lang.Numbers" ((class "java.lang.Object") (class "java.lang.Object")) boolean))) 
                                      (7 (ifeq 119)) ;;to TAG_0
                                      (10 (getstatic (fieldCP "const__1" "clojure.core$_LT_" (class "clojure.lang.Var")))) 
                                      (13 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (16 (checkcast (class "clojure.lang.IFn"))) 
                                      (19 (aload_3)) 
                                      (20 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (25 (dup)) 
                                      (26 (ifnull 79)) ;;to TAG_1
                                      (29 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (32 (if_acmpeq 80))  ;;to TAG_2
                                      (35 (aload_2)) 
                                      (36 (aconst_null)) 
                                      (37 (astore_2)) 
                                      (38 (getstatic (fieldCP "const__2" "clojure.core$_LT_" (class "clojure.lang.Var")))) 
                                      (41 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (44 (checkcast (class "clojure.lang.IFn"))) 
                                      (47 (aload_3)) 
                                      (48 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (53 (getstatic (fieldCP "const__1" "clojure.core$_LT_" (class "clojure.lang.Var")))) 
                                      (56 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (59 (checkcast (class "clojure.lang.IFn"))) 
                                      (62 (aload_3)) 
                                      (63 (aconst_null)) 
                                      (64 (astore_3)) 
                                      (65 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (70 (astore_3)) 
                                      (71 (astore_2)) 
                                      (72 (astore_1)) 
                                      (73 (goto 0)) ;;to TAG_3
                                      (76 (goto 115)) ;;to TAG_4
                                      (79 (pop)) ;;at TAG_1
                                      (80 (aload_2)) ;;at TAG_2
                                      (81 (aconst_null)) 
                                      (82 (astore_2)) 
                                      (83 (getstatic (fieldCP "const__2" "clojure.core$_LT_" (class "clojure.lang.Var")))) 
                                      (86 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (89 (checkcast (class "clojure.lang.IFn"))) 
                                      (92 (aload_3)) 
                                      (93 (aconst_null)) 
                                      (94 (astore_3)) 
                                      (95 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (100 (invokestatic (methodCP "lt" "clojure.lang.Numbers" ((class "java.lang.Object") (class "java.lang.Object")) boolean))) 
                                      (103 (ifeq 112)) ;;to TAG_5
                                      (106 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (109 (goto 115)) ;;to TAG_4
                                      (112 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) ;;at TAG_5
                                      (115 (goto 122)) ;;to TAG_6;;at TAG_4
                                      (118 (pop)) 
                                      (119 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) ;;at TAG_0
                                      (122 (areturn)) ;;at TAG_6
                                      (endofcode 123))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 22)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (aconst_null)) 
                                      (2 (astore_1)) 
                                      (3 (aload_2)) 
                                      (4 (aconst_null)) 
                                      (5 (astore_2)) 
                                      (6 (invokestatic (methodCP "lt" "clojure.lang.Numbers" ((class "java.lang.Object") (class "java.lang.Object")) boolean))) 
                                      (9 (ifeq 18))  ;;to TAG_0
                                      (12 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (15 (goto 21)) ;;to TAG_1
                                      (18 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) ;;at TAG_0
                                      (21 (areturn)) ;;at TAG_1
                                      (endofcode 22))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 4)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean"))))
                                      (3 (areturn))
                                      (endofcode 4))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getRequiredArity"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 2)
                                   (parsedcode
                                      (0 (iconst_2))
                                      (1 (ireturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$_LT_-class-table*
  (make-static-class-decls 
   *clojure.core$_LT_*))

(defconst *package-name-map* 
  ("clojure.core$_LT_" . "clojure"))

