; core$generate_class$fn__5607-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:43 CDT 2014.
;

(defconst *clojure.core$generate_class$fn__5607*
 (make-class-def
      '(class "clojure.core$generate_class$fn__5607"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "contains?")
                        (STRING  "method-sig")
                        (STRING  "assoc"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "emit_forwarding_method" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "emit_unsupported" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 40)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "contains?"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$generate_class$fn__5607" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "method-sig"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$generate_class$fn__5607" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "assoc"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.core$generate_class$fn__5607" (class "clojure.lang.Var"))))
                                      (39 (return))
                                      (endofcode 40))
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
                                      (6 (putfield (fieldCP "emit_forwarding_method" "clojure.core$generate_class$fn__5607" (class "java.lang.Object"))))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "emit_unsupported" "clojure.core$generate_class$fn__5607" (class "java.lang.Object"))))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 3) (code_length . 124)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$generate_class$fn__5607" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (aload_1)) 
                                      (10 (getstatic (fieldCP "const__1" "clojure.core$generate_class$fn__5607" (class "clojure.lang.Var")))) 
                                      (13 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (16 (checkcast (class "clojure.lang.IFn"))) 
                                      (19 (aload_2)) 
                                      (20 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (25 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (30 (dup)) 
                                      (31 (ifnull 46)) ;;to TAG_0
                                      (34 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (37 (if_acmpeq 47)) ;;to TAG_1
                                      (40 (aload_1)) 
                                      (41 (aconst_null)) 
                                      (42 (astore_1)) 
                                      (43 (goto 123))  ;;to TAG_2
                                      (46 (pop)) ;;at TAG_0
                                      (47 (aload_0)) ;;at TAG_1
                                      (48 (getfield (fieldCP "emit_forwarding_method" "clojure.core$generate_class$fn__5607" (class "java.lang.Object")))) 
                                      (51 (checkcast (class "clojure.lang.IFn"))) 
                                      (54 (aload_2)) 
                                      (55 (checkcast (class "java.lang.reflect.Method"))) 
                                      (58 (invokevirtual (methodCP "getName" "java.lang.reflect.Method" () (class "java.lang.String")))) 
                                      (61 (aload_2)) 
                                      (62 (checkcast (class "java.lang.reflect.Method"))) 
                                      (65 (invokevirtual (methodCP "getParameterTypes" "java.lang.reflect.Method" () (array (class "java.lang.Class"))))) 
                                      (68 (aload_2)) 
                                      (69 (checkcast (class "java.lang.reflect.Method"))) 
                                      (72 (invokevirtual (methodCP "getReturnType" "java.lang.reflect.Method" () (class "java.lang.Class")))) 
                                      (75 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (78 (aload_0)) 
                                      (79 (getfield (fieldCP "emit_unsupported" "clojure.core$generate_class$fn__5607" (class "java.lang.Object")))) 
                                      (82 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 6)) 
                                      (87 (pop)) 
                                      (88 (getstatic (fieldCP "const__2" "clojure.core$generate_class$fn__5607" (class "clojure.lang.Var")))) 
                                      (91 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (94 (checkcast (class "clojure.lang.IFn"))) 
                                      (97 (aload_1)) 
                                      (98 (aconst_null)) 
                                      (99 (astore_1)) 
                                      (100 (getstatic (fieldCP "const__1" "clojure.core$generate_class$fn__5607" (class "clojure.lang.Var")))) 
                                      (103 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (106 (checkcast (class "clojure.lang.IFn"))) 
                                      (109 (aload_2)) 
                                      (110 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (115 (aload_2)) 
                                      (116 (aconst_null)) 
                                      (117 (astore_2)) 
                                      (118 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) 
                                      (123 (areturn)) ;;at TAG_2
                                      (endofcode 124))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$generate_class$fn__5607-class-table*
  (make-static-class-decls 
   *clojure.core$generate_class$fn__5607*))

(defconst *package-name-map* 
  ("clojure.core$generate_class$fn__5607" . "clojure"))

