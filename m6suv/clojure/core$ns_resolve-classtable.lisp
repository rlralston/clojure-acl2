; core$ns_resolve-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:44 CDT 2014.
;

(defconst *clojure.core$ns_resolve*
 (make-class-def
      '(class "clojure.core$ns_resolve"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "ns-resolve")
                        (STRING  "contains?")
                        (STRING  "the-ns"))
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
                                      (2 (ldc 1))         ;;STRING:: "ns-resolve"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$ns_resolve" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "contains?"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$ns_resolve" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "the-ns"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.core$ns_resolve" (class "clojure.lang.Var"))))
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
					(methodCP "<init>" "clojure.lang.AFunction" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 4) (code_length . 63)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__1" "clojure.core$ns_resolve" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (aload_2)) 
                                      (10 (aconst_null)) 
                                      (11 (astore_2)) 
                                      (12 (aload_3)) 
                                      (13 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (18 (dup)) 
                                      (19 (ifnull 32)) ;;to TAG_0
                                      (22 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (25 (if_acmpeq 33)) ;;to TAG_1
                                      (28 (aconst_null)) 
                                      (29 (goto 62))  ;;to TAG_2
                                      (32 (pop)) ;;at TAG_0
                                      (33 (getstatic (fieldCP "const__2" "clojure.core$ns_resolve" (class "clojure.lang.Var")))) ;;at TAG_1
                                      (36 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (39 (checkcast (class "clojure.lang.IFn"))) 
                                      (42 (aload_1)) 
                                      (43 (aconst_null)) 
                                      (44 (astore_1)) 
                                      (45 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (50 (checkcast (class "clojure.lang.Namespace"))) 
                                      (53 (aload_3)) 
                                      (54 (aconst_null)) 
                                      (55 (astore_3)) 
                                      (56 (checkcast (class "clojure.lang.Symbol"))) 
                                      (59 (invokestatic (methodCP "maybeResolveIn" "clojure.lang.Compiler" ((class "clojure.lang.Namespace") (class "clojure.lang.Symbol")) (class "java.lang.Object")))) 
                                      (62 (areturn)) ;;at TAG_2
                                      (endofcode 63))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 3) (code_length . 22)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$ns_resolve" (class "clojure.lang.Var"))))
                                      (3 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (6 (checkcast (class "clojure.lang.IFn")))
                                      (9 (aload_1))
                                      (10 (aconst_null))
                                      (11 (astore_1))
                                      (12 (aconst_null))
                                      (13 (aload_2))
                                      (14 (aconst_null))
                                      (15 (astore_2))
                                      (16 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4))
                                      (21 (areturn))
                                      (endofcode 22))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$ns_resolve-class-table*
  (make-static-class-decls 
   *clojure.core$ns_resolve*))

(defconst *package-name-map* 
  ("clojure.core$ns_resolve" . "clojure"))

