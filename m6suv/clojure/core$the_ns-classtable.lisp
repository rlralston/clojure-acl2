; core$the_ns-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:45 CDT 2014.
;

(defconst *clojure.core$the_ns*
 (make-class-def
      '(class "clojure.core$the_ns"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "instance?")
                        (STRING  "find-ns")
                        (STRING  "str")
                        (STRING  "No namespace: ")
                        (STRING  " found"))
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
                                      (2 (ldc 1))         ;;STRING:: "instance?"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$the_ns" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "find-ns"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$the_ns" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "str"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.core$the_ns" (class "clojure.lang.Var"))))
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
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 3) (code_length . 84)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (instanceof (class "clojure.lang.Namespace"))) 
                                      (4 (ifeq 14)) ;;to TAG_0
                                      (7 (aload_1)) 
                                      (8 (aconst_null)) 
                                      (9 (astore_1)) 
                                      (10 (goto 83)) ;;to TAG_1
                                      (13 (pop)) 
                                      (14 (getstatic (fieldCP "const__1" "clojure.core$the_ns" (class "clojure.lang.Var")))) ;;at TAG_0
                                      (17 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (20 (checkcast (class "clojure.lang.IFn"))) 
                                      (23 (aload_1)) 
                                      (24 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (29 (astore_2)) 
                                      (30 (aload_2)) 
                                      (31 (dup)) 
                                      (32 (ifnull 47))  ;;to TAG_2
                                      (35 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (38 (if_acmpeq 48)) ;;to TAG_3
                                      (41 (aload_2)) 
                                      (42 (aconst_null)) 
                                      (43 (astore_2)) 
                                      (44 (goto 83)) ;;to TAG_1
                                      (47 (pop)) ;;at TAG_2
                                      (48 (new (class "java.lang.Exception"))) ;;at TAG_3
                                      (51 (dup)) 
                                      (52 (getstatic (fieldCP "const__2" "clojure.core$the_ns" (class "clojure.lang.Var")))) 
                                      (55 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (58 (checkcast (class "clojure.lang.IFn"))) 
                                      (61 (ldc 4)) ;;STRING:: "No namespace: "
                                      (63 (aload_1)) 
                                      (64 (aconst_null)) 
                                      (65 (astore_1)) 
                                      (66 (ldc 5)) ;;STRING:: " found"
                                      (68 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) 
                                      (73 (checkcast (class "java.lang.String"))) 
                                      (76 (invokespecial (methodCP "<init>" "java.lang.Exception" ((class "java.lang.String")) void))) 
                                      (79 (checkcast (class "java.lang.Throwable"))) 
                                      (82 (athrow)) 
                                      (83 (areturn)) ;;at TAG_1
                                      (endofcode 84))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$the_ns-class-table*
  (make-static-class-decls 
   *clojure.core$the_ns*))

(defconst *package-name-map* 
  ("clojure.core$the_ns" . "clojure"))
