; pprint$check_enumerated_arg-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:55 CDT 2014.
;

(defconst *clojure.pprint$check_enumerated_arg*
 (make-class-def
      '(class "clojure.pprint$check_enumerated_arg"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "not")
                        (STRING  "str")
                        (STRING  "Bad argument: ")
                        (STRING  ". It must be one of "))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 27)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "not"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.pprint$check_enumerated_arg" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "str"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.pprint$check_enumerated_arg" (class "clojure.lang.Var"))))
                                      (26 (return))
                                      (endofcode 27))
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
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 8) (max_locals . 3) (code_length . 78)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.pprint$check_enumerated_arg" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (aload_2)) 
                                      (10 (checkcast (class "clojure.lang.IFn"))) 
                                      (13 (aload_1)) 
                                      (14 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (19 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (24 (dup)) 
                                      (25 (ifnull 75)) ;;to TAG_0
                                      (28 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (31 (if_acmpeq 76)) ;;to TAG_1
                                      (34 (new (class "java.lang.IllegalArgumentException"))) 
                                      (37 (dup)) 
                                      (38 (getstatic (fieldCP "const__1" "clojure.pprint$check_enumerated_arg" (class "clojure.lang.Var")))) 
                                      (41 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (44 (checkcast (class "clojure.lang.IFn"))) 
                                      (47 (ldc 3)) ;;STRING:: "Bad argument: "
                                      (49 (aload_1)) 
                                      (50 (aconst_null)) 
                                      (51 (astore_1)) 
                                      (52 (ldc 4)) ;;STRING:: ". It must be one of "
                                      (54 (aload_2)) 
                                      (55 (aconst_null)) 
                                      (56 (astore_2)) 
                                      (57 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 5)) 
                                      (62 (checkcast (class "java.lang.String"))) 
                                      (65 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (68 (checkcast (class "java.lang.Throwable"))) 
                                      (71 (athrow)) 
                                      (72 (goto 77))  ;;to TAG_2
                                      (75 (pop)) ;;at TAG_0
                                      (76 (aconst_null)) ;;at TAG_1
                                      (77 (areturn)) ;;at TAG_2
                                      (endofcode 78))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *pprint$check_enumerated_arg-class-table*
  (make-static-class-decls 
   *clojure.pprint$check_enumerated_arg*))

(defconst *package-name-map* 
  ("clojure.pprint$check_enumerated_arg" . "clojure"))

