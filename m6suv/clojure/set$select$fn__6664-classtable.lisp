; set$select$fn__6664-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:58 CDT 2014.
;

(defconst *clojure.set$select$fn__6664*
 (make-class-def
      '(class "clojure.set$select$fn__6664"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "disj"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "pred" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 14)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "disj"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.set$select$fn__6664" (class "clojure.lang.Var"))))
                                      (13 (return))
                                      (endofcode 14))
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
                                      (6 (putfield (fieldCP "pred" "clojure.set$select$fn__6664" (class "java.lang.Object"))))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 51)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "pred" "clojure.set$select$fn__6664" (class "java.lang.Object")))) 
                                      (4 (checkcast (class "clojure.lang.IFn"))) 
                                      (7 (aload_2)) 
                                      (8 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (13 (dup)) 
                                      (14 (ifnull 29)) ;;to TAG_0
                                      (17 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (20 (if_acmpeq 30)) ;;to TAG_1
                                      (23 (aload_1)) 
                                      (24 (aconst_null)) 
                                      (25 (astore_1)) 
                                      (26 (goto 50))  ;;to TAG_2
                                      (29 (pop)) ;;at TAG_0
                                      (30 (getstatic (fieldCP "const__0" "clojure.set$select$fn__6664" (class "clojure.lang.Var")))) ;;at TAG_1
                                      (33 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (36 (checkcast (class "clojure.lang.IFn"))) 
                                      (39 (aload_1)) 
                                      (40 (aconst_null)) 
                                      (41 (astore_1)) 
                                      (42 (aload_2)) 
                                      (43 (aconst_null)) 
                                      (44 (astore_2)) 
                                      (45 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (50 (areturn)) ;;at TAG_2
                                      (endofcode 51))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *set$select$fn__6664-class-table*
  (make-static-class-decls 
   *clojure.set$select$fn__6664*))

(defconst *package-name-map* 
  ("clojure.set$select$fn__6664" . "clojure"))

