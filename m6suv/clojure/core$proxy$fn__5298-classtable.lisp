; core$proxy$fn__5298-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:45 CDT 2014.
;

(defconst *clojure.core$proxy$fn__5298*
 (make-class-def
      '(class "clojure.core$proxy$fn__5298"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "resolve")
                        (STRING  "str")
                        (STRING  "Can\nt resolve: "))
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
                                      (2 (ldc 1))         ;;STRING:: "resolve"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$proxy$fn__5298" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "str"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$proxy$fn__5298" (class "clojure.lang.Var"))))
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
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 3) (code_length . 68)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$proxy$fn__5298" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (aload_1)) 
                                      (10 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (15 (astore_2)) 
                                      (16 (aload_2)) 
                                      (17 (dup)) 
                                      (18 (ifnull 33)) ;;to TAG_0
                                      (21 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (24 (if_acmpeq 34)) ;;to TAG_1
                                      (27 (aload_2)) 
                                      (28 (aconst_null)) 
                                      (29 (astore_2)) 
                                      (30 (goto 67))  ;;to TAG_2
                                      (33 (pop)) ;;at TAG_0
                                      (34 (new (class "java.lang.Exception"))) ;;at TAG_1
                                      (37 (dup)) 
                                      (38 (getstatic (fieldCP "const__1" "clojure.core$proxy$fn__5298" (class "clojure.lang.Var")))) 
                                      (41 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (44 (checkcast (class "clojure.lang.IFn"))) 
                                      (47 (ldc 3)) ;;STRING:: "Can\nt resolve: "
                                      (49 (aload_1)) 
                                      (50 (aconst_null)) 
                                      (51 (astore_1)) 
                                      (52 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (57 (checkcast (class "java.lang.String"))) 
                                      (60 (invokespecial (methodCP "<init>" "java.lang.Exception" ((class "java.lang.String")) void))) 
                                      (63 (checkcast (class "java.lang.Throwable"))) 
                                      (66 (athrow)) 
                                      (67 (areturn)) ;;at TAG_2
                                      (endofcode 68))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$proxy$fn__5298-class-table*
  (make-static-class-decls 
   *clojure.core$proxy$fn__5298*))

(defconst *package-name-map* 
  ("clojure.core$proxy$fn__5298" . "clojure"))

