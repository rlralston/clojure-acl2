; Compiler$TheVarExpr$Parser-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:51 CDT 2014.
;

(defconst *clojure.lang.Compiler$TheVarExpr$Parser*
 (make-class-def
      '(class "clojure.lang.Compiler$TheVarExpr$Parser"
            "java.lang.Object"
            (constant_pool
                        (STRING  "Unable to resolve var: ")
                        (STRING  " in this context"))
            (fields)
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "parse"
                              (parameters (class "clojure.lang.Compiler$C") (class "java.lang.Object"))
                              (returntype . (class "clojure.lang.Compiler$Expr"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 5) (code_length . 58)
                                   (parsedcode
                                      (0 (aload_2)) 
                                      (1 (invokestatic (methodCP "second" "clojure.lang.RT" ((class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (4 (checkcast (class "clojure.lang.Symbol"))) 
                                      (7 (astore_3)) 
                                      (8 (aload_3)) 
                                      (9 (iconst_0)) 
                                      (10 (invokestatic (methodCP "lookupVar" "clojure.lang.Compiler" ((class "clojure.lang.Symbol") boolean) (class "clojure.lang.Var")))) 
                                      (13 (astore 4)) 
                                      (15 (aload 4)) 
                                      (17 (ifnull 30))  ;;to TAG_0
                                      (20 (new (class "clojure.lang.Compiler$TheVarExpr"))) 
                                      (23 (dup)) 
                                      (24 (aload 4)) 
                                      (26 (invokespecial (methodCP "<init>" "clojure.lang.Compiler$TheVarExpr" ((class "clojure.lang.Var")) void))) 
                                      (29 (areturn)) 
                                      (30 (new (class "java.lang.StringBuilder"))) ;;at TAG_0
                                      (33 (dup)) 
                                      (34 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (37 (ldc 0)) ;;STRING:: "Unable to resolve var: "
                                      (39 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (42 (aload_3)) 
                                      (43 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.Object")) (class "java.lang.StringBuilder")))) 
                                      (46 (ldc 1)) ;;STRING:: " in this context"
                                      (48 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (51 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (54 (invokestatic (methodCP "runtimeException" "clojure.lang.Util" ((class "java.lang.String")) (class "java.lang.RuntimeException")))) 
                                      (57 (athrow)) 
                                      (endofcode 58))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "clojure.lang.Compiler$IParser")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *Compiler$TheVarExpr$Parser-class-table*
  (make-static-class-decls 
   *clojure.lang.Compiler$TheVarExpr$Parser*))

(defconst *package-name-map* 
  ("clojure.lang.Compiler$TheVarExpr$Parser" . "clojure.lang"))
