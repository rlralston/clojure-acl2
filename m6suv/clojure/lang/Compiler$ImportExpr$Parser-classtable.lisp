; Compiler$ImportExpr$Parser-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:50 CDT 2014.
;

(defconst *clojure.lang.Compiler$ImportExpr$Parser*
 (make-class-def
      '(class "clojure.lang.Compiler$ImportExpr$Parser"
            "java.lang.Object"
            (constant_pool)
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
                                   (max_stack . 3) (max_locals . 3) (code_length . 15)
                                   (parsedcode
                                      (0 (new (class "clojure.lang.Compiler$ImportExpr")))
                                      (3 (dup))
                                      (4 (aload_2))
                                      (5 (invokestatic
					(methodCP "second" "clojure.lang.RT" ((class "java.lang.Object")) (class "java.lang.Object"))))
                                      (8 (checkcast (class "java.lang.String")))
                                      (11 (invokespecial
					(methodCP "<init>" "clojure.lang.Compiler$ImportExpr" ((class "java.lang.String")) void)))
                                      (14 (areturn))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "clojure.lang.Compiler$IParser")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *Compiler$ImportExpr$Parser-class-table*
  (make-static-class-decls 
   *clojure.lang.Compiler$ImportExpr$Parser*))

(defconst *package-name-map* 
  ("clojure.lang.Compiler$ImportExpr$Parser" . "clojure.lang"))

