; Compiler$FieldExpr-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:50 CDT 2014.
;

(defconst *clojure.lang.Compiler$FieldExpr*
 (make-class-def
      '(class "clojure.lang.Compiler$FieldExpr"
            "clojure.lang.Compiler$HostExpr"
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
					(methodCP "<init>" "clojure.lang.Compiler$HostExpr" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *abstract*  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *Compiler$FieldExpr-class-table*
  (make-static-class-decls 
   *clojure.lang.Compiler$FieldExpr*))

(defconst *package-name-map* 
  ("clojure.lang.Compiler$FieldExpr" . "clojure.lang"))
