; Repl-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:52 CDT 2014.
;

(defconst *clojure.lang.Repl*
 (make-class-def
      '(class "clojure.lang.Repl"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
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
                        (method "main"
                              (parameters (array (class "java.lang.String")))
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokestatic
					(methodCP "legacy_repl" "clojure.main" ((array (class "java.lang.String"))) void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *Repl-class-table*
  (make-static-class-decls 
   *clojure.lang.Repl*))

(defconst *package-name-map* 
  ("clojure.lang.Repl" . "clojure.lang"))

