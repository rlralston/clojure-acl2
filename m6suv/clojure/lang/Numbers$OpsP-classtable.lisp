; Numbers$OpsP-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:52 CDT 2014.
;

(defconst *clojure.lang.Numbers$OpsP*
 (make-class-def
      '(class "clojure.lang.Numbers$OpsP"
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
                        (method "addP"
                              (parameters (class "java.lang.Number") (class "java.lang.Number"))
                              (returntype . (class "java.lang.Number"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 7)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (aload_2))
                                      (3 (invokevirtual
					(methodCP "add" "clojure.lang.Numbers$OpsP" ((class "java.lang.Number") (class "java.lang.Number")) (class "java.lang.Number"))))
                                      (6 (areturn))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap )))
                        (method "multiplyP"
                              (parameters (class "java.lang.Number") (class "java.lang.Number"))
                              (returntype . (class "java.lang.Number"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 7)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (aload_2))
                                      (3 (invokevirtual
					(methodCP "multiply" "clojure.lang.Numbers$OpsP" ((class "java.lang.Number") (class "java.lang.Number")) (class "java.lang.Number"))))
                                      (6 (areturn))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap )))
                        (method "negateP"
                              (parameters (class "java.lang.Number"))
                              (returntype . (class "java.lang.Number"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokevirtual
					(methodCP "negate" "clojure.lang.Numbers$OpsP" ((class "java.lang.Number")) (class "java.lang.Number"))))
                                      (5 (areturn))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "incP"
                              (parameters (class "java.lang.Number"))
                              (returntype . (class "java.lang.Number"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokevirtual
					(methodCP "inc" "clojure.lang.Numbers$OpsP" ((class "java.lang.Number")) (class "java.lang.Number"))))
                                      (5 (areturn))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "decP"
                              (parameters (class "java.lang.Number"))
                              (returntype . (class "java.lang.Number"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokevirtual
					(methodCP "dec" "clojure.lang.Numbers$OpsP" ((class "java.lang.Number")) (class "java.lang.Number"))))
                                      (5 (areturn))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "clojure.lang.Numbers$Ops")
            (accessflags  *abstract*  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *Numbers$OpsP-class-table*
  (make-static-class-decls 
   *clojure.lang.Numbers$OpsP*))

(defconst *package-name-map* 
  ("clojure.lang.Numbers$OpsP" . "clojure.lang"))

