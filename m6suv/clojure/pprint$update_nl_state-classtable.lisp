; pprint$update_nl_state-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:57 CDT 2014.
;

(defconst *clojure.pprint$update_nl_state*
 (make-class-def
      '(class "clojure.pprint$update_nl_state"
            "clojure.lang.AFunction"
            (constant_pool)
            (fields)
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 0) (max_locals . 0) (code_length . 1)
                                   (parsedcode
                                      (0 (return))
                                      (endofcode 1))
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
                                   (max_stack . 4) (max_locals . 2) (code_length . 17)
                                   (parsedcode
                                      (0 (new (class "clojure.pprint$update_nl_state$fn__7469")))
                                      (3 (dup))
                                      (4 (aload_1))
                                      (5 (aconst_null))
                                      (6 (astore_1))
                                      (7 (invokespecial
					(methodCP "<init>" "clojure.pprint$update_nl_state$fn__7469" ((class "java.lang.Object")) void)))
                                      (10 (checkcast (class "java.util.concurrent.Callable")))
                                      (13 (invokestatic
					(methodCP "runInTransaction" "clojure.lang.LockingTransaction" ((class "java.util.concurrent.Callable")) (class "java.lang.Object"))))
                                      (16 (areturn))
                                      (endofcode 17))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *pprint$update_nl_state-class-table*
  (make-static-class-decls 
   *clojure.pprint$update_nl_state*))

(defconst *package-name-map* 
  ("clojure.pprint$update_nl_state" . "clojure"))

