; core$push_thread_bindings-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:45 CDT 2014.
;

(defconst *clojure.core$push_thread_bindings*
 (make-class-def
      '(class "clojure.core$push_thread_bindings"
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
                                   (max_stack . 2) (max_locals . 2) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (aconst_null))
                                      (2 (astore_1))
                                      (3 (checkcast (class "clojure.lang.Associative")))
                                      (6 (invokestatic
					(methodCP "pushThreadBindings" "clojure.lang.Var" ((class "clojure.lang.Associative")) void)))
                                      (9 (aconst_null))
                                      (10 (areturn))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$push_thread_bindings-class-table*
  (make-static-class-decls 
   *clojure.core$push_thread_bindings*))

(defconst *package-name-map* 
  ("clojure.core$push_thread_bindings" . "clojure"))
