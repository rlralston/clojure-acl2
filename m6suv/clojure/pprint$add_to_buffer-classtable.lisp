; pprint$add_to_buffer-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:55 CDT 2014.
;

(defconst *clojure.pprint$add_to_buffer*
 (make-class-def
      '(class "clojure.pprint$add_to_buffer"
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
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 3) (code_length . 20)
                                   (parsedcode
                                      (0 (new (class "clojure.pprint$add_to_buffer$fn__7496")))
                                      (3 (dup))
                                      (4 (aload_2))
                                      (5 (aconst_null))
                                      (6 (astore_2))
                                      (7 (aload_1))
                                      (8 (aconst_null))
                                      (9 (astore_1))
                                      (10 (invokespecial
					(methodCP "<init>" "clojure.pprint$add_to_buffer$fn__7496" ((class "java.lang.Object") (class "java.lang.Object")) void)))
                                      (13 (checkcast (class "java.util.concurrent.Callable")))
                                      (16 (invokestatic
					(methodCP "runInTransaction" "clojure.lang.LockingTransaction" ((class "java.util.concurrent.Callable")) (class "java.lang.Object"))))
                                      (19 (areturn))
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *pprint$add_to_buffer-class-table*
  (make-static-class-decls 
   *clojure.pprint$add_to_buffer*))

(defconst *package-name-map* 
  ("clojure.pprint$add_to_buffer" . "clojure"))
