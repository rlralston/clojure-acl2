; core$fn__6240-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:42 CDT 2014.
;

(defconst *clojure.core$fn__6240*
 (make-class-def
      '(class "clojure.core$fn__6240"
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
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 4) (code_length . 19)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (aconst_null))
                                      (2 (astore_1))
                                      (3 (checkcast (class "clojure.lang.PersistentVector")))
                                      (6 (aload_2))
                                      (7 (aconst_null))
                                      (8 (astore_2))
                                      (9 (checkcast (class "clojure.lang.IFn")))
                                      (12 (aload_3))
                                      (13 (aconst_null))
                                      (14 (astore_3))
                                      (15 (invokevirtual
					(methodCP "kvreduce" "clojure.lang.PersistentVector" ((class "clojure.lang.IFn") (class "java.lang.Object")) (class "java.lang.Object"))))
                                      (18 (areturn))
                                      (endofcode 19))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$fn__6240-class-table*
  (make-static-class-decls 
   *clojure.core$fn__6240*))

(defconst *package-name-map* 
  ("clojure.core$fn__6240" . "clojure"))

