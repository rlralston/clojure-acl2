; pprint$use_method-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:57 CDT 2014.
;

(defconst *clojure.pprint$use_method*
 (make-class-def
      '(class "clojure.pprint$use_method"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "addMethod"))
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
                                   (max_stack . 7) (max_locals . 4) (code_length . 25)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (aconst_null))
                                      (2 (astore_1))
                                      (3 (ldc 0))         ;;STRING:: "addMethod"
                                      (5 (iconst_2))
                                      (6 (anewarray (class "java.lang.Object")))
                                      (9 (dup))
                                      (10 (iconst_0))
                                      (11 (aload_2))
                                      (12 (aconst_null))
                                      (13 (astore_2))
                                      (14 (aastore))
                                      (15 (dup))
                                      (16 (iconst_1))
                                      (17 (aload_3))
                                      (18 (aconst_null))
                                      (19 (astore_3))
                                      (20 (aastore))
                                      (21 (invokestatic
					(methodCP "invokeInstanceMethod" "clojure.lang.Reflector" ((class "java.lang.Object") (class "java.lang.String") (array (class "java.lang.Object"))) (class "java.lang.Object"))))
                                      (24 (areturn))
                                      (endofcode 25))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *pprint$use_method-class-table*
  (make-static-class-decls 
   *clojure.pprint$use_method*))

(defconst *package-name-map* 
  ("clojure.pprint$use_method" . "clojure"))

