; core$alength-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:40 CDT 2014.
;

(defconst *clojure.core$alength*
 (make-class-def
      '(class "clojure.core$alength"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.lang.RT")
                        (STRING  "alength"))
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
                                   (max_stack . 7) (max_locals . 2) (code_length . 21)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.lang.RT"
                                      (2 (invokestatic
					(methodCP "forName" "java.lang.Class" ((class "java.lang.String")) (class "java.lang.Class"))))
                                      (5 (ldc 1))         ;;STRING:: "alength"
                                      (7 (iconst_1))
                                      (8 (anewarray (class "java.lang.Object")))
                                      (11 (dup))
                                      (12 (iconst_0))
                                      (13 (aload_1))
                                      (14 (aconst_null))
                                      (15 (astore_1))
                                      (16 (aastore))
                                      (17 (invokestatic
					(methodCP "invokeStaticMethod" "clojure.lang.Reflector" ((class "java.lang.Class") (class "java.lang.String") (array (class "java.lang.Object"))) (class "java.lang.Object"))))
                                      (20 (areturn))
                                      (endofcode 21))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$alength-class-table*
  (make-static-class-decls 
   *clojure.core$alength*))

(defconst *package-name-map* 
  ("clojure.core$alength" . "clojure"))

