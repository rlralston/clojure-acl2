; core$protected_final_methods-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:45 CDT 2014.
;

(defconst *clojure.core$protected_final_methods*
 (make-class-def
      '(class "clojure.core$protected_final_methods"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "filter-methods"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 14)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "filter-methods"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$protected_final_methods" (class "clojure.lang.Var"))))
                                      (13 (return))
                                      (endofcode 14))
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
                                   (max_stack . 4) (max_locals . 3) (code_length . 29)
                                   (parsedcode
                                      (0 (new (class "clojure.core$protected_final_methods$not_exposable_QMARK___5513")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "clojure.core$protected_final_methods$not_exposable_QMARK___5513" () void)))
                                      (7 (astore_2))
                                      (8 (getstatic (fieldCP "const__0" "clojure.core$protected_final_methods" (class "clojure.lang.Var"))))
                                      (11 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (14 (checkcast (class "clojure.lang.IFn")))
                                      (17 (aload_1))
                                      (18 (aconst_null))
                                      (19 (astore_1))
                                      (20 (aload_2))
                                      (21 (aconst_null))
                                      (22 (astore_2))
                                      (23 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (28 (areturn))
                                      (endofcode 29))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$protected_final_methods-class-table*
  (make-static-class-decls 
   *clojure.core$protected_final_methods*))

(defconst *package-name-map* 
  ("clojure.core$protected_final_methods" . "clojure"))

