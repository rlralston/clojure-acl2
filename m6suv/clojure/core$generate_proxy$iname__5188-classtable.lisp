; core$generate_proxy$iname__5188-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:43 CDT 2014.
;

(defconst *clojure.core$generate_proxy$iname__5188*
 (make-class-def
      '(class "clojure.core$generate_proxy$iname__5188"
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
                                   (max_stack . 2) (max_locals . 2) (code_length . 16)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (aconst_null))
                                      (2 (astore_1))
                                      (3 (checkcast (class "java.lang.Class")))
                                      (6 (invokestatic
					(methodCP "getType" "clojure.asm.Type" ((class "java.lang.Class")) (class "clojure.asm.Type"))))
                                      (9 (checkcast (class "clojure.asm.Type")))
                                      (12 (invokevirtual
					(methodCP "getInternalName" "clojure.asm.Type" () (class "java.lang.String"))))
                                      (15 (areturn))
                                      (endofcode 16))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$generate_proxy$iname__5188-class-table*
  (make-static-class-decls 
   *clojure.core$generate_proxy$iname__5188*))

(defconst *package-name-map* 
  ("clojure.core$generate_proxy$iname__5188" . "clojure"))

