; Security$ProviderProperty-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:41 CDT 2014.
;

(defconst *java.security.Security$ProviderProperty*
 (make-class-def
      '(class "java.security.Security$ProviderProperty"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "className" (class "java.lang.String") (accessflags  *class* ) -1)
                        (field "provider" (class "java.security.Provider") (accessflags  *class* ) -1))
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *private* )
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
                        (method "<init>"
                              (parameters (class "java.security.Security$1"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.security.Security$ProviderProperty" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *Security$ProviderProperty-class-table*
  (make-static-class-decls 
   *java.security.Security$ProviderProperty*))

(defconst *package-name-map* 
  ("java.security.Security$ProviderProperty" . "java.security"))
