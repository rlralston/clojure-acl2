; CertPathBuilderSpi-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:40 CDT 2014.
;

(defconst *java.security.cert.CertPathBuilderSpi*
 (make-class-def
      '(class "java.security.cert.CertPathBuilderSpi"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
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
                        (method "engineBuild"
                              (parameters (class "java.security.cert.CertPathParameters"))
                              (returntype . (class "java.security.cert.CertPathBuilderResult"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces)
            (accessflags  *abstract*  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *CertPathBuilderSpi-class-table*
  (make-static-class-decls 
   *java.security.cert.CertPathBuilderSpi*))

(defconst *package-name-map* 
  ("java.security.cert.CertPathBuilderSpi" . "java.security.cert"))

