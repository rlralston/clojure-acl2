; CertificateFactorySpi-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:40 CDT 2014.
;

(defconst *java.security.cert.CertificateFactorySpi*
 (make-class-def
      '(class "java.security.cert.CertificateFactorySpi"
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
                        (method "engineGenerateCertificate"
                              (parameters (class "java.io.InputStream"))
                              (returntype . (class "java.security.cert.Certificate"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "engineGenerateCertPath"
                              (parameters (class "java.io.InputStream"))
                              (returntype . (class "java.security.cert.CertPath"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 8)
                                   (parsedcode
                                      (0 (new (class "java.lang.UnsupportedOperationException")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.lang.UnsupportedOperationException" () void)))
                                      (7 (athrow))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "engineGenerateCertPath"
                              (parameters (class "java.io.InputStream") (class "java.lang.String"))
                              (returntype . (class "java.security.cert.CertPath"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 8)
                                   (parsedcode
                                      (0 (new (class "java.lang.UnsupportedOperationException")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.lang.UnsupportedOperationException" () void)))
                                      (7 (athrow))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "engineGenerateCertPath"
                              (parameters (class "java.util.List"))
                              (returntype . (class "java.security.cert.CertPath"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 8)
                                   (parsedcode
                                      (0 (new (class "java.lang.UnsupportedOperationException")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.lang.UnsupportedOperationException" () void)))
                                      (7 (athrow))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "engineGetCertPathEncodings"
                              (parameters )
                              (returntype . (class "java.util.Iterator"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (new (class "java.lang.UnsupportedOperationException")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.lang.UnsupportedOperationException" () void)))
                                      (7 (athrow))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "engineGenerateCertificates"
                              (parameters (class "java.io.InputStream"))
                              (returntype . (class "java.util.Collection"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "engineGenerateCRL"
                              (parameters (class "java.io.InputStream"))
                              (returntype . (class "java.security.cert.CRL"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "engineGenerateCRLs"
                              (parameters (class "java.io.InputStream"))
                              (returntype . (class "java.util.Collection"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces)
            (accessflags  *abstract*  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *CertificateFactorySpi-class-table*
  (make-static-class-decls 
   *java.security.cert.CertificateFactorySpi*))

(defconst *package-name-map* 
  ("java.security.cert.CertificateFactorySpi" . "java.security.cert"))

