; CertStoreSpi-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:40 CDT 2014.
;

(defconst *java.security.cert.CertStoreSpi*
 (make-class-def
      '(class "java.security.cert.CertStoreSpi"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "<init>"
                              (parameters (class "java.security.cert.CertStoreParameters"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "engineGetCertificates"
                              (parameters (class "java.security.cert.CertSelector"))
                              (returntype . (class "java.util.Collection"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "engineGetCRLs"
                              (parameters (class "java.security.cert.CRLSelector"))
                              (returntype . (class "java.util.Collection"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces)
            (accessflags  *abstract*  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *CertStoreSpi-class-table*
  (make-static-class-decls 
   *java.security.cert.CertStoreSpi*))

(defconst *package-name-map* 
  ("java.security.cert.CertStoreSpi" . "java.security.cert"))

