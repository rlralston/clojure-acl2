; KeyStore$SimpleLoadStoreParameter-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:40 CDT 2014.
;

(defconst *java.security.KeyStore$SimpleLoadStoreParameter*
 (make-class-def
      '(class "java.security.KeyStore$SimpleLoadStoreParameter"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "protection" (class "java.security.KeyStore$ProtectionParameter") (accessflags  *class*  *final*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.security.KeyStore$ProtectionParameter"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "protection" "java.security.KeyStore$SimpleLoadStoreParameter" (class "java.security.KeyStore$ProtectionParameter"))))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getProtectionParameter"
                              (parameters )
                              (returntype . (class "java.security.KeyStore$ProtectionParameter"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "protection" "java.security.KeyStore$SimpleLoadStoreParameter" (class "java.security.KeyStore$ProtectionParameter"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.security.KeyStore$LoadStoreParameter")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *KeyStore$SimpleLoadStoreParameter-class-table*
  (make-static-class-decls 
   *java.security.KeyStore$SimpleLoadStoreParameter*))

(defconst *package-name-map* 
  ("java.security.KeyStore$SimpleLoadStoreParameter" . "java.security"))
