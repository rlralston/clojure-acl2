; CertPathHelperImpl-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:40 CDT 2014.
;

(defconst *java.security.cert.CertPathHelperImpl*
 (make-class-def
      '(class "java.security.cert.CertPathHelperImpl"
            "sun.security.provider.certpath.CertPathHelper"
            (constant_pool)
            (fields)
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
					(methodCP "<init>" "sun.security.provider.certpath.CertPathHelper" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "initialize"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static*  *super*  *synchronized* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 17)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "instance" "sun.security.provider.certpath.CertPathHelper" (class "sun.security.provider.certpath.CertPathHelper")))) 
                                      (3 (ifnonnull 16))  ;;to TAG_0
                                      (6 (new (class "java.security.cert.CertPathHelperImpl"))) 
                                      (9 (dup)) 
                                      (10 (invokespecial (methodCP "<init>" "java.security.cert.CertPathHelperImpl" () void))) 
                                      (13 (putstatic (fieldCP "instance" "sun.security.provider.certpath.CertPathHelper" (class "sun.security.provider.certpath.CertPathHelper")))) 
                                      (16 (return)) ;;at TAG_0
                                      (endofcode 17))
                                   (Exceptions )
                                   (StackMap )))
                        (method "implSetPathToNames"
                              (parameters (class "java.security.cert.X509CertSelector") (class "java.util.Set"))
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (aload_2))
                                      (2 (invokevirtual
					(methodCP "setPathToNamesInternal" "java.security.cert.X509CertSelector" ((class "java.util.Set")) void)))
                                      (5 (return))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "implSetDateAndTime"
                              (parameters (class "java.security.cert.X509CRLSelector") (class "java.util.Date") long)
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 4) (max_locals . 5) (code_length . 7)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (aload_2))
                                      (2 (lload_3))
                                      (3 (invokevirtual
					(methodCP "setDateAndTime" "java.security.cert.X509CRLSelector" ((class "java.util.Date") long) void)))
                                      (6 (return))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *CertPathHelperImpl-class-table*
  (make-static-class-decls 
   *java.security.cert.CertPathHelperImpl*))

(defconst *package-name-map* 
  ("java.security.cert.CertPathHelperImpl" . "java.security.cert"))
