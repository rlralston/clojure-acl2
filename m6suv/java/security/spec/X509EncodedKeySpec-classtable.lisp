; X509EncodedKeySpec-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:41 CDT 2014.
;

(defconst *java.security.spec.X509EncodedKeySpec*
 (make-class-def
      '(class "java.security.spec.X509EncodedKeySpec"
            "java.security.spec.EncodedKeySpec"
            (constant_pool
                        (STRING  "X.509"))
            (fields)
            (methods
                        (method "<init>"
                              (parameters (array byte))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokespecial
					(methodCP "<init>" "java.security.spec.EncodedKeySpec" ((array byte)) void)))
                                      (5 (return))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getEncoded"
                              (parameters )
                              (returntype . (array byte))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "getEncoded" "java.security.spec.EncodedKeySpec" () (array byte))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getFormat"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 3)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "X.509"
                                      (2 (areturn))
                                      (endofcode 3))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *X509EncodedKeySpec-class-table*
  (make-static-class-decls 
   *java.security.spec.X509EncodedKeySpec*))

(defconst *package-name-map* 
  ("java.security.spec.X509EncodedKeySpec" . "java.security.spec"))
