; KeyStore$SecretKeyEntry-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:40 CDT 2014.
;

(defconst *java.security.KeyStore$SecretKeyEntry*
 (make-class-def
      '(class "java.security.KeyStore$SecretKeyEntry"
            "java.lang.Object"
            (constant_pool
                        (STRING  "invalid null input")
                        (STRING  "Secret key entry with algorithm "))
            (fields
                        (field "sKey" (class "javax.crypto.SecretKey") (accessflags  *class*  *final*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "javax.crypto.SecretKey"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 24)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokespecial (methodCP "<init>" "java.lang.Object" () void))) 
                                      (4 (aload_1)) 
                                      (5 (ifnonnull 18))  ;;to TAG_0
                                      (8 (new (class "java.lang.NullPointerException"))) 
                                      (11 (dup)) 
                                      (12 (ldc 0)) ;;STRING:: "invalid null input"
                                      (14 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" ((class "java.lang.String")) void))) 
                                      (17 (athrow)) 
                                      (18 (aload_0)) ;;at TAG_0
                                      (19 (aload_1)) 
                                      (20 (putfield (fieldCP "sKey" "java.security.KeyStore$SecretKeyEntry" (class "javax.crypto.SecretKey")))) 
                                      (23 (return)) 
                                      (endofcode 24))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getSecretKey"
                              (parameters )
                              (returntype . (class "javax.crypto.SecretKey"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "sKey" "java.security.KeyStore$SecretKeyEntry" (class "javax.crypto.SecretKey"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "toString"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 28)
                                   (parsedcode
                                      (0 (new (class "java.lang.StringBuilder")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.lang.StringBuilder" () void)))
                                      (7 (ldc 1))         ;;STRING:: "Secret key entry with algorithm "
                                      (9 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (12 (aload_0))
                                      (13 (getfield (fieldCP "sKey" "java.security.KeyStore$SecretKeyEntry" (class "javax.crypto.SecretKey"))))
                                      (16 (invokeinterface
					(methodCP "getAlgorithm" "javax.crypto.SecretKey" () (class "java.lang.String")) 1))
                                      (21 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (24 (invokevirtual
					(methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String"))))
                                      (27 (areturn))
                                      (endofcode 28))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.security.KeyStore$Entry")
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *KeyStore$SecretKeyEntry-class-table*
  (make-static-class-decls 
   *java.security.KeyStore$SecretKeyEntry*))

(defconst *package-name-map* 
  ("java.security.KeyStore$SecretKeyEntry" . "java.security"))

