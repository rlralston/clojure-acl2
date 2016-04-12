; RSAKeyGenParameterSpec-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:41 CDT 2014.
;

(defconst *java.security.spec.RSAKeyGenParameterSpec*
 (make-class-def
      '(class "java.security.spec.RSAKeyGenParameterSpec"
            "java.lang.Object"
            (constant_pool
                        (LONG 3)
                        (LONG 65537))
            (fields
                        (field "keysize" int (accessflags  *class*  *private* ) -1)
                        (field "publicExponent" (class "java.math.BigInteger") (accessflags  *class*  *private* ) -1)
                        (field "F0" (class "java.math.BigInteger") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "F4" (class "java.math.BigInteger") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<init>"
                              (parameters int (class "java.math.BigInteger"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (iload_1))
                                      (6 (putfield (fieldCP "keysize" "java.security.spec.RSAKeyGenParameterSpec" int)))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "publicExponent" "java.security.spec.RSAKeyGenParameterSpec" (class "java.math.BigInteger"))))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getKeysize"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "keysize" "java.security.spec.RSAKeyGenParameterSpec" int)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getPublicExponent"
                              (parameters )
                              (returntype . (class "java.math.BigInteger"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "publicExponent" "java.security.spec.RSAKeyGenParameterSpec" (class "java.math.BigInteger"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 19)
                                   (parsedcode
                                      (0 (ldc2_w 0))      ;; LONG:: "3"
                                      (3 (invokestatic
					(methodCP "valueOf" "java.math.BigInteger" (long) (class "java.math.BigInteger"))))
                                      (6 (putstatic (fieldCP "F0" "java.security.spec.RSAKeyGenParameterSpec" (class "java.math.BigInteger"))))
                                      (9 (ldc2_w 1))      ;; LONG:: "65537"
                                      (12 (invokestatic
					(methodCP "valueOf" "java.math.BigInteger" (long) (class "java.math.BigInteger"))))
                                      (15 (putstatic (fieldCP "F4" "java.security.spec.RSAKeyGenParameterSpec" (class "java.math.BigInteger"))))
                                      (18 (return))
                                      (endofcode 19))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.security.spec.AlgorithmParameterSpec")
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *RSAKeyGenParameterSpec-class-table*
  (make-static-class-decls 
   *java.security.spec.RSAKeyGenParameterSpec*))

(defconst *package-name-map* 
  ("java.security.spec.RSAKeyGenParameterSpec" . "java.security.spec"))

