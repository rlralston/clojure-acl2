; RSAOtherPrimeInfo-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:41 CDT 2014.
;

(defconst *java.security.spec.RSAOtherPrimeInfo*
 (make-class-def
      '(class "java.security.spec.RSAOtherPrimeInfo"
            "java.lang.Object"
            (constant_pool
                        (STRING  "the prime parameter must be non-null")
                        (STRING  "the primeExponent parameter must be non-null")
                        (STRING  "the crtCoefficient parameter must be non-null"))
            (fields
                        (field "prime" (class "java.math.BigInteger") (accessflags  *class*  *private* ) -1)
                        (field "primeExponent" (class "java.math.BigInteger") (accessflags  *class*  *private* ) -1)
                        (field "crtCoefficient" (class "java.math.BigInteger") (accessflags  *class*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.math.BigInteger") (class "java.math.BigInteger") (class "java.math.BigInteger"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 4) (code_length . 62)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokespecial (methodCP "<init>" "java.lang.Object" () void))) 
                                      (4 (aload_1)) 
                                      (5 (ifnonnull 18)) ;;to TAG_0
                                      (8 (new (class "java.lang.NullPointerException"))) 
                                      (11 (dup)) 
                                      (12 (ldc 0)) ;;STRING:: "the prime parameter must be non-null"
                                      (14 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" ((class "java.lang.String")) void))) 
                                      (17 (athrow)) 
                                      (18 (aload_2)) ;;at TAG_0
                                      (19 (ifnonnull 32)) ;;to TAG_1
                                      (22 (new (class "java.lang.NullPointerException"))) 
                                      (25 (dup)) 
                                      (26 (ldc 1)) ;;STRING:: "the primeExponent parameter must be non-null"
                                      (28 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" ((class "java.lang.String")) void))) 
                                      (31 (athrow)) 
                                      (32 (aload_3)) ;;at TAG_1
                                      (33 (ifnonnull 46))  ;;to TAG_2
                                      (36 (new (class "java.lang.NullPointerException"))) 
                                      (39 (dup)) 
                                      (40 (ldc 2)) ;;STRING:: "the crtCoefficient parameter must be non-null"
                                      (42 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" ((class "java.lang.String")) void))) 
                                      (45 (athrow)) 
                                      (46 (aload_0)) ;;at TAG_2
                                      (47 (aload_1)) 
                                      (48 (putfield (fieldCP "prime" "java.security.spec.RSAOtherPrimeInfo" (class "java.math.BigInteger")))) 
                                      (51 (aload_0)) 
                                      (52 (aload_2)) 
                                      (53 (putfield (fieldCP "primeExponent" "java.security.spec.RSAOtherPrimeInfo" (class "java.math.BigInteger")))) 
                                      (56 (aload_0)) 
                                      (57 (aload_3)) 
                                      (58 (putfield (fieldCP "crtCoefficient" "java.security.spec.RSAOtherPrimeInfo" (class "java.math.BigInteger")))) 
                                      (61 (return)) 
                                      (endofcode 62))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getPrime"
                              (parameters )
                              (returntype . (class "java.math.BigInteger"))
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "prime" "java.security.spec.RSAOtherPrimeInfo" (class "java.math.BigInteger"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getExponent"
                              (parameters )
                              (returntype . (class "java.math.BigInteger"))
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "primeExponent" "java.security.spec.RSAOtherPrimeInfo" (class "java.math.BigInteger"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getCrtCoefficient"
                              (parameters )
                              (returntype . (class "java.math.BigInteger"))
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "crtCoefficient" "java.security.spec.RSAOtherPrimeInfo" (class "java.math.BigInteger"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *RSAOtherPrimeInfo-class-table*
  (make-static-class-decls 
   *java.security.spec.RSAOtherPrimeInfo*))

(defconst *package-name-map* 
  ("java.security.spec.RSAOtherPrimeInfo" . "java.security.spec"))

