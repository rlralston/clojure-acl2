; AlgorithmParameterGenerator-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:40 CDT 2014.
;

(defconst *java.security.AlgorithmParameterGenerator*
 (make-class-def
      '(class "java.security.AlgorithmParameterGenerator"
            "java.lang.Object"
            (constant_pool
                        (STRING  "AlgorithmParameterGenerator")
                        (STRING  " not found")
                        (STRING  "missing provider"))
            (fields
                        (field "provider" (class "java.security.Provider") (accessflags  *class*  *private* ) -1)
                        (field "paramGenSpi" (class "java.security.AlgorithmParameterGeneratorSpi") (accessflags  *class*  *private* ) -1)
                        (field "algorithm" (class "java.lang.String") (accessflags  *class*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.security.AlgorithmParameterGeneratorSpi") (class "java.security.Provider") (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 20)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "paramGenSpi" "java.security.AlgorithmParameterGenerator" (class "java.security.AlgorithmParameterGeneratorSpi"))))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "provider" "java.security.AlgorithmParameterGenerator" (class "java.security.Provider"))))
                                      (14 (aload_0))
                                      (15 (aload_3))
                                      (16 (putfield (fieldCP "algorithm" "java.security.AlgorithmParameterGenerator" (class "java.lang.String"))))
                                      (19 (return))
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getAlgorithm"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "algorithm" "java.security.AlgorithmParameterGenerator" (class "java.lang.String"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getInstance"
                              (parameters (class "java.lang.String"))
                              (returntype . (class "java.security.AlgorithmParameterGenerator"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 5) (max_locals . 2) (code_length . 60)
                                   (parsedcode
                                      (0 (aload_0)) ;;at TAG_0
                                      (1 (ldc 0)) ;;STRING:: "AlgorithmParameterGenerator"
                                      (3 (aconst_null)) 
                                      (4 (checkcast (class "java.lang.String"))) 
                                      (7 (invokestatic (methodCP "getImpl" "java.security.Security" ((class "java.lang.String") (class "java.lang.String") (class "java.lang.String")) (array (class "java.lang.Object"))))) 
                                      (10 (astore_1)) 
                                      (11 (new (class "java.security.AlgorithmParameterGenerator"))) 
                                      (14 (dup)) 
                                      (15 (aload_1)) 
                                      (16 (iconst_0)) 
                                      (17 (aaload)) 
                                      (18 (checkcast (class "java.security.AlgorithmParameterGeneratorSpi"))) 
                                      (21 (aload_1)) 
                                      (22 (iconst_1)) 
                                      (23 (aaload)) 
                                      (24 (checkcast (class "java.security.Provider"))) 
                                      (27 (aload_0)) 
                                      (28 (invokespecial (methodCP "<init>" "java.security.AlgorithmParameterGenerator" ((class "java.security.AlgorithmParameterGeneratorSpi") (class "java.security.Provider") (class "java.lang.String")) void))) 
                                      (31 (areturn)) ;;at TAG_1
                                      (32 (astore_1)) ;;at TAG_2
                                      (33 (new (class "java.security.NoSuchAlgorithmException"))) 
                                      (36 (dup)) 
                                      (37 (new (class "java.lang.StringBuilder"))) 
                                      (40 (dup)) 
                                      (41 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (44 (aload_0)) 
                                      (45 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (48 (ldc 1)) ;;STRING:: " not found"
                                      (50 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (53 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (56 (invokespecial (methodCP "<init>" "java.security.NoSuchAlgorithmException" ((class "java.lang.String")) void))) 
                                      (59 (athrow)) 
                                      (endofcode 60))
                                   (Exceptions 
                                     (handler 0 31  32 (class "java.security.NoSuchProviderException")))
                                   (StackMap )))
                        (method "getInstance"
                              (parameters (class "java.lang.String") (class "java.lang.String"))
                              (returntype . (class "java.security.AlgorithmParameterGenerator"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 5) (max_locals . 3) (code_length . 50)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (ifnull 11))  ;;to TAG_0
                                      (4 (aload_1)) 
                                      (5 (invokevirtual (methodCP "length" "java.lang.String" () int))) 
                                      (8 (ifne 21)) ;;to TAG_1
                                      (11 (new (class "java.lang.IllegalArgumentException"))) ;;at TAG_0
                                      (14 (dup)) 
                                      (15 (ldc 2)) ;;STRING:: "missing provider"
                                      (17 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (20 (athrow)) 
                                      (21 (aload_0)) ;;at TAG_1
                                      (22 (ldc 0)) ;;STRING:: "AlgorithmParameterGenerator"
                                      (24 (aload_1)) 
                                      (25 (invokestatic (methodCP "getImpl" "java.security.Security" ((class "java.lang.String") (class "java.lang.String") (class "java.lang.String")) (array (class "java.lang.Object"))))) 
                                      (28 (astore_2)) 
                                      (29 (new (class "java.security.AlgorithmParameterGenerator"))) 
                                      (32 (dup)) 
                                      (33 (aload_2)) 
                                      (34 (iconst_0)) 
                                      (35 (aaload)) 
                                      (36 (checkcast (class "java.security.AlgorithmParameterGeneratorSpi"))) 
                                      (39 (aload_2)) 
                                      (40 (iconst_1)) 
                                      (41 (aaload)) 
                                      (42 (checkcast (class "java.security.Provider"))) 
                                      (45 (aload_0)) 
                                      (46 (invokespecial (methodCP "<init>" "java.security.AlgorithmParameterGenerator" ((class "java.security.AlgorithmParameterGeneratorSpi") (class "java.security.Provider") (class "java.lang.String")) void))) 
                                      (49 (areturn)) 
                                      (endofcode 50))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getInstance"
                              (parameters (class "java.lang.String") (class "java.security.Provider"))
                              (returntype . (class "java.security.AlgorithmParameterGenerator"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 5) (max_locals . 3) (code_length . 43)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (ifnonnull 14))  ;;to TAG_0
                                      (4 (new (class "java.lang.IllegalArgumentException"))) 
                                      (7 (dup)) 
                                      (8 (ldc 2)) ;;STRING:: "missing provider"
                                      (10 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (13 (athrow)) 
                                      (14 (aload_0)) ;;at TAG_0
                                      (15 (ldc 0)) ;;STRING:: "AlgorithmParameterGenerator"
                                      (17 (aload_1)) 
                                      (18 (invokestatic (methodCP "getImpl" "java.security.Security" ((class "java.lang.String") (class "java.lang.String") (class "java.security.Provider")) (array (class "java.lang.Object"))))) 
                                      (21 (astore_2)) 
                                      (22 (new (class "java.security.AlgorithmParameterGenerator"))) 
                                      (25 (dup)) 
                                      (26 (aload_2)) 
                                      (27 (iconst_0)) 
                                      (28 (aaload)) 
                                      (29 (checkcast (class "java.security.AlgorithmParameterGeneratorSpi"))) 
                                      (32 (aload_2)) 
                                      (33 (iconst_1)) 
                                      (34 (aaload)) 
                                      (35 (checkcast (class "java.security.Provider"))) 
                                      (38 (aload_0)) 
                                      (39 (invokespecial (methodCP "<init>" "java.security.AlgorithmParameterGenerator" ((class "java.security.AlgorithmParameterGeneratorSpi") (class "java.security.Provider") (class "java.lang.String")) void))) 
                                      (42 (areturn)) 
                                      (endofcode 43))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getProvider"
                              (parameters )
                              (returntype . (class "java.security.Provider"))
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "provider" "java.security.AlgorithmParameterGenerator" (class "java.security.Provider"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "init"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 16)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "paramGenSpi" "java.security.AlgorithmParameterGenerator" (class "java.security.AlgorithmParameterGeneratorSpi"))))
                                      (4 (iload_1))
                                      (5 (new (class "java.security.SecureRandom")))
                                      (8 (dup))
                                      (9 (invokespecial
					(methodCP "<init>" "java.security.SecureRandom" () void)))
                                      (12 (invokevirtual
					(methodCP "engineInit" "java.security.AlgorithmParameterGeneratorSpi" (int (class "java.security.SecureRandom")) void)))
                                      (15 (return))
                                      (endofcode 16))
                                   (Exceptions )
                                   (StackMap )))
                        (method "init"
                              (parameters int (class "java.security.SecureRandom"))
                              (returntype . void)
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "paramGenSpi" "java.security.AlgorithmParameterGenerator" (class "java.security.AlgorithmParameterGeneratorSpi"))))
                                      (4 (iload_1))
                                      (5 (aload_2))
                                      (6 (invokevirtual
					(methodCP "engineInit" "java.security.AlgorithmParameterGeneratorSpi" (int (class "java.security.SecureRandom")) void)))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "init"
                              (parameters (class "java.security.spec.AlgorithmParameterSpec"))
                              (returntype . void)
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 16)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "paramGenSpi" "java.security.AlgorithmParameterGenerator" (class "java.security.AlgorithmParameterGeneratorSpi"))))
                                      (4 (aload_1))
                                      (5 (new (class "java.security.SecureRandom")))
                                      (8 (dup))
                                      (9 (invokespecial
					(methodCP "<init>" "java.security.SecureRandom" () void)))
                                      (12 (invokevirtual
					(methodCP "engineInit" "java.security.AlgorithmParameterGeneratorSpi" ((class "java.security.spec.AlgorithmParameterSpec") (class "java.security.SecureRandom")) void)))
                                      (15 (return))
                                      (endofcode 16))
                                   (Exceptions )
                                   (StackMap )))
                        (method "init"
                              (parameters (class "java.security.spec.AlgorithmParameterSpec") (class "java.security.SecureRandom"))
                              (returntype . void)
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "paramGenSpi" "java.security.AlgorithmParameterGenerator" (class "java.security.AlgorithmParameterGeneratorSpi"))))
                                      (4 (aload_1))
                                      (5 (aload_2))
                                      (6 (invokevirtual
					(methodCP "engineInit" "java.security.AlgorithmParameterGeneratorSpi" ((class "java.security.spec.AlgorithmParameterSpec") (class "java.security.SecureRandom")) void)))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "generateParameters"
                              (parameters )
                              (returntype . (class "java.security.AlgorithmParameters"))
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "paramGenSpi" "java.security.AlgorithmParameterGenerator" (class "java.security.AlgorithmParameterGeneratorSpi"))))
                                      (4 (invokevirtual
					(methodCP "engineGenerateParameters" "java.security.AlgorithmParameterGeneratorSpi" () (class "java.security.AlgorithmParameters"))))
                                      (7 (areturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *AlgorithmParameterGenerator-class-table*
  (make-static-class-decls 
   *java.security.AlgorithmParameterGenerator*))

(defconst *package-name-map* 
  ("java.security.AlgorithmParameterGenerator" . "java.security"))

