; SignatureSpi-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:41 CDT 2014.
;

(defconst *java.security.SignatureSpi*
 (make-class-def
      '(class "java.security.SignatureSpi"
            "java.lang.Object"
            (constant_pool
                        (STRING  "update() failed")
                        (STRING  "partial signatures not returned")
                        (STRING  "insufficient space in the output buffer to store the signature"))
            (fields
                        (field "appRandom" (class "java.security.SecureRandom") (accessflags  *class*  *protected* ) -1))
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (aconst_null))
                                      (6 (putfield (fieldCP "appRandom" "java.security.SignatureSpi" (class "java.security.SecureRandom"))))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "engineInitVerify"
                              (parameters (class "java.security.PublicKey"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *protected* )
                              (code))
                        (method "engineInitSign"
                              (parameters (class "java.security.PrivateKey"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *protected* )
                              (code))
                        (method "engineInitSign"
                              (parameters (class "java.security.PrivateKey") (class "java.security.SecureRandom"))
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_2))
                                      (2 (putfield (fieldCP "appRandom" "java.security.SignatureSpi" (class "java.security.SecureRandom"))))
                                      (5 (aload_0))
                                      (6 (aload_1))
                                      (7 (invokevirtual
					(methodCP "engineInitSign" "java.security.SignatureSpi" ((class "java.security.PrivateKey")) void)))
                                      (10 (return))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "engineUpdate"
                              (parameters byte)
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *protected* )
                              (code))
                        (method "engineUpdate"
                              (parameters (array byte) int int)
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *protected* )
                              (code))
                        (method "engineUpdate"
                              (parameters (class "java.nio.ByteBuffer"))
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 5) (max_locals . 6) (code_length . 126)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (invokevirtual (methodCP "hasRemaining" "java.nio.ByteBuffer" () boolean))) 
                                      (4 (ifne 8)) ;;to TAG_0
                                      (7 (return)) 
                                      (8 (aload_1)) ;;at TAG_0
                                      (9 (invokevirtual (methodCP "hasArray" "java.nio.ByteBuffer" () boolean))) 
                                      (12 (ifeq 61)) ;;to TAG_1
                                      (15 (aload_1)) 
                                      (16 (invokevirtual (methodCP "array" "java.nio.ByteBuffer" () (array byte)))) 
                                      (19 (astore_2)) 
                                      (20 (aload_1)) 
                                      (21 (invokevirtual (methodCP "arrayOffset" "java.nio.ByteBuffer" () int))) 
                                      (24 (istore_3)) 
                                      (25 (aload_1)) 
                                      (26 (invokevirtual (methodCP "position" "java.nio.ByteBuffer" () int))) 
                                      (29 (istore 4)) 
                                      (31 (aload_1)) 
                                      (32 (invokevirtual (methodCP "limit" "java.nio.ByteBuffer" () int))) 
                                      (35 (istore 5)) 
                                      (37 (aload_0)) 
                                      (38 (aload_2)) 
                                      (39 (iload_3)) 
                                      (40 (iload 4)) 
                                      (42 (iadd)) 
                                      (43 (iload 5)) 
                                      (45 (iload 4)) 
                                      (47 (isub)) 
                                      (48 (invokevirtual (methodCP "engineUpdate" "java.security.SignatureSpi" ((array byte) int int) void))) 
                                      (51 (aload_1)) 
                                      (52 (iload 5)) 
                                      (54 (invokevirtual (methodCP "position" "java.nio.ByteBuffer" (int) (class "java.nio.Buffer")))) 
                                      (57 (pop)) 
                                      (58 (goto 110))  ;;to TAG_2
                                      (61 (aload_1)) ;;at TAG_1
                                      (62 (invokevirtual (methodCP "remaining" "java.nio.ByteBuffer" () int))) 
                                      (65 (istore_2)) 
                                      (66 (iload_2)) 
                                      (67 (invokestatic (methodCP "getTempArraySize" "sun.security.jca.JCAUtil" (int) int))) 
                                      (70 (newarray BYTE)) 
                                      (72 (astore_3)) 
                                      (73 (iload_2)) ;;at TAG_3
                                      (74 (ifle 110))  ;;to TAG_2
                                      (77 (iload_2)) 
                                      (78 (aload_3)) 
                                      (79 (arraylength)) 
                                      (80 (invokestatic (methodCP "min" "java.lang.Math" (int int) int))) 
                                      (83 (istore 4)) 
                                      (85 (aload_1)) 
                                      (86 (aload_3)) 
                                      (87 (iconst_0)) 
                                      (88 (iload 4)) 
                                      (90 (invokevirtual (methodCP "get" "java.nio.ByteBuffer" ((array byte) int int) (class "java.nio.ByteBuffer")))) 
                                      (93 (pop)) 
                                      (94 (aload_0)) 
                                      (95 (aload_3)) 
                                      (96 (iconst_0)) 
                                      (97 (iload 4)) 
                                      (99 (invokevirtual (methodCP "engineUpdate" "java.security.SignatureSpi" ((array byte) int int) void))) 
                                      (102 (iload_2)) 
                                      (103 (iload 4)) 
                                      (105 (isub)) 
                                      (106 (istore_2)) 
                                      (107 (goto 73)) ;;to TAG_3
                                      (110 (goto 125)) ;;to TAG_4;;at TAG_2
                                      (113 (astore_2)) ;;at TAG_5
                                      (114 (new (class "java.security.ProviderException"))) 
                                      (117 (dup)) 
                                      (118 (ldc 0)) ;;STRING:: "update() failed"
                                      (120 (aload_2)) 
                                      (121 (invokespecial (methodCP "<init>" "java.security.ProviderException" ((class "java.lang.String") (class "java.lang.Throwable")) void))) 
                                      (124 (athrow)) 
                                      (125 (return)) ;;at TAG_4
                                      (endofcode 126))
                                   (Exceptions 
                                     (handler 8 110  113 (class "java.security.SignatureException")))
                                   (StackMap )))
                        (method "engineSign"
                              (parameters )
                              (returntype . (array byte))
                              (accessflags  *abstract*  *class*  *protected* )
                              (code))
                        (method "engineSign"
                              (parameters (array byte) int int)
                              (returntype . int)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 5) (max_locals . 5) (code_length . 58)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "engineSign" "java.security.SignatureSpi" () (array byte)))) 
                                      (4 (astore 4)) 
                                      (6 (iload_3)) 
                                      (7 (aload 4)) 
                                      (9 (arraylength)) 
                                      (10 (if_icmpge 23))  ;;to TAG_0
                                      (13 (new (class "java.security.SignatureException"))) 
                                      (16 (dup)) 
                                      (17 (ldc 1)) ;;STRING:: "partial signatures not returned"
                                      (19 (invokespecial (methodCP "<init>" "java.security.SignatureException" ((class "java.lang.String")) void))) 
                                      (22 (athrow)) 
                                      (23 (aload_1)) ;;at TAG_0
                                      (24 (arraylength)) 
                                      (25 (iload_2)) 
                                      (26 (isub)) 
                                      (27 (aload 4)) 
                                      (29 (arraylength)) 
                                      (30 (if_icmpge 43)) ;;to TAG_1
                                      (33 (new (class "java.security.SignatureException"))) 
                                      (36 (dup)) 
                                      (37 (ldc 2)) ;;STRING:: "insufficient space in the output buffer to store the signature"
                                      (39 (invokespecial (methodCP "<init>" "java.security.SignatureException" ((class "java.lang.String")) void))) 
                                      (42 (athrow)) 
                                      (43 (aload 4)) ;;at TAG_1
                                      (45 (iconst_0)) 
                                      (46 (aload_1)) 
                                      (47 (iload_2)) 
                                      (48 (aload 4)) 
                                      (50 (arraylength)) 
                                      (51 (invokestatic (methodCP "arraycopy" "java.lang.System" ((class "java.lang.Object") int (class "java.lang.Object") int int) void))) 
                                      (54 (aload 4)) 
                                      (56 (arraylength)) 
                                      (57 (ireturn)) 
                                      (endofcode 58))
                                   (Exceptions )
                                   (StackMap )))
                        (method "engineVerify"
                              (parameters (array byte))
                              (returntype . boolean)
                              (accessflags  *abstract*  *class*  *protected* )
                              (code))
                        (method "engineVerify"
                              (parameters (array byte) int int)
                              (returntype . boolean)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 5) (max_locals . 5) (code_length . 21)
                                   (parsedcode
                                      (0 (iload_3))
                                      (1 (newarray BYTE))
                                      (3 (astore 4))
                                      (5 (aload_1))
                                      (6 (iload_2))
                                      (7 (aload 4))
                                      (9 (iconst_0))
                                      (10 (iload_3))
                                      (11 (invokestatic
					(methodCP "arraycopy" "java.lang.System" ((class "java.lang.Object") int (class "java.lang.Object") int int) void)))
                                      (14 (aload_0))
                                      (15 (aload 4))
                                      (17 (invokevirtual
					(methodCP "engineVerify" "java.security.SignatureSpi" ((array byte)) boolean)))
                                      (20 (ireturn))
                                      (endofcode 21))
                                   (Exceptions )
                                   (StackMap )))
                        (method "engineSetParameter"
                              (parameters (class "java.lang.String") (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *protected* )
                              (code))
                        (method "engineSetParameter"
                              (parameters (class "java.security.spec.AlgorithmParameterSpec"))
                              (returntype . void)
                              (accessflags  *class*  *protected* )
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
                        (method "engineGetParameters"
                              (parameters )
                              (returntype . (class "java.security.AlgorithmParameters"))
                              (accessflags  *class*  *protected* )
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
                        (method "engineGetParameter"
                              (parameters (class "java.lang.String"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *abstract*  *class*  *protected* )
                              (code))
                        (method "clone"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 20)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (instanceof (class "java.lang.Cloneable"))) 
                                      (4 (ifeq 12))  ;;to TAG_0
                                      (7 (aload_0)) 
                                      (8 (invokespecial (methodCP "clone" "java.lang.Object" () (class "java.lang.Object")))) 
                                      (11 (areturn)) 
                                      (12 (new (class "java.lang.CloneNotSupportedException"))) ;;at TAG_0
                                      (15 (dup)) 
                                      (16 (invokespecial (methodCP "<init>" "java.lang.CloneNotSupportedException" () void))) 
                                      (19 (athrow)) 
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *abstract*  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *SignatureSpi-class-table*
  (make-static-class-decls 
   *java.security.SignatureSpi*))

(defconst *package-name-map* 
  ("java.security.SignatureSpi" . "java.security"))
