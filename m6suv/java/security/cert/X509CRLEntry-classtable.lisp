; X509CRLEntry-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:40 CDT 2014.
;

(defconst *java.security.cert.X509CRLEntry*
 (make-class-def
      '(class "java.security.cert.X509CRLEntry"
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
                        (method "equals"
                              (parameters (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 5) (code_length . 75)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (aload_1)) 
                                      (2 (if_acmpne 7)) ;;to TAG_0
                                      (5 (iconst_1)) 
                                      (6 (ireturn)) 
                                      (7 (aload_1)) ;;at TAG_0
                                      (8 (instanceof (class "java.security.cert.X509CRLEntry"))) 
                                      (11 (ifne 16))  ;;to TAG_1
                                      (14 (iconst_0)) 
                                      (15 (ireturn)) 
                                      (16 (aload_0)) ;;at TAG_1
                                      (17 (invokevirtual (methodCP "getEncoded" "java.security.cert.X509CRLEntry" () (array byte)))) 
                                      (20 (astore_2)) 
                                      (21 (aload_1)) 
                                      (22 (checkcast (class "java.security.cert.X509CRLEntry"))) 
                                      (25 (invokevirtual (methodCP "getEncoded" "java.security.cert.X509CRLEntry" () (array byte)))) 
                                      (28 (astore_3)) 
                                      (29 (aload_2)) 
                                      (30 (arraylength)) 
                                      (31 (aload_3)) 
                                      (32 (arraylength)) 
                                      (33 (if_icmpeq 38)) ;;to TAG_2
                                      (36 (iconst_0)) 
                                      (37 (ireturn)) ;;at TAG_7
                                      (38 (iconst_0)) ;;at TAG_2
                                      (39 (istore 4)) 
                                      (41 (iload 4)) ;;at TAG_5
                                      (43 (aload_2)) 
                                      (44 (arraylength)) 
                                      (45 (if_icmpge 67)) ;;to TAG_3
                                      (48 (aload_2)) 
                                      (49 (iload 4)) 
                                      (51 (baload)) 
                                      (52 (aload_3)) 
                                      (53 (iload 4)) 
                                      (55 (baload)) 
                                      (56 (if_icmpeq 61)) ;;to TAG_4
                                      (59 (iconst_0)) 
                                      (60 (ireturn)) ;;at TAG_9
                                      (61 (iinc 4 1)) ;;at TAG_4
                                      (64 (goto 41)) ;;to TAG_5
                                      (67 (goto 73)) ;;to TAG_6;;at TAG_3
                                      (70 (astore_2)) ;;at TAG_8
                                      (71 (iconst_0)) 
                                      (72 (ireturn)) 
                                      (73 (iconst_1)) ;;at TAG_6
                                      (74 (ireturn)) 
                                      (endofcode 75))
                                   (Exceptions 
                                     (handler 16 37  70 (class "java.security.cert.CRLException"))
                                     (handler 38 60  70 (class "java.security.cert.CRLException"))
                                     (handler 61 67  70 (class "java.security.cert.CRLException")))
                                   (StackMap )))
                        (method "hashCode"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 4) (code_length . 37)
                                   (parsedcode
                                      (0 (iconst_0)) 
                                      (1 (istore_1)) 
                                      (2 (aload_0)) ;;at TAG_3
                                      (3 (invokevirtual (methodCP "getEncoded" "java.security.cert.X509CRLEntry" () (array byte)))) 
                                      (6 (astore_2)) 
                                      (7 (iconst_1)) 
                                      (8 (istore_3)) 
                                      (9 (iload_3)) ;;at TAG_1
                                      (10 (aload_2)) 
                                      (11 (arraylength)) 
                                      (12 (if_icmpge 29)) ;;to TAG_0
                                      (15 (iload_1)) 
                                      (16 (aload_2)) 
                                      (17 (iload_3)) 
                                      (18 (baload)) 
                                      (19 (iload_3)) 
                                      (20 (imul)) 
                                      (21 (iadd)) 
                                      (22 (istore_1)) 
                                      (23 (iinc 3 1)) 
                                      (26 (goto 9)) ;;to TAG_1
                                      (29 (goto 35))  ;;to TAG_2;;at TAG_0
                                      (32 (astore_2)) ;;at TAG_4
                                      (33 (iload_1)) 
                                      (34 (ireturn)) 
                                      (35 (iload_1)) ;;at TAG_2
                                      (36 (ireturn)) 
                                      (endofcode 37))
                                   (Exceptions 
                                     (handler 2 29  32 (class "java.security.cert.CRLException")))
                                   (StackMap )))
                        (method "getEncoded"
                              (parameters )
                              (returntype . (array byte))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getSerialNumber"
                              (parameters )
                              (returntype . (class "java.math.BigInteger"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getCertificateIssuer"
                              (parameters )
                              (returntype . (class "javax.security.auth.x500.X500Principal"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 2)
                                   (parsedcode
                                      (0 (aconst_null))
                                      (1 (areturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getRevocationDate"
                              (parameters )
                              (returntype . (class "java.util.Date"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "hasExtensions"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "toString"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getRevocationReason"
                              (parameters )
                              (returntype . (class "java.security.cert.CRLReason"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 14)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "hasExtensions" "java.security.cert.X509CRLEntry" () boolean))) 
                                      (4 (ifne 9))  ;;to TAG_0
                                      (7 (aconst_null)) 
                                      (8 (areturn)) 
                                      (9 (aload_0)) ;;at TAG_0
                                      (10 (invokestatic (methodCP "getRevocationReason" "sun.security.x509.X509CRLEntryImpl" ((class "java.security.cert.X509CRLEntry")) (class "java.security.cert.CRLReason")))) 
                                      (13 (areturn)) 
                                      (endofcode 14))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.security.cert.X509Extension")
            (accessflags  *abstract*  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *X509CRLEntry-class-table*
  (make-static-class-decls 
   *java.security.cert.X509CRLEntry*))

(defconst *package-name-map* 
  ("java.security.cert.X509CRLEntry" . "java.security.cert"))

