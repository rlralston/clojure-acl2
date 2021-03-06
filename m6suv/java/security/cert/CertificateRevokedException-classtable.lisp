; CertificateRevokedException-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:40 CDT 2014.
;

(defconst *java.security.cert.CertificateRevokedException*
 (make-class-def
      '(class "java.security.cert.CertificateRevokedException"
            "java.security.cert.CertificateException"
            (constant_pool
                        (LONG 7839996631571608627)
                        (STRING  "2.5.29.24")
                        (STRING  "DATE")
                        (STRING  "Certificate has been revoked, reason: ")
                        (STRING  ", revocation date: ")
                        (STRING  ", authority: ")
                        (STRING  ", extensions: "))
            (fields
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0)
                        (field "revocationDate" (class "java.util.Date") (accessflags  *class*  *private* ) -1)
                        (field "reason" (class "java.security.cert.CRLReason") (accessflags  *class*  *final*  *private* ) -1)
                        (field "authority" (class "javax.security.auth.x500.X500Principal") (accessflags  *class*  *final*  *private* ) -1)
                        (field "extensions" (class "java.util.Map") (accessflags  *class*  *private*  *transient* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.Date") (class "java.security.cert.CRLReason") (class "javax.security.auth.x500.X500Principal") (class "java.util.Map"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 5) (code_length . 68)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokespecial (methodCP "<init>" "java.security.cert.CertificateException" () void))) 
                                      (4 (aload_1)) 
                                      (5 (ifnull 21))  ;;to TAG_0
                                      (8 (aload_2)) 
                                      (9 (ifnull 21))  ;;to TAG_0
                                      (12 (aload_3)) 
                                      (13 (ifnull 21))  ;;to TAG_0
                                      (16 (aload 4)) 
                                      (18 (ifnonnull 29)) ;;to TAG_1
                                      (21 (new (class "java.lang.NullPointerException"))) ;;at TAG_0
                                      (24 (dup)) 
                                      (25 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" () void))) 
                                      (28 (athrow)) 
                                      (29 (aload_0)) ;;at TAG_1
                                      (30 (new (class "java.util.Date"))) 
                                      (33 (dup)) 
                                      (34 (aload_1)) 
                                      (35 (invokevirtual (methodCP "getTime" "java.util.Date" () long))) 
                                      (38 (invokespecial (methodCP "<init>" "java.util.Date" (long) void))) 
                                      (41 (putfield (fieldCP "revocationDate" "java.security.cert.CertificateRevokedException" (class "java.util.Date")))) 
                                      (44 (aload_0)) 
                                      (45 (aload_2)) 
                                      (46 (putfield (fieldCP "reason" "java.security.cert.CertificateRevokedException" (class "java.security.cert.CRLReason")))) 
                                      (49 (aload_0)) 
                                      (50 (aload_3)) 
                                      (51 (putfield (fieldCP "authority" "java.security.cert.CertificateRevokedException" (class "javax.security.auth.x500.X500Principal")))) 
                                      (54 (aload_0)) 
                                      (55 (new (class "java.util.HashMap"))) 
                                      (58 (dup)) 
                                      (59 (aload 4)) 
                                      (61 (invokespecial (methodCP "<init>" "java.util.HashMap" ((class "java.util.Map")) void))) 
                                      (64 (putfield (fieldCP "extensions" "java.security.cert.CertificateRevokedException" (class "java.util.Map")))) 
                                      (67 (return)) 
                                      (endofcode 68))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getRevocationDate"
                              (parameters )
                              (returntype . (class "java.util.Date"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "revocationDate" "java.security.cert.CertificateRevokedException" (class "java.util.Date"))))
                                      (4 (invokevirtual
					(methodCP "clone" "java.util.Date" () (class "java.lang.Object"))))
                                      (7 (checkcast (class "java.util.Date")))
                                      (10 (areturn))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getRevocationReason"
                              (parameters )
                              (returntype . (class "java.security.cert.CRLReason"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "reason" "java.security.cert.CertificateRevokedException" (class "java.security.cert.CRLReason"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getAuthorityName"
                              (parameters )
                              (returntype . (class "javax.security.auth.x500.X500Principal"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "authority" "java.security.cert.CertificateRevokedException" (class "javax.security.auth.x500.X500Principal"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getInvalidityDate"
                              (parameters )
                              (returntype . (class "java.util.Date"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 49)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "getExtensions" "java.security.cert.CertificateRevokedException" () (class "java.util.Map")))) 
                                      (4 (ldc 1)) ;;STRING:: "2.5.29.24"
                                      (6 (invokeinterface (methodCP "get" "java.util.Map" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (11 (checkcast (class "java.security.cert.Extension"))) 
                                      (14 (astore_1)) 
                                      (15 (aload_1)) 
                                      (16 (ifnonnull 21)) ;;to TAG_0
                                      (19 (aconst_null)) 
                                      (20 (areturn)) 
                                      (21 (aload_1)) ;;at TAG_0
                                      (22 (invokestatic (methodCP "toImpl" "sun.security.x509.InvalidityDateExtension" ((class "java.security.cert.Extension")) (class "sun.security.x509.InvalidityDateExtension")))) 
                                      (25 (ldc 2)) ;;STRING:: "DATE"
                                      (27 (invokevirtual (methodCP "get" "sun.security.x509.InvalidityDateExtension" ((class "java.lang.String")) (class "java.lang.Object")))) 
                                      (30 (checkcast (class "java.util.Date"))) 
                                      (33 (astore_2)) 
                                      (34 (new (class "java.util.Date"))) 
                                      (37 (dup)) 
                                      (38 (aload_2)) 
                                      (39 (invokevirtual (methodCP "getTime" "java.util.Date" () long))) 
                                      (42 (invokespecial (methodCP "<init>" "java.util.Date" (long) void))) 
                                      (45 (areturn)) ;;at TAG_1
                                      (46 (astore_2)) ;;at TAG_2
                                      (47 (aconst_null)) 
                                      (48 (areturn)) 
                                      (endofcode 49))
                                   (Exceptions 
                                     (handler 21 45  46 (class "java.io.IOException")))
                                   (StackMap )))
                        (method "getExtensions"
                              (parameters )
                              (returntype . (class "java.util.Map"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "extensions" "java.security.cert.CertificateRevokedException" (class "java.util.Map"))))
                                      (4 (invokestatic
					(methodCP "unmodifiableMap" "java.util.Collections" ((class "java.util.Map")) (class "java.util.Map"))))
                                      (7 (areturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getMessage"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 59)
                                   (parsedcode
                                      (0 (new (class "java.lang.StringBuilder")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.lang.StringBuilder" () void)))
                                      (7 (ldc 3))         ;;STRING:: "Certificate has been revoked, reason: "
                                      (9 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (12 (aload_0))
                                      (13 (getfield (fieldCP "reason" "java.security.cert.CertificateRevokedException" (class "java.security.cert.CRLReason"))))
                                      (16 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.Object")) (class "java.lang.StringBuilder"))))
                                      (19 (ldc 4))        ;;STRING:: ", revocation date: "
                                      (21 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (24 (aload_0))
                                      (25 (getfield (fieldCP "revocationDate" "java.security.cert.CertificateRevokedException" (class "java.util.Date"))))
                                      (28 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.Object")) (class "java.lang.StringBuilder"))))
                                      (31 (ldc 5))        ;;STRING:: ", authority: "
                                      (33 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (36 (aload_0))
                                      (37 (getfield (fieldCP "authority" "java.security.cert.CertificateRevokedException" (class "javax.security.auth.x500.X500Principal"))))
                                      (40 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.Object")) (class "java.lang.StringBuilder"))))
                                      (43 (ldc 6))        ;;STRING:: ", extensions: "
                                      (45 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (48 (aload_0))
                                      (49 (getfield (fieldCP "extensions" "java.security.cert.CertificateRevokedException" (class "java.util.Map"))))
                                      (52 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.Object")) (class "java.lang.StringBuilder"))))
                                      (55 (invokevirtual
					(methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String"))))
                                      (58 (areturn))
                                      (endofcode 59))
                                   (Exceptions )
                                   (StackMap )))
                        (method "writeObject"
                              (parameters (class "java.io.ObjectOutputStream"))
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 2) (max_locals . 6) (code_length . 110)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (invokevirtual (methodCP "defaultWriteObject" "java.io.ObjectOutputStream" () void))) 
                                      (4 (aload_1)) 
                                      (5 (aload_0)) 
                                      (6 (getfield (fieldCP "extensions" "java.security.cert.CertificateRevokedException" (class "java.util.Map")))) 
                                      (9 (invokeinterface (methodCP "size" "java.util.Map" () int) 1)) 
                                      (14 (invokevirtual (methodCP "writeInt" "java.io.ObjectOutputStream" (int) void))) 
                                      (17 (aload_0)) 
                                      (18 (getfield (fieldCP "extensions" "java.security.cert.CertificateRevokedException" (class "java.util.Map")))) 
                                      (21 (invokeinterface (methodCP "entrySet" "java.util.Map" () (class "java.util.Set")) 1)) 
                                      (26 (invokeinterface (methodCP "iterator" "java.util.Set" () (class "java.util.Iterator")) 1)) 
                                      (31 (astore_2)) 
                                      (32 (aload_2)) ;;at TAG_1
                                      (33 (invokeinterface (methodCP "hasNext" "java.util.Iterator" () boolean) 1)) 
                                      (38 (ifeq 109))  ;;to TAG_0
                                      (41 (aload_2)) 
                                      (42 (invokeinterface (methodCP "next" "java.util.Iterator" () (class "java.lang.Object")) 1)) 
                                      (47 (checkcast (class "java.util.Map$Entry"))) 
                                      (50 (astore_3)) 
                                      (51 (aload_3)) 
                                      (52 (invokeinterface (methodCP "getValue" "java.util.Map$Entry" () (class "java.lang.Object")) 1)) 
                                      (57 (checkcast (class "java.security.cert.Extension"))) 
                                      (60 (astore 4)) 
                                      (62 (aload_1)) 
                                      (63 (aload 4)) 
                                      (65 (invokeinterface (methodCP "getId" "java.security.cert.Extension" () (class "java.lang.String")) 1)) 
                                      (70 (invokevirtual (methodCP "writeObject" "java.io.ObjectOutputStream" ((class "java.lang.Object")) void))) 
                                      (73 (aload_1)) 
                                      (74 (aload 4)) 
                                      (76 (invokeinterface (methodCP "isCritical" "java.security.cert.Extension" () boolean) 1)) 
                                      (81 (invokevirtual (methodCP "writeBoolean" "java.io.ObjectOutputStream" (boolean) void))) 
                                      (84 (aload 4)) 
                                      (86 (invokeinterface (methodCP "getValue" "java.security.cert.Extension" () (array byte)) 1)) 
                                      (91 (astore 5)) 
                                      (93 (aload_1)) 
                                      (94 (aload 5)) 
                                      (96 (arraylength)) 
                                      (97 (invokevirtual (methodCP "writeInt" "java.io.ObjectOutputStream" (int) void))) 
                                      (100 (aload_1)) 
                                      (101 (aload 5)) 
                                      (103 (invokevirtual (methodCP "write" "java.io.ObjectOutputStream" ((array byte)) void))) 
                                      (106 (goto 32)) ;;to TAG_1
                                      (109 (return)) ;;at TAG_0
                                      (endofcode 110))
                                   (Exceptions )
                                   (StackMap )))
                        (method "readObject"
                              (parameters (class "java.io.ObjectInputStream"))
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 5) (max_locals . 9) (code_length . 132)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (invokevirtual (methodCP "defaultReadObject" "java.io.ObjectInputStream" () void))) 
                                      (4 (aload_0)) 
                                      (5 (new (class "java.util.Date"))) 
                                      (8 (dup)) 
                                      (9 (aload_0)) 
                                      (10 (getfield (fieldCP "revocationDate" "java.security.cert.CertificateRevokedException" (class "java.util.Date")))) 
                                      (13 (invokevirtual (methodCP "getTime" "java.util.Date" () long))) 
                                      (16 (invokespecial (methodCP "<init>" "java.util.Date" (long) void))) 
                                      (19 (putfield (fieldCP "revocationDate" "java.security.cert.CertificateRevokedException" (class "java.util.Date")))) 
                                      (22 (aload_1)) 
                                      (23 (invokevirtual (methodCP "readInt" "java.io.ObjectInputStream" () int))) 
                                      (26 (istore_2)) 
                                      (27 (iload_2)) 
                                      (28 (ifne 41)) ;;to TAG_0
                                      (31 (aload_0)) 
                                      (32 (invokestatic (methodCP "emptyMap" "java.util.Collections" () (class "java.util.Map")))) 
                                      (35 (putfield (fieldCP "extensions" "java.security.cert.CertificateRevokedException" (class "java.util.Map")))) 
                                      (38 (goto 53)) ;;to TAG_1
                                      (41 (aload_0)) ;;at TAG_0
                                      (42 (new (class "java.util.HashMap"))) 
                                      (45 (dup)) 
                                      (46 (iload_2)) 
                                      (47 (invokespecial (methodCP "<init>" "java.util.HashMap" (int) void))) 
                                      (50 (putfield (fieldCP "extensions" "java.security.cert.CertificateRevokedException" (class "java.util.Map")))) 
                                      (53 (iconst_0)) ;;at TAG_1
                                      (54 (istore_3)) 
                                      (55 (iload_3)) ;;at TAG_3
                                      (56 (iload_2)) 
                                      (57 (if_icmpge 131))  ;;to TAG_2
                                      (60 (aload_1)) 
                                      (61 (invokevirtual (methodCP "readObject" "java.io.ObjectInputStream" () (class "java.lang.Object")))) 
                                      (64 (checkcast (class "java.lang.String"))) 
                                      (67 (astore 4)) 
                                      (69 (aload_1)) 
                                      (70 (invokevirtual (methodCP "readBoolean" "java.io.ObjectInputStream" () boolean))) 
                                      (73 (istore 5)) 
                                      (75 (aload_1)) 
                                      (76 (invokevirtual (methodCP "readInt" "java.io.ObjectInputStream" () int))) 
                                      (79 (istore 6)) 
                                      (81 (iload 6)) 
                                      (83 (newarray BYTE)) 
                                      (85 (astore 7)) 
                                      (87 (aload_1)) 
                                      (88 (aload 7)) 
                                      (90 (invokevirtual (methodCP "readFully" "java.io.ObjectInputStream" ((array byte)) void))) 
                                      (93 (new (class "sun.security.util.ObjectIdentifier"))) 
                                      (96 (dup)) 
                                      (97 (aload 4)) 
                                      (99 (invokespecial (methodCP "<init>" "sun.security.util.ObjectIdentifier" ((class "java.lang.String")) void))) 
                                      (102 (iload 5)) 
                                      (104 (aload 7)) 
                                      (106 (invokestatic (methodCP "newExtension" "sun.security.x509.Extension" ((class "sun.security.util.ObjectIdentifier") boolean (array byte)) (class "sun.security.x509.Extension")))) 
                                      (109 (astore 8)) 
                                      (111 (aload_0)) 
                                      (112 (getfield (fieldCP "extensions" "java.security.cert.CertificateRevokedException" (class "java.util.Map")))) 
                                      (115 (aload 4)) 
                                      (117 (aload 8)) 
                                      (119 (invokeinterface (methodCP "put" "java.util.Map" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (124 (pop)) 
                                      (125 (iinc 3 1)) 
                                      (128 (goto 55)) ;;to TAG_3
                                      (131 (return)) ;;at TAG_2
                                      (endofcode 132))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *CertificateRevokedException-class-table*
  (make-static-class-decls 
   *java.security.cert.CertificateRevokedException*))

(defconst *package-name-map* 
  ("java.security.cert.CertificateRevokedException" . "java.security.cert"))

