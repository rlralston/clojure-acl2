; Timestamp-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:41 CDT 2014.
;

(defconst *java.security.Timestamp*
 (make-class-def
      '(class "java.security.Timestamp"
            "java.lang.Object"
            (constant_pool
                        (LONG -5502683707821851294)
                        (STRING  "(")
                        (STRING  "timestamp: ")
                        (STRING  "TSA: ")
                        (STRING  "TSA: <empty>")
                        (STRING  ")"))
            (fields
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0)
                        (field "timestamp" (class "java.util.Date") (accessflags  *class*  *private* ) -1)
                        (field "signerCertPath" (class "java.security.cert.CertPath") (accessflags  *class*  *private* ) -1)
                        (field "myhash" int (accessflags  *class*  *private*  *transient* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.Date") (class "java.security.cert.CertPath"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 3) (code_length . 46)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokespecial (methodCP "<init>" "java.lang.Object" () void))) 
                                      (4 (aload_0)) 
                                      (5 (iconst_m1)) 
                                      (6 (putfield (fieldCP "myhash" "java.security.Timestamp" int))) 
                                      (9 (aload_1)) 
                                      (10 (ifnull 17))  ;;to TAG_0
                                      (13 (aload_2)) 
                                      (14 (ifnonnull 25)) ;;to TAG_1
                                      (17 (new (class "java.lang.NullPointerException"))) ;;at TAG_0
                                      (20 (dup)) 
                                      (21 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" () void))) 
                                      (24 (athrow)) 
                                      (25 (aload_0)) ;;at TAG_1
                                      (26 (new (class "java.util.Date"))) 
                                      (29 (dup)) 
                                      (30 (aload_1)) 
                                      (31 (invokevirtual (methodCP "getTime" "java.util.Date" () long))) 
                                      (34 (invokespecial (methodCP "<init>" "java.util.Date" (long) void))) 
                                      (37 (putfield (fieldCP "timestamp" "java.security.Timestamp" (class "java.util.Date")))) 
                                      (40 (aload_0)) 
                                      (41 (aload_2)) 
                                      (42 (putfield (fieldCP "signerCertPath" "java.security.Timestamp" (class "java.security.cert.CertPath")))) 
                                      (45 (return)) 
                                      (endofcode 46))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getTimestamp"
                              (parameters )
                              (returntype . (class "java.util.Date"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 1) (code_length . 15)
                                   (parsedcode
                                      (0 (new (class "java.util.Date")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "timestamp" "java.security.Timestamp" (class "java.util.Date"))))
                                      (8 (invokevirtual
					(methodCP "getTime" "java.util.Date" () long)))
                                      (11 (invokespecial
					(methodCP "<init>" "java.util.Date" (long) void)))
                                      (14 (areturn))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getSignerCertPath"
                              (parameters )
                              (returntype . (class "java.security.cert.CertPath"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "signerCertPath" "java.security.Timestamp" (class "java.security.cert.CertPath"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hashCode"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 32)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "myhash" "java.security.Timestamp" int))) 
                                      (4 (iconst_m1)) 
                                      (5 (if_icmpne 27))  ;;to TAG_0
                                      (8 (aload_0)) 
                                      (9 (aload_0)) 
                                      (10 (getfield (fieldCP "timestamp" "java.security.Timestamp" (class "java.util.Date")))) 
                                      (13 (invokevirtual (methodCP "hashCode" "java.util.Date" () int))) 
                                      (16 (aload_0)) 
                                      (17 (getfield (fieldCP "signerCertPath" "java.security.Timestamp" (class "java.security.cert.CertPath")))) 
                                      (20 (invokevirtual (methodCP "hashCode" "java.security.cert.CertPath" () int))) 
                                      (23 (iadd)) 
                                      (24 (putfield (fieldCP "myhash" "java.security.Timestamp" int))) 
                                      (27 (aload_0)) ;;at TAG_0
                                      (28 (getfield (fieldCP "myhash" "java.security.Timestamp" int))) 
                                      (31 (ireturn)) 
                                      (endofcode 32))
                                   (Exceptions )
                                   (StackMap )))
                        (method "equals"
                              (parameters (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 59)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (ifnull 11)) ;;to TAG_0
                                      (4 (aload_1)) 
                                      (5 (instanceof (class "java.security.Timestamp"))) 
                                      (8 (ifne 13)) ;;to TAG_1
                                      (11 (iconst_0)) ;;at TAG_0
                                      (12 (ireturn)) 
                                      (13 (aload_1)) ;;at TAG_1
                                      (14 (checkcast (class "java.security.Timestamp"))) 
                                      (17 (astore_2)) 
                                      (18 (aload_0)) 
                                      (19 (aload_2)) 
                                      (20 (if_acmpne 25))  ;;to TAG_2
                                      (23 (iconst_1)) 
                                      (24 (ireturn)) 
                                      (25 (aload_0)) ;;at TAG_2
                                      (26 (getfield (fieldCP "timestamp" "java.security.Timestamp" (class "java.util.Date")))) 
                                      (29 (aload_2)) 
                                      (30 (invokevirtual (methodCP "getTimestamp" "java.security.Timestamp" () (class "java.util.Date")))) 
                                      (33 (invokevirtual (methodCP "equals" "java.util.Date" ((class "java.lang.Object")) boolean))) 
                                      (36 (ifeq 57)) ;;to TAG_3
                                      (39 (aload_0)) 
                                      (40 (getfield (fieldCP "signerCertPath" "java.security.Timestamp" (class "java.security.cert.CertPath")))) 
                                      (43 (aload_2)) 
                                      (44 (invokevirtual (methodCP "getSignerCertPath" "java.security.Timestamp" () (class "java.security.cert.CertPath")))) 
                                      (47 (invokevirtual (methodCP "equals" "java.security.cert.CertPath" ((class "java.lang.Object")) boolean))) 
                                      (50 (ifeq 57)) ;;to TAG_3
                                      (53 (iconst_1)) 
                                      (54 (goto 58)) ;;to TAG_4
                                      (57 (iconst_0)) ;;at TAG_3
                                      (58 (ireturn)) ;;at TAG_4
                                      (endofcode 59))
                                   (Exceptions )
                                   (StackMap )))
                        (method "toString"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 111)
                                   (parsedcode
                                      (0 (new (class "java.lang.StringBuffer"))) 
                                      (3 (dup)) 
                                      (4 (invokespecial (methodCP "<init>" "java.lang.StringBuffer" () void))) 
                                      (7 (astore_1)) 
                                      (8 (aload_1)) 
                                      (9 (ldc 1)) ;;STRING:: "("
                                      (11 (invokevirtual (methodCP "append" "java.lang.StringBuffer" ((class "java.lang.String")) (class "java.lang.StringBuffer")))) 
                                      (14 (pop)) 
                                      (15 (aload_1)) 
                                      (16 (new (class "java.lang.StringBuilder"))) 
                                      (19 (dup)) 
                                      (20 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (23 (ldc 2)) ;;STRING:: "timestamp: "
                                      (25 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (28 (aload_0)) 
                                      (29 (getfield (fieldCP "timestamp" "java.security.Timestamp" (class "java.util.Date")))) 
                                      (32 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.Object")) (class "java.lang.StringBuilder")))) 
                                      (35 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (38 (invokevirtual (methodCP "append" "java.lang.StringBuffer" ((class "java.lang.String")) (class "java.lang.StringBuffer")))) 
                                      (41 (pop)) 
                                      (42 (aload_0)) 
                                      (43 (getfield (fieldCP "signerCertPath" "java.security.Timestamp" (class "java.security.cert.CertPath")))) 
                                      (46 (invokevirtual (methodCP "getCertificates" "java.security.cert.CertPath" () (class "java.util.List")))) 
                                      (49 (astore_2)) 
                                      (50 (aload_2)) 
                                      (51 (invokeinterface (methodCP "isEmpty" "java.util.List" () boolean) 1)) 
                                      (56 (ifne 92))  ;;to TAG_0
                                      (59 (aload_1)) 
                                      (60 (new (class "java.lang.StringBuilder"))) 
                                      (63 (dup)) 
                                      (64 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (67 (ldc 3)) ;;STRING:: "TSA: "
                                      (69 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (72 (aload_2)) 
                                      (73 (iconst_0)) 
                                      (74 (invokeinterface (methodCP "get" "java.util.List" (int) (class "java.lang.Object")) 2)) 
                                      (79 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.Object")) (class "java.lang.StringBuilder")))) 
                                      (82 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (85 (invokevirtual (methodCP "append" "java.lang.StringBuffer" ((class "java.lang.String")) (class "java.lang.StringBuffer")))) 
                                      (88 (pop)) 
                                      (89 (goto 99)) ;;to TAG_1
                                      (92 (aload_1)) ;;at TAG_0
                                      (93 (ldc 4)) ;;STRING:: "TSA: <empty>"
                                      (95 (invokevirtual (methodCP "append" "java.lang.StringBuffer" ((class "java.lang.String")) (class "java.lang.StringBuffer")))) 
                                      (98 (pop)) 
                                      (99 (aload_1)) ;;at TAG_1
                                      (100 (ldc 5)) ;;STRING:: ")"
                                      (102 (invokevirtual (methodCP "append" "java.lang.StringBuffer" ((class "java.lang.String")) (class "java.lang.StringBuffer")))) 
                                      (105 (pop)) 
                                      (106 (aload_1)) 
                                      (107 (invokevirtual (methodCP "toString" "java.lang.StringBuffer" () (class "java.lang.String")))) 
                                      (110 (areturn)) 
                                      (endofcode 111))
                                   (Exceptions )
                                   (StackMap )))
                        (method "readObject"
                              (parameters (class "java.io.ObjectInputStream"))
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 5) (max_locals . 2) (code_length . 28)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (invokevirtual
					(methodCP "defaultReadObject" "java.io.ObjectInputStream" () void)))
                                      (4 (aload_0))
                                      (5 (iconst_m1))
                                      (6 (putfield (fieldCP "myhash" "java.security.Timestamp" int)))
                                      (9 (aload_0))
                                      (10 (new (class "java.util.Date")))
                                      (13 (dup))
                                      (14 (aload_0))
                                      (15 (getfield (fieldCP "timestamp" "java.security.Timestamp" (class "java.util.Date"))))
                                      (18 (invokevirtual
					(methodCP "getTime" "java.util.Date" () long)))
                                      (21 (invokespecial
					(methodCP "<init>" "java.util.Date" (long) void)))
                                      (24 (putfield (fieldCP "timestamp" "java.security.Timestamp" (class "java.util.Date"))))
                                      (27 (return))
                                      (endofcode 28))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.io.Serializable")
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *Timestamp-class-table*
  (make-static-class-decls 
   *java.security.Timestamp*))

(defconst *package-name-map* 
  ("java.security.Timestamp" . "java.security"))

