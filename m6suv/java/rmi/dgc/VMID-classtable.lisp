; VMID-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:39 CDT 2014.
;

(defconst *java.rmi.dgc.VMID*
 (make-class-def
      '(class "java.rmi.dgc.VMID"
            "java.lang.Object"
            (constant_pool
                        (LONG -538642295484486218)
                        (STRING  "0")
                        (STRING  "")
                        (STRING  "SHA"))
            (fields
                        (field "localAddr" (array byte) (accessflags  *class*  *private*  *static* ) -1)
                        (field "addr" (array byte) (accessflags  *class*  *private* ) -1)
                        (field "uid" (class "java.rmi.server.UID") (accessflags  *class*  *private* ) -1)
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0))
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 23)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (getstatic (fieldCP "localAddr" "java.rmi.dgc.VMID" (array byte))))
                                      (8 (putfield (fieldCP "addr" "java.rmi.dgc.VMID" (array byte))))
                                      (11 (aload_0))
                                      (12 (new (class "java.rmi.server.UID")))
                                      (15 (dup))
                                      (16 (invokespecial
					(methodCP "<init>" "java.rmi.server.UID" () void)))
                                      (19 (putfield (fieldCP "uid" "java.rmi.dgc.VMID" (class "java.rmi.server.UID"))))
                                      (22 (return))
                                      (endofcode 23))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isUnique"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 0) (code_length . 2)
                                   (parsedcode
                                      (0 (iconst_1))
                                      (1 (ireturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hashCode"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "uid" "java.rmi.dgc.VMID" (class "java.rmi.server.UID"))))
                                      (4 (invokevirtual
					(methodCP "hashCode" "java.rmi.server.UID" () int)))
                                      (7 (ireturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "equals"
                              (parameters (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 4) (code_length . 118)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (instanceof (class "java.rmi.dgc.VMID"))) 
                                      (4 (ifeq 116)) ;;to TAG_0
                                      (7 (aload_1)) 
                                      (8 (checkcast (class "java.rmi.dgc.VMID"))) 
                                      (11 (astore_2)) 
                                      (12 (aload_0)) 
                                      (13 (getfield (fieldCP "uid" "java.rmi.dgc.VMID" (class "java.rmi.server.UID")))) 
                                      (16 (aload_2)) 
                                      (17 (getfield (fieldCP "uid" "java.rmi.dgc.VMID" (class "java.rmi.server.UID")))) 
                                      (20 (invokevirtual (methodCP "equals" "java.rmi.server.UID" ((class "java.lang.Object")) boolean))) 
                                      (23 (ifne 28))  ;;to TAG_1
                                      (26 (iconst_0)) 
                                      (27 (ireturn)) 
                                      (28 (aload_0)) ;;at TAG_1
                                      (29 (getfield (fieldCP "addr" "java.rmi.dgc.VMID" (array byte)))) 
                                      (32 (ifnonnull 39)) ;;to TAG_2
                                      (35 (iconst_1)) 
                                      (36 (goto 40)) ;;to TAG_3
                                      (39 (iconst_0)) ;;at TAG_2
                                      (40 (aload_2)) ;;at TAG_3
                                      (41 (getfield (fieldCP "addr" "java.rmi.dgc.VMID" (array byte)))) 
                                      (44 (ifnonnull 51)) ;;to TAG_4
                                      (47 (iconst_1)) 
                                      (48 (goto 52)) ;;to TAG_5
                                      (51 (iconst_0)) ;;at TAG_4
                                      (52 (ixor)) ;;at TAG_5
                                      (53 (ifeq 58)) ;;to TAG_6
                                      (56 (iconst_0)) 
                                      (57 (ireturn)) 
                                      (58 (aload_0)) ;;at TAG_6
                                      (59 (getfield (fieldCP "addr" "java.rmi.dgc.VMID" (array byte)))) 
                                      (62 (ifnull 114)) ;;to TAG_7
                                      (65 (aload_0)) 
                                      (66 (getfield (fieldCP "addr" "java.rmi.dgc.VMID" (array byte)))) 
                                      (69 (arraylength)) 
                                      (70 (aload_2)) 
                                      (71 (getfield (fieldCP "addr" "java.rmi.dgc.VMID" (array byte)))) 
                                      (74 (arraylength)) 
                                      (75 (if_icmpeq 80)) ;;to TAG_8
                                      (78 (iconst_0)) 
                                      (79 (ireturn)) 
                                      (80 (iconst_0)) ;;at TAG_8
                                      (81 (istore_3)) 
                                      (82 (iload_3)) ;;at TAG_10
                                      (83 (aload_0)) 
                                      (84 (getfield (fieldCP "addr" "java.rmi.dgc.VMID" (array byte)))) 
                                      (87 (arraylength)) 
                                      (88 (if_icmpge 114)) ;;to TAG_7
                                      (91 (aload_0)) 
                                      (92 (getfield (fieldCP "addr" "java.rmi.dgc.VMID" (array byte)))) 
                                      (95 (iload_3)) 
                                      (96 (baload)) 
                                      (97 (aload_2)) 
                                      (98 (getfield (fieldCP "addr" "java.rmi.dgc.VMID" (array byte)))) 
                                      (101 (iload_3)) 
                                      (102 (baload)) 
                                      (103 (if_icmpeq 108)) ;;to TAG_9
                                      (106 (iconst_0)) 
                                      (107 (ireturn)) 
                                      (108 (iinc 3 1)) ;;at TAG_9
                                      (111 (goto 82)) ;;to TAG_10
                                      (114 (iconst_1)) ;;at TAG_7
                                      (115 (ireturn)) 
                                      (116 (iconst_0)) ;;at TAG_0
                                      (117 (ireturn)) 
                                      (endofcode 118))
                                   (Exceptions )
                                   (StackMap )))
                        (method "toString"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 4) (code_length . 107)
                                   (parsedcode
                                      (0 (new (class "java.lang.StringBuffer"))) 
                                      (3 (dup)) 
                                      (4 (invokespecial (methodCP "<init>" "java.lang.StringBuffer" () void))) 
                                      (7 (astore_1)) 
                                      (8 (aload_0)) 
                                      (9 (getfield (fieldCP "addr" "java.rmi.dgc.VMID" (array byte)))) 
                                      (12 (ifnull 83)) ;;to TAG_0
                                      (15 (iconst_0)) 
                                      (16 (istore_2)) 
                                      (17 (iload_2)) ;;at TAG_3
                                      (18 (aload_0)) 
                                      (19 (getfield (fieldCP "addr" "java.rmi.dgc.VMID" (array byte)))) 
                                      (22 (arraylength)) 
                                      (23 (if_icmpge 83)) ;;to TAG_0
                                      (26 (aload_0)) 
                                      (27 (getfield (fieldCP "addr" "java.rmi.dgc.VMID" (array byte)))) 
                                      (30 (iload_2)) 
                                      (31 (baload)) 
                                      (32 (sipush 255)) 
                                      (35 (iand)) 
                                      (36 (istore_3)) 
                                      (37 (aload_1)) 
                                      (38 (new (class "java.lang.StringBuilder"))) 
                                      (41 (dup)) 
                                      (42 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (45 (iload_3)) 
                                      (46 (bipush 16)) 
                                      (48 (if_icmpge 56)) ;;to TAG_1
                                      (51 (ldc 1)) ;;STRING:: "0"
                                      (53 (goto 58))  ;;to TAG_2
                                      (56 (ldc 2)) ;;at TAG_1;;STRING:: ""
                                      (58 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) ;;at TAG_2
                                      (61 (iload_3)) 
                                      (62 (bipush 16)) 
                                      (64 (invokestatic (methodCP "toString" "java.lang.Integer" (int int) (class "java.lang.String")))) 
                                      (67 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (70 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (73 (invokevirtual (methodCP "append" "java.lang.StringBuffer" ((class "java.lang.String")) (class "java.lang.StringBuffer")))) 
                                      (76 (pop)) 
                                      (77 (iinc 2 1)) 
                                      (80 (goto 17)) ;;to TAG_3
                                      (83 (aload_1)) ;;at TAG_0
                                      (84 (bipush 58)) 
                                      (86 (invokevirtual (methodCP "append" "java.lang.StringBuffer" (char) (class "java.lang.StringBuffer")))) 
                                      (89 (pop)) 
                                      (90 (aload_1)) 
                                      (91 (aload_0)) 
                                      (92 (getfield (fieldCP "uid" "java.rmi.dgc.VMID" (class "java.rmi.server.UID")))) 
                                      (95 (invokevirtual (methodCP "toString" "java.rmi.server.UID" () (class "java.lang.String")))) 
                                      (98 (invokevirtual (methodCP "append" "java.lang.StringBuffer" ((class "java.lang.String")) (class "java.lang.StringBuffer")))) 
                                      (101 (pop)) 
                                      (102 (aload_1)) 
                                      (103 (invokevirtual (methodCP "toString" "java.lang.StringBuffer" () (class "java.lang.String")))) 
                                      (106 (areturn)) 
                                      (endofcode 107))
                                   (Exceptions )
                                   (StackMap )))
                        (method "computeAddressHash"
                              (parameters )
                              (returntype . (array byte))
                              (accessflags  *class*  *private*  *static* )
                              (code
                                   (max_stack . 6) (max_locals . 8) (code_length . 121)
                                   (parsedcode
                                      (0 (new (class "java.rmi.dgc.VMID$1"))) 
                                      (3 (dup)) 
                                      (4 (invokespecial (methodCP "<init>" "java.rmi.dgc.VMID$1" () void))) 
                                      (7 (invokestatic (methodCP "doPrivileged" "java.security.AccessController" ((class "java.security.PrivilegedAction")) (class "java.lang.Object")))) 
                                      (10 (checkcast (array byte))) 
                                      (13 (astore_0)) 
                                      (14 (ldc 3)) ;;at TAG_1;;STRING:: "SHA"
                                      (16 (invokestatic (methodCP "getInstance" "java.security.MessageDigest" ((class "java.lang.String")) (class "java.security.MessageDigest")))) 
                                      (19 (astore_3)) 
                                      (20 (new (class "java.io.ByteArrayOutputStream"))) 
                                      (23 (dup)) 
                                      (24 (bipush 64)) 
                                      (26 (invokespecial (methodCP "<init>" "java.io.ByteArrayOutputStream" (int) void))) 
                                      (29 (astore 4)) 
                                      (31 (new (class "java.io.DataOutputStream"))) 
                                      (34 (dup)) 
                                      (35 (new (class "java.security.DigestOutputStream"))) 
                                      (38 (dup)) 
                                      (39 (aload 4)) 
                                      (41 (aload_3)) 
                                      (42 (invokespecial (methodCP "<init>" "java.security.DigestOutputStream" ((class "java.io.OutputStream") (class "java.security.MessageDigest")) void))) 
                                      (45 (invokespecial (methodCP "<init>" "java.io.DataOutputStream" ((class "java.io.OutputStream")) void))) 
                                      (48 (astore 5)) 
                                      (50 (aload 5)) 
                                      (52 (aload_0)) 
                                      (53 (iconst_0)) 
                                      (54 (aload_0)) 
                                      (55 (arraylength)) 
                                      (56 (invokevirtual (methodCP "write" "java.io.DataOutputStream" ((array byte) int int) void))) 
                                      (59 (aload 5)) 
                                      (61 (invokevirtual (methodCP "flush" "java.io.DataOutputStream" () void))) 
                                      (64 (aload_3)) 
                                      (65 (invokevirtual (methodCP "digest" "java.security.MessageDigest" () (array byte)))) 
                                      (68 (astore 6)) 
                                      (70 (bipush 8)) 
                                      (72 (aload 6)) 
                                      (74 (arraylength)) 
                                      (75 (invokestatic (methodCP "min" "java.lang.Math" (int int) int))) 
                                      (78 (istore 7)) 
                                      (80 (iload 7)) 
                                      (82 (newarray BYTE)) 
                                      (84 (astore_1)) 
                                      (85 (aload 6)) 
                                      (87 (iconst_0)) 
                                      (88 (aload_1)) 
                                      (89 (iconst_0)) 
                                      (90 (iload 7)) 
                                      (92 (invokestatic (methodCP "arraycopy" "java.lang.System" ((class "java.lang.Object") int (class "java.lang.Object") int int) void))) 
                                      (95 (goto 119)) ;;to TAG_0;;at TAG_2
                                      (98 (astore_3)) ;;at TAG_3
                                      (99 (iconst_0)) 
                                      (100 (newarray BYTE)) 
                                      (102 (astore_1)) 
                                      (103 (goto 119)) ;;to TAG_0
                                      (106 (astore_3)) ;;at TAG_4
                                      (107 (new (class "java.lang.InternalError"))) 
                                      (110 (dup)) 
                                      (111 (aload_3)) 
                                      (112 (invokevirtual (methodCP "toString" "java.security.NoSuchAlgorithmException" () (class "java.lang.String")))) 
                                      (115 (invokespecial (methodCP "<init>" "java.lang.InternalError" ((class "java.lang.String")) void))) 
                                      (118 (athrow)) 
                                      (119 (aload_1)) ;;at TAG_0
                                      (120 (areturn)) 
                                      (endofcode 121))
                                   (Exceptions 
                                     (handler 14 95  98 (class "java.io.IOException"))
                                     (handler 14 95  106 (class "java.security.NoSuchAlgorithmException")))
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 0) (code_length . 7)
                                   (parsedcode
                                      (0 (invokestatic
					(methodCP "computeAddressHash" "java.rmi.dgc.VMID" () (array byte))))
                                      (3 (putstatic (fieldCP "localAddr" "java.rmi.dgc.VMID" (array byte))))
                                      (6 (return))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.io.Serializable")
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *VMID-class-table*
  (make-static-class-decls 
   *java.rmi.dgc.VMID*))

(defconst *package-name-map* 
  ("java.rmi.dgc.VMID" . "java.rmi.dgc"))

