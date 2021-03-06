; Permissions-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:40 CDT 2014.
;

(defconst *java.security.Permissions*
 (make-class-def
      '(class "java.security.Permissions"
            "java.security.PermissionCollection"
            (constant_pool
                        (LONG 4858622370623524688)
                        (STRING  "attempt to add a Permission to a readonly Permissions object")
                        (STRING  "allPermission")
                        (STRING  "perms"))
            (fields
                        (field "permsMap" (class "java.util.Map") (accessflags  *class*  *private*  *transient* ) -1)
                        (field "hasUnresolved" boolean (accessflags  *class*  *private*  *transient* ) -1)
                        (field "allPermission" (class "java.security.PermissionCollection") (accessflags  *class* ) -1)
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0)
                        (field "serialPersistentFields" (array (class "java.io.ObjectStreamField")) (accessflags  *class*  *final*  *private*  *static* ) -1))
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 1) (code_length . 28)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.security.PermissionCollection" () void)))
                                      (4 (aload_0))
                                      (5 (iconst_0))
                                      (6 (putfield (fieldCP "hasUnresolved" "java.security.Permissions" boolean)))
                                      (9 (aload_0))
                                      (10 (new (class "java.util.HashMap")))
                                      (13 (dup))
                                      (14 (bipush 11))
                                      (16 (invokespecial
					(methodCP "<init>" "java.util.HashMap" (int) void)))
                                      (19 (putfield (fieldCP "permsMap" "java.security.Permissions" (class "java.util.Map"))))
                                      (22 (aload_0))
                                      (23 (aconst_null))
                                      (24 (putfield (fieldCP "allPermission" "java.security.Permissions" (class "java.security.PermissionCollection"))))
                                      (27 (return))
                                      (endofcode 28))
                                   (Exceptions )
                                   (StackMap )))
                        (method "add"
                              (parameters (class "java.security.Permission"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 5) (code_length . 70)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "isReadOnly" "java.security.Permissions" () boolean))) 
                                      (4 (ifeq 17)) ;;to TAG_0
                                      (7 (new (class "java.lang.SecurityException"))) 
                                      (10 (dup)) 
                                      (11 (ldc 1)) ;;STRING:: "attempt to add a Permission to a readonly Permissions object"
                                      (13 (invokespecial (methodCP "<init>" "java.lang.SecurityException" ((class "java.lang.String")) void))) 
                                      (16 (athrow)) 
                                      (17 (aload_0)) ;;at TAG_0
                                      (18 (dup)) 
                                      (19 (astore_3)) 
                                      (20 (monitorenter)) 
                                      (21 (aload_0)) ;;at TAG_4
                                      (22 (aload_1)) 
                                      (23 (iconst_1)) 
                                      (24 (invokespecial (methodCP "getPermissionCollection" "java.security.Permissions" ((class "java.security.Permission") boolean) (class "java.security.PermissionCollection")))) 
                                      (27 (astore_2)) 
                                      (28 (aload_2)) 
                                      (29 (aload_1)) 
                                      (30 (invokevirtual (methodCP "add" "java.security.PermissionCollection" ((class "java.security.Permission")) void))) 
                                      (33 (aload_3)) 
                                      (34 (monitorexit)) 
                                      (35 (goto 45)) ;;to TAG_1;;at TAG_5
                                      (38 (astore 4)) ;;at TAG_6
                                      (40 (aload_3)) 
                                      (41 (monitorexit)) 
                                      (42 (aload 4)) ;;at TAG_7
                                      (44 (athrow)) 
                                      (45 (aload_1)) ;;at TAG_1
                                      (46 (instanceof (class "java.security.AllPermission"))) 
                                      (49 (ifeq 57))  ;;to TAG_2
                                      (52 (aload_0)) 
                                      (53 (aload_2)) 
                                      (54 (putfield (fieldCP "allPermission" "java.security.Permissions" (class "java.security.PermissionCollection")))) 
                                      (57 (aload_1)) ;;at TAG_2
                                      (58 (instanceof (class "java.security.UnresolvedPermission"))) 
                                      (61 (ifeq 69)) ;;to TAG_3
                                      (64 (aload_0)) 
                                      (65 (iconst_1)) 
                                      (66 (putfield (fieldCP "hasUnresolved" "java.security.Permissions" boolean))) 
                                      (69 (return)) ;;at TAG_3
                                      (endofcode 70))
                                   (Exceptions 
                                     (handler 21 35  38 (class "java.lang.Throwable"))
                                     (handler 38 42  38 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "implies"
                              (parameters (class "java.security.Permission"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 5) (code_length . 43)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "allPermission" "java.security.Permissions" (class "java.security.PermissionCollection")))) 
                                      (4 (ifnull 9)) ;;to TAG_0
                                      (7 (iconst_1)) 
                                      (8 (ireturn)) 
                                      (9 (aload_0)) ;;at TAG_0
                                      (10 (dup)) 
                                      (11 (astore_2)) 
                                      (12 (monitorenter)) 
                                      (13 (aload_0)) ;;at TAG_2
                                      (14 (aload_1)) 
                                      (15 (iconst_0)) 
                                      (16 (invokespecial (methodCP "getPermissionCollection" "java.security.Permissions" ((class "java.security.Permission") boolean) (class "java.security.PermissionCollection")))) 
                                      (19 (astore_3)) 
                                      (20 (aload_3)) 
                                      (21 (ifnull 32)) ;;to TAG_1
                                      (24 (aload_3)) 
                                      (25 (aload_1)) 
                                      (26 (invokevirtual (methodCP "implies" "java.security.PermissionCollection" ((class "java.security.Permission")) boolean))) 
                                      (29 (aload_2)) 
                                      (30 (monitorexit)) 
                                      (31 (ireturn)) ;;at TAG_3
                                      (32 (iconst_0)) ;;at TAG_1
                                      (33 (aload_2)) 
                                      (34 (monitorexit)) 
                                      (35 (ireturn)) ;;at TAG_5
                                      (36 (astore 4)) ;;at TAG_4
                                      (38 (aload_2)) 
                                      (39 (monitorexit)) 
                                      (40 (aload 4)) ;;at TAG_6
                                      (42 (athrow)) 
                                      (endofcode 43))
                                   (Exceptions 
                                     (handler 13 31  36 (class "java.lang.Throwable"))
                                     (handler 32 35  36 (class "java.lang.Throwable"))
                                     (handler 36 40  36 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "elements"
                              (parameters )
                              (returntype . (class "java.util.Enumeration"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 33)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (dup)) 
                                      (2 (astore_1)) 
                                      (3 (monitorenter)) 
                                      (4 (new (class "java.security.PermissionsEnumerator"))) ;;at TAG_0
                                      (7 (dup)) 
                                      (8 (aload_0)) 
                                      (9 (getfield (fieldCP "permsMap" "java.security.Permissions" (class "java.util.Map")))) 
                                      (12 (invokeinterface (methodCP "values" "java.util.Map" () (class "java.util.Collection")) 1)) 
                                      (17 (invokeinterface (methodCP "iterator" "java.util.Collection" () (class "java.util.Iterator")) 1)) 
                                      (22 (invokespecial (methodCP "<init>" "java.security.PermissionsEnumerator" ((class "java.util.Iterator")) void))) 
                                      (25 (aload_1)) 
                                      (26 (monitorexit)) 
                                      (27 (areturn)) ;;at TAG_1
                                      (28 (astore_2)) ;;at TAG_2
                                      (29 (aload_1)) 
                                      (30 (monitorexit)) 
                                      (31 (aload_2)) ;;at TAG_3
                                      (32 (athrow)) 
                                      (endofcode 33))
                                   (Exceptions 
                                     (handler 4 27  28 (class "java.lang.Throwable"))
                                     (handler 28 31  28 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "getPermissionCollection"
                              (parameters (class "java.security.Permission") boolean)
                              (returntype . (class "java.security.PermissionCollection"))
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 3) (max_locals . 5) (code_length . 107)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (invokevirtual (methodCP "getClass" "java.lang.Object" () (class "java.lang.Class")))) 
                                      (4 (astore_3)) 
                                      (5 (aload_0)) 
                                      (6 (getfield (fieldCP "permsMap" "java.security.Permissions" (class "java.util.Map")))) 
                                      (9 (aload_3)) 
                                      (10 (invokeinterface (methodCP "get" "java.util.Map" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (15 (checkcast (class "java.security.PermissionCollection"))) 
                                      (18 (astore 4)) 
                                      (20 (aload_0)) 
                                      (21 (getfield (fieldCP "hasUnresolved" "java.security.Permissions" boolean))) 
                                      (24 (ifne 34)) ;;to TAG_0
                                      (27 (iload_2)) 
                                      (28 (ifne 34)) ;;to TAG_0
                                      (31 (aload 4)) 
                                      (33 (areturn)) 
                                      (34 (aload 4)) ;;at TAG_0
                                      (36 (ifnonnull 104)) ;;to TAG_1
                                      (39 (aload_0)) 
                                      (40 (getfield (fieldCP "hasUnresolved" "java.security.Permissions" boolean))) 
                                      (43 (ifeq 54))  ;;to TAG_2
                                      (46 (aload_0)) 
                                      (47 (aload_1)) 
                                      (48 (invokespecial (methodCP "getUnresolvedPermissions" "java.security.Permissions" ((class "java.security.Permission")) (class "java.security.PermissionCollection")))) 
                                      (51 (goto 55)) ;;to TAG_3
                                      (54 (aconst_null)) ;;at TAG_2
                                      (55 (astore 4)) ;;at TAG_3
                                      (57 (aload 4)) 
                                      (59 (ifnonnull 86)) ;;to TAG_4
                                      (62 (iload_2)) 
                                      (63 (ifeq 86)) ;;to TAG_4
                                      (66 (aload_1)) 
                                      (67 (invokevirtual (methodCP "newPermissionCollection" "java.security.Permission" () (class "java.security.PermissionCollection")))) 
                                      (70 (astore 4)) 
                                      (72 (aload 4)) 
                                      (74 (ifnonnull 86)) ;;to TAG_4
                                      (77 (new (class "java.security.PermissionsHash"))) 
                                      (80 (dup)) 
                                      (81 (invokespecial (methodCP "<init>" "java.security.PermissionsHash" () void))) 
                                      (84 (astore 4)) 
                                      (86 (aload 4)) ;;at TAG_4
                                      (88 (ifnull 104)) ;;to TAG_1
                                      (91 (aload_0)) 
                                      (92 (getfield (fieldCP "permsMap" "java.security.Permissions" (class "java.util.Map")))) 
                                      (95 (aload_3)) 
                                      (96 (aload 4)) 
                                      (98 (invokeinterface (methodCP "put" "java.util.Map" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (103 (pop)) 
                                      (104 (aload 4)) ;;at TAG_1
                                      (106 (areturn)) 
                                      (endofcode 107))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getUnresolvedPermissions"
                              (parameters (class "java.security.Permission"))
                              (returntype . (class "java.security.PermissionCollection"))
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 4) (max_locals . 14) (code_length . 248)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "permsMap" "java.security.Permissions" (class "java.util.Map")))) 
                                      (4 (ldc_w )) 
                                      (7 (invokeinterface (methodCP "get" "java.util.Map" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (12 (checkcast (class "java.security.UnresolvedPermissionCollection"))) 
                                      (15 (astore_2)) 
                                      (16 (aload_2)) 
                                      (17 (ifnonnull 22)) ;;to TAG_0
                                      (20 (aconst_null)) 
                                      (21 (areturn)) 
                                      (22 (aload_2)) ;;at TAG_0
                                      (23 (aload_1)) 
                                      (24 (invokevirtual (methodCP "getUnresolvedPermissions" "java.security.UnresolvedPermissionCollection" ((class "java.security.Permission")) (class "java.util.List")))) 
                                      (27 (astore_3)) 
                                      (28 (aload_3)) 
                                      (29 (ifnonnull 34))  ;;to TAG_1
                                      (32 (aconst_null)) 
                                      (33 (areturn)) 
                                      (34 (aconst_null)) ;;at TAG_1
                                      (35 (astore 4)) 
                                      (37 (aload_1)) 
                                      (38 (invokevirtual (methodCP "getClass" "java.lang.Object" () (class "java.lang.Class")))) 
                                      (41 (invokevirtual (methodCP "getSigners" "java.lang.Class" () (array (class "java.lang.Object"))))) 
                                      (44 (astore 5)) 
                                      (46 (iconst_0)) 
                                      (47 (istore 6)) 
                                      (49 (aload 5)) 
                                      (51 (ifnull 139)) ;;to TAG_2
                                      (54 (iconst_0)) 
                                      (55 (istore 7)) 
                                      (57 (iload 7)) ;;at TAG_5
                                      (59 (aload 5)) 
                                      (61 (arraylength)) 
                                      (62 (if_icmpge 85)) ;;to TAG_3
                                      (65 (aload 5)) 
                                      (67 (iload 7)) 
                                      (69 (aaload)) 
                                      (70 (instanceof (class "java.security.cert.Certificate"))) 
                                      (73 (ifeq 79)) ;;to TAG_4
                                      (76 (iinc 6 1)) 
                                      (79 (iinc 7 1)) ;;at TAG_4
                                      (82 (goto 57)) ;;to TAG_5
                                      (85 (iload 6)) ;;at TAG_3
                                      (87 (anewarray (class "java.security.cert.Certificate"))) 
                                      (90 (astore 4)) 
                                      (92 (iconst_0)) 
                                      (93 (istore 6)) 
                                      (95 (iconst_0)) 
                                      (96 (istore 7)) 
                                      (98 (iload 7)) ;;at TAG_7
                                      (100 (aload 5)) 
                                      (102 (arraylength)) 
                                      (103 (if_icmpge 139)) ;;to TAG_2
                                      (106 (aload 5)) 
                                      (108 (iload 7)) 
                                      (110 (aaload)) 
                                      (111 (instanceof (class "java.security.cert.Certificate"))) 
                                      (114 (ifeq 133)) ;;to TAG_6
                                      (117 (aload 4)) 
                                      (119 (iload 6)) 
                                      (121 (iinc 6 1)) 
                                      (124 (aload 5)) 
                                      (126 (iload 7)) 
                                      (128 (aaload)) 
                                      (129 (checkcast (class "java.security.cert.Certificate"))) 
                                      (132 (aastore)) 
                                      (133 (iinc 7 1)) ;;at TAG_6
                                      (136 (goto 98)) ;;to TAG_7
                                      (139 (aconst_null)) ;;at TAG_2
                                      (140 (astore 7)) 
                                      (142 (aload_3)) 
                                      (143 (dup)) 
                                      (144 (astore 8)) 
                                      (146 (monitorenter)) 
                                      (147 (aload_3)) ;;at TAG_13
                                      (148 (invokeinterface (methodCP "size" "java.util.List" () int) 1)) 
                                      (153 (istore 9)) 
                                      (155 (iconst_0)) 
                                      (156 (istore 10)) 
                                      (158 (iload 10)) ;;at TAG_11
                                      (160 (iload 9)) 
                                      (162 (if_icmpge 231)) ;;to TAG_8
                                      (165 (aload_3)) 
                                      (166 (iload 10)) 
                                      (168 (invokeinterface (methodCP "get" "java.util.List" (int) (class "java.lang.Object")) 2)) 
                                      (173 (checkcast (class "java.security.UnresolvedPermission"))) 
                                      (176 (astore 11)) 
                                      (178 (aload 11)) 
                                      (180 (aload_1)) 
                                      (181 (aload 4)) 
                                      (183 (invokevirtual (methodCP "resolve" "java.security.UnresolvedPermission" ((class "java.security.Permission") (array (class "java.security.cert.Certificate"))) (class "java.security.Permission")))) 
                                      (186 (astore 12)) 
                                      (188 (aload 12)) 
                                      (190 (ifnull 225)) ;;to TAG_9
                                      (193 (aload 7)) 
                                      (195 (ifnonnull 218)) ;;to TAG_10
                                      (198 (aload_1)) 
                                      (199 (invokevirtual (methodCP "newPermissionCollection" "java.security.Permission" () (class "java.security.PermissionCollection")))) 
                                      (202 (astore 7)) 
                                      (204 (aload 7)) 
                                      (206 (ifnonnull 218)) ;;to TAG_10
                                      (209 (new (class "java.security.PermissionsHash"))) 
                                      (212 (dup)) 
                                      (213 (invokespecial (methodCP "<init>" "java.security.PermissionsHash" () void))) 
                                      (216 (astore 7)) 
                                      (218 (aload 7)) ;;at TAG_10
                                      (220 (aload 12)) 
                                      (222 (invokevirtual (methodCP "add" "java.security.PermissionCollection" ((class "java.security.Permission")) void))) 
                                      (225 (iinc 10 1)) ;;at TAG_9
                                      (228 (goto 158)) ;;to TAG_11
                                      (231 (aload 8)) ;;at TAG_8
                                      (233 (monitorexit)) 
                                      (234 (goto 245)) ;;to TAG_12;;at TAG_14
                                      (237 (astore 13)) ;;at TAG_15
                                      (239 (aload 8)) 
                                      (241 (monitorexit)) 
                                      (242 (aload 13)) ;;at TAG_16
                                      (244 (athrow)) 
                                      (245 (aload 7)) ;;at TAG_12
                                      (247 (areturn)) 
                                      (endofcode 248))
                                   (Exceptions 
                                     (handler 147 234  237 (class "java.lang.Throwable"))
                                     (handler 237 242  237 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "writeObject"
                              (parameters (class "java.io.ObjectOutputStream"))
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 4) (max_locals . 5) (code_length . 70)
                                   (parsedcode
                                      (0 (new (class "java.util.Hashtable"))) 
                                      (3 (dup)) 
                                      (4 (aload_0)) 
                                      (5 (getfield (fieldCP "permsMap" "java.security.Permissions" (class "java.util.Map")))) 
                                      (8 (invokeinterface (methodCP "size" "java.util.Map" () int) 1)) 
                                      (13 (iconst_2)) 
                                      (14 (imul)) 
                                      (15 (invokespecial (methodCP "<init>" "java.util.Hashtable" (int) void))) 
                                      (18 (astore_2)) 
                                      (19 (aload_0)) 
                                      (20 (dup)) 
                                      (21 (astore_3)) 
                                      (22 (monitorenter)) 
                                      (23 (aload_2)) ;;at TAG_1
                                      (24 (aload_0)) 
                                      (25 (getfield (fieldCP "permsMap" "java.security.Permissions" (class "java.util.Map")))) 
                                      (28 (invokevirtual (methodCP "putAll" "java.util.Hashtable" ((class "java.util.Map")) void))) 
                                      (31 (aload_3)) 
                                      (32 (monitorexit)) 
                                      (33 (goto 43)) ;;to TAG_0;;at TAG_2
                                      (36 (astore 4)) ;;at TAG_3
                                      (38 (aload_3)) 
                                      (39 (monitorexit)) 
                                      (40 (aload 4)) ;;at TAG_4
                                      (42 (athrow)) 
                                      (43 (aload_1)) ;;at TAG_0
                                      (44 (invokevirtual (methodCP "putFields" "java.io.ObjectOutputStream" () (class "java.io.ObjectOutputStream$PutField")))) 
                                      (47 (astore_3)) 
                                      (48 (aload_3)) 
                                      (49 (ldc 2)) ;;STRING:: "allPermission"
                                      (51 (aload_0)) 
                                      (52 (getfield (fieldCP "allPermission" "java.security.Permissions" (class "java.security.PermissionCollection")))) 
                                      (55 (invokevirtual (methodCP "put" "java.io.ObjectOutputStream$PutField" ((class "java.lang.String") (class "java.lang.Object")) void))) 
                                      (58 (aload_3)) 
                                      (59 (ldc 3)) ;;STRING:: "perms"
                                      (61 (aload_2)) 
                                      (62 (invokevirtual (methodCP "put" "java.io.ObjectOutputStream$PutField" ((class "java.lang.String") (class "java.lang.Object")) void))) 
                                      (65 (aload_1)) 
                                      (66 (invokevirtual (methodCP "writeFields" "java.io.ObjectOutputStream" () void))) 
                                      (69 (return)) 
                                      (endofcode 70))
                                   (Exceptions 
                                     (handler 23 33  36 (class "java.lang.Throwable"))
                                     (handler 36 40  36 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "readObject"
                              (parameters (class "java.io.ObjectInputStream"))
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 5) (max_locals . 5) (code_length . 102)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (invokevirtual (methodCP "readFields" "java.io.ObjectInputStream" () (class "java.io.ObjectInputStream$GetField")))) 
                                      (4 (astore_2)) 
                                      (5 (aload_0)) 
                                      (6 (aload_2)) 
                                      (7 (ldc 2)) ;;STRING:: "allPermission"
                                      (9 (aconst_null)) 
                                      (10 (invokevirtual (methodCP "get" "java.io.ObjectInputStream$GetField" ((class "java.lang.String") (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (13 (checkcast (class "java.security.PermissionCollection"))) 
                                      (16 (putfield (fieldCP "allPermission" "java.security.Permissions" (class "java.security.PermissionCollection")))) 
                                      (19 (aload_2)) 
                                      (20 (ldc 3)) ;;STRING:: "perms"
                                      (22 (aconst_null)) 
                                      (23 (invokevirtual (methodCP "get" "java.io.ObjectInputStream$GetField" ((class "java.lang.String") (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (26 (checkcast (class "java.util.Hashtable"))) 
                                      (29 (astore_3)) 
                                      (30 (aload_0)) 
                                      (31 (new (class "java.util.HashMap"))) 
                                      (34 (dup)) 
                                      (35 (aload_3)) 
                                      (36 (invokevirtual (methodCP "size" "java.util.Hashtable" () int))) 
                                      (39 (iconst_2)) 
                                      (40 (imul)) 
                                      (41 (invokespecial (methodCP "<init>" "java.util.HashMap" (int) void))) 
                                      (44 (putfield (fieldCP "permsMap" "java.security.Permissions" (class "java.util.Map")))) 
                                      (47 (aload_0)) 
                                      (48 (getfield (fieldCP "permsMap" "java.security.Permissions" (class "java.util.Map")))) 
                                      (51 (aload_3)) 
                                      (52 (invokeinterface (methodCP "putAll" "java.util.Map" ((class "java.util.Map")) void) 2)) 
                                      (57 (aload_0)) 
                                      (58 (getfield (fieldCP "permsMap" "java.security.Permissions" (class "java.util.Map")))) 
                                      (61 (ldc_w )) 
                                      (64 (invokeinterface (methodCP "get" "java.util.Map" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (69 (checkcast (class "java.security.UnresolvedPermissionCollection"))) 
                                      (72 (astore 4)) 
                                      (74 (aload_0)) 
                                      (75 (aload 4)) 
                                      (77 (ifnull 97))  ;;to TAG_0
                                      (80 (aload 4)) 
                                      (82 (invokevirtual (methodCP "elements" "java.security.UnresolvedPermissionCollection" () (class "java.util.Enumeration")))) 
                                      (85 (invokeinterface (methodCP "hasMoreElements" "java.util.Enumeration" () boolean) 1)) 
                                      (90 (ifeq 97))  ;;to TAG_0
                                      (93 (iconst_1)) 
                                      (94 (goto 98)) ;;to TAG_1
                                      (97 (iconst_0)) ;;at TAG_0
                                      (98 (putfield (fieldCP "hasUnresolved" "java.security.Permissions" boolean))) ;;at TAG_1
                                      (101 (return)) 
                                      (endofcode 102))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 7) (max_locals . 0) (code_length . 38)
                                   (parsedcode
                                      (0 (iconst_2))
                                      (1 (anewarray (class "java.io.ObjectStreamField")))
                                      (4 (dup))
                                      (5 (iconst_0))
                                      (6 (new (class "java.io.ObjectStreamField")))
                                      (9 (dup))
                                      (10 (ldc 3))        ;;STRING:: "perms"
                                      (12 (ldc_w ))
                                      (15 (invokespecial
					(methodCP "<init>" "java.io.ObjectStreamField" ((class "java.lang.String") (class "java.lang.Class")) void)))
                                      (18 (aastore))
                                      (19 (dup))
                                      (20 (iconst_1))
                                      (21 (new (class "java.io.ObjectStreamField")))
                                      (24 (dup))
                                      (25 (ldc 2))        ;;STRING:: "allPermission"
                                      (27 (ldc_w ))
                                      (30 (invokespecial
					(methodCP "<init>" "java.io.ObjectStreamField" ((class "java.lang.String") (class "java.lang.Class")) void)))
                                      (33 (aastore))
                                      (34 (putstatic (fieldCP "serialPersistentFields" "java.security.Permissions" (array (class "java.io.ObjectStreamField")))))
                                      (37 (return))
                                      (endofcode 38))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.io.Serializable")
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *Permissions-class-table*
  (make-static-class-decls 
   *java.security.Permissions*))

(defconst *package-name-map* 
  ("java.security.Permissions" . "java.security"))

