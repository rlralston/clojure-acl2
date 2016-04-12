; FileSystems-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:39 CDT 2014.
;

(defconst *java.nio.file.FileSystems*
 (make-class-def
      '(class "java.nio.file.FileSystems"
            "java.lang.Object"
            (constant_pool
                        (STRING  "Provider \"")
                        (STRING  "\" not found")
                        (STRING  "Provider not found"))
            (fields)
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *private* )
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
                        (method "getDefault"
                              (parameters )
                              (returntype . (class "java.nio.file.FileSystem"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 0) (code_length . 4)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "defaultFileSystem" "java.nio.file.FileSystems$DefaultFileSystemHolder" (class "java.nio.file.FileSystem"))))
                                      (3 (areturn))
                                      (endofcode 4))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getFileSystem"
                              (parameters (class "java.net.URI"))
                              (returntype . (class "java.nio.file.FileSystem"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 4) (code_length . 85)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "getScheme" "java.net.URI" () (class "java.lang.String")))) 
                                      (4 (astore_1)) 
                                      (5 (invokestatic (methodCP "installedProviders" "java.nio.file.spi.FileSystemProvider" () (class "java.util.List")))) 
                                      (8 (invokeinterface (methodCP "iterator" "java.util.List" () (class "java.util.Iterator")) 1)) 
                                      (13 (astore_2)) 
                                      (14 (aload_2)) ;;at TAG_2
                                      (15 (invokeinterface (methodCP "hasNext" "java.util.Iterator" () boolean) 1)) 
                                      (20 (ifeq 53)) ;;to TAG_0
                                      (23 (aload_2)) 
                                      (24 (invokeinterface (methodCP "next" "java.util.Iterator" () (class "java.lang.Object")) 1)) 
                                      (29 (checkcast (class "java.nio.file.spi.FileSystemProvider"))) 
                                      (32 (astore_3)) 
                                      (33 (aload_1)) 
                                      (34 (aload_3)) 
                                      (35 (invokevirtual (methodCP "getScheme" "java.nio.file.spi.FileSystemProvider" () (class "java.lang.String")))) 
                                      (38 (invokevirtual (methodCP "equalsIgnoreCase" "java.lang.String" ((class "java.lang.String")) boolean))) 
                                      (41 (ifeq 50)) ;;to TAG_1
                                      (44 (aload_3)) 
                                      (45 (aload_0)) 
                                      (46 (invokevirtual (methodCP "getFileSystem" "java.nio.file.spi.FileSystemProvider" ((class "java.net.URI")) (class "java.nio.file.FileSystem")))) 
                                      (49 (areturn)) 
                                      (50 (goto 14))  ;;to TAG_2;;at TAG_1
                                      (53 (new (class "java.nio.file.ProviderNotFoundException"))) ;;at TAG_0
                                      (56 (dup)) 
                                      (57 (new (class "java.lang.StringBuilder"))) 
                                      (60 (dup)) 
                                      (61 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (64 (ldc 0)) ;;STRING:: "Provider \""
                                      (66 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (69 (aload_1)) 
                                      (70 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (73 (ldc 1)) ;;STRING:: "\" not found"
                                      (75 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (78 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (81 (invokespecial (methodCP "<init>" "java.nio.file.ProviderNotFoundException" ((class "java.lang.String")) void))) 
                                      (84 (athrow)) 
                                      (endofcode 85))
                                   (Exceptions )
                                   (StackMap )))
                        (method "newFileSystem"
                              (parameters (class "java.net.URI") (class "java.util.Map"))
                              (returntype . (class "java.nio.file.FileSystem"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 7)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (aconst_null))
                                      (3 (invokestatic
					(methodCP "newFileSystem" "java.nio.file.FileSystems" ((class "java.net.URI") (class "java.util.Map") (class "java.lang.ClassLoader")) (class "java.nio.file.FileSystem"))))
                                      (6 (areturn))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap )))
                        (method "newFileSystem"
                              (parameters (class "java.net.URI") (class "java.util.Map") (class "java.lang.ClassLoader"))
                              (returntype . (class "java.nio.file.FileSystem"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 7) (code_length . 157)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "getScheme" "java.net.URI" () (class "java.lang.String")))) 
                                      (4 (astore_3)) 
                                      (5 (invokestatic (methodCP "installedProviders" "java.nio.file.spi.FileSystemProvider" () (class "java.util.List")))) 
                                      (8 (invokeinterface (methodCP "iterator" "java.util.List" () (class "java.util.Iterator")) 1)) 
                                      (13 (astore 4)) 
                                      (15 (aload 4)) ;;at TAG_2
                                      (17 (invokeinterface (methodCP "hasNext" "java.util.Iterator" () boolean) 1)) 
                                      (22 (ifeq 60)) ;;to TAG_0
                                      (25 (aload 4)) 
                                      (27 (invokeinterface (methodCP "next" "java.util.Iterator" () (class "java.lang.Object")) 1)) 
                                      (32 (checkcast (class "java.nio.file.spi.FileSystemProvider"))) 
                                      (35 (astore 5)) 
                                      (37 (aload_3)) 
                                      (38 (aload 5)) 
                                      (40 (invokevirtual (methodCP "getScheme" "java.nio.file.spi.FileSystemProvider" () (class "java.lang.String")))) 
                                      (43 (invokevirtual (methodCP "equalsIgnoreCase" "java.lang.String" ((class "java.lang.String")) boolean))) 
                                      (46 (ifeq 57)) ;;to TAG_1
                                      (49 (aload 5)) 
                                      (51 (aload_0)) 
                                      (52 (aload_1)) 
                                      (53 (invokevirtual (methodCP "newFileSystem" "java.nio.file.spi.FileSystemProvider" ((class "java.net.URI") (class "java.util.Map")) (class "java.nio.file.FileSystem")))) 
                                      (56 (areturn)) 
                                      (57 (goto 15))  ;;to TAG_2;;at TAG_1
                                      (60 (aload_2)) ;;at TAG_0
                                      (61 (ifnull 125)) ;;to TAG_3
                                      (64 (ldc_w )) 
                                      (67 (aload_2)) 
                                      (68 (invokestatic (methodCP "load" "java.util.ServiceLoader" ((class "java.lang.Class") (class "java.lang.ClassLoader")) (class "java.util.ServiceLoader")))) 
                                      (71 (astore 4)) 
                                      (73 (aload 4)) 
                                      (75 (invokevirtual (methodCP "iterator" "java.util.ServiceLoader" () (class "java.util.Iterator")))) 
                                      (78 (astore 5)) 
                                      (80 (aload 5)) ;;at TAG_5
                                      (82 (invokeinterface (methodCP "hasNext" "java.util.Iterator" () boolean) 1)) 
                                      (87 (ifeq 125)) ;;to TAG_3
                                      (90 (aload 5)) 
                                      (92 (invokeinterface (methodCP "next" "java.util.Iterator" () (class "java.lang.Object")) 1)) 
                                      (97 (checkcast (class "java.nio.file.spi.FileSystemProvider"))) 
                                      (100 (astore 6)) 
                                      (102 (aload_3)) 
                                      (103 (aload 6)) 
                                      (105 (invokevirtual (methodCP "getScheme" "java.nio.file.spi.FileSystemProvider" () (class "java.lang.String")))) 
                                      (108 (invokevirtual (methodCP "equalsIgnoreCase" "java.lang.String" ((class "java.lang.String")) boolean))) 
                                      (111 (ifeq 122)) ;;to TAG_4
                                      (114 (aload 6)) 
                                      (116 (aload_0)) 
                                      (117 (aload_1)) 
                                      (118 (invokevirtual (methodCP "newFileSystem" "java.nio.file.spi.FileSystemProvider" ((class "java.net.URI") (class "java.util.Map")) (class "java.nio.file.FileSystem")))) 
                                      (121 (areturn)) 
                                      (122 (goto 80)) ;;to TAG_5;;at TAG_4
                                      (125 (new (class "java.nio.file.ProviderNotFoundException"))) ;;at TAG_3
                                      (128 (dup)) 
                                      (129 (new (class "java.lang.StringBuilder"))) 
                                      (132 (dup)) 
                                      (133 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (136 (ldc 0)) ;;STRING:: "Provider \""
                                      (138 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (141 (aload_3)) 
                                      (142 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (145 (ldc 1)) ;;STRING:: "\" not found"
                                      (147 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (150 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (153 (invokespecial (methodCP "<init>" "java.nio.file.ProviderNotFoundException" ((class "java.lang.String")) void))) 
                                      (156 (athrow)) 
                                      (endofcode 157))
                                   (Exceptions )
                                   (StackMap )))
                        (method "newFileSystem"
                              (parameters (class "java.nio.file.Path") (class "java.lang.ClassLoader"))
                              (returntype . (class "java.nio.file.FileSystem"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 7) (code_length . 121)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (ifnonnull 12)) ;;to TAG_0
                                      (4 (new (class "java.lang.NullPointerException"))) 
                                      (7 (dup)) 
                                      (8 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" () void))) 
                                      (11 (athrow)) 
                                      (12 (invokestatic (methodCP "emptyMap" "java.util.Collections" () (class "java.util.Map")))) ;;at TAG_0
                                      (15 (astore_2)) 
                                      (16 (invokestatic (methodCP "installedProviders" "java.nio.file.spi.FileSystemProvider" () (class "java.util.List")))) 
                                      (19 (invokeinterface (methodCP "iterator" "java.util.List" () (class "java.util.Iterator")) 1)) 
                                      (24 (astore_3)) 
                                      (25 (aload_3)) ;;at TAG_2
                                      (26 (invokeinterface (methodCP "hasNext" "java.util.Iterator" () boolean) 1)) 
                                      (31 (ifeq 58))  ;;to TAG_1
                                      (34 (aload_3)) 
                                      (35 (invokeinterface (methodCP "next" "java.util.Iterator" () (class "java.lang.Object")) 1)) 
                                      (40 (checkcast (class "java.nio.file.spi.FileSystemProvider"))) 
                                      (43 (astore 4)) 
                                      (45 (aload 4)) ;;at TAG_5
                                      (47 (aload_0)) 
                                      (48 (aload_2)) 
                                      (49 (invokevirtual (methodCP "newFileSystem" "java.nio.file.spi.FileSystemProvider" ((class "java.nio.file.Path") (class "java.util.Map")) (class "java.nio.file.FileSystem")))) 
                                      (52 (areturn)) ;;at TAG_6
                                      (53 (astore 5)) ;;at TAG_7
                                      (55 (goto 25)) ;;to TAG_2
                                      (58 (aload_1)) ;;at TAG_1
                                      (59 (ifnull 111)) ;;to TAG_3
                                      (62 (ldc_w )) 
                                      (65 (aload_1)) 
                                      (66 (invokestatic (methodCP "load" "java.util.ServiceLoader" ((class "java.lang.Class") (class "java.lang.ClassLoader")) (class "java.util.ServiceLoader")))) 
                                      (69 (astore_3)) 
                                      (70 (aload_3)) 
                                      (71 (invokevirtual (methodCP "iterator" "java.util.ServiceLoader" () (class "java.util.Iterator")))) 
                                      (74 (astore 4)) 
                                      (76 (aload 4)) ;;at TAG_4
                                      (78 (invokeinterface (methodCP "hasNext" "java.util.Iterator" () boolean) 1)) 
                                      (83 (ifeq 111)) ;;to TAG_3
                                      (86 (aload 4)) 
                                      (88 (invokeinterface (methodCP "next" "java.util.Iterator" () (class "java.lang.Object")) 1)) 
                                      (93 (checkcast (class "java.nio.file.spi.FileSystemProvider"))) 
                                      (96 (astore 5)) 
                                      (98 (aload 5)) ;;at TAG_8
                                      (100 (aload_0)) 
                                      (101 (aload_2)) 
                                      (102 (invokevirtual (methodCP "newFileSystem" "java.nio.file.spi.FileSystemProvider" ((class "java.nio.file.Path") (class "java.util.Map")) (class "java.nio.file.FileSystem")))) 
                                      (105 (areturn)) ;;at TAG_9
                                      (106 (astore 6)) ;;at TAG_10
                                      (108 (goto 76)) ;;to TAG_4
                                      (111 (new (class "java.nio.file.ProviderNotFoundException"))) ;;at TAG_3
                                      (114 (dup)) 
                                      (115 (ldc 2)) ;;STRING:: "Provider not found"
                                      (117 (invokespecial (methodCP "<init>" "java.nio.file.ProviderNotFoundException" ((class "java.lang.String")) void))) 
                                      (120 (athrow)) 
                                      (endofcode 121))
                                   (Exceptions 
                                     (handler 45 52  53 (class "java.lang.UnsupportedOperationException"))
                                     (handler 98 105  106 (class "java.lang.UnsupportedOperationException")))
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *FileSystems-class-table*
  (make-static-class-decls 
   *java.nio.file.FileSystems*))

(defconst *package-name-map* 
  ("java.nio.file.FileSystems" . "java.nio.file"))

