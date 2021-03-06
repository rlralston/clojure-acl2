; ServiceLoader$LazyIterator-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:48 CDT 2014.
;

(defconst *java.util.ServiceLoader$LazyIterator*
 (make-class-def
      '(class "java.util.ServiceLoader$LazyIterator"
            "java.lang.Object"
            (constant_pool
                        (STRING  "META-INF/services/")
                        (STRING  "Error locating configuration files")
                        (STRING  "Provider ")
                        (STRING  " not found")
                        (STRING  " could not be instantiated: "))
            (fields
                        (field "service" (class "java.lang.Class") (accessflags  *class* ) -1)
                        (field "loader" (class "java.lang.ClassLoader") (accessflags  *class* ) -1)
                        (field "configs" (class "java.util.Enumeration") (accessflags  *class* ) -1)
                        (field "pending" (class "java.util.Iterator") (accessflags  *class* ) -1)
                        (field "nextName" (class "java.lang.String") (accessflags  *class* ) -1)
                        (field "this$0" (class "java.util.ServiceLoader") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.ServiceLoader") (class "java.lang.Class") (class "java.lang.ClassLoader"))
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 35)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.util.ServiceLoader$LazyIterator" (class "java.util.ServiceLoader"))))
                                      (5 (aload_0))
                                      (6 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (9 (aload_0))
                                      (10 (aconst_null))
                                      (11 (putfield (fieldCP "configs" "java.util.ServiceLoader$LazyIterator" (class "java.util.Enumeration"))))
                                      (14 (aload_0))
                                      (15 (aconst_null))
                                      (16 (putfield (fieldCP "pending" "java.util.ServiceLoader$LazyIterator" (class "java.util.Iterator"))))
                                      (19 (aload_0))
                                      (20 (aconst_null))
                                      (21 (putfield (fieldCP "nextName" "java.util.ServiceLoader$LazyIterator" (class "java.lang.String"))))
                                      (24 (aload_0))
                                      (25 (aload_2))
                                      (26 (putfield (fieldCP "service" "java.util.ServiceLoader$LazyIterator" (class "java.lang.Class"))))
                                      (29 (aload_0))
                                      (30 (aload_3))
                                      (31 (putfield (fieldCP "loader" "java.util.ServiceLoader$LazyIterator" (class "java.lang.ClassLoader"))))
                                      (34 (return))
                                      (endofcode 35))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hasNext"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 167)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "nextName" "java.util.ServiceLoader$LazyIterator" (class "java.lang.String")))) 
                                      (4 (ifnull 9)) ;;to TAG_0
                                      (7 (iconst_1)) 
                                      (8 (ireturn)) 
                                      (9 (aload_0)) ;;at TAG_0
                                      (10 (getfield (fieldCP "configs" "java.util.ServiceLoader$LazyIterator" (class "java.util.Enumeration")))) 
                                      (13 (ifnonnull 86))  ;;to TAG_1
                                      (16 (new (class "java.lang.StringBuilder"))) ;;at TAG_7
                                      (19 (dup)) 
                                      (20 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (23 (ldc 0)) ;;STRING:: "META-INF/services/"
                                      (25 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (28 (aload_0)) 
                                      (29 (getfield (fieldCP "service" "java.util.ServiceLoader$LazyIterator" (class "java.lang.Class")))) 
                                      (32 (invokevirtual (methodCP "getName" "java.lang.Class" () (class "java.lang.String")))) 
                                      (35 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (38 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (41 (astore_1)) 
                                      (42 (aload_0)) 
                                      (43 (getfield (fieldCP "loader" "java.util.ServiceLoader$LazyIterator" (class "java.lang.ClassLoader")))) 
                                      (46 (ifnonnull 60)) ;;to TAG_2
                                      (49 (aload_0)) 
                                      (50 (aload_1)) 
                                      (51 (invokestatic (methodCP "getSystemResources" "java.lang.ClassLoader" ((class "java.lang.String")) (class "java.util.Enumeration")))) 
                                      (54 (putfield (fieldCP "configs" "java.util.ServiceLoader$LazyIterator" (class "java.util.Enumeration")))) 
                                      (57 (goto 72)) ;;to TAG_3
                                      (60 (aload_0)) ;;at TAG_2
                                      (61 (aload_0)) 
                                      (62 (getfield (fieldCP "loader" "java.util.ServiceLoader$LazyIterator" (class "java.lang.ClassLoader")))) 
                                      (65 (aload_1)) 
                                      (66 (invokevirtual (methodCP "getResources" "java.lang.ClassLoader" ((class "java.lang.String")) (class "java.util.Enumeration")))) 
                                      (69 (putfield (fieldCP "configs" "java.util.ServiceLoader$LazyIterator" (class "java.util.Enumeration")))) 
                                      (72 (goto 86))  ;;to TAG_1;;at TAG_3
                                      (75 (astore_1)) ;;at TAG_8
                                      (76 (aload_0)) 
                                      (77 (getfield (fieldCP "service" "java.util.ServiceLoader$LazyIterator" (class "java.lang.Class")))) 
                                      (80 (ldc 1)) ;;STRING:: "Error locating configuration files"
                                      (82 (aload_1)) 
                                      (83 (invokestatic (methodCP "access$100" "java.util.ServiceLoader" ((class "java.lang.Class") (class "java.lang.String") (class "java.lang.Throwable")) void))) 
                                      (86 (aload_0)) ;;at TAG_1
                                      (87 (getfield (fieldCP "pending" "java.util.ServiceLoader$LazyIterator" (class "java.util.Iterator")))) 
                                      (90 (ifnull 105)) ;;to TAG_4
                                      (93 (aload_0)) 
                                      (94 (getfield (fieldCP "pending" "java.util.ServiceLoader$LazyIterator" (class "java.util.Iterator")))) 
                                      (97 (invokeinterface (methodCP "hasNext" "java.util.Iterator" () boolean) 1)) 
                                      (102 (ifne 149)) ;;to TAG_5
                                      (105 (aload_0)) ;;at TAG_4
                                      (106 (getfield (fieldCP "configs" "java.util.ServiceLoader$LazyIterator" (class "java.util.Enumeration")))) 
                                      (109 (invokeinterface (methodCP "hasMoreElements" "java.util.Enumeration" () boolean) 1)) 
                                      (114 (ifne 119)) ;;to TAG_6
                                      (117 (iconst_0)) 
                                      (118 (ireturn)) 
                                      (119 (aload_0)) ;;at TAG_6
                                      (120 (aload_0)) 
                                      (121 (getfield (fieldCP "this$0" "java.util.ServiceLoader$LazyIterator" (class "java.util.ServiceLoader")))) 
                                      (124 (aload_0)) 
                                      (125 (getfield (fieldCP "service" "java.util.ServiceLoader$LazyIterator" (class "java.lang.Class")))) 
                                      (128 (aload_0)) 
                                      (129 (getfield (fieldCP "configs" "java.util.ServiceLoader$LazyIterator" (class "java.util.Enumeration")))) 
                                      (132 (invokeinterface (methodCP "nextElement" "java.util.Enumeration" () (class "java.lang.Object")) 1)) 
                                      (137 (checkcast (class "java.net.URL"))) 
                                      (140 (invokestatic (methodCP "access$200" "java.util.ServiceLoader" ((class "java.util.ServiceLoader") (class "java.lang.Class") (class "java.net.URL")) (class "java.util.Iterator")))) 
                                      (143 (putfield (fieldCP "pending" "java.util.ServiceLoader$LazyIterator" (class "java.util.Iterator")))) 
                                      (146 (goto 86))  ;;to TAG_1
                                      (149 (aload_0)) ;;at TAG_5
                                      (150 (aload_0)) 
                                      (151 (getfield (fieldCP "pending" "java.util.ServiceLoader$LazyIterator" (class "java.util.Iterator")))) 
                                      (154 (invokeinterface (methodCP "next" "java.util.Iterator" () (class "java.lang.Object")) 1)) 
                                      (159 (checkcast (class "java.lang.String"))) 
                                      (162 (putfield (fieldCP "nextName" "java.util.ServiceLoader$LazyIterator" (class "java.lang.String")))) 
                                      (165 (iconst_1)) 
                                      (166 (ireturn)) 
                                      (endofcode 167))
                                   (Exceptions 
                                     (handler 16 72  75 (class "java.io.IOException")))
                                   (StackMap )))
                        (method "next"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 140)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "hasNext" "java.util.ServiceLoader$LazyIterator" () boolean))) 
                                      (4 (ifne 15)) ;;to TAG_0
                                      (7 (new (class "java.util.NoSuchElementException"))) 
                                      (10 (dup)) 
                                      (11 (invokespecial (methodCP "<init>" "java.util.NoSuchElementException" () void))) 
                                      (14 (athrow)) 
                                      (15 (aload_0)) ;;at TAG_0
                                      (16 (getfield (fieldCP "nextName" "java.util.ServiceLoader$LazyIterator" (class "java.lang.String")))) 
                                      (19 (astore_1)) 
                                      (20 (aload_0)) 
                                      (21 (aconst_null)) 
                                      (22 (putfield (fieldCP "nextName" "java.util.ServiceLoader$LazyIterator" (class "java.lang.String")))) 
                                      (25 (aload_0)) ;;at TAG_2
                                      (26 (getfield (fieldCP "service" "java.util.ServiceLoader$LazyIterator" (class "java.lang.Class")))) 
                                      (29 (aload_1)) 
                                      (30 (iconst_1)) 
                                      (31 (aload_0)) 
                                      (32 (getfield (fieldCP "loader" "java.util.ServiceLoader$LazyIterator" (class "java.lang.ClassLoader")))) 
                                      (35 (invokestatic (methodCP "forName" "java.lang.Class" ((class "java.lang.String") boolean (class "java.lang.ClassLoader")) (class "java.lang.Class")))) 
                                      (38 (invokevirtual (methodCP "newInstance" "java.lang.Class" () (class "java.lang.Object")))) 
                                      (41 (invokevirtual (methodCP "cast" "java.lang.Class" ((class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (44 (astore_2)) 
                                      (45 (aload_0)) 
                                      (46 (getfield (fieldCP "this$0" "java.util.ServiceLoader$LazyIterator" (class "java.util.ServiceLoader")))) 
                                      (49 (invokestatic (methodCP "access$300" "java.util.ServiceLoader" ((class "java.util.ServiceLoader")) (class "java.util.LinkedHashMap")))) 
                                      (52 (aload_1)) 
                                      (53 (aload_2)) 
                                      (54 (invokevirtual (methodCP "put" "java.util.LinkedHashMap" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (57 (pop)) 
                                      (58 (aload_2)) 
                                      (59 (areturn)) ;;at TAG_3
                                      (60 (astore_2)) ;;at TAG_4
                                      (61 (aload_0)) 
                                      (62 (getfield (fieldCP "service" "java.util.ServiceLoader$LazyIterator" (class "java.lang.Class")))) 
                                      (65 (new (class "java.lang.StringBuilder"))) 
                                      (68 (dup)) 
                                      (69 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (72 (ldc 2)) ;;STRING:: "Provider "
                                      (74 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (77 (aload_1)) 
                                      (78 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (81 (ldc 3)) ;;STRING:: " not found"
                                      (83 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (86 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (89 (invokestatic (methodCP "access$400" "java.util.ServiceLoader" ((class "java.lang.Class") (class "java.lang.String")) void))) 
                                      (92 (goto 132)) ;;to TAG_1
                                      (95 (astore_2)) ;;at TAG_5
                                      (96 (aload_0)) 
                                      (97 (getfield (fieldCP "service" "java.util.ServiceLoader$LazyIterator" (class "java.lang.Class")))) 
                                      (100 (new (class "java.lang.StringBuilder"))) 
                                      (103 (dup)) 
                                      (104 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (107 (ldc 2)) ;;STRING:: "Provider "
                                      (109 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (112 (aload_1)) 
                                      (113 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (116 (ldc 4)) ;;STRING:: " could not be instantiated: "
                                      (118 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (121 (aload_2)) 
                                      (122 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.Object")) (class "java.lang.StringBuilder")))) 
                                      (125 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (128 (aload_2)) 
                                      (129 (invokestatic (methodCP "access$100" "java.util.ServiceLoader" ((class "java.lang.Class") (class "java.lang.String") (class "java.lang.Throwable")) void))) 
                                      (132 (new (class "java.lang.Error"))) ;;at TAG_1
                                      (135 (dup)) 
                                      (136 (invokespecial (methodCP "<init>" "java.lang.Error" () void))) 
                                      (139 (athrow)) 
                                      (endofcode 140))
                                   (Exceptions 
                                     (handler 25 59  60 (class "java.lang.ClassNotFoundException"))
                                     (handler 25 59  95 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "remove"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
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
                        (method "<init>"
                              (parameters (class "java.util.ServiceLoader") (class "java.lang.Class") (class "java.lang.ClassLoader") (class "java.util.ServiceLoader$1"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 4) (max_locals . 5) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (aload_2))
                                      (3 (aload_3))
                                      (4 (invokespecial
					(methodCP "<init>" "java.util.ServiceLoader$LazyIterator" ((class "java.util.ServiceLoader") (class "java.lang.Class") (class "java.lang.ClassLoader")) void)))
                                      (7 (return))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.util.Iterator")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *ServiceLoader$LazyIterator-class-table*
  (make-static-class-decls 
   *java.util.ServiceLoader$LazyIterator*))

(defconst *package-name-map* 
  ("java.util.ServiceLoader$LazyIterator" . "java.util"))

