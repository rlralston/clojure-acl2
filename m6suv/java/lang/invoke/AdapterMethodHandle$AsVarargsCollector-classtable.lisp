; AdapterMethodHandle$AsVarargsCollector-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:34 CDT 2014.
;

(defconst *java.lang.invoke.AdapterMethodHandle$AsVarargsCollector*
 (make-class-def
      '(class "java.lang.invoke.AdapterMethodHandle$AsVarargsCollector"
            "java.lang.invoke.AdapterMethodHandle"
            (constant_pool
                        (STRING  "cannot build collector"))
            (fields
                        (field "target" (class "java.lang.invoke.MethodHandle") (accessflags  *class*  *final* ) -1)
                        (field "arrayType" (class "java.lang.Class") (accessflags  *class*  *final* ) -1)
                        (field "cache" (class "java.lang.invoke.MethodHandle") (accessflags  *class* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.lang.invoke.MethodHandle") (class "java.lang.Class"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 6) (max_locals . 3) (code_length . 35)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (aload_1))
                                      (3 (invokevirtual
					(methodCP "type" "java.lang.invoke.MethodHandle" () (class "java.lang.invoke.MethodType"))))
                                      (6 (iconst_0))
                                      (7 (invokestatic
					(methodCP "access$000" "java.lang.invoke.AdapterMethodHandle" (int) long)))
                                      (10 (aconst_null))
                                      (11 (invokespecial
					(methodCP "<init>" "java.lang.invoke.AdapterMethodHandle" ((class "java.lang.invoke.MethodHandle") (class "java.lang.invoke.MethodType") long (class "java.lang.invoke.AdapterMethodHandle$1")) void)))
                                      (14 (aload_0))
                                      (15 (aload_1))
                                      (16 (putfield (fieldCP "target" "java.lang.invoke.AdapterMethodHandle$AsVarargsCollector" (class "java.lang.invoke.MethodHandle"))))
                                      (19 (aload_0))
                                      (20 (aload_2))
                                      (21 (putfield (fieldCP "arrayType" "java.lang.invoke.AdapterMethodHandle$AsVarargsCollector" (class "java.lang.Class"))))
                                      (24 (aload_0))
                                      (25 (aload_1))
                                      (26 (aload_2))
                                      (27 (iconst_0))
                                      (28 (invokevirtual
					(methodCP "asCollector" "java.lang.invoke.MethodHandle" ((class "java.lang.Class") int) (class "java.lang.invoke.MethodHandle"))))
                                      (31 (putfield (fieldCP "cache" "java.lang.invoke.AdapterMethodHandle$AsVarargsCollector" (class "java.lang.invoke.MethodHandle"))))
                                      (34 (return))
                                      (endofcode 35))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isVarargsCollector"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 2)
                                   (parsedcode
                                      (0 (iconst_1))
                                      (1 (ireturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap )))
                        (method "asFixedArity"
                              (parameters )
                              (returntype . (class "java.lang.invoke.MethodHandle"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "target" "java.lang.invoke.AdapterMethodHandle$AsVarargsCollector" (class "java.lang.invoke.MethodHandle"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "asType"
                              (parameters (class "java.lang.invoke.MethodType"))
                              (returntype . (class "java.lang.invoke.MethodHandle"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 8) (code_length . 121)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "type" "java.lang.invoke.AdapterMethodHandle$AsVarargsCollector" () (class "java.lang.invoke.MethodType")))) 
                                      (4 (astore_2)) 
                                      (5 (aload_2)) 
                                      (6 (invokevirtual (methodCP "parameterCount" "java.lang.invoke.MethodType" () int))) 
                                      (9 (iconst_1)) 
                                      (10 (isub)) 
                                      (11 (istore_3)) 
                                      (12 (aload_1)) 
                                      (13 (invokevirtual (methodCP "parameterCount" "java.lang.invoke.MethodType" () int))) 
                                      (16 (istore 4)) 
                                      (18 (iload 4)) 
                                      (20 (iload_3)) 
                                      (21 (iconst_1)) 
                                      (22 (iadd)) 
                                      (23 (if_icmpne 48)) ;;to TAG_0
                                      (26 (aload_2)) 
                                      (27 (iload_3)) 
                                      (28 (invokevirtual (methodCP "parameterType" "java.lang.invoke.MethodType" (int) (class "java.lang.Class")))) 
                                      (31 (aload_1)) 
                                      (32 (iload_3)) 
                                      (33 (invokevirtual (methodCP "parameterType" "java.lang.invoke.MethodType" (int) (class "java.lang.Class")))) 
                                      (36 (invokevirtual (methodCP "isAssignableFrom" "java.lang.Class" ((class "java.lang.Class")) boolean))) 
                                      (39 (ifeq 48)) ;;to TAG_0
                                      (42 (aload_0)) 
                                      (43 (aload_1)) 
                                      (44 (invokespecial (methodCP "asType" "java.lang.invoke.AdapterMethodHandle" ((class "java.lang.invoke.MethodType")) (class "java.lang.invoke.MethodHandle")))) 
                                      (47 (areturn)) 
                                      (48 (aload_0)) ;;at TAG_0
                                      (49 (getfield (fieldCP "cache" "java.lang.invoke.AdapterMethodHandle$AsVarargsCollector" (class "java.lang.invoke.MethodHandle")))) 
                                      (52 (invokevirtual (methodCP "type" "java.lang.invoke.MethodHandle" () (class "java.lang.invoke.MethodType")))) 
                                      (55 (invokevirtual (methodCP "parameterCount" "java.lang.invoke.MethodType" () int))) 
                                      (58 (iload 4)) 
                                      (60 (if_icmpne 72)) ;;to TAG_1
                                      (63 (aload_0)) 
                                      (64 (getfield (fieldCP "cache" "java.lang.invoke.AdapterMethodHandle$AsVarargsCollector" (class "java.lang.invoke.MethodHandle")))) 
                                      (67 (aload_1)) 
                                      (68 (invokevirtual (methodCP "asType" "java.lang.invoke.MethodHandle" ((class "java.lang.invoke.MethodType")) (class "java.lang.invoke.MethodHandle")))) 
                                      (71 (areturn)) 
                                      (72 (iload 4)) ;;at TAG_1
                                      (74 (iload_3)) 
                                      (75 (isub)) 
                                      (76 (istore 5)) 
                                      (78 (aload_0)) ;;at TAG_3
                                      (79 (getfield (fieldCP "target" "java.lang.invoke.AdapterMethodHandle$AsVarargsCollector" (class "java.lang.invoke.MethodHandle")))) 
                                      (82 (aload_0)) 
                                      (83 (getfield (fieldCP "arrayType" "java.lang.invoke.AdapterMethodHandle$AsVarargsCollector" (class "java.lang.Class")))) 
                                      (86 (iload 5)) 
                                      (88 (invokevirtual (methodCP "asCollector" "java.lang.invoke.MethodHandle" ((class "java.lang.Class") int) (class "java.lang.invoke.MethodHandle")))) 
                                      (91 (astore 6)) 
                                      (93 (goto 108))  ;;to TAG_2;;at TAG_4
                                      (96 (astore 7)) ;;at TAG_5
                                      (98 (new (class "java.lang.invoke.WrongMethodTypeException"))) 
                                      (101 (dup)) 
                                      (102 (ldc 0)) ;;STRING:: "cannot build collector"
                                      (104 (invokespecial (methodCP "<init>" "java.lang.invoke.WrongMethodTypeException" ((class "java.lang.String")) void))) 
                                      (107 (athrow)) 
                                      (108 (aload_0)) ;;at TAG_2
                                      (109 (aload 6)) 
                                      (111 (putfield (fieldCP "cache" "java.lang.invoke.AdapterMethodHandle$AsVarargsCollector" (class "java.lang.invoke.MethodHandle")))) 
                                      (114 (aload 6)) 
                                      (116 (aload_1)) 
                                      (117 (invokevirtual (methodCP "asType" "java.lang.invoke.MethodHandle" ((class "java.lang.invoke.MethodType")) (class "java.lang.invoke.MethodHandle")))) 
                                      (120 (areturn)) 
                                      (endofcode 121))
                                   (Exceptions 
                                     (handler 78 93  96 (class "java.lang.IllegalArgumentException")))
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *AdapterMethodHandle$AsVarargsCollector-class-table*
  (make-static-class-decls 
   *java.lang.invoke.AdapterMethodHandle$AsVarargsCollector*))

(defconst *package-name-map* 
  ("java.lang.invoke.AdapterMethodHandle$AsVarargsCollector" . "java.lang.invoke"))
