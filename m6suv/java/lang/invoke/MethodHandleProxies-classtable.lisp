; MethodHandleProxies-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:35 CDT 2014.
;

(defconst *java.lang.invoke.MethodHandleProxies*
 (make-class-def
      '(class "java.lang.invoke.MethodHandleProxies"
            "java.lang.Object"
            (constant_pool
                        (STRING  "not a single-method interface: ")
                        (STRING  "not a wrapper instance")
                        (STRING  "toString")
                        (STRING  "hashCode")
                        (STRING  "equals")
                        (STRING  "@"))
            (fields
                        (field "$assertionsDisabled" boolean (accessflags  *class*  *final*  *static* ) -1))
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
                        (method "asInterfaceInstance"
                              (parameters (class "java.lang.Class") (class "java.lang.invoke.MethodHandle"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 9) (max_locals . 6) (code_length . 128)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokestatic (methodCP "getSingleMethod" "java.lang.invoke.MethodHandleProxies" ((class "java.lang.Class")) (class "java.lang.reflect.Method")))) 
                                      (4 (astore_2)) 
                                      (5 (aload_2)) 
                                      (6 (ifnonnull 39))  ;;to TAG_0
                                      (9 (new (class "java.lang.IllegalArgumentException"))) 
                                      (12 (dup)) 
                                      (13 (new (class "java.lang.StringBuilder"))) 
                                      (16 (dup)) 
                                      (17 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (20 (ldc 0)) ;;STRING:: "not a single-method interface: "
                                      (22 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (25 (aload_0)) 
                                      (26 (invokevirtual (methodCP "getName" "java.lang.Class" () (class "java.lang.String")))) 
                                      (29 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (32 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (35 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (38 (athrow)) 
                                      (39 (aload_2)) ;;at TAG_0
                                      (40 (invokevirtual (methodCP "getReturnType" "java.lang.reflect.Method" () (class "java.lang.Class")))) 
                                      (43 (aload_2)) 
                                      (44 (invokevirtual (methodCP "getParameterTypes" "java.lang.reflect.Method" () (array (class "java.lang.Class"))))) 
                                      (47 (invokestatic (methodCP "methodType" "java.lang.invoke.MethodType" ((class "java.lang.Class") (array (class "java.lang.Class"))) (class "java.lang.invoke.MethodType")))) 
                                      (50 (astore_3)) 
                                      (51 (aload_1)) 
                                      (52 (aload_3)) 
                                      (53 (invokevirtual (methodCP "asType" "java.lang.invoke.MethodHandle" ((class "java.lang.invoke.MethodType")) (class "java.lang.invoke.MethodHandle")))) 
                                      (56 (astore 4)) 
                                      (58 (aload 4)) 
                                      (60 (aload 4)) 
                                      (62 (invokevirtual (methodCP "type" "java.lang.invoke.MethodHandle" () (class "java.lang.invoke.MethodType")))) 
                                      (65 (ldc_w )) 
                                      (68 (invokevirtual (methodCP "changeReturnType" "java.lang.invoke.MethodType" ((class "java.lang.Class")) (class "java.lang.invoke.MethodType")))) 
                                      (71 (invokevirtual (methodCP "asType" "java.lang.invoke.MethodHandle" ((class "java.lang.invoke.MethodType")) (class "java.lang.invoke.MethodHandle")))) 
                                      (74 (astore 4)) 
                                      (76 (aload 4)) 
                                      (78 (ldc_w )) 
                                      (81 (aload_3)) 
                                      (82 (invokevirtual (methodCP "parameterCount" "java.lang.invoke.MethodType" () int))) 
                                      (85 (invokevirtual (methodCP "asSpreader" "java.lang.invoke.MethodHandle" ((class "java.lang.Class") int) (class "java.lang.invoke.MethodHandle")))) 
                                      (88 (astore 5)) 
                                      (90 (aload_0)) 
                                      (91 (aload_0)) 
                                      (92 (invokevirtual (methodCP "getClassLoader" "java.lang.Class" () (class "java.lang.ClassLoader")))) 
                                      (95 (iconst_2)) 
                                      (96 (anewarray (class "java.lang.Class"))) 
                                      (99 (dup)) 
                                      (100 (iconst_0)) 
                                      (101 (aload_0)) 
                                      (102 (aastore)) 
                                      (103 (dup)) 
                                      (104 (iconst_1)) 
                                      (105 (ldc_w )) 
                                      (108 (aastore)) 
                                      (109 (new (class "java.lang.invoke.MethodHandleProxies$1"))) 
                                      (112 (dup)) 
                                      (113 (aload_1)) 
                                      (114 (aload_0)) 
                                      (115 (aload_2)) 
                                      (116 (aload 5)) 
                                      (118 (invokespecial (methodCP "<init>" "java.lang.invoke.MethodHandleProxies$1" ((class "java.lang.invoke.MethodHandle") (class "java.lang.Class") (class "java.lang.reflect.Method") (class "java.lang.invoke.MethodHandle")) void))) 
                                      (121 (invokestatic (methodCP "newProxyInstance" "java.lang.reflect.Proxy" ((class "java.lang.ClassLoader") (array (class "java.lang.Class")) (class "java.lang.reflect.InvocationHandler")) (class "java.lang.Object")))) 
                                      (124 (invokevirtual (methodCP "cast" "java.lang.Class" ((class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (127 (areturn)) 
                                      (endofcode 128))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isWrapperInstance"
                              (parameters (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (instanceof (class "sun.invoke.WrapperInstance")))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "asWrapperInstance"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "sun.invoke.WrapperInstance"))
                              (accessflags  *class*  *private*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 23)
                                   (parsedcode
                                      (0 (aload_0)) ;;at TAG_2
                                      (1 (ifnull 9)) ;;to TAG_0
                                      (4 (aload_0)) 
                                      (5 (checkcast (class "sun.invoke.WrapperInstance"))) 
                                      (8 (areturn)) ;;at TAG_3
                                      (9 (goto 13)) ;;to TAG_1;;at TAG_0
                                      (12 (astore_1)) ;;at TAG_4
                                      (13 (new (class "java.lang.IllegalArgumentException"))) ;;at TAG_1
                                      (16 (dup)) 
                                      (17 (ldc 1)) ;;STRING:: "not a wrapper instance"
                                      (19 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (22 (athrow)) 
                                      (endofcode 23))
                                   (Exceptions 
                                     (handler 0 8  12 (class "java.lang.ClassCastException")))
                                   (StackMap )))
                        (method "wrapperInstanceTarget"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.invoke.MethodHandle"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokestatic
					(methodCP "asWrapperInstance" "java.lang.invoke.MethodHandleProxies" ((class "java.lang.Object")) (class "sun.invoke.WrapperInstance"))))
                                      (4 (invokeinterface
					(methodCP "getWrapperInstanceTarget" "sun.invoke.WrapperInstance" () (class "java.lang.invoke.MethodHandle")) 1))
                                      (9 (areturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "wrapperInstanceType"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Class"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokestatic
					(methodCP "asWrapperInstance" "java.lang.invoke.MethodHandleProxies" ((class "java.lang.Object")) (class "sun.invoke.WrapperInstance"))))
                                      (4 (invokeinterface
					(methodCP "getWrapperInstanceType" "sun.invoke.WrapperInstance" () (class "java.lang.Class")) 1))
                                      (9 (areturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isObjectMethod"
                              (parameters (class "java.lang.reflect.Method"))
                              (returntype . boolean)
                              (accessflags  *class*  *private*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 199)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "getName" "java.lang.reflect.Method" () (class "java.lang.String")))) 
                                      (4 (astore_1)) 
                                      (5 (iconst_m1)) 
                                      (6 (istore_2)) 
                                      (7 (aload_1)) 
                                      (8 (invokevirtual (methodCP "hashCode" "java.lang.String" () int))) 
                                      (11 (lookupswitch (lookupswitchinfo 83 3 ((-1776922004 . 44) (-1295482945 . 72) (147696667 . 58)))))  ;;to TAG_1;;to TAG_2;;to TAG_3;;to TAG_0
                                      (44 (aload_1)) ;;at TAG_1
                                      (45 (ldc 2)) ;;STRING:: "toString"
                                      (47 (invokevirtual (methodCP "equals" "java.lang.String" ((class "java.lang.Object")) boolean))) 
                                      (50 (ifeq 83)) ;;to TAG_0
                                      (53 (iconst_0)) 
                                      (54 (istore_2)) 
                                      (55 (goto 83)) ;;to TAG_0
                                      (58 (aload_1)) ;;at TAG_3
                                      (59 (ldc 3)) ;;STRING:: "hashCode"
                                      (61 (invokevirtual (methodCP "equals" "java.lang.String" ((class "java.lang.Object")) boolean))) 
                                      (64 (ifeq 83)) ;;to TAG_0
                                      (67 (iconst_1)) 
                                      (68 (istore_2)) 
                                      (69 (goto 83)) ;;to TAG_0
                                      (72 (aload_1)) ;;at TAG_2
                                      (73 (ldc 4)) ;;STRING:: "equals"
                                      (75 (invokevirtual (methodCP "equals" "java.lang.String" ((class "java.lang.Object")) boolean))) 
                                      (78 (ifeq 83)) ;;to TAG_0
                                      (81 (iconst_2)) 
                                      (82 (istore_2)) 
                                      (83 (iload_2)) ;;at TAG_0
                                      (84 (tableswitch (tableswitchinfo 197 (0 . 2) (112 136 160)))) ;;to TAG_4;;to TAG_5;;to TAG_6;;to TAG_7
                                      (112 (aload_0)) ;;at TAG_5
                                      (113 (invokevirtual (methodCP "getReturnType" "java.lang.reflect.Method" () (class "java.lang.Class")))) 
                                      (116 (ldc_w )) 
                                      (119 (if_acmpne 134)) ;;to TAG_8
                                      (122 (aload_0)) 
                                      (123 (invokevirtual (methodCP "getParameterTypes" "java.lang.reflect.Method" () (array (class "java.lang.Class"))))) 
                                      (126 (arraylength)) 
                                      (127 (ifne 134)) ;;to TAG_8
                                      (130 (iconst_1)) 
                                      (131 (goto 135)) ;;to TAG_9
                                      (134 (iconst_0)) ;;at TAG_8
                                      (135 (ireturn)) ;;at TAG_9
                                      (136 (aload_0)) ;;at TAG_6
                                      (137 (invokevirtual (methodCP "getReturnType" "java.lang.reflect.Method" () (class "java.lang.Class")))) 
                                      (140 (getstatic (fieldCP "TYPE" "java.lang.Integer" (class "java.lang.Class")))) 
                                      (143 (if_acmpne 158)) ;;to TAG_10
                                      (146 (aload_0)) 
                                      (147 (invokevirtual (methodCP "getParameterTypes" "java.lang.reflect.Method" () (array (class "java.lang.Class"))))) 
                                      (150 (arraylength)) 
                                      (151 (ifne 158)) ;;to TAG_10
                                      (154 (iconst_1)) 
                                      (155 (goto 159)) ;;to TAG_11
                                      (158 (iconst_0)) ;;at TAG_10
                                      (159 (ireturn)) ;;at TAG_11
                                      (160 (aload_0)) ;;at TAG_7
                                      (161 (invokevirtual (methodCP "getReturnType" "java.lang.reflect.Method" () (class "java.lang.Class")))) 
                                      (164 (getstatic (fieldCP "TYPE" "java.lang.Boolean" (class "java.lang.Class")))) 
                                      (167 (if_acmpne 195)) ;;to TAG_12
                                      (170 (aload_0)) 
                                      (171 (invokevirtual (methodCP "getParameterTypes" "java.lang.reflect.Method" () (array (class "java.lang.Class"))))) 
                                      (174 (arraylength)) 
                                      (175 (iconst_1)) 
                                      (176 (if_icmpne 195)) ;;to TAG_12
                                      (179 (aload_0)) 
                                      (180 (invokevirtual (methodCP "getParameterTypes" "java.lang.reflect.Method" () (array (class "java.lang.Class"))))) 
                                      (183 (iconst_0)) 
                                      (184 (aaload)) 
                                      (185 (ldc_w )) 
                                      (188 (if_acmpne 195)) ;;to TAG_12
                                      (191 (iconst_1)) 
                                      (192 (goto 196)) ;;to TAG_13
                                      (195 (iconst_0)) ;;at TAG_12
                                      (196 (ireturn)) ;;at TAG_13
                                      (197 (iconst_0)) ;;at TAG_4
                                      (198 (ireturn)) 
                                      (endofcode 199))
                                   (Exceptions )
                                   (StackMap )))
                        (method "callObjectMethod"
                              (parameters (class "java.lang.Object") (class "java.lang.reflect.Method") (array (class "java.lang.Object")))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *private*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 5) (code_length . 202)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "$assertionsDisabled" "java.lang.invoke.MethodHandleProxies" boolean))) 
                                      (3 (ifne 22)) ;;to TAG_0
                                      (6 (aload_1)) 
                                      (7 (invokestatic (methodCP "isObjectMethod" "java.lang.invoke.MethodHandleProxies" ((class "java.lang.reflect.Method")) boolean))) 
                                      (10 (ifne 22)) ;;to TAG_0
                                      (13 (new (class "java.lang.AssertionError"))) 
                                      (16 (dup)) 
                                      (17 (aload_1)) 
                                      (18 (invokespecial (methodCP "<init>" "java.lang.AssertionError" ((class "java.lang.Object")) void))) 
                                      (21 (athrow)) 
                                      (22 (aload_1)) ;;at TAG_0
                                      (23 (invokevirtual (methodCP "getName" "java.lang.reflect.Method" () (class "java.lang.String")))) 
                                      (26 (astore_3)) 
                                      (27 (iconst_m1)) 
                                      (28 (istore 4)) 
                                      (30 (aload_3)) 
                                      (31 (invokevirtual (methodCP "hashCode" "java.lang.String" () int))) 
                                      (34 (lookupswitch (lookupswitchinfo 110 3 ((-1776922004 . 68) (-1295482945 . 98) (147696667 . 83)))))  ;;to TAG_1;;to TAG_2;;to TAG_3;;to TAG_4
                                      (68 (aload_3)) ;;at TAG_2
                                      (69 (ldc 2)) ;;STRING:: "toString"
                                      (71 (invokevirtual (methodCP "equals" "java.lang.String" ((class "java.lang.Object")) boolean))) 
                                      (74 (ifeq 110))  ;;to TAG_1
                                      (77 (iconst_0)) 
                                      (78 (istore 4)) 
                                      (80 (goto 110))  ;;to TAG_1
                                      (83 (aload_3)) ;;at TAG_4
                                      (84 (ldc 3)) ;;STRING:: "hashCode"
                                      (86 (invokevirtual (methodCP "equals" "java.lang.String" ((class "java.lang.Object")) boolean))) 
                                      (89 (ifeq 110))  ;;to TAG_1
                                      (92 (iconst_1)) 
                                      (93 (istore 4)) 
                                      (95 (goto 110))  ;;to TAG_1
                                      (98 (aload_3)) ;;at TAG_3
                                      (99 (ldc 4)) ;;STRING:: "equals"
                                      (101 (invokevirtual (methodCP "equals" "java.lang.String" ((class "java.lang.Object")) boolean))) 
                                      (104 (ifeq 110))  ;;to TAG_1
                                      (107 (iconst_2)) 
                                      (108 (istore 4)) 
                                      (110 (iload 4)) ;;at TAG_1
                                      (112 (tableswitch (tableswitchinfo 200 (0 . 2) (140 176 184)))) ;;to TAG_5;;to TAG_6;;to TAG_7;;to TAG_8
                                      (140 (new (class "java.lang.StringBuilder"))) ;;at TAG_6
                                      (143 (dup)) 
                                      (144 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (147 (aload_0)) 
                                      (148 (invokevirtual (methodCP "getClass" "java.lang.Object" () (class "java.lang.Class")))) 
                                      (151 (invokevirtual (methodCP "getName" "java.lang.Class" () (class "java.lang.String")))) 
                                      (154 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (157 (ldc 5)) ;;STRING:: "@"
                                      (159 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (162 (aload_0)) 
                                      (163 (invokevirtual (methodCP "hashCode" "java.lang.Object" () int))) 
                                      (166 (invokestatic (methodCP "toHexString" "java.lang.Integer" (int) (class "java.lang.String")))) 
                                      (169 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (172 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (175 (areturn)) 
                                      (176 (aload_0)) ;;at TAG_7
                                      (177 (invokestatic (methodCP "identityHashCode" "java.lang.System" ((class "java.lang.Object")) int))) 
                                      (180 (invokestatic (methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer")))) 
                                      (183 (areturn)) 
                                      (184 (aload_0)) ;;at TAG_8
                                      (185 (aload_2)) 
                                      (186 (iconst_0)) 
                                      (187 (aaload)) 
                                      (188 (if_acmpne 195)) ;;to TAG_9
                                      (191 (iconst_1)) 
                                      (192 (goto 196)) ;;to TAG_10
                                      (195 (iconst_0)) ;;at TAG_9
                                      (196 (invokestatic (methodCP "valueOf" "java.lang.Boolean" (boolean) (class "java.lang.Boolean")))) ;;at TAG_10
                                      (199 (areturn)) 
                                      (200 (aconst_null)) ;;at TAG_5
                                      (201 (areturn)) 
                                      (endofcode 202))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getSingleMethod"
                              (parameters (class "java.lang.Class"))
                              (returntype . (class "java.lang.reflect.Method"))
                              (accessflags  *class*  *private*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 7) (code_length . 73)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "isInterface" "java.lang.Class" () boolean))) 
                                      (4 (ifne 9)) ;;to TAG_0
                                      (7 (aconst_null)) 
                                      (8 (areturn)) 
                                      (9 (aconst_null)) ;;at TAG_0
                                      (10 (astore_1)) 
                                      (11 (aload_0)) 
                                      (12 (invokevirtual (methodCP "getMethods" "java.lang.Class" () (array (class "java.lang.reflect.Method"))))) 
                                      (15 (astore_2)) 
                                      (16 (aload_2)) 
                                      (17 (arraylength)) 
                                      (18 (istore_3)) 
                                      (19 (iconst_0)) 
                                      (20 (istore 4)) 
                                      (22 (iload 4)) ;;at TAG_4
                                      (24 (iload_3)) 
                                      (25 (if_icmpge 71)) ;;to TAG_1
                                      (28 (aload_2)) 
                                      (29 (iload 4)) 
                                      (31 (aaload)) 
                                      (32 (astore 5)) 
                                      (34 (aload 5)) 
                                      (36 (invokevirtual (methodCP "getModifiers" "java.lang.reflect.Method" () int))) 
                                      (39 (istore 6)) 
                                      (41 (iload 6)) 
                                      (43 (invokestatic (methodCP "isAbstract" "java.lang.reflect.Modifier" (int) boolean))) 
                                      (46 (ifeq 65))  ;;to TAG_2
                                      (49 (aload_1)) 
                                      (50 (ifnull 62)) ;;to TAG_3
                                      (53 (aload_1)) 
                                      (54 (invokestatic (methodCP "isObjectMethod" "java.lang.invoke.MethodHandleProxies" ((class "java.lang.reflect.Method")) boolean))) 
                                      (57 (ifne 62)) ;;to TAG_3
                                      (60 (aconst_null)) 
                                      (61 (areturn)) 
                                      (62 (aload 5)) ;;at TAG_3
                                      (64 (astore_1)) 
                                      (65 (iinc 4 1)) ;;at TAG_2
                                      (68 (goto 22)) ;;to TAG_4
                                      (71 (aload_1)) ;;at TAG_1
                                      (72 (areturn)) 
                                      (endofcode 73))
                                   (Exceptions )
                                   (StackMap )))
                        (method "access$000"
                              (parameters (class "java.lang.reflect.Method"))
                              (returntype . boolean)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokestatic
					(methodCP "isObjectMethod" "java.lang.invoke.MethodHandleProxies" ((class "java.lang.reflect.Method")) boolean)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "access$100"
                              (parameters (class "java.lang.Object") (class "java.lang.reflect.Method") (array (class "java.lang.Object")))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 7)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (aload_2))
                                      (3 (invokestatic
					(methodCP "callObjectMethod" "java.lang.invoke.MethodHandleProxies" ((class "java.lang.Object") (class "java.lang.reflect.Method") (array (class "java.lang.Object"))) (class "java.lang.Object"))))
                                      (6 (areturn))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 0) (code_length . 18)
                                   (parsedcode
                                      (0 (ldc_w )) 
                                      (3 (invokevirtual (methodCP "desiredAssertionStatus" "java.lang.Class" () boolean))) 
                                      (6 (ifne 13))  ;;to TAG_0
                                      (9 (iconst_1)) 
                                      (10 (goto 14)) ;;to TAG_1
                                      (13 (iconst_0)) ;;at TAG_0
                                      (14 (putstatic (fieldCP "$assertionsDisabled" "java.lang.invoke.MethodHandleProxies" boolean))) ;;at TAG_1
                                      (17 (return)) 
                                      (endofcode 18))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *MethodHandleProxies-class-table*
  (make-static-class-decls 
   *java.lang.invoke.MethodHandleProxies*))

(defconst *package-name-map* 
  ("java.lang.invoke.MethodHandleProxies" . "java.lang.invoke"))
