; MethodHandleStatics-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:35 CDT 2014.
;

(defconst *java.lang.invoke.MethodHandleStatics*
 (make-class-def
      '(class "java.lang.invoke.MethodHandleStatics"
            "java.lang.Object"
            (constant_pool
                        (STRING  "invoke")
                        (STRING  "Array is not of length ")
                        (STRING  "uncaught exception")
                        (STRING  ": ")
                        (STRING  ", "))
            (fields
                        (field "DEBUG_METHOD_HANDLE_NAMES" boolean (accessflags  *class*  *final*  *static* ) -1))
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
                        (method "getNameString"
                              (parameters (class "java.lang.invoke.MethodHandle") (class "java.lang.invoke.MethodType"))
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 66)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (ifnonnull 9)) ;;to TAG_0
                                      (4 (aload_0)) 
                                      (5 (invokevirtual (methodCP "type" "java.lang.invoke.MethodHandle" () (class "java.lang.invoke.MethodType")))) 
                                      (8 (astore_1)) 
                                      (9 (aconst_null)) ;;at TAG_0
                                      (10 (astore_2)) 
                                      (11 (aload_0)) 
                                      (12 (ifnull 20)) ;;to TAG_1
                                      (15 (aload_0)) 
                                      (16 (invokestatic (methodCP "getMethodName" "java.lang.invoke.MethodHandleNatives" ((class "java.lang.invoke.MethodHandle")) (class "java.lang.invoke.MemberName")))) 
                                      (19 (astore_2)) 
                                      (20 (aload_2)) ;;at TAG_1
                                      (21 (ifnonnull 44))  ;;to TAG_2
                                      (24 (new (class "java.lang.StringBuilder"))) 
                                      (27 (dup)) 
                                      (28 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (31 (ldc 0)) ;;STRING:: "invoke"
                                      (33 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (36 (aload_1)) 
                                      (37 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.Object")) (class "java.lang.StringBuilder")))) 
                                      (40 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (43 (areturn)) 
                                      (44 (new (class "java.lang.StringBuilder"))) ;;at TAG_2
                                      (47 (dup)) 
                                      (48 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (51 (aload_2)) 
                                      (52 (invokevirtual (methodCP "getName" "java.lang.invoke.MemberName" () (class "java.lang.String")))) 
                                      (55 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (58 (aload_1)) 
                                      (59 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.Object")) (class "java.lang.StringBuilder")))) 
                                      (62 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (65 (areturn)) 
                                      (endofcode 66))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getNameString"
                              (parameters (class "java.lang.invoke.MethodHandle") (class "java.lang.invoke.MethodHandle"))
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 20)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (aload_1)) 
                                      (2 (ifnonnull 12))  ;;to TAG_0
                                      (5 (aconst_null)) 
                                      (6 (checkcast (class "java.lang.invoke.MethodType"))) 
                                      (9 (goto 16)) ;;to TAG_1
                                      (12 (aload_1)) ;;at TAG_0
                                      (13 (invokevirtual (methodCP "type" "java.lang.invoke.MethodHandle" () (class "java.lang.invoke.MethodType")))) 
                                      (16 (invokestatic (methodCP "getNameString" "java.lang.invoke.MethodHandleStatics" ((class "java.lang.invoke.MethodHandle") (class "java.lang.invoke.MethodType")) (class "java.lang.String")))) ;;at TAG_1
                                      (19 (areturn)) 
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getNameString"
                              (parameters (class "java.lang.invoke.MethodHandle"))
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aconst_null))
                                      (2 (checkcast (class "java.lang.invoke.MethodType")))
                                      (5 (invokestatic
					(methodCP "getNameString" "java.lang.invoke.MethodHandleStatics" ((class "java.lang.invoke.MethodHandle") (class "java.lang.invoke.MethodType")) (class "java.lang.String"))))
                                      (8 (areturn))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "addTypeString"
                              (parameters (class "java.lang.Object") (class "java.lang.invoke.MethodHandle"))
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 4) (code_length . 51)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokestatic (methodCP "valueOf" "java.lang.String" ((class "java.lang.Object")) (class "java.lang.String")))) 
                                      (4 (astore_2)) 
                                      (5 (aload_1)) 
                                      (6 (ifnonnull 11))  ;;to TAG_0
                                      (9 (aload_2)) 
                                      (10 (areturn)) 
                                      (11 (aload_2)) ;;at TAG_0
                                      (12 (bipush 40)) 
                                      (14 (invokevirtual (methodCP "indexOf" "java.lang.String" (int) int))) 
                                      (17 (istore_3)) 
                                      (18 (iload_3)) 
                                      (19 (iflt 29)) ;;to TAG_1
                                      (22 (aload_2)) 
                                      (23 (iconst_0)) 
                                      (24 (iload_3)) 
                                      (25 (invokevirtual (methodCP "substring" "java.lang.String" (int int) (class "java.lang.String")))) 
                                      (28 (astore_2)) 
                                      (29 (new (class "java.lang.StringBuilder"))) ;;at TAG_1
                                      (32 (dup)) 
                                      (33 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (36 (aload_2)) 
                                      (37 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (40 (aload_1)) 
                                      (41 (invokevirtual (methodCP "type" "java.lang.invoke.MethodHandle" () (class "java.lang.invoke.MethodType")))) 
                                      (44 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.Object")) (class "java.lang.StringBuilder")))) 
                                      (47 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (50 (areturn)) 
                                      (endofcode 51))
                                   (Exceptions )
                                   (StackMap )))
                        (method "checkSpreadArgument"
                              (parameters (class "java.lang.Object") int)
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 68)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (ifnonnull 9)) ;;to TAG_0
                                      (4 (iload_1)) 
                                      (5 (ifne 45)) ;;to TAG_1
                                      (8 (return)) 
                                      (9 (aload_0)) ;;at TAG_0
                                      (10 (instanceof (array (class "java.lang.Object")))) 
                                      (13 (ifeq 34))  ;;to TAG_2
                                      (16 (aload_0)) 
                                      (17 (checkcast (array (class "java.lang.Object")))) 
                                      (20 (checkcast (array (class "java.lang.Object")))) 
                                      (23 (arraylength)) 
                                      (24 (istore_2)) 
                                      (25 (iload_2)) 
                                      (26 (iload_1)) 
                                      (27 (if_icmpne 31)) ;;to TAG_3
                                      (30 (return)) 
                                      (31 (goto 45)) ;;to TAG_1;;at TAG_3
                                      (34 (aload_0)) ;;at TAG_2
                                      (35 (invokestatic (methodCP "getLength" "java.lang.reflect.Array" ((class "java.lang.Object")) int))) 
                                      (38 (istore_2)) 
                                      (39 (iload_2)) 
                                      (40 (iload_1)) 
                                      (41 (if_icmpne 45)) ;;to TAG_1
                                      (44 (return)) 
                                      (45 (new (class "java.lang.StringBuilder"))) ;;at TAG_1
                                      (48 (dup)) 
                                      (49 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (52 (ldc 1)) ;;STRING:: "Array is not of length "
                                      (54 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (57 (iload_1)) 
                                      (58 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (int) (class "java.lang.StringBuilder")))) 
                                      (61 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (64 (invokestatic (methodCP "newIllegalArgumentException" "java.lang.invoke.MethodHandleStatics" ((class "java.lang.String")) (class "java.lang.RuntimeException")))) 
                                      (67 (athrow)) 
                                      (endofcode 68))
                                   (Exceptions )
                                   (StackMap )))
                        (method "newIllegalStateException"
                              (parameters (class "java.lang.String"))
                              (returntype . (class "java.lang.RuntimeException"))
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 9)
                                   (parsedcode
                                      (0 (new (class "java.lang.IllegalStateException")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (invokespecial
					(methodCP "<init>" "java.lang.IllegalStateException" ((class "java.lang.String")) void)))
                                      (8 (areturn))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "newIllegalStateException"
                              (parameters (class "java.lang.String") (class "java.lang.Object"))
                              (returntype . (class "java.lang.RuntimeException"))
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 13)
                                   (parsedcode
                                      (0 (new (class "java.lang.IllegalStateException")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (invokestatic
					(methodCP "message" "java.lang.invoke.MethodHandleStatics" ((class "java.lang.String") (class "java.lang.Object")) (class "java.lang.String"))))
                                      (9 (invokespecial
					(methodCP "<init>" "java.lang.IllegalStateException" ((class "java.lang.String")) void)))
                                      (12 (areturn))
                                      (endofcode 13))
                                   (Exceptions )
                                   (StackMap )))
                        (method "newIllegalArgumentException"
                              (parameters (class "java.lang.String"))
                              (returntype . (class "java.lang.RuntimeException"))
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 9)
                                   (parsedcode
                                      (0 (new (class "java.lang.IllegalArgumentException")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (invokespecial
					(methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void)))
                                      (8 (areturn))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "newIllegalArgumentException"
                              (parameters (class "java.lang.String") (class "java.lang.Object"))
                              (returntype . (class "java.lang.RuntimeException"))
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 13)
                                   (parsedcode
                                      (0 (new (class "java.lang.IllegalArgumentException")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (invokestatic
					(methodCP "message" "java.lang.invoke.MethodHandleStatics" ((class "java.lang.String") (class "java.lang.Object")) (class "java.lang.String"))))
                                      (9 (invokespecial
					(methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void)))
                                      (12 (areturn))
                                      (endofcode 13))
                                   (Exceptions )
                                   (StackMap )))
                        (method "newIllegalArgumentException"
                              (parameters (class "java.lang.String") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.RuntimeException"))
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 5) (max_locals . 3) (code_length . 14)
                                   (parsedcode
                                      (0 (new (class "java.lang.IllegalArgumentException")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (aload_2))
                                      (7 (invokestatic
					(methodCP "message" "java.lang.invoke.MethodHandleStatics" ((class "java.lang.String") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.String"))))
                                      (10 (invokespecial
					(methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void)))
                                      (13 (areturn))
                                      (endofcode 14))
                                   (Exceptions )
                                   (StackMap )))
                        (method "uncaughtException"
                              (parameters (class "java.lang.Exception"))
                              (returntype . (class "java.lang.Error"))
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 18)
                                   (parsedcode
                                      (0 (new (class "java.lang.InternalError")))
                                      (3 (dup))
                                      (4 (ldc 2))         ;;STRING:: "uncaught exception"
                                      (6 (invokespecial
					(methodCP "<init>" "java.lang.InternalError" ((class "java.lang.String")) void)))
                                      (9 (astore_1))
                                      (10 (aload_1))
                                      (11 (aload_0))
                                      (12 (invokevirtual
					(methodCP "initCause" "java.lang.Error" ((class "java.lang.Throwable")) (class "java.lang.Throwable"))))
                                      (15 (pop))
                                      (16 (aload_1))
                                      (17 (areturn))
                                      (endofcode 18))
                                   (Exceptions )
                                   (StackMap )))
                        (method "message"
                              (parameters (class "java.lang.String") (class "java.lang.Object"))
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *private*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 30)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (ifnull 28))  ;;to TAG_0
                                      (4 (new (class "java.lang.StringBuilder"))) 
                                      (7 (dup)) 
                                      (8 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (11 (aload_0)) 
                                      (12 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (15 (ldc 3)) ;;STRING:: ": "
                                      (17 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (20 (aload_1)) 
                                      (21 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.Object")) (class "java.lang.StringBuilder")))) 
                                      (24 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (27 (astore_0)) 
                                      (28 (aload_0)) ;;at TAG_0
                                      (29 (areturn)) 
                                      (endofcode 30))
                                   (Exceptions )
                                   (StackMap )))
                        (method "message"
                              (parameters (class "java.lang.String") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *private*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 43)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (ifnonnull 8))  ;;to TAG_0
                                      (4 (aload_2)) 
                                      (5 (ifnull 41)) ;;to TAG_1
                                      (8 (new (class "java.lang.StringBuilder"))) ;;at TAG_0
                                      (11 (dup)) 
                                      (12 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (15 (aload_0)) 
                                      (16 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (19 (ldc 3)) ;;STRING:: ": "
                                      (21 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (24 (aload_1)) 
                                      (25 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.Object")) (class "java.lang.StringBuilder")))) 
                                      (28 (ldc 4)) ;;STRING:: ", "
                                      (30 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (33 (aload_2)) 
                                      (34 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.Object")) (class "java.lang.StringBuilder")))) 
                                      (37 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (40 (astore_0)) 
                                      (41 (aload_0)) ;;at TAG_1
                                      (42 (areturn)) 
                                      (endofcode 43))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 1) (code_length . 37)
                                   (parsedcode
                                      (0 (iconst_1))
                                      (1 (anewarray (class "java.lang.Object")))
                                      (4 (dup))
                                      (5 (iconst_0))
                                      (6 (iconst_0))
                                      (7 (invokestatic
					(methodCP "valueOf" "java.lang.Boolean" (boolean) (class "java.lang.Boolean"))))
                                      (10 (aastore))
                                      (11 (astore_0))
                                      (12 (new (class "java.lang.invoke.MethodHandleStatics$1")))
                                      (15 (dup))
                                      (16 (aload_0))
                                      (17 (invokespecial
					(methodCP "<init>" "java.lang.invoke.MethodHandleStatics$1" ((array (class "java.lang.Object"))) void)))
                                      (20 (invokestatic
					(methodCP "doPrivileged" "java.security.AccessController" ((class "java.security.PrivilegedAction")) (class "java.lang.Object"))))
                                      (23 (pop))
                                      (24 (aload_0))
                                      (25 (iconst_0))
                                      (26 (aaload))
                                      (27 (checkcast (class "java.lang.Boolean")))
                                      (30 (invokevirtual
					(methodCP "booleanValue" "java.lang.Boolean" () boolean)))
                                      (33 (putstatic (fieldCP "DEBUG_METHOD_HANDLE_NAMES" "java.lang.invoke.MethodHandleStatics" boolean)))
                                      (36 (return))
                                      (endofcode 37))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *MethodHandleStatics-class-table*
  (make-static-class-decls 
   *java.lang.invoke.MethodHandleStatics*))

(defconst *package-name-map* 
  ("java.lang.invoke.MethodHandleStatics" . "java.lang.invoke"))

