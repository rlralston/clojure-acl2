; Executors$PrivilegedCallableUsingCurrentClassLoader$1-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:44 CDT 2014.
;

(defconst *java.util.concurrent.Executors$PrivilegedCallableUsingCurrentClassLoader$1*
 (make-class-def
      '(class "java.util.concurrent.Executors$PrivilegedCallableUsingCurrentClassLoader$1"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "this$0" (class "java.util.concurrent.Executors$PrivilegedCallableUsingCurrentClassLoader") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.concurrent.Executors$PrivilegedCallableUsingCurrentClassLoader"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.util.concurrent.Executors$PrivilegedCallableUsingCurrentClassLoader$1" (class "java.util.concurrent.Executors$PrivilegedCallableUsingCurrentClassLoader"))))
                                      (5 (aload_0))
                                      (6 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "run"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 6) (code_length . 75)
                                   (parsedcode
                                      (0 (aconst_null)) 
                                      (1 (astore_1)) 
                                      (2 (invokestatic (methodCP "currentThread" "java.lang.Thread" () (class "java.lang.Thread")))) 
                                      (5 (astore_2)) 
                                      (6 (aload_2)) ;;at TAG_3
                                      (7 (invokevirtual (methodCP "getContextClassLoader" "java.lang.Thread" () (class "java.lang.ClassLoader")))) 
                                      (10 (astore_3)) 
                                      (11 (aload_0)) 
                                      (12 (getfield (fieldCP "this$0" "java.util.concurrent.Executors$PrivilegedCallableUsingCurrentClassLoader$1" (class "java.util.concurrent.Executors$PrivilegedCallableUsingCurrentClassLoader")))) 
                                      (15 (invokestatic (methodCP "access$100" "java.util.concurrent.Executors$PrivilegedCallableUsingCurrentClassLoader" ((class "java.util.concurrent.Executors$PrivilegedCallableUsingCurrentClassLoader")) (class "java.lang.ClassLoader")))) 
                                      (18 (aload_3)) 
                                      (19 (if_acmpeq 35)) ;;to TAG_0
                                      (22 (aload_2)) 
                                      (23 (aload_0)) 
                                      (24 (getfield (fieldCP "this$0" "java.util.concurrent.Executors$PrivilegedCallableUsingCurrentClassLoader$1" (class "java.util.concurrent.Executors$PrivilegedCallableUsingCurrentClassLoader")))) 
                                      (27 (invokestatic (methodCP "access$100" "java.util.concurrent.Executors$PrivilegedCallableUsingCurrentClassLoader" ((class "java.util.concurrent.Executors$PrivilegedCallableUsingCurrentClassLoader")) (class "java.lang.ClassLoader")))) 
                                      (30 (invokevirtual (methodCP "setContextClassLoader" "java.lang.Thread" ((class "java.lang.ClassLoader")) void))) 
                                      (33 (aload_3)) 
                                      (34 (astore_1)) 
                                      (35 (aload_0)) ;;at TAG_0
                                      (36 (getfield (fieldCP "this$0" "java.util.concurrent.Executors$PrivilegedCallableUsingCurrentClassLoader$1" (class "java.util.concurrent.Executors$PrivilegedCallableUsingCurrentClassLoader")))) 
                                      (39 (invokestatic (methodCP "access$200" "java.util.concurrent.Executors$PrivilegedCallableUsingCurrentClassLoader" ((class "java.util.concurrent.Executors$PrivilegedCallableUsingCurrentClassLoader")) (class "java.util.concurrent.Callable")))) 
                                      (42 (invokeinterface (methodCP "call" "java.util.concurrent.Callable" () (class "java.lang.Object")) 1)) 
                                      (47 (astore 4)) 
                                      (49 (aload_1)) ;;at TAG_4
                                      (50 (ifnull 58)) ;;to TAG_1
                                      (53 (aload_2)) 
                                      (54 (aload_1)) 
                                      (55 (invokevirtual (methodCP "setContextClassLoader" "java.lang.Thread" ((class "java.lang.ClassLoader")) void))) 
                                      (58 (aload 4)) ;;at TAG_1
                                      (60 (areturn)) 
                                      (61 (astore 5)) ;;at TAG_5
                                      (63 (aload_1)) ;;at TAG_6
                                      (64 (ifnull 72))  ;;to TAG_2
                                      (67 (aload_2)) 
                                      (68 (aload_1)) 
                                      (69 (invokevirtual (methodCP "setContextClassLoader" "java.lang.Thread" ((class "java.lang.ClassLoader")) void))) 
                                      (72 (aload 5)) ;;at TAG_2
                                      (74 (athrow)) 
                                      (endofcode 75))
                                   (Exceptions 
                                     (handler 6 49  61 (class "java.lang.Throwable"))
                                     (handler 61 63  61 (class "java.lang.Throwable")))
                                   (StackMap ))))
            (interfaces "java.security.PrivilegedExceptionAction")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *Executors$PrivilegedCallableUsingCurrentClassLoader$1-class-table*
  (make-static-class-decls 
   *java.util.concurrent.Executors$PrivilegedCallableUsingCurrentClassLoader$1*))

(defconst *package-name-map* 
  ("java.util.concurrent.Executors$PrivilegedCallableUsingCurrentClassLoader$1" . "java.util.concurrent"))

