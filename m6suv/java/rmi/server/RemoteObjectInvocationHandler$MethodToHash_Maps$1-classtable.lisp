; RemoteObjectInvocationHandler$MethodToHash_Maps$1-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:39 CDT 2014.
;

(defconst *java.rmi.server.RemoteObjectInvocationHandler$MethodToHash_Maps$1*
 (make-class-def
      '(class "java.rmi.server.RemoteObjectInvocationHandler$MethodToHash_Maps$1"
            "java.util.WeakHashMap"
            (constant_pool)
            (fields
                        (field "this$0" (class "java.rmi.server.RemoteObjectInvocationHandler$MethodToHash_Maps") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.rmi.server.RemoteObjectInvocationHandler$MethodToHash_Maps"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.rmi.server.RemoteObjectInvocationHandler$MethodToHash_Maps$1" (class "java.rmi.server.RemoteObjectInvocationHandler$MethodToHash_Maps"))))
                                      (5 (aload_0))
                                      (6 (invokespecial
					(methodCP "<init>" "java.util.WeakHashMap" () void)))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "get"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Long"))
                              (accessflags  *class*  *public*  *super*  *synchronized* )
                              (code
                                   (max_stack . 3) (max_locals . 4) (code_length . 35)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (aload_1)) 
                                      (2 (invokespecial (methodCP "get" "java.util.WeakHashMap" ((class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (5 (checkcast (class "java.lang.Long"))) 
                                      (8 (astore_2)) 
                                      (9 (aload_2)) 
                                      (10 (ifnonnull 33))  ;;to TAG_0
                                      (13 (aload_1)) 
                                      (14 (checkcast (class "java.lang.reflect.Method"))) 
                                      (17 (astore_3)) 
                                      (18 (aload_3)) 
                                      (19 (invokestatic (methodCP "computeMethodHash" "sun.rmi.server.Util" ((class "java.lang.reflect.Method")) long))) 
                                      (22 (invokestatic (methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long")))) 
                                      (25 (astore_2)) 
                                      (26 (aload_0)) 
                                      (27 (aload_3)) 
                                      (28 (aload_2)) 
                                      (29 (invokevirtual (methodCP "put" "java.rmi.server.RemoteObjectInvocationHandler$MethodToHash_Maps$1" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (32 (pop)) 
                                      (33 (aload_2)) ;;at TAG_0
                                      (34 (areturn)) 
                                      (endofcode 35))
                                   (Exceptions )
                                   (StackMap )))
                        (method "get"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public*  *volatile* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokevirtual
					(methodCP "get" "java.rmi.server.RemoteObjectInvocationHandler$MethodToHash_Maps$1" ((class "java.lang.Object")) (class "java.lang.Long"))))
                                      (5 (areturn))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *RemoteObjectInvocationHandler$MethodToHash_Maps$1-class-table*
  (make-static-class-decls 
   *java.rmi.server.RemoteObjectInvocationHandler$MethodToHash_Maps$1*))

(defconst *package-name-map* 
  ("java.rmi.server.RemoteObjectInvocationHandler$MethodToHash_Maps$1" . "java.rmi.server"))

