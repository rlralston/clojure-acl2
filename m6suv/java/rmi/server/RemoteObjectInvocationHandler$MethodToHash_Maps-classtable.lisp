; RemoteObjectInvocationHandler$MethodToHash_Maps-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:40 CDT 2014.
;

(defconst *java.rmi.server.RemoteObjectInvocationHandler$MethodToHash_Maps*
 (make-class-def
      '(class "java.rmi.server.RemoteObjectInvocationHandler$MethodToHash_Maps"
            "sun.rmi.server.WeakClassHashMap"
            (constant_pool)
            (fields)
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "sun.rmi.server.WeakClassHashMap" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "computeValue"
                              (parameters (class "java.lang.Class"))
                              (returntype . (class "java.util.Map"))
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 9)
                                   (parsedcode
                                      (0 (new (class "java.rmi.server.RemoteObjectInvocationHandler$MethodToHash_Maps$1")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (invokespecial
					(methodCP "<init>" "java.rmi.server.RemoteObjectInvocationHandler$MethodToHash_Maps$1" ((class "java.rmi.server.RemoteObjectInvocationHandler$MethodToHash_Maps")) void)))
                                      (8 (areturn))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "computeValue"
                              (parameters (class "java.lang.Class"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *protected*  *volatile* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokevirtual
					(methodCP "computeValue" "java.rmi.server.RemoteObjectInvocationHandler$MethodToHash_Maps" ((class "java.lang.Class")) (class "java.util.Map"))))
                                      (5 (areturn))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *RemoteObjectInvocationHandler$MethodToHash_Maps-class-table*
  (make-static-class-decls 
   *java.rmi.server.RemoteObjectInvocationHandler$MethodToHash_Maps*))

(defconst *package-name-map* 
  ("java.rmi.server.RemoteObjectInvocationHandler$MethodToHash_Maps" . "java.rmi.server"))

