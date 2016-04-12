; AsynchronousChannelProvider-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:38 CDT 2014.
;

(defconst *java.nio.channels.spi.AsynchronousChannelProvider*
 (make-class-def
      '(class "java.nio.channels.spi.AsynchronousChannelProvider"
            "java.lang.Object"
            (constant_pool
                        (STRING  "asynchronousChannelProvider"))
            (fields)
            (methods
                        (method "checkPermission"
                              (parameters )
                              (returntype . (class "java.lang.Void"))
                              (accessflags  *class*  *private*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 1) (code_length . 23)
                                   (parsedcode
                                      (0 (invokestatic (methodCP "getSecurityManager" "java.lang.System" () (class "java.lang.SecurityManager")))) 
                                      (3 (astore_0)) 
                                      (4 (aload_0)) 
                                      (5 (ifnull 21))  ;;to TAG_0
                                      (8 (aload_0)) 
                                      (9 (new (class "java.lang.RuntimePermission"))) 
                                      (12 (dup)) 
                                      (13 (ldc 0)) ;;STRING:: "asynchronousChannelProvider"
                                      (15 (invokespecial (methodCP "<init>" "java.lang.RuntimePermission" ((class "java.lang.String")) void))) 
                                      (18 (invokevirtual (methodCP "checkPermission" "java.lang.SecurityManager" ((class "java.security.Permission")) void))) 
                                      (21 (aconst_null)) ;;at TAG_0
                                      (22 (areturn)) 
                                      (endofcode 23))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.Void"))
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokestatic
					(methodCP "checkPermission" "java.nio.channels.spi.AsynchronousChannelProvider" () (class "java.lang.Void"))))
                                      (4 (invokespecial
					(methodCP "<init>" "java.nio.channels.spi.AsynchronousChannelProvider" ((class "java.lang.Void")) void)))
                                      (7 (return))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "provider"
                              (parameters )
                              (returntype . (class "java.nio.channels.spi.AsynchronousChannelProvider"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 0) (code_length . 4)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "provider" "java.nio.channels.spi.AsynchronousChannelProvider$ProviderHolder" (class "java.nio.channels.spi.AsynchronousChannelProvider"))))
                                      (3 (areturn))
                                      (endofcode 4))
                                   (Exceptions )
                                   (StackMap )))
                        (method "openAsynchronousChannelGroup"
                              (parameters int (class "java.util.concurrent.ThreadFactory"))
                              (returntype . (class "java.nio.channels.AsynchronousChannelGroup"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "openAsynchronousChannelGroup"
                              (parameters (class "java.util.concurrent.ExecutorService") int)
                              (returntype . (class "java.nio.channels.AsynchronousChannelGroup"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "openAsynchronousServerSocketChannel"
                              (parameters (class "java.nio.channels.AsynchronousChannelGroup"))
                              (returntype . (class "java.nio.channels.AsynchronousServerSocketChannel"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "openAsynchronousSocketChannel"
                              (parameters (class "java.nio.channels.AsynchronousChannelGroup"))
                              (returntype . (class "java.nio.channels.AsynchronousSocketChannel"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces)
            (accessflags  *abstract*  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *AsynchronousChannelProvider-class-table*
  (make-static-class-decls 
   *java.nio.channels.spi.AsynchronousChannelProvider*))

(defconst *package-name-map* 
  ("java.nio.channels.spi.AsynchronousChannelProvider" . "java.nio.channels.spi"))
