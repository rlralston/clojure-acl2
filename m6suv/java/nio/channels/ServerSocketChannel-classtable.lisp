; ServerSocketChannel-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:38 CDT 2014.
;

(defconst *java.nio.channels.ServerSocketChannel*
 (make-class-def
      '(class "java.nio.channels.ServerSocketChannel"
            "java.nio.channels.spi.AbstractSelectableChannel"
            (constant_pool)
            (fields)
            (methods
                        (method "<init>"
                              (parameters (class "java.nio.channels.spi.SelectorProvider"))
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokespecial
					(methodCP "<init>" "java.nio.channels.spi.AbstractSelectableChannel" ((class "java.nio.channels.spi.SelectorProvider")) void)))
                                      (5 (return))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "open"
                              (parameters )
                              (returntype . (class "java.nio.channels.ServerSocketChannel"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 0) (code_length . 7)
                                   (parsedcode
                                      (0 (invokestatic
					(methodCP "provider" "java.nio.channels.spi.SelectorProvider" () (class "java.nio.channels.spi.SelectorProvider"))))
                                      (3 (invokevirtual
					(methodCP "openServerSocketChannel" "java.nio.channels.spi.SelectorProvider" () (class "java.nio.channels.ServerSocketChannel"))))
                                      (6 (areturn))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap )))
                        (method "validOps"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 3)
                                   (parsedcode
                                      (0 (bipush 16))
                                      (2 (ireturn))
                                      (endofcode 3))
                                   (Exceptions )
                                   (StackMap )))
                        (method "bind"
                              (parameters (class "java.net.SocketAddress"))
                              (returntype . (class "java.nio.channels.ServerSocketChannel"))
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 7)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (iconst_0))
                                      (3 (invokevirtual
					(methodCP "bind" "java.nio.channels.ServerSocketChannel" ((class "java.net.SocketAddress") int) (class "java.nio.channels.ServerSocketChannel"))))
                                      (6 (areturn))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap )))
                        (method "bind"
                              (parameters (class "java.net.SocketAddress") int)
                              (returntype . (class "java.nio.channels.ServerSocketChannel"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "setOption"
                              (parameters (class "java.net.SocketOption") (class "java.lang.Object"))
                              (returntype . (class "java.nio.channels.ServerSocketChannel"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "socket"
                              (parameters )
                              (returntype . (class "java.net.ServerSocket"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "accept"
                              (parameters )
                              (returntype . (class "java.nio.channels.SocketChannel"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "setOption"
                              (parameters (class "java.net.SocketOption") (class "java.lang.Object"))
                              (returntype . (class "java.nio.channels.NetworkChannel"))
                              (accessflags  *class*  *public*  *volatile* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 7)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (aload_2))
                                      (3 (invokevirtual
					(methodCP "setOption" "java.nio.channels.ServerSocketChannel" ((class "java.net.SocketOption") (class "java.lang.Object")) (class "java.nio.channels.ServerSocketChannel"))))
                                      (6 (areturn))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap )))
                        (method "bind"
                              (parameters (class "java.net.SocketAddress"))
                              (returntype . (class "java.nio.channels.NetworkChannel"))
                              (accessflags  *class*  *public*  *volatile* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokevirtual
					(methodCP "bind" "java.nio.channels.ServerSocketChannel" ((class "java.net.SocketAddress")) (class "java.nio.channels.ServerSocketChannel"))))
                                      (5 (areturn))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.nio.channels.NetworkChannel")
            (accessflags  *abstract*  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *ServerSocketChannel-class-table*
  (make-static-class-decls 
   *java.nio.channels.ServerSocketChannel*))

(defconst *package-name-map* 
  ("java.nio.channels.ServerSocketChannel" . "java.nio.channels"))
