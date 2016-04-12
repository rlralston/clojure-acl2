; SocketOptions-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:37 CDT 2014.
;

(defconst *java.net.SocketOptions*
 (make-class-def
      '(class "java.net.SocketOptions"
            "java.lang.Object"
            (constant_pool
                        (INT 1)
                        (INT 15)
                        (INT 4)
                        (INT 32)
                        (INT 16)
                        (INT 31)
                        (INT 18)
                        (INT 3)
                        (INT 128)
                        (INT 4102)
                        (INT 4097)
                        (INT 4098)
                        (INT 8)
                        (INT 4099))
            (fields
                        (field "TCP_NODELAY" int (accessflags  *class*  *final*  *public*  *static* ) 0)
                        (field "SO_BINDADDR" int (accessflags  *class*  *final*  *public*  *static* ) 1)
                        (field "SO_REUSEADDR" int (accessflags  *class*  *final*  *public*  *static* ) 2)
                        (field "SO_BROADCAST" int (accessflags  *class*  *final*  *public*  *static* ) 3)
                        (field "IP_MULTICAST_IF" int (accessflags  *class*  *final*  *public*  *static* ) 4)
                        (field "IP_MULTICAST_IF2" int (accessflags  *class*  *final*  *public*  *static* ) 5)
                        (field "IP_MULTICAST_LOOP" int (accessflags  *class*  *final*  *public*  *static* ) 6)
                        (field "IP_TOS" int (accessflags  *class*  *final*  *public*  *static* ) 7)
                        (field "SO_LINGER" int (accessflags  *class*  *final*  *public*  *static* ) 8)
                        (field "SO_TIMEOUT" int (accessflags  *class*  *final*  *public*  *static* ) 9)
                        (field "SO_SNDBUF" int (accessflags  *class*  *final*  *public*  *static* ) 10)
                        (field "SO_RCVBUF" int (accessflags  *class*  *final*  *public*  *static* ) 11)
                        (field "SO_KEEPALIVE" int (accessflags  *class*  *final*  *public*  *static* ) 12)
                        (field "SO_OOBINLINE" int (accessflags  *class*  *final*  *public*  *static* ) 13))
            (methods
                        (method "setOption"
                              (parameters int (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getOption"
                              (parameters int)
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces)
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")))))


(defconst *SocketOptions-class-table*
  (make-static-class-decls 
   *java.net.SocketOptions*))

(defconst *package-name-map* 
  ("java.net.SocketOptions" . "java.net"))

