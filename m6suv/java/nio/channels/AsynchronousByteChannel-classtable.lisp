; AsynchronousByteChannel-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:38 CDT 2014.
;

(defconst *java.nio.channels.AsynchronousByteChannel*
 (make-class-def
      '(class "java.nio.channels.AsynchronousByteChannel"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "read"
                              (parameters (class "java.nio.ByteBuffer") (class "java.lang.Object") (class "java.nio.channels.CompletionHandler"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "read"
                              (parameters (class "java.nio.ByteBuffer"))
                              (returntype . (class "java.util.concurrent.Future"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "write"
                              (parameters (class "java.nio.ByteBuffer") (class "java.lang.Object") (class "java.nio.channels.CompletionHandler"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "write"
                              (parameters (class "java.nio.ByteBuffer"))
                              (returntype . (class "java.util.concurrent.Future"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces "java.nio.channels.AsynchronousChannel")
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")))))


(defconst *AsynchronousByteChannel-class-table*
  (make-static-class-decls 
   *java.nio.channels.AsynchronousByteChannel*))

(defconst *package-name-map* 
  ("java.nio.channels.AsynchronousByteChannel" . "java.nio.channels"))

