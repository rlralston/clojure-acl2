; RemoteRef-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:40 CDT 2014.
;

(defconst *java.rmi.server.RemoteRef*
 (make-class-def
      '(class "java.rmi.server.RemoteRef"
            "java.lang.Object"
            (constant_pool
                        (LONG 3632638527362204081)
                        (STRING  "sun.rmi.server"))
            (fields
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *public*  *static* ) 0)
                        (field "packagePrefix" (class "java.lang.String") (accessflags  *class*  *final*  *public*  *static* ) 1))
            (methods
                        (method "invoke"
                              (parameters (class "java.rmi.Remote") (class "java.lang.reflect.Method") (array (class "java.lang.Object")) long)
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "newCall"
                              (parameters (class "java.rmi.server.RemoteObject") (array (class "java.rmi.server.Operation")) int long)
                              (returntype . (class "java.rmi.server.RemoteCall"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "invoke"
                              (parameters (class "java.rmi.server.RemoteCall"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "done"
                              (parameters (class "java.rmi.server.RemoteCall"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getRefClass"
                              (parameters (class "java.io.ObjectOutput"))
                              (returntype . (class "java.lang.String"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "remoteHashCode"
                              (parameters )
                              (returntype . int)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "remoteEquals"
                              (parameters (class "java.rmi.server.RemoteRef"))
                              (returntype . boolean)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "remoteToString"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces "java.io.Externalizable")
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")))))


(defconst *RemoteRef-class-table*
  (make-static-class-decls 
   *java.rmi.server.RemoteRef*))

(defconst *package-name-map* 
  ("java.rmi.server.RemoteRef" . "java.rmi.server"))

