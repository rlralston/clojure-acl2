; LoaderHandler-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:39 CDT 2014.
;

(defconst *java.rmi.server.LoaderHandler*
 (make-class-def
      '(class "java.rmi.server.LoaderHandler"
            "java.lang.Object"
            (constant_pool
                        (STRING  "sun.rmi.server"))
            (fields
                        (field "packagePrefix" (class "java.lang.String") (accessflags  *class*  *final*  *public*  *static* ) 0))
            (methods
                        (method "loadClass"
                              (parameters (class "java.lang.String"))
                              (returntype . (class "java.lang.Class"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "loadClass"
                              (parameters (class "java.net.URL") (class "java.lang.String"))
                              (returntype . (class "java.lang.Class"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getSecurityContext"
                              (parameters (class "java.lang.ClassLoader"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces)
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")
              (attribute "Deprecated")
              (attribute "RuntimeVisibleAnnotations")))))


(defconst *LoaderHandler-class-table*
  (make-static-class-decls 
   *java.rmi.server.LoaderHandler*))

(defconst *package-name-map* 
  ("java.rmi.server.LoaderHandler" . "java.rmi.server"))

