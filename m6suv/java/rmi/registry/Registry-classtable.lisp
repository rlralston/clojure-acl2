; Registry-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:39 CDT 2014.
;

(defconst *java.rmi.registry.Registry*
 (make-class-def
      '(class "java.rmi.registry.Registry"
            "java.lang.Object"
            (constant_pool
                        (INT 1099))
            (fields
                        (field "REGISTRY_PORT" int (accessflags  *class*  *final*  *public*  *static* ) 0))
            (methods
                        (method "lookup"
                              (parameters (class "java.lang.String"))
                              (returntype . (class "java.rmi.Remote"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "bind"
                              (parameters (class "java.lang.String") (class "java.rmi.Remote"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "unbind"
                              (parameters (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "rebind"
                              (parameters (class "java.lang.String") (class "java.rmi.Remote"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "list"
                              (parameters )
                              (returntype . (array (class "java.lang.String")))
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces "java.rmi.Remote")
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")))))


(defconst *Registry-class-table*
  (make-static-class-decls 
   *java.rmi.registry.Registry*))

(defconst *package-name-map* 
  ("java.rmi.registry.Registry" . "java.rmi.registry"))
