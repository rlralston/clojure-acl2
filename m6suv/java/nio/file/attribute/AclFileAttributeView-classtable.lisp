; AclFileAttributeView-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:39 CDT 2014.
;

(defconst *java.nio.file.attribute.AclFileAttributeView*
 (make-class-def
      '(class "java.nio.file.attribute.AclFileAttributeView"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "name"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getAcl"
                              (parameters )
                              (returntype . (class "java.util.List"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "setAcl"
                              (parameters (class "java.util.List"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces "java.nio.file.attribute.FileOwnerAttributeView")
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")))))


(defconst *AclFileAttributeView-class-table*
  (make-static-class-decls 
   *java.nio.file.attribute.AclFileAttributeView*))

(defconst *package-name-map* 
  ("java.nio.file.attribute.AclFileAttributeView" . "java.nio.file.attribute"))

