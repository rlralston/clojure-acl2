; UserDefinedFileAttributeView-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:39 CDT 2014.
;

(defconst *java.nio.file.attribute.UserDefinedFileAttributeView*
 (make-class-def
      '(class "java.nio.file.attribute.UserDefinedFileAttributeView"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "name"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "list"
                              (parameters )
                              (returntype . (class "java.util.List"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "size"
                              (parameters (class "java.lang.String"))
                              (returntype . int)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "read"
                              (parameters (class "java.lang.String") (class "java.nio.ByteBuffer"))
                              (returntype . int)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "write"
                              (parameters (class "java.lang.String") (class "java.nio.ByteBuffer"))
                              (returntype . int)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "delete"
                              (parameters (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces "java.nio.file.attribute.FileAttributeView")
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")))))


(defconst *UserDefinedFileAttributeView-class-table*
  (make-static-class-decls 
   *java.nio.file.attribute.UserDefinedFileAttributeView*))

(defconst *package-name-map* 
  ("java.nio.file.attribute.UserDefinedFileAttributeView" . "java.nio.file.attribute"))

