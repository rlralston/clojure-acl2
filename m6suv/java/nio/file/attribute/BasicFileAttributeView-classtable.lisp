; BasicFileAttributeView-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:39 CDT 2014.
;

(defconst *java.nio.file.attribute.BasicFileAttributeView*
 (make-class-def
      '(class "java.nio.file.attribute.BasicFileAttributeView"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "name"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "readAttributes"
                              (parameters )
                              (returntype . (class "java.nio.file.attribute.BasicFileAttributes"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "setTimes"
                              (parameters (class "java.nio.file.attribute.FileTime") (class "java.nio.file.attribute.FileTime") (class "java.nio.file.attribute.FileTime"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces "java.nio.file.attribute.FileAttributeView")
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")))))


(defconst *BasicFileAttributeView-class-table*
  (make-static-class-decls 
   *java.nio.file.attribute.BasicFileAttributeView*))

(defconst *package-name-map* 
  ("java.nio.file.attribute.BasicFileAttributeView" . "java.nio.file.attribute"))
