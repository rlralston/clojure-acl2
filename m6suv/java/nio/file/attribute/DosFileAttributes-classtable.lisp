; DosFileAttributes-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:39 CDT 2014.
;

(defconst *java.nio.file.attribute.DosFileAttributes*
 (make-class-def
      '(class "java.nio.file.attribute.DosFileAttributes"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "isReadOnly"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "isHidden"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "isArchive"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "isSystem"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces "java.nio.file.attribute.BasicFileAttributes")
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")))))


(defconst *DosFileAttributes-class-table*
  (make-static-class-decls 
   *java.nio.file.attribute.DosFileAttributes*))

(defconst *package-name-map* 
  ("java.nio.file.attribute.DosFileAttributes" . "java.nio.file.attribute"))

