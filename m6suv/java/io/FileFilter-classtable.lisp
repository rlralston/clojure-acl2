; FileFilter-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:32 CDT 2014.
;

(defconst *java.io.FileFilter*
 (make-class-def
      '(class "java.io.FileFilter"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "accept"
                              (parameters (class "java.io.File"))
                              (returntype . boolean)
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces)
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")))))


(defconst *FileFilter-class-table*
  (make-static-class-decls 
   *java.io.FileFilter*))

(defconst *package-name-map* 
  ("java.io.FileFilter" . "java.io"))
