; Checksum-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:48 CDT 2014.
;

(defconst *java.util.zip.Checksum*
 (make-class-def
      '(class "java.util.zip.Checksum"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "update"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "update"
                              (parameters (array byte) int int)
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getValue"
                              (parameters )
                              (returntype . long)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "reset"
                              (parameters )
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces)
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")))))


(defconst *Checksum-class-table*
  (make-static-class-decls 
   *java.util.zip.Checksum*))

(defconst *package-name-map* 
  ("java.util.zip.Checksum" . "java.util.zip"))

