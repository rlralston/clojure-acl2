; Key-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:40 CDT 2014.
;

(defconst *java.security.Key*
 (make-class-def
      '(class "java.security.Key"
            "java.lang.Object"
            (constant_pool
                        (LONG 6603384152749567654))
            (fields
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *public*  *static* ) 0))
            (methods
                        (method "getAlgorithm"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getFormat"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getEncoded"
                              (parameters )
                              (returntype . (array byte))
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces "java.io.Serializable")
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")))))


(defconst *Key-class-table*
  (make-static-class-decls 
   *java.security.Key*))

(defconst *package-name-map* 
  ("java.security.Key" . "java.security"))

