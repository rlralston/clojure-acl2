; ObjectOutput-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:32 CDT 2014.
;

(defconst *java.io.ObjectOutput*
 (make-class-def
      '(class "java.io.ObjectOutput"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "writeObject"
                              (parameters (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "write"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "write"
                              (parameters (array byte))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "write"
                              (parameters (array byte) int int)
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "flush"
                              (parameters )
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "close"
                              (parameters )
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces "java.io.DataOutput" "java.lang.AutoCloseable")
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")))))


(defconst *ObjectOutput-class-table*
  (make-static-class-decls 
   *java.io.ObjectOutput*))

(defconst *package-name-map* 
  ("java.io.ObjectOutput" . "java.io"))
