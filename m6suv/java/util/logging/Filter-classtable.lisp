; Filter-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:46 CDT 2014.
;

(defconst *java.util.logging.Filter*
 (make-class-def
      '(class "java.util.logging.Filter"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "isLoggable"
                              (parameters (class "java.util.logging.LogRecord"))
                              (returntype . boolean)
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces)
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")))))


(defconst *Filter-class-table*
  (make-static-class-decls 
   *java.util.logging.Filter*))

(defconst *package-name-map* 
  ("java.util.logging.Filter" . "java.util.logging"))
