; PlatformLoggingMXBean-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:35 CDT 2014.
;

(defconst *java.lang.management.PlatformLoggingMXBean*
 (make-class-def
      '(class "java.lang.management.PlatformLoggingMXBean"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "getLoggerNames"
                              (parameters )
                              (returntype . (class "java.util.List"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getLoggerLevel"
                              (parameters (class "java.lang.String"))
                              (returntype . (class "java.lang.String"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "setLoggerLevel"
                              (parameters (class "java.lang.String") (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getParentLoggerName"
                              (parameters (class "java.lang.String"))
                              (returntype . (class "java.lang.String"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces "java.lang.management.PlatformManagedObject")
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")))))


(defconst *PlatformLoggingMXBean-class-table*
  (make-static-class-decls 
   *java.lang.management.PlatformLoggingMXBean*))

(defconst *package-name-map* 
  ("java.lang.management.PlatformLoggingMXBean" . "java.lang.management"))

