; MemoryMXBean-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:35 CDT 2014.
;

(defconst *java.lang.management.MemoryMXBean*
 (make-class-def
      '(class "java.lang.management.MemoryMXBean"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "getObjectPendingFinalizationCount"
                              (parameters )
                              (returntype . int)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getHeapMemoryUsage"
                              (parameters )
                              (returntype . (class "java.lang.management.MemoryUsage"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getNonHeapMemoryUsage"
                              (parameters )
                              (returntype . (class "java.lang.management.MemoryUsage"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "isVerbose"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "setVerbose"
                              (parameters boolean)
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "gc"
                              (parameters )
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces "java.lang.management.PlatformManagedObject")
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")))))


(defconst *MemoryMXBean-class-table*
  (make-static-class-decls 
   *java.lang.management.MemoryMXBean*))

(defconst *package-name-map* 
  ("java.lang.management.MemoryMXBean" . "java.lang.management"))

