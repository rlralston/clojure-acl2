; MemoryPoolMXBean-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:35 CDT 2014.
;

(defconst *java.lang.management.MemoryPoolMXBean*
 (make-class-def
      '(class "java.lang.management.MemoryPoolMXBean"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "getName"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getType"
                              (parameters )
                              (returntype . (class "java.lang.management.MemoryType"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getUsage"
                              (parameters )
                              (returntype . (class "java.lang.management.MemoryUsage"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getPeakUsage"
                              (parameters )
                              (returntype . (class "java.lang.management.MemoryUsage"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "resetPeakUsage"
                              (parameters )
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "isValid"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getMemoryManagerNames"
                              (parameters )
                              (returntype . (array (class "java.lang.String")))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getUsageThreshold"
                              (parameters )
                              (returntype . long)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "setUsageThreshold"
                              (parameters long)
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "isUsageThresholdExceeded"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getUsageThresholdCount"
                              (parameters )
                              (returntype . long)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "isUsageThresholdSupported"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getCollectionUsageThreshold"
                              (parameters )
                              (returntype . long)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "setCollectionUsageThreshold"
                              (parameters long)
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "isCollectionUsageThresholdExceeded"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getCollectionUsageThresholdCount"
                              (parameters )
                              (returntype . long)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getCollectionUsage"
                              (parameters )
                              (returntype . (class "java.lang.management.MemoryUsage"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "isCollectionUsageThresholdSupported"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces "java.lang.management.PlatformManagedObject")
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")))))


(defconst *MemoryPoolMXBean-class-table*
  (make-static-class-decls 
   *java.lang.management.MemoryPoolMXBean*))

(defconst *package-name-map* 
  ("java.lang.management.MemoryPoolMXBean" . "java.lang.management"))

