; BufferPoolMXBean-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:35 CDT 2014.
;

(defconst *java.lang.management.BufferPoolMXBean*
 (make-class-def
      '(class "java.lang.management.BufferPoolMXBean"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "getName"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getCount"
                              (parameters )
                              (returntype . long)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getTotalCapacity"
                              (parameters )
                              (returntype . long)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getMemoryUsed"
                              (parameters )
                              (returntype . long)
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces "java.lang.management.PlatformManagedObject")
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")))))


(defconst *BufferPoolMXBean-class-table*
  (make-static-class-decls 
   *java.lang.management.BufferPoolMXBean*))

(defconst *package-name-map* 
  ("java.lang.management.BufferPoolMXBean" . "java.lang.management"))

