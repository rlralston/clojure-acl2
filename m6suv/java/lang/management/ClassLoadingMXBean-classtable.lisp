; ClassLoadingMXBean-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:35 CDT 2014.
;

(defconst *java.lang.management.ClassLoadingMXBean*
 (make-class-def
      '(class "java.lang.management.ClassLoadingMXBean"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "getTotalLoadedClassCount"
                              (parameters )
                              (returntype . long)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getLoadedClassCount"
                              (parameters )
                              (returntype . int)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getUnloadedClassCount"
                              (parameters )
                              (returntype . long)
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
                              (code)))
            (interfaces "java.lang.management.PlatformManagedObject")
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")))))


(defconst *ClassLoadingMXBean-class-table*
  (make-static-class-decls 
   *java.lang.management.ClassLoadingMXBean*))

(defconst *package-name-map* 
  ("java.lang.management.ClassLoadingMXBean" . "java.lang.management"))

