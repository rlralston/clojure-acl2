; ForkJoinPool$ForkJoinWorkerThreadFactory-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:44 CDT 2014.
;

(defconst *java.util.concurrent.ForkJoinPool$ForkJoinWorkerThreadFactory*
 (make-class-def
      '(class "java.util.concurrent.ForkJoinPool$ForkJoinWorkerThreadFactory"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "newThread"
                              (parameters (class "java.util.concurrent.ForkJoinPool"))
                              (returntype . (class "java.util.concurrent.ForkJoinWorkerThread"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces)
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *ForkJoinPool$ForkJoinWorkerThreadFactory-class-table*
  (make-static-class-decls 
   *java.util.concurrent.ForkJoinPool$ForkJoinWorkerThreadFactory*))

(defconst *package-name-map* 
  ("java.util.concurrent.ForkJoinPool$ForkJoinWorkerThreadFactory" . "java.util.concurrent"))

