; ReadWriteLock-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:44 CDT 2014.
;

(defconst *java.util.concurrent.locks.ReadWriteLock*
 (make-class-def
      '(class "java.util.concurrent.locks.ReadWriteLock"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "readLock"
                              (parameters )
                              (returntype . (class "java.util.concurrent.locks.Lock"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "writeLock"
                              (parameters )
                              (returntype . (class "java.util.concurrent.locks.Lock"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces)
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")))))


(defconst *ReadWriteLock-class-table*
  (make-static-class-decls 
   *java.util.concurrent.locks.ReadWriteLock*))

(defconst *package-name-map* 
  ("java.util.concurrent.locks.ReadWriteLock" . "java.util.concurrent.locks"))

