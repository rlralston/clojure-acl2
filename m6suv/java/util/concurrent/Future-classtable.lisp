; Future-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:44 CDT 2014.
;

(defconst *java.util.concurrent.Future*
 (make-class-def
      '(class "java.util.concurrent.Future"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "cancel"
                              (parameters boolean)
                              (returntype . boolean)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "isCancelled"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "isDone"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "get"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "get"
                              (parameters long (class "java.util.concurrent.TimeUnit"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces)
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")))))


(defconst *Future-class-table*
  (make-static-class-decls 
   *java.util.concurrent.Future*))

(defconst *package-name-map* 
  ("java.util.concurrent.Future" . "java.util.concurrent"))
