; WatchEvent-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:39 CDT 2014.
;

(defconst *java.nio.file.WatchEvent*
 (make-class-def
      '(class "java.nio.file.WatchEvent"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "kind"
                              (parameters )
                              (returntype . (class "java.nio.file.WatchEvent$Kind"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "count"
                              (parameters )
                              (returntype . int)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "context"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces)
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *WatchEvent-class-table*
  (make-static-class-decls 
   *java.nio.file.WatchEvent*))

(defconst *package-name-map* 
  ("java.nio.file.WatchEvent" . "java.nio.file"))

