; ConcurrentMap-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:43 CDT 2014.
;

(defconst *java.util.concurrent.ConcurrentMap*
 (make-class-def
      '(class "java.util.concurrent.ConcurrentMap"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "putIfAbsent"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "remove"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "replace"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "replace"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces "java.util.Map")
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")))))


(defconst *ConcurrentMap-class-table*
  (make-static-class-decls 
   *java.util.concurrent.ConcurrentMap*))

(defconst *package-name-map* 
  ("java.util.concurrent.ConcurrentMap" . "java.util.concurrent"))

