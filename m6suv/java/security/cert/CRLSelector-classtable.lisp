; CRLSelector-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:40 CDT 2014.
;

(defconst *java.security.cert.CRLSelector*
 (make-class-def
      '(class "java.security.cert.CRLSelector"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "match"
                              (parameters (class "java.security.cert.CRL"))
                              (returntype . boolean)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "clone"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces "java.lang.Cloneable")
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")))))


(defconst *CRLSelector-class-table*
  (make-static-class-decls 
   *java.security.cert.CRLSelector*))

(defconst *package-name-map* 
  ("java.security.cert.CRLSelector" . "java.security.cert"))
