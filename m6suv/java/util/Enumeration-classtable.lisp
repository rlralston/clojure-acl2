; Enumeration-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:45 CDT 2014.
;

(defconst *java.util.Enumeration*
 (make-class-def
      '(class "java.util.Enumeration"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "hasMoreElements"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "nextElement"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces)
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")))))


(defconst *Enumeration-class-table*
  (make-static-class-decls 
   *java.util.Enumeration*))

(defconst *package-name-map* 
  ("java.util.Enumeration" . "java.util"))

