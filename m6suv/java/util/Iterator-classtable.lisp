; Iterator-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:46 CDT 2014.
;

(defconst *java.util.Iterator*
 (make-class-def
      '(class "java.util.Iterator"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "hasNext"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "next"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "remove"
                              (parameters )
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces)
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")))))


(defconst *Iterator-class-table*
  (make-static-class-decls 
   *java.util.Iterator*))

(defconst *package-name-map* 
  ("java.util.Iterator" . "java.util"))

