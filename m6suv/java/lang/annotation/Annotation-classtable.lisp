; Annotation-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:33 CDT 2014.
;

(defconst *java.lang.annotation.Annotation*
 (make-class-def
      '(class "java.lang.annotation.Annotation"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "equals"
                              (parameters (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "hashCode"
                              (parameters )
                              (returntype . int)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "toString"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "annotationType"
                              (parameters )
                              (returntype . (class "java.lang.Class"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces)
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")))))


(defconst *Annotation-class-table*
  (make-static-class-decls 
   *java.lang.annotation.Annotation*))

(defconst *package-name-map* 
  ("java.lang.annotation.Annotation" . "java.lang.annotation"))

