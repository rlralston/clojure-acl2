; BeanContextServicesListener-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:30 CDT 2014.
;

(defconst *java.beans.beancontext.BeanContextServicesListener*
 (make-class-def
      '(class "java.beans.beancontext.BeanContextServicesListener"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "serviceAvailable"
                              (parameters (class "java.beans.beancontext.BeanContextServiceAvailableEvent"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces "java.beans.beancontext.BeanContextServiceRevokedListener")
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")))))


(defconst *BeanContextServicesListener-class-table*
  (make-static-class-decls 
   *java.beans.beancontext.BeanContextServicesListener*))

(defconst *package-name-map* 
  ("java.beans.beancontext.BeanContextServicesListener" . "java.beans.beancontext"))
