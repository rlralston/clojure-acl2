; BeanContextServicesSupport$BCSSServiceProvider-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:30 CDT 2014.
;

(defconst *java.beans.beancontext.BeanContextServicesSupport$BCSSServiceProvider*
 (make-class-def
      '(class "java.beans.beancontext.BeanContextServicesSupport$BCSSServiceProvider"
            "java.lang.Object"
            (constant_pool
                        (LONG 861278251667444782))
            (fields
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0)
                        (field "serviceProvider" (class "java.beans.beancontext.BeanContextServiceProvider") (accessflags  *class*  *protected* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.lang.Class") (class "java.beans.beancontext.BeanContextServiceProvider"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (aload_2))
                                      (6 (putfield (fieldCP "serviceProvider" "java.beans.beancontext.BeanContextServicesSupport$BCSSServiceProvider" (class "java.beans.beancontext.BeanContextServiceProvider"))))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getServiceProvider"
                              (parameters )
                              (returntype . (class "java.beans.beancontext.BeanContextServiceProvider"))
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "serviceProvider" "java.beans.beancontext.BeanContextServicesSupport$BCSSServiceProvider" (class "java.beans.beancontext.BeanContextServiceProvider"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.io.Serializable")
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *BeanContextServicesSupport$BCSSServiceProvider-class-table*
  (make-static-class-decls 
   *java.beans.beancontext.BeanContextServicesSupport$BCSSServiceProvider*))

(defconst *package-name-map* 
  ("java.beans.beancontext.BeanContextServicesSupport$BCSSServiceProvider" . "java.beans.beancontext"))

