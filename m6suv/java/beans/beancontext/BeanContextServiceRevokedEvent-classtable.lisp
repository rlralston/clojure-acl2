; BeanContextServiceRevokedEvent-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:30 CDT 2014.
;

(defconst *java.beans.beancontext.BeanContextServiceRevokedEvent*
 (make-class-def
      '(class "java.beans.beancontext.BeanContextServiceRevokedEvent"
            "java.beans.beancontext.BeanContextEvent"
            (constant_pool
                        (LONG -1295543154724961754))
            (fields
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0)
                        (field "serviceClass" (class "java.lang.Class") (accessflags  *class*  *protected* ) -1)
                        (field "invalidateRefs" boolean (accessflags  *class*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.beans.beancontext.BeanContextServices") (class "java.lang.Class") boolean)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 16)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokespecial
					(methodCP "<init>" "java.beans.beancontext.BeanContextEvent" ((class "java.beans.beancontext.BeanContext")) void)))
                                      (5 (aload_0))
                                      (6 (aload_2))
                                      (7 (putfield (fieldCP "serviceClass" "java.beans.beancontext.BeanContextServiceRevokedEvent" (class "java.lang.Class"))))
                                      (10 (aload_0))
                                      (11 (iload_3))
                                      (12 (putfield (fieldCP "invalidateRefs" "java.beans.beancontext.BeanContextServiceRevokedEvent" boolean)))
                                      (15 (return))
                                      (endofcode 16))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getSourceAsBeanContextServices"
                              (parameters )
                              (returntype . (class "java.beans.beancontext.BeanContextServices"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokevirtual
					(methodCP "getBeanContext" "java.beans.beancontext.BeanContextServiceRevokedEvent" () (class "java.beans.beancontext.BeanContext"))))
                                      (4 (checkcast (class "java.beans.beancontext.BeanContextServices")))
                                      (7 (areturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getServiceClass"
                              (parameters )
                              (returntype . (class "java.lang.Class"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "serviceClass" "java.beans.beancontext.BeanContextServiceRevokedEvent" (class "java.lang.Class"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isServiceClass"
                              (parameters (class "java.lang.Class"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "serviceClass" "java.beans.beancontext.BeanContextServiceRevokedEvent" (class "java.lang.Class"))))
                                      (4 (aload_1))
                                      (5 (invokevirtual
					(methodCP "equals" "java.lang.Object" ((class "java.lang.Object")) boolean)))
                                      (8 (ireturn))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isCurrentServiceInvalidNow"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "invalidateRefs" "java.beans.beancontext.BeanContextServiceRevokedEvent" boolean)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *BeanContextServiceRevokedEvent-class-table*
  (make-static-class-decls 
   *java.beans.beancontext.BeanContextServiceRevokedEvent*))

(defconst *package-name-map* 
  ("java.beans.beancontext.BeanContextServiceRevokedEvent" . "java.beans.beancontext"))
