; BeanContextServiceAvailableEvent-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:30 CDT 2014.
;

(defconst *java.beans.beancontext.BeanContextServiceAvailableEvent*
 (make-class-def
      '(class "java.beans.beancontext.BeanContextServiceAvailableEvent"
            "java.beans.beancontext.BeanContextEvent"
            (constant_pool
                        (LONG -5333985775656400778))
            (fields
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0)
                        (field "serviceClass" (class "java.lang.Class") (accessflags  *class*  *protected* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.beans.beancontext.BeanContextServices") (class "java.lang.Class"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokespecial
					(methodCP "<init>" "java.beans.beancontext.BeanContextEvent" ((class "java.beans.beancontext.BeanContext")) void)))
                                      (5 (aload_0))
                                      (6 (aload_2))
                                      (7 (putfield (fieldCP "serviceClass" "java.beans.beancontext.BeanContextServiceAvailableEvent" (class "java.lang.Class"))))
                                      (10 (return))
                                      (endofcode 11))
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
					(methodCP "getBeanContext" "java.beans.beancontext.BeanContextServiceAvailableEvent" () (class "java.beans.beancontext.BeanContext"))))
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
                                      (1 (getfield (fieldCP "serviceClass" "java.beans.beancontext.BeanContextServiceAvailableEvent" (class "java.lang.Class"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getCurrentServiceSelectors"
                              (parameters )
                              (returntype . (class "java.util.Iterator"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 17)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokevirtual
					(methodCP "getSource" "java.beans.beancontext.BeanContextServiceAvailableEvent" () (class "java.lang.Object"))))
                                      (4 (checkcast (class "java.beans.beancontext.BeanContextServices")))
                                      (7 (aload_0))
                                      (8 (getfield (fieldCP "serviceClass" "java.beans.beancontext.BeanContextServiceAvailableEvent" (class "java.lang.Class"))))
                                      (11 (invokeinterface
					(methodCP "getCurrentServiceSelectors" "java.beans.beancontext.BeanContextServices" ((class "java.lang.Class")) (class "java.util.Iterator")) 2))
                                      (16 (areturn))
                                      (endofcode 17))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *BeanContextServiceAvailableEvent-class-table*
  (make-static-class-decls 
   *java.beans.beancontext.BeanContextServiceAvailableEvent*))

(defconst *package-name-map* 
  ("java.beans.beancontext.BeanContextServiceAvailableEvent" . "java.beans.beancontext"))

