; BeanContextSupport$2-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:30 CDT 2014.
;

(defconst *java.beans.beancontext.BeanContextSupport$2*
 (make-class-def
      '(class "java.beans.beancontext.BeanContextSupport$2"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "this$0" (class "java.beans.beancontext.BeanContextSupport") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.beans.beancontext.BeanContextSupport"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.beans.beancontext.BeanContextSupport$2" (class "java.beans.beancontext.BeanContextSupport"))))
                                      (5 (aload_0))
                                      (6 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "vetoableChange"
                              (parameters (class "java.beans.PropertyChangeEvent"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "this$0" "java.beans.beancontext.BeanContextSupport$2" (class "java.beans.beancontext.BeanContextSupport"))))
                                      (4 (aload_1))
                                      (5 (invokevirtual
					(methodCP "vetoableChange" "java.beans.beancontext.BeanContextSupport" ((class "java.beans.PropertyChangeEvent")) void)))
                                      (8 (return))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.beans.VetoableChangeListener")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "InnerClasses")))))


(defconst *BeanContextSupport$2-class-table*
  (make-static-class-decls 
   *java.beans.beancontext.BeanContextSupport$2*))

(defconst *package-name-map* 
  ("java.beans.beancontext.BeanContextSupport$2" . "java.beans.beancontext"))

