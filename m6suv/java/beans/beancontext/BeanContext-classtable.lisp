; BeanContext-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:30 CDT 2014.
;

(defconst *java.beans.beancontext.BeanContext*
 (make-class-def
      '(class "java.beans.beancontext.BeanContext"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "globalHierarchyLock" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "instantiateChild"
                              (parameters (class "java.lang.String"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getResourceAsStream"
                              (parameters (class "java.lang.String") (class "java.beans.beancontext.BeanContextChild"))
                              (returntype . (class "java.io.InputStream"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getResource"
                              (parameters (class "java.lang.String") (class "java.beans.beancontext.BeanContextChild"))
                              (returntype . (class "java.net.URL"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "addBeanContextMembershipListener"
                              (parameters (class "java.beans.beancontext.BeanContextMembershipListener"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "removeBeanContextMembershipListener"
                              (parameters (class "java.beans.beancontext.BeanContextMembershipListener"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 11)
                                   (parsedcode
                                      (0 (new (class "java.lang.Object")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (7 (putstatic (fieldCP "globalHierarchyLock" "java.beans.beancontext.BeanContext" (class "java.lang.Object"))))
                                      (10 (return))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.beans.beancontext.BeanContextChild" "java.util.Collection" "java.beans.DesignMode" "java.beans.Visibility")
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")))))


(defconst *BeanContext-class-table*
  (make-static-class-decls 
   *java.beans.beancontext.BeanContext*))

(defconst *package-name-map* 
  ("java.beans.beancontext.BeanContext" . "java.beans.beancontext"))

