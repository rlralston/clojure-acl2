; Panel$AccessibleAWTPanel-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:30 CDT 2014.
;

(defconst *java.awt.Panel$AccessibleAWTPanel*
 (make-class-def
      '(class "java.awt.Panel$AccessibleAWTPanel"
            "java.awt.Container$AccessibleAWTContainer"
            (constant_pool
                        (LONG -6409552226660031050))
            (fields
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0)
                        (field "this$0" (class "java.awt.Panel") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.awt.Panel"))
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.awt.Panel$AccessibleAWTPanel" (class "java.awt.Panel"))))
                                      (5 (aload_0))
                                      (6 (aload_1))
                                      (7 (invokespecial
					(methodCP "<init>" "java.awt.Container$AccessibleAWTContainer" ((class "java.awt.Container")) void)))
                                      (10 (return))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getAccessibleRole"
                              (parameters )
                              (returntype . (class "javax.accessibility.AccessibleRole"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 4)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "PANEL" "javax.accessibility.AccessibleRole" (class "javax.accessibility.AccessibleRole"))))
                                      (3 (areturn))
                                      (endofcode 4))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *Panel$AccessibleAWTPanel-class-table*
  (make-static-class-decls 
   *java.awt.Panel$AccessibleAWTPanel*))

(defconst *package-name-map* 
  ("java.awt.Panel$AccessibleAWTPanel" . "java.awt"))

