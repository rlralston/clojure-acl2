; EventQueue$2-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:26 CDT 2014.
;

(defconst *java.awt.EventQueue$2*
 (make-class-def
      '(class "java.awt.EventQueue$2"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getDispatchThread"
                              (parameters (class "java.awt.EventQueue"))
                              (returntype . (class "java.lang.Thread"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (invokevirtual
					(methodCP "getDispatchThread" "java.awt.EventQueue" () (class "java.awt.EventDispatchThread"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isDispatchThreadImpl"
                              (parameters (class "java.awt.EventQueue"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (invokevirtual
					(methodCP "isDispatchThreadImpl" "java.awt.EventQueue" () boolean)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "sun.awt.AWTAccessor$EventQueueAccessor")
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "InnerClasses")))))


(defconst *EventQueue$2-class-table*
  (make-static-class-decls 
   *java.awt.EventQueue$2*))

(defconst *package-name-map* 
  ("java.awt.EventQueue$2" . "java.awt"))
