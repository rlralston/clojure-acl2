; EventListenerProxy-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:45 CDT 2014.
;

(defconst *java.util.EventListenerProxy*
 (make-class-def
      '(class "java.util.EventListenerProxy"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "listener" (class "java.util.EventListener") (accessflags  *class*  *final*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.EventListener"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "listener" "java.util.EventListenerProxy" (class "java.util.EventListener"))))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getListener"
                              (parameters )
                              (returntype . (class "java.util.EventListener"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "listener" "java.util.EventListenerProxy" (class "java.util.EventListener"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.util.EventListener")
            (accessflags  *abstract*  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")))))


(defconst *EventListenerProxy-class-table*
  (make-static-class-decls 
   *java.util.EventListenerProxy*))

(defconst *package-name-map* 
  ("java.util.EventListenerProxy" . "java.util"))

