; SynchronousQueue$LifoWaitQueue-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:44 CDT 2014.
;

(defconst *java.util.concurrent.SynchronousQueue$LifoWaitQueue*
 (make-class-def
      '(class "java.util.concurrent.SynchronousQueue$LifoWaitQueue"
            "java.util.concurrent.SynchronousQueue$WaitQueue"
            (constant_pool
                        (LONG -3633113410248163686))
            (fields
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0))
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
					(methodCP "<init>" "java.util.concurrent.SynchronousQueue$WaitQueue" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *SynchronousQueue$LifoWaitQueue-class-table*
  (make-static-class-decls 
   *java.util.concurrent.SynchronousQueue$LifoWaitQueue*))

(defconst *package-name-map* 
  ("java.util.concurrent.SynchronousQueue$LifoWaitQueue" . "java.util.concurrent"))

