; ReentrantReadWriteLock$FairSync-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:44 CDT 2014.
;

(defconst *java.util.concurrent.locks.ReentrantReadWriteLock$FairSync*
 (make-class-def
      '(class "java.util.concurrent.locks.ReentrantReadWriteLock$FairSync"
            "java.util.concurrent.locks.ReentrantReadWriteLock$Sync"
            (constant_pool
                        (LONG -2274990926593161451))
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
					(methodCP "<init>" "java.util.concurrent.locks.ReentrantReadWriteLock$Sync" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "writerShouldBlock"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *final* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokevirtual
					(methodCP "hasQueuedPredecessors" "java.util.concurrent.locks.ReentrantReadWriteLock$FairSync" () boolean)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "readerShouldBlock"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *final* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokevirtual
					(methodCP "hasQueuedPredecessors" "java.util.concurrent.locks.ReentrantReadWriteLock$FairSync" () boolean)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *ReentrantReadWriteLock$FairSync-class-table*
  (make-static-class-decls 
   *java.util.concurrent.locks.ReentrantReadWriteLock$FairSync*))

(defconst *package-name-map* 
  ("java.util.concurrent.locks.ReentrantReadWriteLock$FairSync" . "java.util.concurrent.locks"))

