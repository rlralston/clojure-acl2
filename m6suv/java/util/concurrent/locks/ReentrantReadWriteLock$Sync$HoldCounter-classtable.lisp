; ReentrantReadWriteLock$Sync$HoldCounter-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:44 CDT 2014.
;

(defconst *java.util.concurrent.locks.ReentrantReadWriteLock$Sync$HoldCounter*
 (make-class-def
      '(class "java.util.concurrent.locks.ReentrantReadWriteLock$Sync$HoldCounter"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "count" int (accessflags  *class* ) -1)
                        (field "tid" long (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 20)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (iconst_0))
                                      (6 (putfield (fieldCP "count" "java.util.concurrent.locks.ReentrantReadWriteLock$Sync$HoldCounter" int)))
                                      (9 (aload_0))
                                      (10 (invokestatic
					(methodCP "currentThread" "java.lang.Thread" () (class "java.lang.Thread"))))
                                      (13 (invokevirtual
					(methodCP "getId" "java.lang.Thread" () long)))
                                      (16 (putfield (fieldCP "tid" "java.util.concurrent.locks.ReentrantReadWriteLock$Sync$HoldCounter" long)))
                                      (19 (return))
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *ReentrantReadWriteLock$Sync$HoldCounter-class-table*
  (make-static-class-decls 
   *java.util.concurrent.locks.ReentrantReadWriteLock$Sync$HoldCounter*))

(defconst *package-name-map* 
  ("java.util.concurrent.locks.ReentrantReadWriteLock$Sync$HoldCounter" . "java.util.concurrent.locks"))

