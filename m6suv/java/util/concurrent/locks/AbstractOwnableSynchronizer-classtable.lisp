; AbstractOwnableSynchronizer-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:44 CDT 2014.
;

(defconst *java.util.concurrent.locks.AbstractOwnableSynchronizer*
 (make-class-def
      '(class "java.util.concurrent.locks.AbstractOwnableSynchronizer"
            "java.lang.Object"
            (constant_pool
                        (LONG 3737899427754241961))
            (fields
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0)
                        (field "exclusiveOwnerThread" (class "java.lang.Thread") (accessflags  *class*  *private*  *transient* ) -1))
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *protected* )
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
                        (method "setExclusiveOwnerThread"
                              (parameters (class "java.lang.Thread"))
                              (returntype . void)
                              (accessflags  *class*  *final*  *protected* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "exclusiveOwnerThread" "java.util.concurrent.locks.AbstractOwnableSynchronizer" (class "java.lang.Thread"))))
                                      (5 (return))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getExclusiveOwnerThread"
                              (parameters )
                              (returntype . (class "java.lang.Thread"))
                              (accessflags  *class*  *final*  *protected* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "exclusiveOwnerThread" "java.util.concurrent.locks.AbstractOwnableSynchronizer" (class "java.lang.Thread"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.io.Serializable")
            (accessflags  *abstract*  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *AbstractOwnableSynchronizer-class-table*
  (make-static-class-decls 
   *java.util.concurrent.locks.AbstractOwnableSynchronizer*))

(defconst *package-name-map* 
  ("java.util.concurrent.locks.AbstractOwnableSynchronizer" . "java.util.concurrent.locks"))

