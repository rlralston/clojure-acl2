; AbstractQueuedSynchronizer$Node-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:44 CDT 2014.
;

(defconst *java.util.concurrent.locks.AbstractQueuedSynchronizer$Node*
 (make-class-def
      '(class "java.util.concurrent.locks.AbstractQueuedSynchronizer$Node"
            "java.lang.Object"
            (constant_pool
                        (INT 1)
                        (INT -1)
                        (INT -2)
                        (INT -3))
            (fields
                        (field "SHARED" (class "java.util.concurrent.locks.AbstractQueuedSynchronizer$Node") (accessflags  *class*  *final*  *static* ) -1)
                        (field "EXCLUSIVE" (class "java.util.concurrent.locks.AbstractQueuedSynchronizer$Node") (accessflags  *class*  *final*  *static* ) -1)
                        (field "CANCELLED" int (accessflags  *class*  *final*  *static* ) 0)
                        (field "SIGNAL" int (accessflags  *class*  *final*  *static* ) 1)
                        (field "CONDITION" int (accessflags  *class*  *final*  *static* ) 2)
                        (field "PROPAGATE" int (accessflags  *class*  *final*  *static* ) 3)
                        (field "waitStatus" int (accessflags  *class*  *volatile* ) -1)
                        (field "prev" (class "java.util.concurrent.locks.AbstractQueuedSynchronizer$Node") (accessflags  *class*  *volatile* ) -1)
                        (field "next" (class "java.util.concurrent.locks.AbstractQueuedSynchronizer$Node") (accessflags  *class*  *volatile* ) -1)
                        (field "thread" (class "java.lang.Thread") (accessflags  *class*  *volatile* ) -1)
                        (field "nextWaiter" (class "java.util.concurrent.locks.AbstractQueuedSynchronizer$Node") (accessflags  *class* ) -1))
            (methods
                        (method "isShared"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *final* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 16)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "nextWaiter" "java.util.concurrent.locks.AbstractQueuedSynchronizer$Node" (class "java.util.concurrent.locks.AbstractQueuedSynchronizer$Node")))) 
                                      (4 (getstatic (fieldCP "SHARED" "java.util.concurrent.locks.AbstractQueuedSynchronizer$Node" (class "java.util.concurrent.locks.AbstractQueuedSynchronizer$Node")))) 
                                      (7 (if_acmpne 14))  ;;to TAG_0
                                      (10 (iconst_1)) 
                                      (11 (goto 15)) ;;to TAG_1
                                      (14 (iconst_0)) ;;at TAG_0
                                      (15 (ireturn)) ;;at TAG_1
                                      (endofcode 16))
                                   (Exceptions )
                                   (StackMap )))
                        (method "predecessor"
                              (parameters )
                              (returntype . (class "java.util.concurrent.locks.AbstractQueuedSynchronizer$Node"))
                              (accessflags  *class*  *final* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 19)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "prev" "java.util.concurrent.locks.AbstractQueuedSynchronizer$Node" (class "java.util.concurrent.locks.AbstractQueuedSynchronizer$Node")))) 
                                      (4 (astore_1)) 
                                      (5 (aload_1)) 
                                      (6 (ifnonnull 17))  ;;to TAG_0
                                      (9 (new (class "java.lang.NullPointerException"))) 
                                      (12 (dup)) 
                                      (13 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" () void))) 
                                      (16 (athrow)) 
                                      (17 (aload_1)) ;;at TAG_0
                                      (18 (areturn)) 
                                      (endofcode 19))
                                   (Exceptions )
                                   (StackMap )))
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
                        (method "<init>"
                              (parameters (class "java.lang.Thread") (class "java.util.concurrent.locks.AbstractQueuedSynchronizer$Node"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (aload_2))
                                      (6 (putfield (fieldCP "nextWaiter" "java.util.concurrent.locks.AbstractQueuedSynchronizer$Node" (class "java.util.concurrent.locks.AbstractQueuedSynchronizer$Node"))))
                                      (9 (aload_0))
                                      (10 (aload_1))
                                      (11 (putfield (fieldCP "thread" "java.util.concurrent.locks.AbstractQueuedSynchronizer$Node" (class "java.lang.Thread"))))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.Thread") int)
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (iload_2))
                                      (6 (putfield (fieldCP "waitStatus" "java.util.concurrent.locks.AbstractQueuedSynchronizer$Node" int)))
                                      (9 (aload_0))
                                      (10 (aload_1))
                                      (11 (putfield (fieldCP "thread" "java.util.concurrent.locks.AbstractQueuedSynchronizer$Node" (class "java.lang.Thread"))))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 15)
                                   (parsedcode
                                      (0 (new (class "java.util.concurrent.locks.AbstractQueuedSynchronizer$Node")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.util.concurrent.locks.AbstractQueuedSynchronizer$Node" () void)))
                                      (7 (putstatic (fieldCP "SHARED" "java.util.concurrent.locks.AbstractQueuedSynchronizer$Node" (class "java.util.concurrent.locks.AbstractQueuedSynchronizer$Node"))))
                                      (10 (aconst_null))
                                      (11 (putstatic (fieldCP "EXCLUSIVE" "java.util.concurrent.locks.AbstractQueuedSynchronizer$Node" (class "java.util.concurrent.locks.AbstractQueuedSynchronizer$Node"))))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *AbstractQueuedSynchronizer$Node-class-table*
  (make-static-class-decls 
   *java.util.concurrent.locks.AbstractQueuedSynchronizer$Node*))

(defconst *package-name-map* 
  ("java.util.concurrent.locks.AbstractQueuedSynchronizer$Node" . "java.util.concurrent.locks"))

