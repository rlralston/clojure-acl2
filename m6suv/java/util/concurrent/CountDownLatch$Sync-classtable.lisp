; CountDownLatch$Sync-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:43 CDT 2014.
;

(defconst *java.util.concurrent.CountDownLatch$Sync*
 (make-class-def
      '(class "java.util.concurrent.CountDownLatch$Sync"
            "java.util.concurrent.locks.AbstractQueuedSynchronizer"
            (constant_pool
                        (LONG 4982264981922014374))
            (fields
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0))
            (methods
                        (method "<init>"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.util.concurrent.locks.AbstractQueuedSynchronizer" () void)))
                                      (4 (aload_0))
                                      (5 (iload_1))
                                      (6 (invokevirtual
					(methodCP "setState" "java.util.concurrent.CountDownLatch$Sync" (int) void)))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getCount"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokevirtual
					(methodCP "getState" "java.util.concurrent.CountDownLatch$Sync" () int)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "tryAcquireShared"
                              (parameters int)
                              (returntype . int)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 13)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "getState" "java.util.concurrent.CountDownLatch$Sync" () int))) 
                                      (4 (ifne 11))  ;;to TAG_0
                                      (7 (iconst_1)) 
                                      (8 (goto 12)) ;;to TAG_1
                                      (11 (iconst_m1)) ;;at TAG_0
                                      (12 (ireturn)) ;;at TAG_1
                                      (endofcode 13))
                                   (Exceptions )
                                   (StackMap )))
                        (method "tryReleaseShared"
                              (parameters int)
                              (returntype . boolean)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 3) (max_locals . 4) (code_length . 37)
                                   (parsedcode
                                      (0 (aload_0)) ;;at TAG_4
                                      (1 (invokevirtual (methodCP "getState" "java.util.concurrent.CountDownLatch$Sync" () int))) 
                                      (4 (istore_2)) 
                                      (5 (iload_2)) 
                                      (6 (ifne 11)) ;;to TAG_0
                                      (9 (iconst_0)) 
                                      (10 (ireturn)) 
                                      (11 (iload_2)) ;;at TAG_0
                                      (12 (iconst_1)) 
                                      (13 (isub)) 
                                      (14 (istore_3)) 
                                      (15 (aload_0)) 
                                      (16 (iload_2)) 
                                      (17 (iload_3)) 
                                      (18 (invokevirtual (methodCP "compareAndSetState" "java.util.concurrent.CountDownLatch$Sync" (int int) boolean))) 
                                      (21 (ifeq 34)) ;;to TAG_1
                                      (24 (iload_3)) 
                                      (25 (ifne 32))  ;;to TAG_2
                                      (28 (iconst_1)) 
                                      (29 (goto 33)) ;;to TAG_3
                                      (32 (iconst_0)) ;;at TAG_2
                                      (33 (ireturn)) ;;at TAG_3
                                      (34 (goto 0)) ;;to TAG_4;;at TAG_1
                                      (endofcode 37))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *CountDownLatch$Sync-class-table*
  (make-static-class-decls 
   *java.util.concurrent.CountDownLatch$Sync*))

(defconst *package-name-map* 
  ("java.util.concurrent.CountDownLatch$Sync" . "java.util.concurrent"))
