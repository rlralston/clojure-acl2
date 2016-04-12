; CountDownLatch-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:43 CDT 2014.
;

(defconst *java.util.concurrent.CountDownLatch*
 (make-class-def
      '(class "java.util.concurrent.CountDownLatch"
            "java.lang.Object"
            (constant_pool
                        (STRING  "count < 0")
                        (STRING  "[Count = ")
                        (STRING  "]"))
            (fields
                        (field "sync" (class "java.util.concurrent.CountDownLatch$Sync") (accessflags  *class*  *final*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 31)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokespecial (methodCP "<init>" "java.lang.Object" () void))) 
                                      (4 (iload_1)) 
                                      (5 (ifge 18))  ;;to TAG_0
                                      (8 (new (class "java.lang.IllegalArgumentException"))) 
                                      (11 (dup)) 
                                      (12 (ldc 0)) ;;STRING:: "count < 0"
                                      (14 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (17 (athrow)) 
                                      (18 (aload_0)) ;;at TAG_0
                                      (19 (new (class "java.util.concurrent.CountDownLatch$Sync"))) 
                                      (22 (dup)) 
                                      (23 (iload_1)) 
                                      (24 (invokespecial (methodCP "<init>" "java.util.concurrent.CountDownLatch$Sync" (int) void))) 
                                      (27 (putfield (fieldCP "sync" "java.util.concurrent.CountDownLatch" (class "java.util.concurrent.CountDownLatch$Sync")))) 
                                      (30 (return)) 
                                      (endofcode 31))
                                   (Exceptions )
                                   (StackMap )))
                        (method "await"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "sync" "java.util.concurrent.CountDownLatch" (class "java.util.concurrent.CountDownLatch$Sync"))))
                                      (4 (iconst_1))
                                      (5 (invokevirtual
					(methodCP "acquireSharedInterruptibly" "java.util.concurrent.CountDownLatch$Sync" (int) void)))
                                      (8 (return))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "await"
                              (parameters long (class "java.util.concurrent.TimeUnit"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 4) (code_length . 14)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "sync" "java.util.concurrent.CountDownLatch" (class "java.util.concurrent.CountDownLatch$Sync"))))
                                      (4 (iconst_1))
                                      (5 (aload_3))
                                      (6 (lload_1))
                                      (7 (invokevirtual
					(methodCP "toNanos" "java.util.concurrent.TimeUnit" (long) long)))
                                      (10 (invokevirtual
					(methodCP "tryAcquireSharedNanos" "java.util.concurrent.CountDownLatch$Sync" (int long) boolean)))
                                      (13 (ireturn))
                                      (endofcode 14))
                                   (Exceptions )
                                   (StackMap )))
                        (method "countDown"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "sync" "java.util.concurrent.CountDownLatch" (class "java.util.concurrent.CountDownLatch$Sync"))))
                                      (4 (iconst_1))
                                      (5 (invokevirtual
					(methodCP "releaseShared" "java.util.concurrent.CountDownLatch$Sync" (int) boolean)))
                                      (8 (pop))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getCount"
                              (parameters )
                              (returntype . long)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "sync" "java.util.concurrent.CountDownLatch" (class "java.util.concurrent.CountDownLatch$Sync"))))
                                      (4 (invokevirtual
					(methodCP "getCount" "java.util.concurrent.CountDownLatch$Sync" () int)))
                                      (7 (i2l))
                                      (8 (lreturn))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "toString"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 38)
                                   (parsedcode
                                      (0 (new (class "java.lang.StringBuilder")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.lang.StringBuilder" () void)))
                                      (7 (aload_0))
                                      (8 (invokespecial
					(methodCP "toString" "java.lang.Object" () (class "java.lang.String"))))
                                      (11 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (14 (ldc 1))        ;;STRING:: "[Count = "
                                      (16 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (19 (aload_0))
                                      (20 (getfield (fieldCP "sync" "java.util.concurrent.CountDownLatch" (class "java.util.concurrent.CountDownLatch$Sync"))))
                                      (23 (invokevirtual
					(methodCP "getCount" "java.util.concurrent.CountDownLatch$Sync" () int)))
                                      (26 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" (int) (class "java.lang.StringBuilder"))))
                                      (29 (ldc 2))        ;;STRING:: "]"
                                      (31 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (34 (invokevirtual
					(methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String"))))
                                      (37 (areturn))
                                      (endofcode 38))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *CountDownLatch-class-table*
  (make-static-class-decls 
   *java.util.concurrent.CountDownLatch*))

(defconst *package-name-map* 
  ("java.util.concurrent.CountDownLatch" . "java.util.concurrent"))

