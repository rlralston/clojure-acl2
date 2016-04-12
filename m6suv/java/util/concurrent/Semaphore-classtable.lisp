; Semaphore-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:44 CDT 2014.
;

(defconst *java.util.concurrent.Semaphore*
 (make-class-def
      '(class "java.util.concurrent.Semaphore"
            "java.lang.Object"
            (constant_pool
                        (LONG -3222578661600680210)
                        (STRING  "[Permits = ")
                        (STRING  "]"))
            (fields
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0)
                        (field "sync" (class "java.util.concurrent.Semaphore$Sync") (accessflags  *class*  *final*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 17)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (new (class "java.util.concurrent.Semaphore$NonfairSync")))
                                      (8 (dup))
                                      (9 (iload_1))
                                      (10 (invokespecial
					(methodCP "<init>" "java.util.concurrent.Semaphore$NonfairSync" (int) void)))
                                      (13 (putfield (fieldCP "sync" "java.util.concurrent.Semaphore" (class "java.util.concurrent.Semaphore$Sync"))))
                                      (16 (return))
                                      (endofcode 17))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters int boolean)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 32)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokespecial (methodCP "<init>" "java.lang.Object" () void))) 
                                      (4 (aload_0)) 
                                      (5 (iload_2)) 
                                      (6 (ifeq 20))  ;;to TAG_0
                                      (9 (new (class "java.util.concurrent.Semaphore$FairSync"))) 
                                      (12 (dup)) 
                                      (13 (iload_1)) 
                                      (14 (invokespecial (methodCP "<init>" "java.util.concurrent.Semaphore$FairSync" (int) void))) 
                                      (17 (goto 28)) ;;to TAG_1
                                      (20 (new (class "java.util.concurrent.Semaphore$NonfairSync"))) ;;at TAG_0
                                      (23 (dup)) 
                                      (24 (iload_1)) 
                                      (25 (invokespecial (methodCP "<init>" "java.util.concurrent.Semaphore$NonfairSync" (int) void))) 
                                      (28 (putfield (fieldCP "sync" "java.util.concurrent.Semaphore" (class "java.util.concurrent.Semaphore$Sync")))) ;;at TAG_1
                                      (31 (return)) 
                                      (endofcode 32))
                                   (Exceptions )
                                   (StackMap )))
                        (method "acquire"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "sync" "java.util.concurrent.Semaphore" (class "java.util.concurrent.Semaphore$Sync"))))
                                      (4 (iconst_1))
                                      (5 (invokevirtual
					(methodCP "acquireSharedInterruptibly" "java.util.concurrent.Semaphore$Sync" (int) void)))
                                      (8 (return))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "acquireUninterruptibly"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "sync" "java.util.concurrent.Semaphore" (class "java.util.concurrent.Semaphore$Sync"))))
                                      (4 (iconst_1))
                                      (5 (invokevirtual
					(methodCP "acquireShared" "java.util.concurrent.Semaphore$Sync" (int) void)))
                                      (8 (return))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "tryAcquire"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 17)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "sync" "java.util.concurrent.Semaphore" (class "java.util.concurrent.Semaphore$Sync")))) 
                                      (4 (iconst_1)) 
                                      (5 (invokevirtual (methodCP "nonfairTryAcquireShared" "java.util.concurrent.Semaphore$Sync" (int) int))) 
                                      (8 (iflt 15))  ;;to TAG_0
                                      (11 (iconst_1)) 
                                      (12 (goto 16)) ;;to TAG_1
                                      (15 (iconst_0)) ;;at TAG_0
                                      (16 (ireturn)) ;;at TAG_1
                                      (endofcode 17))
                                   (Exceptions )
                                   (StackMap )))
                        (method "tryAcquire"
                              (parameters long (class "java.util.concurrent.TimeUnit"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 4) (code_length . 14)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "sync" "java.util.concurrent.Semaphore" (class "java.util.concurrent.Semaphore$Sync"))))
                                      (4 (iconst_1))
                                      (5 (aload_3))
                                      (6 (lload_1))
                                      (7 (invokevirtual
					(methodCP "toNanos" "java.util.concurrent.TimeUnit" (long) long)))
                                      (10 (invokevirtual
					(methodCP "tryAcquireSharedNanos" "java.util.concurrent.Semaphore$Sync" (int long) boolean)))
                                      (13 (ireturn))
                                      (endofcode 14))
                                   (Exceptions )
                                   (StackMap )))
                        (method "release"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "sync" "java.util.concurrent.Semaphore" (class "java.util.concurrent.Semaphore$Sync"))))
                                      (4 (iconst_1))
                                      (5 (invokevirtual
					(methodCP "releaseShared" "java.util.concurrent.Semaphore$Sync" (int) boolean)))
                                      (8 (pop))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "acquire"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 21)
                                   (parsedcode
                                      (0 (iload_1)) 
                                      (1 (ifge 12))  ;;to TAG_0
                                      (4 (new (class "java.lang.IllegalArgumentException"))) 
                                      (7 (dup)) 
                                      (8 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" () void))) 
                                      (11 (athrow)) 
                                      (12 (aload_0)) ;;at TAG_0
                                      (13 (getfield (fieldCP "sync" "java.util.concurrent.Semaphore" (class "java.util.concurrent.Semaphore$Sync")))) 
                                      (16 (iload_1)) 
                                      (17 (invokevirtual (methodCP "acquireSharedInterruptibly" "java.util.concurrent.Semaphore$Sync" (int) void))) 
                                      (20 (return)) 
                                      (endofcode 21))
                                   (Exceptions )
                                   (StackMap )))
                        (method "acquireUninterruptibly"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 21)
                                   (parsedcode
                                      (0 (iload_1)) 
                                      (1 (ifge 12))  ;;to TAG_0
                                      (4 (new (class "java.lang.IllegalArgumentException"))) 
                                      (7 (dup)) 
                                      (8 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" () void))) 
                                      (11 (athrow)) 
                                      (12 (aload_0)) ;;at TAG_0
                                      (13 (getfield (fieldCP "sync" "java.util.concurrent.Semaphore" (class "java.util.concurrent.Semaphore$Sync")))) 
                                      (16 (iload_1)) 
                                      (17 (invokevirtual (methodCP "acquireShared" "java.util.concurrent.Semaphore$Sync" (int) void))) 
                                      (20 (return)) 
                                      (endofcode 21))
                                   (Exceptions )
                                   (StackMap )))
                        (method "tryAcquire"
                              (parameters int)
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 29)
                                   (parsedcode
                                      (0 (iload_1)) 
                                      (1 (ifge 12)) ;;to TAG_0
                                      (4 (new (class "java.lang.IllegalArgumentException"))) 
                                      (7 (dup)) 
                                      (8 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" () void))) 
                                      (11 (athrow)) 
                                      (12 (aload_0)) ;;at TAG_0
                                      (13 (getfield (fieldCP "sync" "java.util.concurrent.Semaphore" (class "java.util.concurrent.Semaphore$Sync")))) 
                                      (16 (iload_1)) 
                                      (17 (invokevirtual (methodCP "nonfairTryAcquireShared" "java.util.concurrent.Semaphore$Sync" (int) int))) 
                                      (20 (iflt 27)) ;;to TAG_1
                                      (23 (iconst_1)) 
                                      (24 (goto 28))  ;;to TAG_2
                                      (27 (iconst_0)) ;;at TAG_1
                                      (28 (ireturn)) ;;at TAG_2
                                      (endofcode 29))
                                   (Exceptions )
                                   (StackMap )))
                        (method "tryAcquire"
                              (parameters int long (class "java.util.concurrent.TimeUnit"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 5) (code_length . 27)
                                   (parsedcode
                                      (0 (iload_1)) 
                                      (1 (ifge 12))  ;;to TAG_0
                                      (4 (new (class "java.lang.IllegalArgumentException"))) 
                                      (7 (dup)) 
                                      (8 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" () void))) 
                                      (11 (athrow)) 
                                      (12 (aload_0)) ;;at TAG_0
                                      (13 (getfield (fieldCP "sync" "java.util.concurrent.Semaphore" (class "java.util.concurrent.Semaphore$Sync")))) 
                                      (16 (iload_1)) 
                                      (17 (aload 4)) 
                                      (19 (lload_2)) 
                                      (20 (invokevirtual (methodCP "toNanos" "java.util.concurrent.TimeUnit" (long) long))) 
                                      (23 (invokevirtual (methodCP "tryAcquireSharedNanos" "java.util.concurrent.Semaphore$Sync" (int long) boolean))) 
                                      (26 (ireturn)) 
                                      (endofcode 27))
                                   (Exceptions )
                                   (StackMap )))
                        (method "release"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 22)
                                   (parsedcode
                                      (0 (iload_1)) 
                                      (1 (ifge 12))  ;;to TAG_0
                                      (4 (new (class "java.lang.IllegalArgumentException"))) 
                                      (7 (dup)) 
                                      (8 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" () void))) 
                                      (11 (athrow)) 
                                      (12 (aload_0)) ;;at TAG_0
                                      (13 (getfield (fieldCP "sync" "java.util.concurrent.Semaphore" (class "java.util.concurrent.Semaphore$Sync")))) 
                                      (16 (iload_1)) 
                                      (17 (invokevirtual (methodCP "releaseShared" "java.util.concurrent.Semaphore$Sync" (int) boolean))) 
                                      (20 (pop)) 
                                      (21 (return)) 
                                      (endofcode 22))
                                   (Exceptions )
                                   (StackMap )))
                        (method "availablePermits"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "sync" "java.util.concurrent.Semaphore" (class "java.util.concurrent.Semaphore$Sync"))))
                                      (4 (invokevirtual
					(methodCP "getPermits" "java.util.concurrent.Semaphore$Sync" () int)))
                                      (7 (ireturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "drainPermits"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "sync" "java.util.concurrent.Semaphore" (class "java.util.concurrent.Semaphore$Sync"))))
                                      (4 (invokevirtual
					(methodCP "drainPermits" "java.util.concurrent.Semaphore$Sync" () int)))
                                      (7 (ireturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "reducePermits"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 21)
                                   (parsedcode
                                      (0 (iload_1)) 
                                      (1 (ifge 12))  ;;to TAG_0
                                      (4 (new (class "java.lang.IllegalArgumentException"))) 
                                      (7 (dup)) 
                                      (8 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" () void))) 
                                      (11 (athrow)) 
                                      (12 (aload_0)) ;;at TAG_0
                                      (13 (getfield (fieldCP "sync" "java.util.concurrent.Semaphore" (class "java.util.concurrent.Semaphore$Sync")))) 
                                      (16 (iload_1)) 
                                      (17 (invokevirtual (methodCP "reducePermits" "java.util.concurrent.Semaphore$Sync" (int) void))) 
                                      (20 (return)) 
                                      (endofcode 21))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isFair"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "sync" "java.util.concurrent.Semaphore" (class "java.util.concurrent.Semaphore$Sync"))))
                                      (4 (instanceof (class "java.util.concurrent.Semaphore$FairSync")))
                                      (7 (ireturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hasQueuedThreads"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "sync" "java.util.concurrent.Semaphore" (class "java.util.concurrent.Semaphore$Sync"))))
                                      (4 (invokevirtual
					(methodCP "hasQueuedThreads" "java.util.concurrent.Semaphore$Sync" () boolean)))
                                      (7 (ireturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getQueueLength"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "sync" "java.util.concurrent.Semaphore" (class "java.util.concurrent.Semaphore$Sync"))))
                                      (4 (invokevirtual
					(methodCP "getQueueLength" "java.util.concurrent.Semaphore$Sync" () int)))
                                      (7 (ireturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getQueuedThreads"
                              (parameters )
                              (returntype . (class "java.util.Collection"))
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "sync" "java.util.concurrent.Semaphore" (class "java.util.concurrent.Semaphore$Sync"))))
                                      (4 (invokevirtual
					(methodCP "getQueuedThreads" "java.util.concurrent.Semaphore$Sync" () (class "java.util.Collection"))))
                                      (7 (areturn))
                                      (endofcode 8))
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
                                      (14 (ldc 1))        ;;STRING:: "[Permits = "
                                      (16 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (19 (aload_0))
                                      (20 (getfield (fieldCP "sync" "java.util.concurrent.Semaphore" (class "java.util.concurrent.Semaphore$Sync"))))
                                      (23 (invokevirtual
					(methodCP "getPermits" "java.util.concurrent.Semaphore$Sync" () int)))
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
            (interfaces "java.io.Serializable")
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *Semaphore-class-table*
  (make-static-class-decls 
   *java.util.concurrent.Semaphore*))

(defconst *package-name-map* 
  ("java.util.concurrent.Semaphore" . "java.util.concurrent"))
