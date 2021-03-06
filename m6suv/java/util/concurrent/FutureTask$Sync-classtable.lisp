; FutureTask$Sync-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:44 CDT 2014.
;

(defconst *java.util.concurrent.FutureTask$Sync*
 (make-class-def
      '(class "java.util.concurrent.FutureTask$Sync"
            "java.util.concurrent.locks.AbstractQueuedSynchronizer"
            (constant_pool
                        (LONG -7828117401763700385)
                        (INT 0)
                        (INT 1)
                        (INT 2)
                        (INT 4))
            (fields
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0)
                        (field "READY" int (accessflags  *class*  *final*  *private*  *static* ) 1)
                        (field "RUNNING" int (accessflags  *class*  *final*  *private*  *static* ) 2)
                        (field "RAN" int (accessflags  *class*  *final*  *private*  *static* ) 3)
                        (field "CANCELLED" int (accessflags  *class*  *final*  *private*  *static* ) 4)
                        (field "callable" (class "java.util.concurrent.Callable") (accessflags  *class*  *final*  *private* ) -1)
                        (field "result" (class "java.lang.Object") (accessflags  *class*  *private* ) -1)
                        (field "exception" (class "java.lang.Throwable") (accessflags  *class*  *private* ) -1)
                        (field "runner" (class "java.lang.Thread") (accessflags  *class*  *private*  *volatile* ) -1)
                        (field "this$0" (class "java.util.concurrent.FutureTask") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.concurrent.FutureTask") (class "java.util.concurrent.Callable"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.util.concurrent.FutureTask$Sync" (class "java.util.concurrent.FutureTask"))))
                                      (5 (aload_0))
                                      (6 (invokespecial
					(methodCP "<init>" "java.util.concurrent.locks.AbstractQueuedSynchronizer" () void)))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "callable" "java.util.concurrent.FutureTask$Sync" (class "java.util.concurrent.Callable"))))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "ranOrCancelled"
                              (parameters int)
                              (returntype . boolean)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 13)
                                   (parsedcode
                                      (0 (iload_1)) 
                                      (1 (bipush 6)) 
                                      (3 (iand)) 
                                      (4 (ifeq 11))  ;;to TAG_0
                                      (7 (iconst_1)) 
                                      (8 (goto 12)) ;;to TAG_1
                                      (11 (iconst_0)) ;;at TAG_0
                                      (12 (ireturn)) ;;at TAG_1
                                      (endofcode 13))
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
                                      (1 (invokevirtual (methodCP "innerIsDone" "java.util.concurrent.FutureTask$Sync" () boolean))) 
                                      (4 (ifeq 11))  ;;to TAG_0
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
                                   (max_stack . 2) (max_locals . 2) (code_length . 7)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aconst_null))
                                      (2 (putfield (fieldCP "runner" "java.util.concurrent.FutureTask$Sync" (class "java.lang.Thread"))))
                                      (5 (iconst_1))
                                      (6 (ireturn))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap )))
                        (method "innerIsCancelled"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 14)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "getState" "java.util.concurrent.FutureTask$Sync" () int))) 
                                      (4 (iconst_4)) 
                                      (5 (if_icmpne 12))  ;;to TAG_0
                                      (8 (iconst_1)) 
                                      (9 (goto 13)) ;;to TAG_1
                                      (12 (iconst_0)) ;;at TAG_0
                                      (13 (ireturn)) ;;at TAG_1
                                      (endofcode 14))
                                   (Exceptions )
                                   (StackMap )))
                        (method "innerIsDone"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 24)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (aload_0)) 
                                      (2 (invokevirtual (methodCP "getState" "java.util.concurrent.FutureTask$Sync" () int))) 
                                      (5 (invokespecial (methodCP "ranOrCancelled" "java.util.concurrent.FutureTask$Sync" (int) boolean))) 
                                      (8 (ifeq 22))  ;;to TAG_0
                                      (11 (aload_0)) 
                                      (12 (getfield (fieldCP "runner" "java.util.concurrent.FutureTask$Sync" (class "java.lang.Thread")))) 
                                      (15 (ifnonnull 22))  ;;to TAG_0
                                      (18 (iconst_1)) 
                                      (19 (goto 23)) ;;to TAG_1
                                      (22 (iconst_0)) ;;at TAG_0
                                      (23 (ireturn)) ;;at TAG_1
                                      (endofcode 24))
                                   (Exceptions )
                                   (StackMap )))
                        (method "innerGet"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 45)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (iconst_0)) 
                                      (2 (invokevirtual (methodCP "acquireSharedInterruptibly" "java.util.concurrent.FutureTask$Sync" (int) void))) 
                                      (5 (aload_0)) 
                                      (6 (invokevirtual (methodCP "getState" "java.util.concurrent.FutureTask$Sync" () int))) 
                                      (9 (iconst_4)) 
                                      (10 (if_icmpne 21))  ;;to TAG_0
                                      (13 (new (class "java.util.concurrent.CancellationException"))) 
                                      (16 (dup)) 
                                      (17 (invokespecial (methodCP "<init>" "java.util.concurrent.CancellationException" () void))) 
                                      (20 (athrow)) 
                                      (21 (aload_0)) ;;at TAG_0
                                      (22 (getfield (fieldCP "exception" "java.util.concurrent.FutureTask$Sync" (class "java.lang.Throwable")))) 
                                      (25 (ifnull 40)) ;;to TAG_1
                                      (28 (new (class "java.util.concurrent.ExecutionException"))) 
                                      (31 (dup)) 
                                      (32 (aload_0)) 
                                      (33 (getfield (fieldCP "exception" "java.util.concurrent.FutureTask$Sync" (class "java.lang.Throwable")))) 
                                      (36 (invokespecial (methodCP "<init>" "java.util.concurrent.ExecutionException" ((class "java.lang.Throwable")) void))) 
                                      (39 (athrow)) 
                                      (40 (aload_0)) ;;at TAG_1
                                      (41 (getfield (fieldCP "result" "java.util.concurrent.FutureTask$Sync" (class "java.lang.Object")))) 
                                      (44 (areturn)) 
                                      (endofcode 45))
                                   (Exceptions )
                                   (StackMap )))
                        (method "innerGet"
                              (parameters long)
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 57)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (iconst_0)) 
                                      (2 (lload_1)) 
                                      (3 (invokevirtual (methodCP "tryAcquireSharedNanos" "java.util.concurrent.FutureTask$Sync" (int long) boolean))) 
                                      (6 (ifne 17)) ;;to TAG_0
                                      (9 (new (class "java.util.concurrent.TimeoutException"))) 
                                      (12 (dup)) 
                                      (13 (invokespecial (methodCP "<init>" "java.util.concurrent.TimeoutException" () void))) 
                                      (16 (athrow)) 
                                      (17 (aload_0)) ;;at TAG_0
                                      (18 (invokevirtual (methodCP "getState" "java.util.concurrent.FutureTask$Sync" () int))) 
                                      (21 (iconst_4)) 
                                      (22 (if_icmpne 33)) ;;to TAG_1
                                      (25 (new (class "java.util.concurrent.CancellationException"))) 
                                      (28 (dup)) 
                                      (29 (invokespecial (methodCP "<init>" "java.util.concurrent.CancellationException" () void))) 
                                      (32 (athrow)) 
                                      (33 (aload_0)) ;;at TAG_1
                                      (34 (getfield (fieldCP "exception" "java.util.concurrent.FutureTask$Sync" (class "java.lang.Throwable")))) 
                                      (37 (ifnull 52))  ;;to TAG_2
                                      (40 (new (class "java.util.concurrent.ExecutionException"))) 
                                      (43 (dup)) 
                                      (44 (aload_0)) 
                                      (45 (getfield (fieldCP "exception" "java.util.concurrent.FutureTask$Sync" (class "java.lang.Throwable")))) 
                                      (48 (invokespecial (methodCP "<init>" "java.util.concurrent.ExecutionException" ((class "java.lang.Throwable")) void))) 
                                      (51 (athrow)) 
                                      (52 (aload_0)) ;;at TAG_2
                                      (53 (getfield (fieldCP "result" "java.util.concurrent.FutureTask$Sync" (class "java.lang.Object")))) 
                                      (56 (areturn)) 
                                      (endofcode 57))
                                   (Exceptions )
                                   (StackMap )))
                        (method "innerSet"
                              (parameters (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 54)
                                   (parsedcode
                                      (0 (aload_0)) ;;at TAG_3
                                      (1 (invokevirtual (methodCP "getState" "java.util.concurrent.FutureTask$Sync" () int))) 
                                      (4 (istore_2)) 
                                      (5 (iload_2)) 
                                      (6 (iconst_2)) 
                                      (7 (if_icmpne 11)) ;;to TAG_0
                                      (10 (return)) 
                                      (11 (iload_2)) ;;at TAG_0
                                      (12 (iconst_4)) 
                                      (13 (if_icmpne 23)) ;;to TAG_1
                                      (16 (aload_0)) 
                                      (17 (iconst_0)) 
                                      (18 (invokevirtual (methodCP "releaseShared" "java.util.concurrent.FutureTask$Sync" (int) boolean))) 
                                      (21 (pop)) 
                                      (22 (return)) 
                                      (23 (aload_0)) ;;at TAG_1
                                      (24 (iload_2)) 
                                      (25 (iconst_2)) 
                                      (26 (invokevirtual (methodCP "compareAndSetState" "java.util.concurrent.FutureTask$Sync" (int int) boolean))) 
                                      (29 (ifeq 51))  ;;to TAG_2
                                      (32 (aload_0)) 
                                      (33 (aload_1)) 
                                      (34 (putfield (fieldCP "result" "java.util.concurrent.FutureTask$Sync" (class "java.lang.Object")))) 
                                      (37 (aload_0)) 
                                      (38 (iconst_0)) 
                                      (39 (invokevirtual (methodCP "releaseShared" "java.util.concurrent.FutureTask$Sync" (int) boolean))) 
                                      (42 (pop)) 
                                      (43 (aload_0)) 
                                      (44 (getfield (fieldCP "this$0" "java.util.concurrent.FutureTask$Sync" (class "java.util.concurrent.FutureTask")))) 
                                      (47 (invokevirtual (methodCP "done" "java.util.concurrent.FutureTask" () void))) 
                                      (50 (return)) 
                                      (51 (goto 0)) ;;to TAG_3;;at TAG_2
                                      (endofcode 54))
                                   (Exceptions )
                                   (StackMap )))
                        (method "innerSetException"
                              (parameters (class "java.lang.Throwable"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 54)
                                   (parsedcode
                                      (0 (aload_0)) ;;at TAG_3
                                      (1 (invokevirtual (methodCP "getState" "java.util.concurrent.FutureTask$Sync" () int))) 
                                      (4 (istore_2)) 
                                      (5 (iload_2)) 
                                      (6 (iconst_2)) 
                                      (7 (if_icmpne 11)) ;;to TAG_0
                                      (10 (return)) 
                                      (11 (iload_2)) ;;at TAG_0
                                      (12 (iconst_4)) 
                                      (13 (if_icmpne 23)) ;;to TAG_1
                                      (16 (aload_0)) 
                                      (17 (iconst_0)) 
                                      (18 (invokevirtual (methodCP "releaseShared" "java.util.concurrent.FutureTask$Sync" (int) boolean))) 
                                      (21 (pop)) 
                                      (22 (return)) 
                                      (23 (aload_0)) ;;at TAG_1
                                      (24 (iload_2)) 
                                      (25 (iconst_2)) 
                                      (26 (invokevirtual (methodCP "compareAndSetState" "java.util.concurrent.FutureTask$Sync" (int int) boolean))) 
                                      (29 (ifeq 51))  ;;to TAG_2
                                      (32 (aload_0)) 
                                      (33 (aload_1)) 
                                      (34 (putfield (fieldCP "exception" "java.util.concurrent.FutureTask$Sync" (class "java.lang.Throwable")))) 
                                      (37 (aload_0)) 
                                      (38 (iconst_0)) 
                                      (39 (invokevirtual (methodCP "releaseShared" "java.util.concurrent.FutureTask$Sync" (int) boolean))) 
                                      (42 (pop)) 
                                      (43 (aload_0)) 
                                      (44 (getfield (fieldCP "this$0" "java.util.concurrent.FutureTask$Sync" (class "java.util.concurrent.FutureTask")))) 
                                      (47 (invokevirtual (methodCP "done" "java.util.concurrent.FutureTask" () void))) 
                                      (50 (return)) 
                                      (51 (goto 0)) ;;to TAG_3;;at TAG_2
                                      (endofcode 54))
                                   (Exceptions )
                                   (StackMap )))
                        (method "innerCancel"
                              (parameters boolean)
                              (returntype . boolean)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 62)
                                   (parsedcode
                                      (0 (aload_0)) ;;at TAG_3
                                      (1 (invokevirtual (methodCP "getState" "java.util.concurrent.FutureTask$Sync" () int))) 
                                      (4 (istore_2)) 
                                      (5 (aload_0)) 
                                      (6 (iload_2)) 
                                      (7 (invokespecial (methodCP "ranOrCancelled" "java.util.concurrent.FutureTask$Sync" (int) boolean))) 
                                      (10 (ifeq 15)) ;;to TAG_0
                                      (13 (iconst_0)) 
                                      (14 (ireturn)) 
                                      (15 (aload_0)) ;;at TAG_0
                                      (16 (iload_2)) 
                                      (17 (iconst_4)) 
                                      (18 (invokevirtual (methodCP "compareAndSetState" "java.util.concurrent.FutureTask$Sync" (int int) boolean))) 
                                      (21 (ifeq 27)) ;;to TAG_1
                                      (24 (goto 30))  ;;to TAG_2
                                      (27 (goto 0)) ;;to TAG_3;;at TAG_1
                                      (30 (iload_1)) ;;at TAG_2
                                      (31 (ifeq 47)) ;;to TAG_4
                                      (34 (aload_0)) 
                                      (35 (getfield (fieldCP "runner" "java.util.concurrent.FutureTask$Sync" (class "java.lang.Thread")))) 
                                      (38 (astore_2)) 
                                      (39 (aload_2)) 
                                      (40 (ifnull 47)) ;;to TAG_4
                                      (43 (aload_2)) 
                                      (44 (invokevirtual (methodCP "interrupt" "java.lang.Thread" () void))) 
                                      (47 (aload_0)) ;;at TAG_4
                                      (48 (iconst_0)) 
                                      (49 (invokevirtual (methodCP "releaseShared" "java.util.concurrent.FutureTask$Sync" (int) boolean))) 
                                      (52 (pop)) 
                                      (53 (aload_0)) 
                                      (54 (getfield (fieldCP "this$0" "java.util.concurrent.FutureTask$Sync" (class "java.util.concurrent.FutureTask")))) 
                                      (57 (invokevirtual (methodCP "done" "java.util.concurrent.FutureTask" () void))) 
                                      (60 (iconst_1)) 
                                      (61 (ireturn)) 
                                      (endofcode 62))
                                   (Exceptions )
                                   (StackMap )))
                        (method "innerRun"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 66)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (iconst_0)) 
                                      (2 (iconst_1)) 
                                      (3 (invokevirtual (methodCP "compareAndSetState" "java.util.concurrent.FutureTask$Sync" (int int) boolean))) 
                                      (6 (ifne 10)) ;;to TAG_0
                                      (9 (return)) 
                                      (10 (aload_0)) ;;at TAG_0
                                      (11 (invokestatic (methodCP "currentThread" "java.lang.Thread" () (class "java.lang.Thread")))) 
                                      (14 (putfield (fieldCP "runner" "java.util.concurrent.FutureTask$Sync" (class "java.lang.Thread")))) 
                                      (17 (aload_0)) 
                                      (18 (invokevirtual (methodCP "getState" "java.util.concurrent.FutureTask$Sync" () int))) 
                                      (21 (iconst_1)) 
                                      (22 (if_icmpne 59)) ;;to TAG_1
                                      (25 (aload_0)) ;;at TAG_4
                                      (26 (getfield (fieldCP "callable" "java.util.concurrent.FutureTask$Sync" (class "java.util.concurrent.Callable")))) 
                                      (29 (invokeinterface (methodCP "call" "java.util.concurrent.Callable" () (class "java.lang.Object")) 1)) 
                                      (34 (astore_1)) 
                                      (35 (goto 48))  ;;to TAG_2;;at TAG_5
                                      (38 (astore_2)) ;;at TAG_6
                                      (39 (aload_0)) 
                                      (40 (getfield (fieldCP "this$0" "java.util.concurrent.FutureTask$Sync" (class "java.util.concurrent.FutureTask")))) 
                                      (43 (aload_2)) 
                                      (44 (invokevirtual (methodCP "setException" "java.util.concurrent.FutureTask" ((class "java.lang.Throwable")) void))) 
                                      (47 (return)) 
                                      (48 (aload_0)) ;;at TAG_2
                                      (49 (getfield (fieldCP "this$0" "java.util.concurrent.FutureTask$Sync" (class "java.util.concurrent.FutureTask")))) 
                                      (52 (aload_1)) 
                                      (53 (invokevirtual (methodCP "set" "java.util.concurrent.FutureTask" ((class "java.lang.Object")) void))) 
                                      (56 (goto 65)) ;;to TAG_3
                                      (59 (aload_0)) ;;at TAG_1
                                      (60 (iconst_0)) 
                                      (61 (invokevirtual (methodCP "releaseShared" "java.util.concurrent.FutureTask$Sync" (int) boolean))) 
                                      (64 (pop)) 
                                      (65 (return)) ;;at TAG_3
                                      (endofcode 66))
                                   (Exceptions 
                                     (handler 25 35  38 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "innerRunAndReset"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 59)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (iconst_0)) 
                                      (2 (iconst_1)) 
                                      (3 (invokevirtual (methodCP "compareAndSetState" "java.util.concurrent.FutureTask$Sync" (int int) boolean))) 
                                      (6 (ifne 11)) ;;to TAG_0
                                      (9 (iconst_0)) 
                                      (10 (ireturn)) 
                                      (11 (aload_0)) ;;at TAG_0
                                      (12 (invokestatic (methodCP "currentThread" "java.lang.Thread" () (class "java.lang.Thread")))) 
                                      (15 (putfield (fieldCP "runner" "java.util.concurrent.FutureTask$Sync" (class "java.lang.Thread")))) 
                                      (18 (aload_0)) 
                                      (19 (invokevirtual (methodCP "getState" "java.util.concurrent.FutureTask$Sync" () int))) 
                                      (22 (iconst_1)) 
                                      (23 (if_icmpne 36)) ;;to TAG_1
                                      (26 (aload_0)) 
                                      (27 (getfield (fieldCP "callable" "java.util.concurrent.FutureTask$Sync" (class "java.util.concurrent.Callable")))) 
                                      (30 (invokeinterface (methodCP "call" "java.util.concurrent.Callable" () (class "java.lang.Object")) 1)) 
                                      (35 (pop)) 
                                      (36 (aload_0)) ;;at TAG_1
                                      (37 (aconst_null)) 
                                      (38 (putfield (fieldCP "runner" "java.util.concurrent.FutureTask$Sync" (class "java.lang.Thread")))) 
                                      (41 (aload_0)) 
                                      (42 (iconst_1)) 
                                      (43 (iconst_0)) 
                                      (44 (invokevirtual (methodCP "compareAndSetState" "java.util.concurrent.FutureTask$Sync" (int int) boolean))) 
                                      (47 (ireturn)) ;;at TAG_2
                                      (48 (astore_1)) ;;at TAG_3
                                      (49 (aload_0)) 
                                      (50 (getfield (fieldCP "this$0" "java.util.concurrent.FutureTask$Sync" (class "java.util.concurrent.FutureTask")))) 
                                      (53 (aload_1)) 
                                      (54 (invokevirtual (methodCP "setException" "java.util.concurrent.FutureTask" ((class "java.lang.Throwable")) void))) 
                                      (57 (iconst_0)) 
                                      (58 (ireturn)) 
                                      (endofcode 59))
                                   (Exceptions 
                                     (handler 11 47  48 (class "java.lang.Throwable")))
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *FutureTask$Sync-class-table*
  (make-static-class-decls 
   *java.util.concurrent.FutureTask$Sync*))

(defconst *package-name-map* 
  ("java.util.concurrent.FutureTask$Sync" . "java.util.concurrent"))

