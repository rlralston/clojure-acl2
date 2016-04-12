; Timer-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:48 CDT 2014.
;

(defconst *java.util.Timer*
 (make-class-def
      '(class "java.util.Timer"
            "java.lang.Object"
            (constant_pool
                        (STRING  "Timer-")
                        (STRING  "Negative delay.")
                        (STRING  "Non-positive period.")
                        (STRING  "Illegal execution time.")
                        (LONG 4611686018427387903)
                        (STRING  "Timer already cancelled.")
                        (STRING  "Task already scheduled or cancelled"))
            (fields
                        (field "queue" (class "java.util.TaskQueue") (accessflags  *class*  *final*  *private* ) -1)
                        (field "thread" (class "java.util.TimerThread") (accessflags  *class*  *final*  *private* ) -1)
                        (field "threadReaper" (class "java.lang.Object") (accessflags  *class*  *final*  *private* ) -1)
                        (field "nextSerialNumber" (class "java.util.concurrent.atomic.AtomicInteger") (accessflags  *class*  *final*  *private*  *static* ) -1))
            (methods
                        (method "serialNumber"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *private*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 0) (code_length . 7)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "nextSerialNumber" "java.util.Timer" (class "java.util.concurrent.atomic.AtomicInteger"))))
                                      (3 (invokevirtual
					(methodCP "getAndIncrement" "java.util.concurrent.atomic.AtomicInteger" () int)))
                                      (6 (ireturn))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 26)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (new (class "java.lang.StringBuilder")))
                                      (4 (dup))
                                      (5 (invokespecial
					(methodCP "<init>" "java.lang.StringBuilder" () void)))
                                      (8 (ldc 0))         ;;STRING:: "Timer-"
                                      (10 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (13 (invokestatic
					(methodCP "serialNumber" "java.util.Timer" () int)))
                                      (16 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" (int) (class "java.lang.StringBuilder"))))
                                      (19 (invokevirtual
					(methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String"))))
                                      (22 (invokespecial
					(methodCP "<init>" "java.util.Timer" ((class "java.lang.String")) void)))
                                      (25 (return))
                                      (endofcode 26))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters boolean)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 27)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (new (class "java.lang.StringBuilder")))
                                      (4 (dup))
                                      (5 (invokespecial
					(methodCP "<init>" "java.lang.StringBuilder" () void)))
                                      (8 (ldc 0))         ;;STRING:: "Timer-"
                                      (10 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (13 (invokestatic
					(methodCP "serialNumber" "java.util.Timer" () int)))
                                      (16 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" (int) (class "java.lang.StringBuilder"))))
                                      (19 (invokevirtual
					(methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String"))))
                                      (22 (iload_1))
                                      (23 (invokespecial
					(methodCP "<init>" "java.util.Timer" ((class "java.lang.String") boolean) void)))
                                      (26 (return))
                                      (endofcode 27))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 58)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (new (class "java.util.TaskQueue")))
                                      (8 (dup))
                                      (9 (invokespecial
					(methodCP "<init>" "java.util.TaskQueue" () void)))
                                      (12 (putfield (fieldCP "queue" "java.util.Timer" (class "java.util.TaskQueue"))))
                                      (15 (aload_0))
                                      (16 (new (class "java.util.TimerThread")))
                                      (19 (dup))
                                      (20 (aload_0))
                                      (21 (getfield (fieldCP "queue" "java.util.Timer" (class "java.util.TaskQueue"))))
                                      (24 (invokespecial
					(methodCP "<init>" "java.util.TimerThread" ((class "java.util.TaskQueue")) void)))
                                      (27 (putfield (fieldCP "thread" "java.util.Timer" (class "java.util.TimerThread"))))
                                      (30 (aload_0))
                                      (31 (new (class "java.util.Timer$1")))
                                      (34 (dup))
                                      (35 (aload_0))
                                      (36 (invokespecial
					(methodCP "<init>" "java.util.Timer$1" ((class "java.util.Timer")) void)))
                                      (39 (putfield (fieldCP "threadReaper" "java.util.Timer" (class "java.lang.Object"))))
                                      (42 (aload_0))
                                      (43 (getfield (fieldCP "thread" "java.util.Timer" (class "java.util.TimerThread"))))
                                      (46 (aload_1))
                                      (47 (invokevirtual
					(methodCP "setName" "java.util.TimerThread" ((class "java.lang.String")) void)))
                                      (50 (aload_0))
                                      (51 (getfield (fieldCP "thread" "java.util.Timer" (class "java.util.TimerThread"))))
                                      (54 (invokevirtual
					(methodCP "start" "java.util.TimerThread" () void)))
                                      (57 (return))
                                      (endofcode 58))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.String") boolean)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 66)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (new (class "java.util.TaskQueue")))
                                      (8 (dup))
                                      (9 (invokespecial
					(methodCP "<init>" "java.util.TaskQueue" () void)))
                                      (12 (putfield (fieldCP "queue" "java.util.Timer" (class "java.util.TaskQueue"))))
                                      (15 (aload_0))
                                      (16 (new (class "java.util.TimerThread")))
                                      (19 (dup))
                                      (20 (aload_0))
                                      (21 (getfield (fieldCP "queue" "java.util.Timer" (class "java.util.TaskQueue"))))
                                      (24 (invokespecial
					(methodCP "<init>" "java.util.TimerThread" ((class "java.util.TaskQueue")) void)))
                                      (27 (putfield (fieldCP "thread" "java.util.Timer" (class "java.util.TimerThread"))))
                                      (30 (aload_0))
                                      (31 (new (class "java.util.Timer$1")))
                                      (34 (dup))
                                      (35 (aload_0))
                                      (36 (invokespecial
					(methodCP "<init>" "java.util.Timer$1" ((class "java.util.Timer")) void)))
                                      (39 (putfield (fieldCP "threadReaper" "java.util.Timer" (class "java.lang.Object"))))
                                      (42 (aload_0))
                                      (43 (getfield (fieldCP "thread" "java.util.Timer" (class "java.util.TimerThread"))))
                                      (46 (aload_1))
                                      (47 (invokevirtual
					(methodCP "setName" "java.util.TimerThread" ((class "java.lang.String")) void)))
                                      (50 (aload_0))
                                      (51 (getfield (fieldCP "thread" "java.util.Timer" (class "java.util.TimerThread"))))
                                      (54 (iload_2))
                                      (55 (invokevirtual
					(methodCP "setDaemon" "java.util.TimerThread" (boolean) void)))
                                      (58 (aload_0))
                                      (59 (getfield (fieldCP "thread" "java.util.Timer" (class "java.util.TimerThread"))))
                                      (62 (invokevirtual
					(methodCP "start" "java.util.TimerThread" () void)))
                                      (65 (return))
                                      (endofcode 66))
                                   (Exceptions )
                                   (StackMap )))
                        (method "schedule"
                              (parameters (class "java.util.TimerTask") long)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 4) (code_length . 28)
                                   (parsedcode
                                      (0 (lload_2)) 
                                      (1 (lconst_0)) 
                                      (2 (lcmp)) 
                                      (3 (ifge 16))  ;;to TAG_0
                                      (6 (new (class "java.lang.IllegalArgumentException"))) 
                                      (9 (dup)) 
                                      (10 (ldc 1)) ;;STRING:: "Negative delay."
                                      (12 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (15 (athrow)) 
                                      (16 (aload_0)) ;;at TAG_0
                                      (17 (aload_1)) 
                                      (18 (invokestatic (methodCP "currentTimeMillis" "java.lang.System" () long))) 
                                      (21 (lload_2)) 
                                      (22 (ladd)) 
                                      (23 (lconst_0)) 
                                      (24 (invokespecial (methodCP "sched" "java.util.Timer" ((class "java.util.TimerTask") long long) void))) 
                                      (27 (return)) 
                                      (endofcode 28))
                                   (Exceptions )
                                   (StackMap )))
                        (method "schedule"
                              (parameters (class "java.util.TimerTask") (class "java.util.Date"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 3) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (aload_2))
                                      (3 (invokevirtual
					(methodCP "getTime" "java.util.Date" () long)))
                                      (6 (lconst_0))
                                      (7 (invokespecial
					(methodCP "sched" "java.util.Timer" ((class "java.util.TimerTask") long long) void)))
                                      (10 (return))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "schedule"
                              (parameters (class "java.util.TimerTask") long long)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 6) (code_length . 47)
                                   (parsedcode
                                      (0 (lload_2)) 
                                      (1 (lconst_0)) 
                                      (2 (lcmp)) 
                                      (3 (ifge 16))  ;;to TAG_0
                                      (6 (new (class "java.lang.IllegalArgumentException"))) 
                                      (9 (dup)) 
                                      (10 (ldc 1)) ;;STRING:: "Negative delay."
                                      (12 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (15 (athrow)) 
                                      (16 (lload 4)) ;;at TAG_0
                                      (18 (lconst_0)) 
                                      (19 (lcmp)) 
                                      (20 (ifgt 33)) ;;to TAG_1
                                      (23 (new (class "java.lang.IllegalArgumentException"))) 
                                      (26 (dup)) 
                                      (27 (ldc 2)) ;;STRING:: "Non-positive period."
                                      (29 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (32 (athrow)) 
                                      (33 (aload_0)) ;;at TAG_1
                                      (34 (aload_1)) 
                                      (35 (invokestatic (methodCP "currentTimeMillis" "java.lang.System" () long))) 
                                      (38 (lload_2)) 
                                      (39 (ladd)) 
                                      (40 (lload 4)) 
                                      (42 (lneg)) 
                                      (43 (invokespecial (methodCP "sched" "java.util.Timer" ((class "java.util.TimerTask") long long) void))) 
                                      (46 (return)) 
                                      (endofcode 47))
                                   (Exceptions )
                                   (StackMap )))
                        (method "schedule"
                              (parameters (class "java.util.TimerTask") (class "java.util.Date") long)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 5) (code_length . 28)
                                   (parsedcode
                                      (0 (lload_3)) 
                                      (1 (lconst_0)) 
                                      (2 (lcmp)) 
                                      (3 (ifgt 16))  ;;to TAG_0
                                      (6 (new (class "java.lang.IllegalArgumentException"))) 
                                      (9 (dup)) 
                                      (10 (ldc 2)) ;;STRING:: "Non-positive period."
                                      (12 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (15 (athrow)) 
                                      (16 (aload_0)) ;;at TAG_0
                                      (17 (aload_1)) 
                                      (18 (aload_2)) 
                                      (19 (invokevirtual (methodCP "getTime" "java.util.Date" () long))) 
                                      (22 (lload_3)) 
                                      (23 (lneg)) 
                                      (24 (invokespecial (methodCP "sched" "java.util.Timer" ((class "java.util.TimerTask") long long) void))) 
                                      (27 (return)) 
                                      (endofcode 28))
                                   (Exceptions )
                                   (StackMap )))
                        (method "scheduleAtFixedRate"
                              (parameters (class "java.util.TimerTask") long long)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 6) (code_length . 46)
                                   (parsedcode
                                      (0 (lload_2)) 
                                      (1 (lconst_0)) 
                                      (2 (lcmp)) 
                                      (3 (ifge 16))  ;;to TAG_0
                                      (6 (new (class "java.lang.IllegalArgumentException"))) 
                                      (9 (dup)) 
                                      (10 (ldc 1)) ;;STRING:: "Negative delay."
                                      (12 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (15 (athrow)) 
                                      (16 (lload 4)) ;;at TAG_0
                                      (18 (lconst_0)) 
                                      (19 (lcmp)) 
                                      (20 (ifgt 33)) ;;to TAG_1
                                      (23 (new (class "java.lang.IllegalArgumentException"))) 
                                      (26 (dup)) 
                                      (27 (ldc 2)) ;;STRING:: "Non-positive period."
                                      (29 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (32 (athrow)) 
                                      (33 (aload_0)) ;;at TAG_1
                                      (34 (aload_1)) 
                                      (35 (invokestatic (methodCP "currentTimeMillis" "java.lang.System" () long))) 
                                      (38 (lload_2)) 
                                      (39 (ladd)) 
                                      (40 (lload 4)) 
                                      (42 (invokespecial (methodCP "sched" "java.util.Timer" ((class "java.util.TimerTask") long long) void))) 
                                      (45 (return)) 
                                      (endofcode 46))
                                   (Exceptions )
                                   (StackMap )))
                        (method "scheduleAtFixedRate"
                              (parameters (class "java.util.TimerTask") (class "java.util.Date") long)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 5) (code_length . 27)
                                   (parsedcode
                                      (0 (lload_3)) 
                                      (1 (lconst_0)) 
                                      (2 (lcmp)) 
                                      (3 (ifgt 16))  ;;to TAG_0
                                      (6 (new (class "java.lang.IllegalArgumentException"))) 
                                      (9 (dup)) 
                                      (10 (ldc 2)) ;;STRING:: "Non-positive period."
                                      (12 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (15 (athrow)) 
                                      (16 (aload_0)) ;;at TAG_0
                                      (17 (aload_1)) 
                                      (18 (aload_2)) 
                                      (19 (invokevirtual (methodCP "getTime" "java.util.Date" () long))) 
                                      (22 (lload_3)) 
                                      (23 (invokespecial (methodCP "sched" "java.util.Timer" ((class "java.util.TimerTask") long long) void))) 
                                      (26 (return)) 
                                      (endofcode 27))
                                   (Exceptions )
                                   (StackMap )))
                        (method "sched"
                              (parameters (class "java.util.TimerTask") long long)
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 4) (max_locals . 10) (code_length . 158)
                                   (parsedcode
                                      (0 (lload_2)) 
                                      (1 (lconst_0)) 
                                      (2 (lcmp)) 
                                      (3 (ifge 16)) ;;to TAG_0
                                      (6 (new (class "java.lang.IllegalArgumentException"))) 
                                      (9 (dup)) 
                                      (10 (ldc 3)) ;;STRING:: "Illegal execution time."
                                      (12 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (15 (athrow)) 
                                      (16 (lload 4)) ;;at TAG_0
                                      (18 (invokestatic (methodCP "abs" "java.lang.Math" (long) long))) 
                                      (21 (ldc2_w 4)) ;; LONG:: "4611686018427387903"
                                      (24 (lcmp)) 
                                      (25 (ifle 34))  ;;to TAG_1
                                      (28 (lload 4)) 
                                      (30 (iconst_1)) 
                                      (31 (lshr)) 
                                      (32 (lstore 4)) 
                                      (34 (aload_0)) ;;at TAG_1
                                      (35 (getfield (fieldCP "queue" "java.util.Timer" (class "java.util.TaskQueue")))) 
                                      (38 (dup)) 
                                      (39 (astore 6)) 
                                      (41 (monitorenter)) 
                                      (42 (aload_0)) ;;at TAG_11
                                      (43 (getfield (fieldCP "thread" "java.util.Timer" (class "java.util.TimerThread")))) 
                                      (46 (getfield (fieldCP "newTasksMayBeScheduled" "java.util.TimerThread" boolean))) 
                                      (49 (ifne 62)) ;;to TAG_2
                                      (52 (new (class "java.lang.IllegalStateException"))) 
                                      (55 (dup)) 
                                      (56 (ldc 5)) ;;STRING:: "Timer already cancelled."
                                      (58 (invokespecial (methodCP "<init>" "java.lang.IllegalStateException" ((class "java.lang.String")) void))) 
                                      (61 (athrow)) 
                                      (62 (aload_1)) ;;at TAG_2
                                      (63 (getfield (fieldCP "lock" "java.util.TimerTask" (class "java.lang.Object")))) 
                                      (66 (dup)) 
                                      (67 (astore 7)) 
                                      (69 (monitorenter)) 
                                      (70 (aload_1)) ;;at TAG_7
                                      (71 (getfield (fieldCP "state" "java.util.TimerTask" int))) 
                                      (74 (ifeq 87)) ;;to TAG_3
                                      (77 (new (class "java.lang.IllegalStateException"))) 
                                      (80 (dup)) 
                                      (81 (ldc 6)) ;;STRING:: "Task already scheduled or cancelled"
                                      (83 (invokespecial (methodCP "<init>" "java.lang.IllegalStateException" ((class "java.lang.String")) void))) 
                                      (86 (athrow)) 
                                      (87 (aload_1)) ;;at TAG_3
                                      (88 (lload_2)) 
                                      (89 (putfield (fieldCP "nextExecutionTime" "java.util.TimerTask" long))) 
                                      (92 (aload_1)) 
                                      (93 (lload 4)) 
                                      (95 (putfield (fieldCP "period" "java.util.TimerTask" long))) 
                                      (98 (aload_1)) 
                                      (99 (iconst_1)) 
                                      (100 (putfield (fieldCP "state" "java.util.TimerTask" int))) 
                                      (103 (aload 7)) 
                                      (105 (monitorexit)) 
                                      (106 (goto 117)) ;;to TAG_4;;at TAG_8
                                      (109 (astore 8)) ;;at TAG_9
                                      (111 (aload 7)) 
                                      (113 (monitorexit)) 
                                      (114 (aload 8)) ;;at TAG_10
                                      (116 (athrow)) 
                                      (117 (aload_0)) ;;at TAG_4
                                      (118 (getfield (fieldCP "queue" "java.util.Timer" (class "java.util.TaskQueue")))) 
                                      (121 (aload_1)) 
                                      (122 (invokevirtual (methodCP "add" "java.util.TaskQueue" ((class "java.util.TimerTask")) void))) 
                                      (125 (aload_0)) 
                                      (126 (getfield (fieldCP "queue" "java.util.Timer" (class "java.util.TaskQueue")))) 
                                      (129 (invokevirtual (methodCP "getMin" "java.util.TaskQueue" () (class "java.util.TimerTask")))) 
                                      (132 (aload_1)) 
                                      (133 (if_acmpne 143)) ;;to TAG_5
                                      (136 (aload_0)) 
                                      (137 (getfield (fieldCP "queue" "java.util.Timer" (class "java.util.TaskQueue")))) 
                                      (140 (invokevirtual (methodCP "notify" "java.lang.Object" () void))) 
                                      (143 (aload 6)) ;;at TAG_5
                                      (145 (monitorexit)) 
                                      (146 (goto 157)) ;;to TAG_6;;at TAG_12
                                      (149 (astore 9)) ;;at TAG_13
                                      (151 (aload 6)) 
                                      (153 (monitorexit)) 
                                      (154 (aload 9)) ;;at TAG_14
                                      (156 (athrow)) 
                                      (157 (return)) ;;at TAG_6
                                      (endofcode 158))
                                   (Exceptions 
                                     (handler 70 106  109 (class "java.lang.Throwable"))
                                     (handler 109 114  109 (class "java.lang.Throwable"))
                                     (handler 42 146  149 (class "java.lang.Throwable"))
                                     (handler 149 154  149 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "cancel"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 40)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "queue" "java.util.Timer" (class "java.util.TaskQueue")))) 
                                      (4 (dup)) 
                                      (5 (astore_1)) 
                                      (6 (monitorenter)) 
                                      (7 (aload_0)) ;;at TAG_1
                                      (8 (getfield (fieldCP "thread" "java.util.Timer" (class "java.util.TimerThread")))) 
                                      (11 (iconst_0)) 
                                      (12 (putfield (fieldCP "newTasksMayBeScheduled" "java.util.TimerThread" boolean))) 
                                      (15 (aload_0)) 
                                      (16 (getfield (fieldCP "queue" "java.util.Timer" (class "java.util.TaskQueue")))) 
                                      (19 (invokevirtual (methodCP "clear" "java.util.TaskQueue" () void))) 
                                      (22 (aload_0)) 
                                      (23 (getfield (fieldCP "queue" "java.util.Timer" (class "java.util.TaskQueue")))) 
                                      (26 (invokevirtual (methodCP "notify" "java.lang.Object" () void))) 
                                      (29 (aload_1)) 
                                      (30 (monitorexit)) 
                                      (31 (goto 39)) ;;to TAG_0;;at TAG_2
                                      (34 (astore_2)) ;;at TAG_3
                                      (35 (aload_1)) 
                                      (36 (monitorexit)) 
                                      (37 (aload_2)) ;;at TAG_4
                                      (38 (athrow)) 
                                      (39 (return)) ;;at TAG_0
                                      (endofcode 40))
                                   (Exceptions 
                                     (handler 7 31  34 (class "java.lang.Throwable"))
                                     (handler 34 37  34 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "purge"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 5) (code_length . 78)
                                   (parsedcode
                                      (0 (iconst_0)) 
                                      (1 (istore_1)) 
                                      (2 (aload_0)) 
                                      (3 (getfield (fieldCP "queue" "java.util.Timer" (class "java.util.TaskQueue")))) 
                                      (6 (dup)) 
                                      (7 (astore_2)) 
                                      (8 (monitorenter)) 
                                      (9 (aload_0)) ;;at TAG_5
                                      (10 (getfield (fieldCP "queue" "java.util.Timer" (class "java.util.TaskQueue")))) 
                                      (13 (invokevirtual (methodCP "size" "java.util.TaskQueue" () int))) 
                                      (16 (istore_3)) 
                                      (17 (iload_3)) ;;at TAG_2
                                      (18 (ifle 53)) ;;to TAG_0
                                      (21 (aload_0)) 
                                      (22 (getfield (fieldCP "queue" "java.util.Timer" (class "java.util.TaskQueue")))) 
                                      (25 (iload_3)) 
                                      (26 (invokevirtual (methodCP "get" "java.util.TaskQueue" (int) (class "java.util.TimerTask")))) 
                                      (29 (getfield (fieldCP "state" "java.util.TimerTask" int))) 
                                      (32 (iconst_3)) 
                                      (33 (if_icmpne 47))  ;;to TAG_1
                                      (36 (aload_0)) 
                                      (37 (getfield (fieldCP "queue" "java.util.Timer" (class "java.util.TaskQueue")))) 
                                      (40 (iload_3)) 
                                      (41 (invokevirtual (methodCP "quickRemove" "java.util.TaskQueue" (int) void))) 
                                      (44 (iinc 1 1)) 
                                      (47 (iinc 3 -1)) ;;at TAG_1
                                      (50 (goto 17)) ;;to TAG_2
                                      (53 (iload_1)) ;;at TAG_0
                                      (54 (ifeq 64)) ;;to TAG_3
                                      (57 (aload_0)) 
                                      (58 (getfield (fieldCP "queue" "java.util.Timer" (class "java.util.TaskQueue")))) 
                                      (61 (invokevirtual (methodCP "heapify" "java.util.TaskQueue" () void))) 
                                      (64 (aload_2)) ;;at TAG_3
                                      (65 (monitorexit)) 
                                      (66 (goto 76)) ;;to TAG_4;;at TAG_6
                                      (69 (astore 4)) ;;at TAG_7
                                      (71 (aload_2)) 
                                      (72 (monitorexit)) 
                                      (73 (aload 4)) ;;at TAG_8
                                      (75 (athrow)) 
                                      (76 (iload_1)) ;;at TAG_4
                                      (77 (ireturn)) 
                                      (endofcode 78))
                                   (Exceptions 
                                     (handler 9 66  69 (class "java.lang.Throwable"))
                                     (handler 69 73  69 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "access$000"
                              (parameters (class "java.util.Timer"))
                              (returntype . (class "java.util.TaskQueue"))
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "queue" "java.util.Timer" (class "java.util.TaskQueue"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "access$100"
                              (parameters (class "java.util.Timer"))
                              (returntype . (class "java.util.TimerThread"))
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "thread" "java.util.Timer" (class "java.util.TimerThread"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 0) (code_length . 12)
                                   (parsedcode
                                      (0 (new (class "java.util.concurrent.atomic.AtomicInteger")))
                                      (3 (dup))
                                      (4 (iconst_0))
                                      (5 (invokespecial
					(methodCP "<init>" "java.util.concurrent.atomic.AtomicInteger" (int) void)))
                                      (8 (putstatic (fieldCP "nextSerialNumber" "java.util.Timer" (class "java.util.concurrent.atomic.AtomicInteger"))))
                                      (11 (return))
                                      (endofcode 12))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *Timer-class-table*
  (make-static-class-decls 
   *java.util.Timer*))

(defconst *package-name-map* 
  ("java.util.Timer" . "java.util"))

