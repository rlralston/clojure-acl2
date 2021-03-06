; TimerThread-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:48 CDT 2014.
;

(defconst *java.util.TimerThread*
 (make-class-def
      '(class "java.util.TimerThread"
            "java.lang.Thread"
            (constant_pool)
            (fields
                        (field "newTasksMayBeScheduled" boolean (accessflags  *class* ) -1)
                        (field "queue" (class "java.util.TaskQueue") (accessflags  *class*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.TaskQueue"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Thread" () void)))
                                      (4 (aload_0))
                                      (5 (iconst_1))
                                      (6 (putfield (fieldCP "newTasksMayBeScheduled" "java.util.TimerThread" boolean)))
                                      (9 (aload_0))
                                      (10 (aload_1))
                                      (11 (putfield (fieldCP "queue" "java.util.TimerThread" (class "java.util.TaskQueue"))))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "run"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 6) (code_length . 74)
                                   (parsedcode
                                      (0 (aload_0)) ;;at TAG_7
                                      (1 (invokespecial (methodCP "mainLoop" "java.util.TimerThread" () void))) 
                                      (4 (aload_0)) ;;at TAG_8
                                      (5 (getfield (fieldCP "queue" "java.util.TimerThread" (class "java.util.TaskQueue")))) 
                                      (8 (dup)) 
                                      (9 (astore_1)) 
                                      (10 (monitorenter)) 
                                      (11 (aload_0)) ;;at TAG_3
                                      (12 (iconst_0)) 
                                      (13 (putfield (fieldCP "newTasksMayBeScheduled" "java.util.TimerThread" boolean))) 
                                      (16 (aload_0)) 
                                      (17 (getfield (fieldCP "queue" "java.util.TimerThread" (class "java.util.TaskQueue")))) 
                                      (20 (invokevirtual (methodCP "clear" "java.util.TaskQueue" () void))) 
                                      (23 (aload_1)) 
                                      (24 (monitorexit)) 
                                      (25 (goto 33)) ;;to TAG_0;;at TAG_4
                                      (28 (astore_2)) ;;at TAG_5
                                      (29 (aload_1)) 
                                      (30 (monitorexit)) 
                                      (31 (aload_2)) ;;at TAG_6
                                      (32 (athrow)) 
                                      (33 (goto 73)) ;;to TAG_1;;at TAG_0
                                      (36 (astore_3)) ;;at TAG_9
                                      (37 (aload_0)) ;;at TAG_14
                                      (38 (getfield (fieldCP "queue" "java.util.TimerThread" (class "java.util.TaskQueue")))) 
                                      (41 (dup)) 
                                      (42 (astore 4)) 
                                      (44 (monitorenter)) 
                                      (45 (aload_0)) ;;at TAG_10
                                      (46 (iconst_0)) 
                                      (47 (putfield (fieldCP "newTasksMayBeScheduled" "java.util.TimerThread" boolean))) 
                                      (50 (aload_0)) 
                                      (51 (getfield (fieldCP "queue" "java.util.TimerThread" (class "java.util.TaskQueue")))) 
                                      (54 (invokevirtual (methodCP "clear" "java.util.TaskQueue" () void))) 
                                      (57 (aload 4)) 
                                      (59 (monitorexit)) 
                                      (60 (goto 71)) ;;to TAG_2;;at TAG_11
                                      (63 (astore 5)) ;;at TAG_12
                                      (65 (aload 4)) 
                                      (67 (monitorexit)) 
                                      (68 (aload 5)) ;;at TAG_13
                                      (70 (athrow)) 
                                      (71 (aload_3)) ;;at TAG_2
                                      (72 (athrow)) 
                                      (73 (return)) ;;at TAG_1
                                      (endofcode 74))
                                   (Exceptions 
                                     (handler 11 25  28 (class "java.lang.Throwable"))
                                     (handler 28 31  28 (class "java.lang.Throwable"))
                                     (handler 0 4  36 (class "java.lang.Throwable"))
                                     (handler 45 60  63 (class "java.lang.Throwable"))
                                     (handler 63 68  63 (class "java.lang.Throwable"))
                                     (handler 36 37  36 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "mainLoop"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 5) (max_locals . 11) (code_length . 232)
                                   (parsedcode
                                      (0 (aload_0)) ;;at TAG_5
                                      (1 (getfield (fieldCP "queue" "java.util.TimerThread" (class "java.util.TaskQueue")))) 
                                      (4 (dup)) 
                                      (5 (astore_3)) 
                                      (6 (monitorenter)) 
                                      (7 (aload_0)) ;;at TAG_1
                                      (8 (getfield (fieldCP "queue" "java.util.TimerThread" (class "java.util.TaskQueue")))) 
                                      (11 (invokevirtual (methodCP "isEmpty" "java.util.TaskQueue" () boolean))) 
                                      (14 (ifeq 34)) ;;to TAG_0
                                      (17 (aload_0)) 
                                      (18 (getfield (fieldCP "newTasksMayBeScheduled" "java.util.TimerThread" boolean))) 
                                      (21 (ifeq 34)) ;;to TAG_0
                                      (24 (aload_0)) 
                                      (25 (getfield (fieldCP "queue" "java.util.TimerThread" (class "java.util.TaskQueue")))) 
                                      (28 (invokevirtual (methodCP "wait" "java.lang.Object" () void))) 
                                      (31 (goto 7)) ;;to TAG_1
                                      (34 (aload_0)) ;;at TAG_0
                                      (35 (getfield (fieldCP "queue" "java.util.TimerThread" (class "java.util.TaskQueue")))) 
                                      (38 (invokevirtual (methodCP "isEmpty" "java.util.TaskQueue" () boolean))) 
                                      (41 (ifeq 49)) ;;to TAG_2
                                      (44 (aload_3)) 
                                      (45 (monitorexit)) 
                                      (46 (goto 231)) ;;to TAG_3;;at TAG_21
                                      (49 (aload_0)) ;;at TAG_2
                                      (50 (getfield (fieldCP "queue" "java.util.TimerThread" (class "java.util.TaskQueue")))) 
                                      (53 (invokevirtual (methodCP "getMin" "java.util.TaskQueue" () (class "java.util.TimerTask")))) 
                                      (56 (astore_1)) 
                                      (57 (aload_1)) 
                                      (58 (getfield (fieldCP "lock" "java.util.TimerTask" (class "java.lang.Object")))) 
                                      (61 (dup)) 
                                      (62 (astore 8)) 
                                      (64 (monitorenter)) 
                                      (65 (aload_1)) ;;at TAG_16
                                      (66 (getfield (fieldCP "state" "java.util.TimerTask" int))) 
                                      (69 (iconst_3)) 
                                      (70 (if_icmpne 88)) ;;to TAG_4
                                      (73 (aload_0)) 
                                      (74 (getfield (fieldCP "queue" "java.util.TimerThread" (class "java.util.TaskQueue")))) 
                                      (77 (invokevirtual (methodCP "removeMin" "java.util.TaskQueue" () void))) 
                                      (80 (aload 8)) 
                                      (82 (monitorexit)) 
                                      (83 (aload_3)) ;;at TAG_17
                                      (84 (monitorexit)) 
                                      (85 (goto 0))  ;;to TAG_5;;at TAG_23
                                      (88 (invokestatic (methodCP "currentTimeMillis" "java.lang.System" () long))) ;;at TAG_4
                                      (91 (lstore 4)) 
                                      (93 (aload_1)) 
                                      (94 (getfield (fieldCP "nextExecutionTime" "java.util.TimerTask" long))) 
                                      (97 (lstore 6)) 
                                      (99 (lload 6)) 
                                      (101 (lload 4)) 
                                      (103 (lcmp)) 
                                      (104 (ifgt 111)) ;;to TAG_6
                                      (107 (iconst_1)) 
                                      (108 (goto 112)) ;;to TAG_7
                                      (111 (iconst_0)) ;;at TAG_6
                                      (112 (dup)) ;;at TAG_7
                                      (113 (istore_2)) 
                                      (114 (ifeq 174)) ;;to TAG_8
                                      (117 (aload_1)) 
                                      (118 (getfield (fieldCP "period" "java.util.TimerTask" long))) 
                                      (121 (lconst_0)) 
                                      (122 (lcmp)) 
                                      (123 (ifne 141)) ;;to TAG_9
                                      (126 (aload_0)) 
                                      (127 (getfield (fieldCP "queue" "java.util.TimerThread" (class "java.util.TaskQueue")))) 
                                      (130 (invokevirtual (methodCP "removeMin" "java.util.TaskQueue" () void))) 
                                      (133 (aload_1)) 
                                      (134 (iconst_2)) 
                                      (135 (putfield (fieldCP "state" "java.util.TimerTask" int))) 
                                      (138 (goto 174)) ;;to TAG_8
                                      (141 (aload_0)) ;;at TAG_9
                                      (142 (getfield (fieldCP "queue" "java.util.TimerThread" (class "java.util.TaskQueue")))) 
                                      (145 (aload_1)) 
                                      (146 (getfield (fieldCP "period" "java.util.TimerTask" long))) 
                                      (149 (lconst_0)) 
                                      (150 (lcmp)) 
                                      (151 (ifge 164)) ;;to TAG_10
                                      (154 (lload 4)) 
                                      (156 (aload_1)) 
                                      (157 (getfield (fieldCP "period" "java.util.TimerTask" long))) 
                                      (160 (lsub)) 
                                      (161 (goto 171)) ;;to TAG_11
                                      (164 (lload 6)) ;;at TAG_10
                                      (166 (aload_1)) 
                                      (167 (getfield (fieldCP "period" "java.util.TimerTask" long))) 
                                      (170 (ladd)) 
                                      (171 (invokevirtual (methodCP "rescheduleMin" "java.util.TaskQueue" (long) void))) ;;at TAG_11
                                      (174 (aload 8)) ;;at TAG_8
                                      (176 (monitorexit)) 
                                      (177 (goto 188)) ;;to TAG_12;;at TAG_19
                                      (180 (astore 9)) ;;at TAG_18
                                      (182 (aload 8)) 
                                      (184 (monitorexit)) 
                                      (185 (aload 9)) ;;at TAG_20
                                      (187 (athrow)) 
                                      (188 (iload_2)) ;;at TAG_12
                                      (189 (ifne 204)) ;;to TAG_13
                                      (192 (aload_0)) 
                                      (193 (getfield (fieldCP "queue" "java.util.TimerThread" (class "java.util.TaskQueue")))) 
                                      (196 (lload 6)) 
                                      (198 (lload 4)) 
                                      (200 (lsub)) 
                                      (201 (invokevirtual (methodCP "wait" "java.lang.Object" (long) void))) 
                                      (204 (aload_3)) ;;at TAG_13
                                      (205 (monitorexit)) 
                                      (206 (goto 216)) ;;to TAG_14;;at TAG_24
                                      (209 (astore 10)) ;;at TAG_22
                                      (211 (aload_3)) 
                                      (212 (monitorexit)) 
                                      (213 (aload 10)) ;;at TAG_25
                                      (215 (athrow)) 
                                      (216 (iload_2)) ;;at TAG_14
                                      (217 (ifeq 224)) ;;to TAG_15
                                      (220 (aload_1)) 
                                      (221 (invokevirtual (methodCP "run" "java.util.TimerTask" () void))) 
                                      (224 (goto 0))  ;;to TAG_5;;at TAG_15
                                      (227 (astore_1)) ;;at TAG_26
                                      (228 (goto 0))  ;;to TAG_5
                                      (231 (return)) ;;at TAG_3
                                      (endofcode 232))
                                   (Exceptions 
                                     (handler 65 83  180 (class "java.lang.Throwable"))
                                     (handler 88 177  180 (class "java.lang.Throwable"))
                                     (handler 180 185  180 (class "java.lang.Throwable"))
                                     (handler 7 46  209 (class "java.lang.Throwable"))
                                     (handler 49 85  209 (class "java.lang.Throwable"))
                                     (handler 88 206  209 (class "java.lang.Throwable"))
                                     (handler 209 213  209 (class "java.lang.Throwable"))
                                     (handler 0 46  227 (class "java.lang.InterruptedException"))
                                     (handler 49 85  227 (class "java.lang.InterruptedException"))
                                     (handler 88 224  227 (class "java.lang.InterruptedException")))
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *TimerThread-class-table*
  (make-static-class-decls 
   *java.util.TimerThread*))

(defconst *package-name-map* 
  ("java.util.TimerThread" . "java.util"))

