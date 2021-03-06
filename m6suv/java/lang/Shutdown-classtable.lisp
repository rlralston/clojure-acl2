; Shutdown-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:36 CDT 2014.
;

(defconst *java.lang.Shutdown*
 (make-class-def
      '(class "java.lang.Shutdown"
            "java.lang.Object"
            (constant_pool
                        (INT 0)
                        (INT 1)
                        (INT 2)
                        (INT 10)
                        (STRING  "Shutdown hook at slot ")
                        (STRING  " already registered")
                        (STRING  "Shutdown in progress"))
            (fields
                        (field "RUNNING" int (accessflags  *class*  *final*  *private*  *static* ) 0)
                        (field "HOOKS" int (accessflags  *class*  *final*  *private*  *static* ) 1)
                        (field "FINALIZERS" int (accessflags  *class*  *final*  *private*  *static* ) 2)
                        (field "state" int (accessflags  *class*  *private*  *static* ) -1)
                        (field "runFinalizersOnExit" boolean (accessflags  *class*  *private*  *static* ) -1)
                        (field "MAX_SYSTEM_HOOKS" int (accessflags  *class*  *final*  *private*  *static* ) 3)
                        (field "hooks" (array (class "java.lang.Runnable")) (accessflags  *class*  *final*  *private*  *static* ) -1)
                        (field "currentRunningHook" int (accessflags  *class*  *private*  *static* ) -1)
                        (field "lock" (class "java.lang.Object") (accessflags  *class*  *private*  *static* ) -1)
                        (field "haltLock" (class "java.lang.Object") (accessflags  *class*  *private*  *static* ) -1))
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
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "setRunFinalizersOnExit"
                              (parameters boolean)
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 21)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "lock" "java.lang.Shutdown" (class "java.lang.Object")))) 
                                      (3 (dup)) 
                                      (4 (astore_1)) 
                                      (5 (monitorenter)) 
                                      (6 (iload_0)) ;;at TAG_1
                                      (7 (putstatic (fieldCP "runFinalizersOnExit" "java.lang.Shutdown" boolean))) 
                                      (10 (aload_1)) 
                                      (11 (monitorexit)) 
                                      (12 (goto 20)) ;;to TAG_0;;at TAG_2
                                      (15 (astore_2)) ;;at TAG_3
                                      (16 (aload_1)) 
                                      (17 (monitorexit)) 
                                      (18 (aload_2)) ;;at TAG_4
                                      (19 (athrow)) 
                                      (20 (return)) ;;at TAG_0
                                      (endofcode 21))
                                   (Exceptions 
                                     (handler 6 12  15 (class "java.lang.Throwable"))
                                     (handler 15 18  15 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "add"
                              (parameters int boolean (class "java.lang.Runnable"))
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 5) (code_length . 116)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "lock" "java.lang.Shutdown" (class "java.lang.Object")))) 
                                      (3 (dup)) 
                                      (4 (astore_3)) 
                                      (5 (monitorenter)) 
                                      (6 (getstatic (fieldCP "hooks" "java.lang.Shutdown" (array (class "java.lang.Runnable"))))) ;;at TAG_5
                                      (9 (iload_0)) 
                                      (10 (aaload)) 
                                      (11 (ifnull 46)) ;;to TAG_0
                                      (14 (new (class "java.lang.InternalError"))) 
                                      (17 (dup)) 
                                      (18 (new (class "java.lang.StringBuilder"))) 
                                      (21 (dup)) 
                                      (22 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (25 (ldc 4)) ;;STRING:: "Shutdown hook at slot "
                                      (27 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (30 (iload_0)) 
                                      (31 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (int) (class "java.lang.StringBuilder")))) 
                                      (34 (ldc 5)) ;;STRING:: " already registered"
                                      (36 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (39 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (42 (invokespecial (methodCP "<init>" "java.lang.InternalError" ((class "java.lang.String")) void))) 
                                      (45 (athrow)) 
                                      (46 (iload_1)) ;;at TAG_0
                                      (47 (ifne 66))  ;;to TAG_1
                                      (50 (getstatic (fieldCP "state" "java.lang.Shutdown" int))) 
                                      (53 (ifle 97)) ;;to TAG_2
                                      (56 (new (class "java.lang.IllegalStateException"))) 
                                      (59 (dup)) 
                                      (60 (ldc 6)) ;;STRING:: "Shutdown in progress"
                                      (62 (invokespecial (methodCP "<init>" "java.lang.IllegalStateException" ((class "java.lang.String")) void))) 
                                      (65 (athrow)) 
                                      (66 (getstatic (fieldCP "state" "java.lang.Shutdown" int))) ;;at TAG_1
                                      (69 (iconst_1)) 
                                      (70 (if_icmpgt 87)) ;;to TAG_3
                                      (73 (getstatic (fieldCP "state" "java.lang.Shutdown" int))) 
                                      (76 (iconst_1)) 
                                      (77 (if_icmpne 97)) ;;to TAG_2
                                      (80 (iload_0)) 
                                      (81 (getstatic (fieldCP "currentRunningHook" "java.lang.Shutdown" int))) 
                                      (84 (if_icmpgt 97)) ;;to TAG_2
                                      (87 (new (class "java.lang.IllegalStateException"))) ;;at TAG_3
                                      (90 (dup)) 
                                      (91 (ldc 6)) ;;STRING:: "Shutdown in progress"
                                      (93 (invokespecial (methodCP "<init>" "java.lang.IllegalStateException" ((class "java.lang.String")) void))) 
                                      (96 (athrow)) 
                                      (97 (getstatic (fieldCP "hooks" "java.lang.Shutdown" (array (class "java.lang.Runnable"))))) ;;at TAG_2
                                      (100 (iload_0)) 
                                      (101 (aload_2)) 
                                      (102 (aastore)) 
                                      (103 (aload_3)) 
                                      (104 (monitorexit)) 
                                      (105 (goto 115)) ;;to TAG_4;;at TAG_6
                                      (108 (astore 4)) ;;at TAG_7
                                      (110 (aload_3)) 
                                      (111 (monitorexit)) 
                                      (112 (aload 4)) ;;at TAG_8
                                      (114 (athrow)) 
                                      (115 (return)) ;;at TAG_4
                                      (endofcode 116))
                                   (Exceptions 
                                     (handler 6 105  108 (class "java.lang.Throwable"))
                                     (handler 108 112  108 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "runHooks"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *private*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 69)
                                   (parsedcode
                                      (0 (iconst_0)) 
                                      (1 (istore_0)) 
                                      (2 (iload_0)) ;;at TAG_4
                                      (3 (bipush 10)) 
                                      (5 (if_icmpge 68)) ;;to TAG_0
                                      (8 (getstatic (fieldCP "lock" "java.lang.Shutdown" (class "java.lang.Object")))) ;;at TAG_9
                                      (11 (dup)) 
                                      (12 (astore_2)) 
                                      (13 (monitorenter)) 
                                      (14 (iload_0)) ;;at TAG_5
                                      (15 (putstatic (fieldCP "currentRunningHook" "java.lang.Shutdown" int))) 
                                      (18 (getstatic (fieldCP "hooks" "java.lang.Shutdown" (array (class "java.lang.Runnable"))))) 
                                      (21 (iload_0)) 
                                      (22 (aaload)) 
                                      (23 (astore_1)) 
                                      (24 (aload_2)) 
                                      (25 (monitorexit)) 
                                      (26 (goto 34))  ;;to TAG_1;;at TAG_6
                                      (29 (astore_3)) ;;at TAG_7
                                      (30 (aload_2)) 
                                      (31 (monitorexit)) 
                                      (32 (aload_3)) ;;at TAG_8
                                      (33 (athrow)) 
                                      (34 (aload_1)) ;;at TAG_1
                                      (35 (ifnull 44)) ;;to TAG_2
                                      (38 (aload_1)) 
                                      (39 (invokeinterface (methodCP "run" "java.lang.Runnable" () void) 1)) 
                                      (44 (goto 62)) ;;to TAG_3;;at TAG_2
                                      (47 (astore_1)) ;;at TAG_10
                                      (48 (aload_1)) 
                                      (49 (instanceof (class "java.lang.ThreadDeath"))) 
                                      (52 (ifeq 62)) ;;to TAG_3
                                      (55 (aload_1)) 
                                      (56 (checkcast (class "java.lang.ThreadDeath"))) 
                                      (59 (astore_2)) 
                                      (60 (aload_2)) 
                                      (61 (athrow)) 
                                      (62 (iinc 0 1)) ;;at TAG_3
                                      (65 (goto 2)) ;;to TAG_4
                                      (68 (return)) ;;at TAG_0
                                      (endofcode 69))
                                   (Exceptions 
                                     (handler 14 26  29 (class "java.lang.Throwable"))
                                     (handler 29 32  29 (class "java.lang.Throwable"))
                                     (handler 8 44  47 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "halt"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 21)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "haltLock" "java.lang.Shutdown" (class "java.lang.Object")))) 
                                      (3 (dup)) 
                                      (4 (astore_1)) 
                                      (5 (monitorenter)) 
                                      (6 (iload_0)) ;;at TAG_1
                                      (7 (invokestatic (methodCP "halt0" "java.lang.Shutdown" (int) void))) 
                                      (10 (aload_1)) 
                                      (11 (monitorexit)) 
                                      (12 (goto 20)) ;;to TAG_0;;at TAG_2
                                      (15 (astore_2)) ;;at TAG_3
                                      (16 (aload_1)) 
                                      (17 (monitorexit)) 
                                      (18 (aload_2)) ;;at TAG_4
                                      (19 (athrow)) 
                                      (20 (return)) ;;at TAG_0
                                      (endofcode 21))
                                   (Exceptions 
                                     (handler 6 12  15 (class "java.lang.Throwable"))
                                     (handler 15 18  15 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "halt0"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *class*  *native*  *static* )
                              (code))
                        (method "runAllFinalizers"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *native*  *private*  *static* )
                              (code))
                        (method "sequence"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *private*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 61)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "lock" "java.lang.Shutdown" (class "java.lang.Object")))) 
                                      (3 (dup)) 
                                      (4 (astore_0)) 
                                      (5 (monitorenter)) 
                                      (6 (getstatic (fieldCP "state" "java.lang.Shutdown" int))) ;;at TAG_4
                                      (9 (iconst_1)) 
                                      (10 (if_icmpeq 16)) ;;to TAG_0
                                      (13 (aload_0)) 
                                      (14 (monitorexit)) 
                                      (15 (return)) ;;at TAG_5
                                      (16 (aload_0)) ;;at TAG_0
                                      (17 (monitorexit)) 
                                      (18 (goto 26))  ;;to TAG_1;;at TAG_7
                                      (21 (astore_1)) ;;at TAG_6
                                      (22 (aload_0)) 
                                      (23 (monitorexit)) 
                                      (24 (aload_1)) ;;at TAG_8
                                      (25 (athrow)) 
                                      (26 (invokestatic (methodCP "runHooks" "java.lang.Shutdown" () void))) ;;at TAG_1
                                      (29 (getstatic (fieldCP "lock" "java.lang.Shutdown" (class "java.lang.Object")))) 
                                      (32 (dup)) 
                                      (33 (astore_1)) 
                                      (34 (monitorenter)) 
                                      (35 (iconst_2)) ;;at TAG_9
                                      (36 (putstatic (fieldCP "state" "java.lang.Shutdown" int))) 
                                      (39 (getstatic (fieldCP "runFinalizersOnExit" "java.lang.Shutdown" boolean))) 
                                      (42 (istore_0)) 
                                      (43 (aload_1)) 
                                      (44 (monitorexit)) 
                                      (45 (goto 53)) ;;to TAG_2;;at TAG_10
                                      (48 (astore_2)) ;;at TAG_11
                                      (49 (aload_1)) 
                                      (50 (monitorexit)) 
                                      (51 (aload_2)) ;;at TAG_12
                                      (52 (athrow)) 
                                      (53 (iload_0)) ;;at TAG_2
                                      (54 (ifeq 60)) ;;to TAG_3
                                      (57 (invokestatic (methodCP "runAllFinalizers" "java.lang.Shutdown" () void))) 
                                      (60 (return)) ;;at TAG_3
                                      (endofcode 61))
                                   (Exceptions 
                                     (handler 6 15  21 (class "java.lang.Throwable"))
                                     (handler 16 18  21 (class "java.lang.Throwable"))
                                     (handler 21 24  21 (class "java.lang.Throwable"))
                                     (handler 35 45  48 (class "java.lang.Throwable"))
                                     (handler 48 51  48 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "exit"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 5) (code_length . 116)
                                   (parsedcode
                                      (0 (iconst_0)) 
                                      (1 (istore_1)) 
                                      (2 (getstatic (fieldCP "lock" "java.lang.Shutdown" (class "java.lang.Object")))) 
                                      (5 (dup)) 
                                      (6 (astore_2)) 
                                      (7 (monitorenter)) 
                                      (8 (iload_0)) ;;at TAG_9
                                      (9 (ifeq 16)) ;;to TAG_0
                                      (12 (iconst_0)) 
                                      (13 (putstatic (fieldCP "runFinalizersOnExit" "java.lang.Shutdown" boolean))) 
                                      (16 (getstatic (fieldCP "state" "java.lang.Shutdown" int))) ;;at TAG_0
                                      (19 (tableswitch (tableswitchinfo 69 (0 . 2) (44 51 54))))  ;;to TAG_1;;to TAG_2;;to TAG_3;;to TAG_4
                                      (44 (iconst_1)) ;;at TAG_2
                                      (45 (putstatic (fieldCP "state" "java.lang.Shutdown" int))) 
                                      (48 (goto 69))  ;;to TAG_1
                                      (51 (goto 69))  ;;to TAG_1;;at TAG_3
                                      (54 (iload_0)) ;;at TAG_4
                                      (55 (ifeq 65)) ;;to TAG_5
                                      (58 (iload_0)) 
                                      (59 (invokestatic (methodCP "halt" "java.lang.Shutdown" (int) void))) 
                                      (62 (goto 69))  ;;to TAG_1
                                      (65 (getstatic (fieldCP "runFinalizersOnExit" "java.lang.Shutdown" boolean))) ;;at TAG_5
                                      (68 (istore_1)) 
                                      (69 (aload_2)) ;;at TAG_1
                                      (70 (monitorexit)) 
                                      (71 (goto 79)) ;;to TAG_6;;at TAG_10
                                      (74 (astore_3)) ;;at TAG_11
                                      (75 (aload_2)) 
                                      (76 (monitorexit)) 
                                      (77 (aload_3)) ;;at TAG_12
                                      (78 (athrow)) 
                                      (79 (iload_1)) ;;at TAG_6
                                      (80 (ifeq 90)) ;;to TAG_7
                                      (83 (invokestatic (methodCP "runAllFinalizers" "java.lang.Shutdown" () void))) 
                                      (86 (iload_0)) 
                                      (87 (invokestatic (methodCP "halt" "java.lang.Shutdown" (int) void))) 
                                      (90 (ldc_w )) ;;at TAG_7
                                      (93 (dup)) 
                                      (94 (astore_2)) 
                                      (95 (monitorenter)) 
                                      (96 (invokestatic (methodCP "sequence" "java.lang.Shutdown" () void))) ;;at TAG_13
                                      (99 (iload_0)) 
                                      (100 (invokestatic (methodCP "halt" "java.lang.Shutdown" (int) void))) 
                                      (103 (aload_2)) 
                                      (104 (monitorexit)) 
                                      (105 (goto 115)) ;;to TAG_8;;at TAG_14
                                      (108 (astore 4)) ;;at TAG_15
                                      (110 (aload_2)) 
                                      (111 (monitorexit)) 
                                      (112 (aload 4)) ;;at TAG_16
                                      (114 (athrow)) 
                                      (115 (return)) ;;at TAG_8
                                      (endofcode 116))
                                   (Exceptions 
                                     (handler 8 71  74 (class "java.lang.Throwable"))
                                     (handler 74 77  74 (class "java.lang.Throwable"))
                                     (handler 96 105  108 (class "java.lang.Throwable"))
                                     (handler 108 112  108 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "shutdown"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 73)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "lock" "java.lang.Shutdown" (class "java.lang.Object")))) 
                                      (3 (dup)) 
                                      (4 (astore_0)) 
                                      (5 (monitorenter)) 
                                      (6 (getstatic (fieldCP "state" "java.lang.Shutdown" int))) ;;at TAG_4
                                      (9 (tableswitch (tableswitchinfo 43 (0 . 2) (36 43 43))))  ;;to TAG_1;;to TAG_0
                                      (36 (iconst_1)) ;;at TAG_1
                                      (37 (putstatic (fieldCP "state" "java.lang.Shutdown" int))) 
                                      (40 (goto 43)) ;;to TAG_0
                                      (43 (aload_0)) ;;at TAG_0
                                      (44 (monitorexit)) 
                                      (45 (goto 53)) ;;to TAG_2;;at TAG_5
                                      (48 (astore_1)) ;;at TAG_6
                                      (49 (aload_0)) 
                                      (50 (monitorexit)) 
                                      (51 (aload_1)) ;;at TAG_7
                                      (52 (athrow)) 
                                      (53 (ldc_w )) ;;at TAG_2
                                      (56 (dup)) 
                                      (57 (astore_0)) 
                                      (58 (monitorenter)) 
                                      (59 (invokestatic (methodCP "sequence" "java.lang.Shutdown" () void))) ;;at TAG_8
                                      (62 (aload_0)) 
                                      (63 (monitorexit)) 
                                      (64 (goto 72)) ;;to TAG_3;;at TAG_9
                                      (67 (astore_2)) ;;at TAG_10
                                      (68 (aload_0)) 
                                      (69 (monitorexit)) 
                                      (70 (aload_2)) ;;at TAG_11
                                      (71 (athrow)) 
                                      (72 (return)) ;;at TAG_3
                                      (endofcode 73))
                                   (Exceptions 
                                     (handler 6 45  48 (class "java.lang.Throwable"))
                                     (handler 48 51  48 (class "java.lang.Throwable"))
                                     (handler 59 64  67 (class "java.lang.Throwable"))
                                     (handler 67 70  67 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 0) (code_length . 43)
                                   (parsedcode
                                      (0 (iconst_0))
                                      (1 (putstatic (fieldCP "state" "java.lang.Shutdown" int)))
                                      (4 (iconst_0))
                                      (5 (putstatic (fieldCP "runFinalizersOnExit" "java.lang.Shutdown" boolean)))
                                      (8 (bipush 10))
                                      (10 (anewarray (class "java.lang.Runnable")))
                                      (13 (putstatic (fieldCP "hooks" "java.lang.Shutdown" (array (class "java.lang.Runnable")))))
                                      (16 (iconst_0))
                                      (17 (putstatic (fieldCP "currentRunningHook" "java.lang.Shutdown" int)))
                                      (20 (new (class "java.lang.Shutdown$Lock")))
                                      (23 (dup))
                                      (24 (aconst_null))
                                      (25 (invokespecial
					(methodCP "<init>" "java.lang.Shutdown$Lock" ((class "java.lang.Shutdown$1")) void)))
                                      (28 (putstatic (fieldCP "lock" "java.lang.Shutdown" (class "java.lang.Object"))))
                                      (31 (new (class "java.lang.Shutdown$Lock")))
                                      (34 (dup))
                                      (35 (aconst_null))
                                      (36 (invokespecial
					(methodCP "<init>" "java.lang.Shutdown$Lock" ((class "java.lang.Shutdown$1")) void)))
                                      (39 (putstatic (fieldCP "haltLock" "java.lang.Shutdown" (class "java.lang.Object"))))
                                      (42 (return))
                                      (endofcode 43))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *Shutdown-class-table*
  (make-static-class-decls 
   *java.lang.Shutdown*))

(defconst *package-name-map* 
  ("java.lang.Shutdown" . "java.lang"))

