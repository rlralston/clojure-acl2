; TaskQueue-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:48 CDT 2014.
;

(defconst *java.util.TaskQueue*
 (make-class-def
      '(class "java.util.TaskQueue"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "queue" (array (class "java.util.TimerTask")) (accessflags  *class*  *private* ) -1)
                        (field "size" int (accessflags  *class*  *private* ) -1)
                        (field "$assertionsDisabled" boolean (accessflags  *class*  *final*  *static* ) -1))
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 20)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (sipush 128))
                                      (8 (anewarray (class "java.util.TimerTask")))
                                      (11 (putfield (fieldCP "queue" "java.util.TaskQueue" (array (class "java.util.TimerTask")))))
                                      (14 (aload_0))
                                      (15 (iconst_0))
                                      (16 (putfield (fieldCP "size" "java.util.TaskQueue" int)))
                                      (19 (return))
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap )))
                        (method "size"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "size" "java.util.TaskQueue" int)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "add"
                              (parameters (class "java.util.TimerTask"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 61)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "size" "java.util.TaskQueue" int))) 
                                      (4 (iconst_1)) 
                                      (5 (iadd)) 
                                      (6 (aload_0)) 
                                      (7 (getfield (fieldCP "queue" "java.util.TaskQueue" (array (class "java.util.TimerTask"))))) 
                                      (10 (arraylength)) 
                                      (11 (if_icmpne 35))  ;;to TAG_0
                                      (14 (aload_0)) 
                                      (15 (aload_0)) 
                                      (16 (getfield (fieldCP "queue" "java.util.TaskQueue" (array (class "java.util.TimerTask"))))) 
                                      (19 (iconst_2)) 
                                      (20 (aload_0)) 
                                      (21 (getfield (fieldCP "queue" "java.util.TaskQueue" (array (class "java.util.TimerTask"))))) 
                                      (24 (arraylength)) 
                                      (25 (imul)) 
                                      (26 (invokestatic (methodCP "copyOf" "java.util.Arrays" ((array (class "java.lang.Object")) int) (array (class "java.lang.Object"))))) 
                                      (29 (checkcast (array (class "java.util.TimerTask")))) 
                                      (32 (putfield (fieldCP "queue" "java.util.TaskQueue" (array (class "java.util.TimerTask"))))) 
                                      (35 (aload_0)) ;;at TAG_0
                                      (36 (getfield (fieldCP "queue" "java.util.TaskQueue" (array (class "java.util.TimerTask"))))) 
                                      (39 (aload_0)) 
                                      (40 (dup)) 
                                      (41 (getfield (fieldCP "size" "java.util.TaskQueue" int))) 
                                      (44 (iconst_1)) 
                                      (45 (iadd)) 
                                      (46 (dup_x1)) 
                                      (47 (putfield (fieldCP "size" "java.util.TaskQueue" int))) 
                                      (50 (aload_1)) 
                                      (51 (aastore)) 
                                      (52 (aload_0)) 
                                      (53 (aload_0)) 
                                      (54 (getfield (fieldCP "size" "java.util.TaskQueue" int))) 
                                      (57 (invokespecial (methodCP "fixUp" "java.util.TaskQueue" (int) void))) 
                                      (60 (return)) 
                                      (endofcode 61))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getMin"
                              (parameters )
                              (returntype . (class "java.util.TimerTask"))
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 7)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "queue" "java.util.TaskQueue" (array (class "java.util.TimerTask")))))
                                      (4 (iconst_1))
                                      (5 (aaload))
                                      (6 (areturn))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap )))
                        (method "get"
                              (parameters int)
                              (returntype . (class "java.util.TimerTask"))
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 7)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "queue" "java.util.TaskQueue" (array (class "java.util.TimerTask")))))
                                      (4 (iload_1))
                                      (5 (aaload))
                                      (6 (areturn))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap )))
                        (method "removeMin"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 5) (max_locals . 1) (code_length . 38)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "queue" "java.util.TaskQueue" (array (class "java.util.TimerTask")))))
                                      (4 (iconst_1))
                                      (5 (aload_0))
                                      (6 (getfield (fieldCP "queue" "java.util.TaskQueue" (array (class "java.util.TimerTask")))))
                                      (9 (aload_0))
                                      (10 (getfield (fieldCP "size" "java.util.TaskQueue" int)))
                                      (13 (aaload))
                                      (14 (aastore))
                                      (15 (aload_0))
                                      (16 (getfield (fieldCP "queue" "java.util.TaskQueue" (array (class "java.util.TimerTask")))))
                                      (19 (aload_0))
                                      (20 (dup))
                                      (21 (getfield (fieldCP "size" "java.util.TaskQueue" int)))
                                      (24 (dup_x1))
                                      (25 (iconst_1))
                                      (26 (isub))
                                      (27 (putfield (fieldCP "size" "java.util.TaskQueue" int)))
                                      (30 (aconst_null))
                                      (31 (aastore))
                                      (32 (aload_0))
                                      (33 (iconst_1))
                                      (34 (invokespecial
					(methodCP "fixDown" "java.util.TaskQueue" (int) void)))
                                      (37 (return))
                                      (endofcode 38))
                                   (Exceptions )
                                   (StackMap )))
                        (method "quickRemove"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 5) (max_locals . 2) (code_length . 55)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "$assertionsDisabled" "java.util.TaskQueue" boolean))) 
                                      (3 (ifne 22))  ;;to TAG_0
                                      (6 (iload_1)) 
                                      (7 (aload_0)) 
                                      (8 (getfield (fieldCP "size" "java.util.TaskQueue" int))) 
                                      (11 (if_icmple 22))  ;;to TAG_0
                                      (14 (new (class "java.lang.AssertionError"))) 
                                      (17 (dup)) 
                                      (18 (invokespecial (methodCP "<init>" "java.lang.AssertionError" () void))) 
                                      (21 (athrow)) 
                                      (22 (aload_0)) ;;at TAG_0
                                      (23 (getfield (fieldCP "queue" "java.util.TaskQueue" (array (class "java.util.TimerTask"))))) 
                                      (26 (iload_1)) 
                                      (27 (aload_0)) 
                                      (28 (getfield (fieldCP "queue" "java.util.TaskQueue" (array (class "java.util.TimerTask"))))) 
                                      (31 (aload_0)) 
                                      (32 (getfield (fieldCP "size" "java.util.TaskQueue" int))) 
                                      (35 (aaload)) 
                                      (36 (aastore)) 
                                      (37 (aload_0)) 
                                      (38 (getfield (fieldCP "queue" "java.util.TaskQueue" (array (class "java.util.TimerTask"))))) 
                                      (41 (aload_0)) 
                                      (42 (dup)) 
                                      (43 (getfield (fieldCP "size" "java.util.TaskQueue" int))) 
                                      (46 (dup_x1)) 
                                      (47 (iconst_1)) 
                                      (48 (isub)) 
                                      (49 (putfield (fieldCP "size" "java.util.TaskQueue" int))) 
                                      (52 (aconst_null)) 
                                      (53 (aastore)) 
                                      (54 (return)) 
                                      (endofcode 55))
                                   (Exceptions )
                                   (StackMap )))
                        (method "rescheduleMin"
                              (parameters long)
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 16)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "queue" "java.util.TaskQueue" (array (class "java.util.TimerTask")))))
                                      (4 (iconst_1))
                                      (5 (aaload))
                                      (6 (lload_1))
                                      (7 (putfield (fieldCP "nextExecutionTime" "java.util.TimerTask" long)))
                                      (10 (aload_0))
                                      (11 (iconst_1))
                                      (12 (invokespecial
					(methodCP "fixDown" "java.util.TaskQueue" (int) void)))
                                      (15 (return))
                                      (endofcode 16))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isEmpty"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 13)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "size" "java.util.TaskQueue" int))) 
                                      (4 (ifne 11))  ;;to TAG_0
                                      (7 (iconst_1)) 
                                      (8 (goto 12)) ;;to TAG_1
                                      (11 (iconst_0)) ;;at TAG_0
                                      (12 (ireturn)) ;;at TAG_1
                                      (endofcode 13))
                                   (Exceptions )
                                   (StackMap )))
                        (method "clear"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 29)
                                   (parsedcode
                                      (0 (iconst_1)) 
                                      (1 (istore_1)) 
                                      (2 (iload_1)) ;;at TAG_1
                                      (3 (aload_0)) 
                                      (4 (getfield (fieldCP "size" "java.util.TaskQueue" int))) 
                                      (7 (if_icmpgt 23))  ;;to TAG_0
                                      (10 (aload_0)) 
                                      (11 (getfield (fieldCP "queue" "java.util.TaskQueue" (array (class "java.util.TimerTask"))))) 
                                      (14 (iload_1)) 
                                      (15 (aconst_null)) 
                                      (16 (aastore)) 
                                      (17 (iinc 1 1)) 
                                      (20 (goto 2)) ;;to TAG_1
                                      (23 (aload_0)) ;;at TAG_0
                                      (24 (iconst_0)) 
                                      (25 (putfield (fieldCP "size" "java.util.TaskQueue" int))) 
                                      (28 (return)) 
                                      (endofcode 29))
                                   (Exceptions )
                                   (StackMap )))
                        (method "fixUp"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 4) (max_locals . 4) (code_length . 66)
                                   (parsedcode
                                      (0 (iload_1)) ;;at TAG_2
                                      (1 (iconst_1)) 
                                      (2 (if_icmple 65)) ;;to TAG_0
                                      (5 (iload_1)) 
                                      (6 (iconst_1)) 
                                      (7 (ishr)) 
                                      (8 (istore_2)) 
                                      (9 (aload_0)) 
                                      (10 (getfield (fieldCP "queue" "java.util.TaskQueue" (array (class "java.util.TimerTask"))))) 
                                      (13 (iload_2)) 
                                      (14 (aaload)) 
                                      (15 (getfield (fieldCP "nextExecutionTime" "java.util.TimerTask" long))) 
                                      (18 (aload_0)) 
                                      (19 (getfield (fieldCP "queue" "java.util.TaskQueue" (array (class "java.util.TimerTask"))))) 
                                      (22 (iload_1)) 
                                      (23 (aaload)) 
                                      (24 (getfield (fieldCP "nextExecutionTime" "java.util.TimerTask" long))) 
                                      (27 (lcmp)) 
                                      (28 (ifgt 34)) ;;to TAG_1
                                      (31 (goto 65)) ;;to TAG_0
                                      (34 (aload_0)) ;;at TAG_1
                                      (35 (getfield (fieldCP "queue" "java.util.TaskQueue" (array (class "java.util.TimerTask"))))) 
                                      (38 (iload_2)) 
                                      (39 (aaload)) 
                                      (40 (astore_3)) 
                                      (41 (aload_0)) 
                                      (42 (getfield (fieldCP "queue" "java.util.TaskQueue" (array (class "java.util.TimerTask"))))) 
                                      (45 (iload_2)) 
                                      (46 (aload_0)) 
                                      (47 (getfield (fieldCP "queue" "java.util.TaskQueue" (array (class "java.util.TimerTask"))))) 
                                      (50 (iload_1)) 
                                      (51 (aaload)) 
                                      (52 (aastore)) 
                                      (53 (aload_0)) 
                                      (54 (getfield (fieldCP "queue" "java.util.TaskQueue" (array (class "java.util.TimerTask"))))) 
                                      (57 (iload_1)) 
                                      (58 (aload_3)) 
                                      (59 (aastore)) 
                                      (60 (iload_2)) 
                                      (61 (istore_1)) 
                                      (62 (goto 0))  ;;to TAG_2
                                      (65 (return)) ;;at TAG_0
                                      (endofcode 66))
                                   (Exceptions )
                                   (StackMap )))
                        (method "fixDown"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 5) (max_locals . 4) (code_length . 108)
                                   (parsedcode
                                      (0 (iload_1)) ;;at TAG_3
                                      (1 (iconst_1)) 
                                      (2 (ishl)) 
                                      (3 (dup)) 
                                      (4 (istore_2)) 
                                      (5 (aload_0)) 
                                      (6 (getfield (fieldCP "size" "java.util.TaskQueue" int))) 
                                      (9 (if_icmpgt 107)) ;;to TAG_0
                                      (12 (iload_2)) 
                                      (13 (ifle 107)) ;;to TAG_0
                                      (16 (iload_2)) 
                                      (17 (aload_0)) 
                                      (18 (getfield (fieldCP "size" "java.util.TaskQueue" int))) 
                                      (21 (if_icmpge 51)) ;;to TAG_1
                                      (24 (aload_0)) 
                                      (25 (getfield (fieldCP "queue" "java.util.TaskQueue" (array (class "java.util.TimerTask"))))) 
                                      (28 (iload_2)) 
                                      (29 (aaload)) 
                                      (30 (getfield (fieldCP "nextExecutionTime" "java.util.TimerTask" long))) 
                                      (33 (aload_0)) 
                                      (34 (getfield (fieldCP "queue" "java.util.TaskQueue" (array (class "java.util.TimerTask"))))) 
                                      (37 (iload_2)) 
                                      (38 (iconst_1)) 
                                      (39 (iadd)) 
                                      (40 (aaload)) 
                                      (41 (getfield (fieldCP "nextExecutionTime" "java.util.TimerTask" long))) 
                                      (44 (lcmp)) 
                                      (45 (ifle 51)) ;;to TAG_1
                                      (48 (iinc 2 1)) 
                                      (51 (aload_0)) ;;at TAG_1
                                      (52 (getfield (fieldCP "queue" "java.util.TaskQueue" (array (class "java.util.TimerTask"))))) 
                                      (55 (iload_1)) 
                                      (56 (aaload)) 
                                      (57 (getfield (fieldCP "nextExecutionTime" "java.util.TimerTask" long))) 
                                      (60 (aload_0)) 
                                      (61 (getfield (fieldCP "queue" "java.util.TaskQueue" (array (class "java.util.TimerTask"))))) 
                                      (64 (iload_2)) 
                                      (65 (aaload)) 
                                      (66 (getfield (fieldCP "nextExecutionTime" "java.util.TimerTask" long))) 
                                      (69 (lcmp)) 
                                      (70 (ifgt 76))  ;;to TAG_2
                                      (73 (goto 107)) ;;to TAG_0
                                      (76 (aload_0)) ;;at TAG_2
                                      (77 (getfield (fieldCP "queue" "java.util.TaskQueue" (array (class "java.util.TimerTask"))))) 
                                      (80 (iload_2)) 
                                      (81 (aaload)) 
                                      (82 (astore_3)) 
                                      (83 (aload_0)) 
                                      (84 (getfield (fieldCP "queue" "java.util.TaskQueue" (array (class "java.util.TimerTask"))))) 
                                      (87 (iload_2)) 
                                      (88 (aload_0)) 
                                      (89 (getfield (fieldCP "queue" "java.util.TaskQueue" (array (class "java.util.TimerTask"))))) 
                                      (92 (iload_1)) 
                                      (93 (aaload)) 
                                      (94 (aastore)) 
                                      (95 (aload_0)) 
                                      (96 (getfield (fieldCP "queue" "java.util.TaskQueue" (array (class "java.util.TimerTask"))))) 
                                      (99 (iload_1)) 
                                      (100 (aload_3)) 
                                      (101 (aastore)) 
                                      (102 (iload_2)) 
                                      (103 (istore_1)) 
                                      (104 (goto 0)) ;;to TAG_3
                                      (107 (return)) ;;at TAG_0
                                      (endofcode 108))
                                   (Exceptions )
                                   (StackMap )))
                        (method "heapify"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 24)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "size" "java.util.TaskQueue" int))) 
                                      (4 (iconst_2)) 
                                      (5 (idiv)) 
                                      (6 (istore_1)) 
                                      (7 (iload_1)) ;;at TAG_1
                                      (8 (iconst_1)) 
                                      (9 (if_icmplt 23))  ;;to TAG_0
                                      (12 (aload_0)) 
                                      (13 (iload_1)) 
                                      (14 (invokespecial (methodCP "fixDown" "java.util.TaskQueue" (int) void))) 
                                      (17 (iinc 1 -1)) 
                                      (20 (goto 7)) ;;to TAG_1
                                      (23 (return)) ;;at TAG_0
                                      (endofcode 24))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 0) (code_length . 18)
                                   (parsedcode
                                      (0 (ldc_w )) 
                                      (3 (invokevirtual (methodCP "desiredAssertionStatus" "java.lang.Class" () boolean))) 
                                      (6 (ifne 13))  ;;to TAG_0
                                      (9 (iconst_1)) 
                                      (10 (goto 14)) ;;to TAG_1
                                      (13 (iconst_0)) ;;at TAG_0
                                      (14 (putstatic (fieldCP "$assertionsDisabled" "java.util.TaskQueue" boolean))) ;;at TAG_1
                                      (17 (return)) 
                                      (endofcode 18))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *TaskQueue-class-table*
  (make-static-class-decls 
   *java.util.TaskQueue*))

(defconst *package-name-map* 
  ("java.util.TaskQueue" . "java.util"))
