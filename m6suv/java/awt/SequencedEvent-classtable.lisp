; SequencedEvent-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:30 CDT 2014.
;

(defconst *java.awt.SequencedEvent*
 (make-class-def
      '(class "java.awt.SequencedEvent"
            "java.awt.AWTEvent"
            (constant_pool
                        (LONG 547742659238625067)
                        (INT 1006)
                        (LONG 1000))
            (fields
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0)
                        (field "ID" int (accessflags  *class*  *final*  *private*  *static* ) 1)
                        (field "list" (class "java.util.LinkedList") (accessflags  *class*  *final*  *private*  *static* ) -1)
                        (field "nested" (class "java.awt.AWTEvent") (accessflags  *class*  *final*  *private* ) -1)
                        (field "appContext" (class "sun.awt.AppContext") (accessflags  *class*  *private* ) -1)
                        (field "disposed" boolean (accessflags  *class*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.awt.AWTEvent"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 4) (code_length . 45)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (aload_1)) 
                                      (2 (invokevirtual (methodCP "getSource" "java.awt.AWTEvent" () (class "java.lang.Object")))) 
                                      (5 (sipush 1006)) 
                                      (8 (invokespecial (methodCP "<init>" "java.awt.AWTEvent" ((class "java.lang.Object") int) void))) 
                                      (11 (aload_0)) 
                                      (12 (aload_1)) 
                                      (13 (putfield (fieldCP "nested" "java.awt.SequencedEvent" (class "java.awt.AWTEvent")))) 
                                      (16 (aload_1)) 
                                      (17 (invokestatic (methodCP "setSystemGenerated" "sun.awt.SunToolkit" ((class "java.awt.AWTEvent")) void))) 
                                      (20 (ldc_w )) 
                                      (23 (dup)) 
                                      (24 (astore_2)) 
                                      (25 (monitorenter)) 
                                      (26 (getstatic (fieldCP "list" "java.awt.SequencedEvent" (class "java.util.LinkedList")))) ;;at TAG_1
                                      (29 (aload_0)) 
                                      (30 (invokevirtual (methodCP "add" "java.util.LinkedList" ((class "java.lang.Object")) boolean))) 
                                      (33 (pop)) 
                                      (34 (aload_2)) 
                                      (35 (monitorexit)) 
                                      (36 (goto 44)) ;;to TAG_0;;at TAG_2
                                      (39 (astore_3)) ;;at TAG_3
                                      (40 (aload_2)) 
                                      (41 (monitorexit)) 
                                      (42 (aload_3)) ;;at TAG_4
                                      (43 (athrow)) 
                                      (44 (return)) ;;at TAG_0
                                      (endofcode 45))
                                   (Exceptions 
                                     (handler 26 36  39 (class "java.lang.Throwable"))
                                     (handler 39 42  39 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "dispatch"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 5) (code_length . 130)
                                   (parsedcode
                                      (0 (aload_0)) ;;at TAG_13
                                      (1 (invokestatic (methodCP "getAppContext" "sun.awt.AppContext" () (class "sun.awt.AppContext")))) 
                                      (4 (putfield (fieldCP "appContext" "java.awt.SequencedEvent" (class "sun.awt.AppContext")))) 
                                      (7 (invokestatic (methodCP "getFirst" "java.awt.SequencedEvent" () (class "java.awt.SequencedEvent")))) 
                                      (10 (aload_0)) 
                                      (11 (if_acmpeq 89)) ;;to TAG_0
                                      (14 (invokestatic (methodCP "isDispatchThread" "java.awt.EventQueue" () boolean))) 
                                      (17 (ifeq 45))  ;;to TAG_1
                                      (20 (invokestatic (methodCP "currentThread" "java.lang.Thread" () (class "java.lang.Thread")))) 
                                      (23 (checkcast (class "java.awt.EventDispatchThread"))) 
                                      (26 (astore_1)) 
                                      (27 (aload_1)) 
                                      (28 (sipush 1007)) 
                                      (31 (new (class "java.awt.SequencedEvent$1"))) 
                                      (34 (dup)) 
                                      (35 (aload_0)) 
                                      (36 (invokespecial (methodCP "<init>" "java.awt.SequencedEvent$1" ((class "java.awt.SequencedEvent")) void))) 
                                      (39 (invokevirtual (methodCP "pumpEvents" "java.awt.EventDispatchThread" (int (class "java.awt.Conditional")) void))) 
                                      (42 (goto 89)) ;;to TAG_0
                                      (45 (aload_0)) ;;at TAG_1
                                      (46 (invokevirtual (methodCP "isFirstOrDisposed" "java.awt.SequencedEvent" () boolean))) 
                                      (49 (ifne 89)) ;;to TAG_0
                                      (52 (ldc_w )) 
                                      (55 (dup)) 
                                      (56 (astore_1)) 
                                      (57 (monitorenter)) 
                                      (58 (ldc_w )) ;;at TAG_6
                                      (61 (ldc2_w 2)) ;; LONG:: "1000"
                                      (64 (invokevirtual (methodCP "wait" "java.lang.Object" (long) void))) 
                                      (67 (goto 76)) ;;to TAG_2;;at TAG_7
                                      (70 (astore_2)) ;;at TAG_8
                                      (71 (aload_1)) 
                                      (72 (monitorexit)) 
                                      (73 (goto 89)) ;;to TAG_0;;at TAG_9
                                      (76 (aload_1)) ;;at TAG_2
                                      (77 (monitorexit)) 
                                      (78 (goto 86)) ;;to TAG_3;;at TAG_11
                                      (81 (astore_3)) ;;at TAG_10
                                      (82 (aload_1)) 
                                      (83 (monitorexit)) 
                                      (84 (aload_3)) ;;at TAG_12
                                      (85 (athrow)) 
                                      (86 (goto 45))  ;;to TAG_1;;at TAG_3
                                      (89 (aload_0)) ;;at TAG_0
                                      (90 (getfield (fieldCP "disposed" "java.awt.SequencedEvent" boolean))) 
                                      (93 (ifne 113)) ;;to TAG_4
                                      (96 (invokestatic (methodCP "getCurrentKeyboardFocusManager" "java.awt.KeyboardFocusManager" () (class "java.awt.KeyboardFocusManager")))) 
                                      (99 (aload_0)) 
                                      (100 (invokevirtual (methodCP "setCurrentSequencedEvent" "java.awt.KeyboardFocusManager" ((class "java.awt.SequencedEvent")) void))) 
                                      (103 (invokestatic (methodCP "getEventQueue" "java.awt.Toolkit" () (class "java.awt.EventQueue")))) 
                                      (106 (aload_0)) 
                                      (107 (getfield (fieldCP "nested" "java.awt.SequencedEvent" (class "java.awt.AWTEvent")))) 
                                      (110 (invokevirtual (methodCP "dispatchEvent" "java.awt.EventQueue" ((class "java.awt.AWTEvent")) void))) 
                                      (113 (aload_0)) ;;at TAG_4
                                      (114 (invokevirtual (methodCP "dispose" "java.awt.SequencedEvent" () void))) 
                                      (117 (goto 129)) ;;to TAG_5
                                      (120 (astore 4)) ;;at TAG_14
                                      (122 (aload_0)) ;;at TAG_15
                                      (123 (invokevirtual (methodCP "dispose" "java.awt.SequencedEvent" () void))) 
                                      (126 (aload 4)) 
                                      (128 (athrow)) 
                                      (129 (return)) ;;at TAG_5
                                      (endofcode 130))
                                   (Exceptions 
                                     (handler 58 67  70 (class "java.lang.InterruptedException"))
                                     (handler 58 73  81 (class "java.lang.Throwable"))
                                     (handler 76 78  81 (class "java.lang.Throwable"))
                                     (handler 81 84  81 (class "java.lang.Throwable"))
                                     (handler 0 113  120 (class "java.lang.Throwable"))
                                     (handler 120 122  120 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "isOwnerAppContextDisposed"
                              (parameters (class "java.awt.SequencedEvent"))
                              (returntype . boolean)
                              (accessflags  *class*  *final*  *private*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 32)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (ifnull 30))  ;;to TAG_0
                                      (4 (aload_0)) 
                                      (5 (getfield (fieldCP "nested" "java.awt.SequencedEvent" (class "java.awt.AWTEvent")))) 
                                      (8 (invokevirtual (methodCP "getSource" "java.awt.AWTEvent" () (class "java.lang.Object")))) 
                                      (11 (astore_1)) 
                                      (12 (aload_1)) 
                                      (13 (instanceof (class "java.awt.Component"))) 
                                      (16 (ifeq 30))  ;;to TAG_0
                                      (19 (aload_1)) 
                                      (20 (checkcast (class "java.awt.Component"))) 
                                      (23 (getfield (fieldCP "appContext" "java.awt.Component" (class "sun.awt.AppContext")))) 
                                      (26 (invokevirtual (methodCP "isDisposed" "sun.awt.AppContext" () boolean))) 
                                      (29 (ireturn)) 
                                      (30 (iconst_0)) ;;at TAG_0
                                      (31 (ireturn)) 
                                      (endofcode 32))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isFirstOrDisposed"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 29)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "disposed" "java.awt.SequencedEvent" boolean))) 
                                      (4 (ifeq 9)) ;;to TAG_0
                                      (7 (iconst_1)) 
                                      (8 (ireturn)) 
                                      (9 (aload_0)) ;;at TAG_0
                                      (10 (invokestatic (methodCP "getFirstWithContext" "java.awt.SequencedEvent" () (class "java.awt.SequencedEvent")))) 
                                      (13 (if_acmpeq 23)) ;;to TAG_1
                                      (16 (aload_0)) 
                                      (17 (getfield (fieldCP "disposed" "java.awt.SequencedEvent" boolean))) 
                                      (20 (ifeq 27))  ;;to TAG_2
                                      (23 (iconst_1)) ;;at TAG_1
                                      (24 (goto 28)) ;;to TAG_3
                                      (27 (iconst_0)) ;;at TAG_2
                                      (28 (ireturn)) ;;at TAG_3
                                      (endofcode 29))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getFirst"
                              (parameters )
                              (returntype . (class "java.awt.SequencedEvent"))
                              (accessflags  *class*  *final*  *private*  *static*  *super*  *synchronized* )
                              (code
                                   (max_stack . 1) (max_locals . 0) (code_length . 10)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "list" "java.awt.SequencedEvent" (class "java.util.LinkedList"))))
                                      (3 (invokevirtual
					(methodCP "getFirst" "java.util.LinkedList" () (class "java.lang.Object"))))
                                      (6 (checkcast (class "java.awt.SequencedEvent")))
                                      (9 (areturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getFirstWithContext"
                              (parameters )
                              (returntype . (class "java.awt.SequencedEvent"))
                              (accessflags  *class*  *final*  *private*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 24)
                                   (parsedcode
                                      (0 (invokestatic (methodCP "getFirst" "java.awt.SequencedEvent" () (class "java.awt.SequencedEvent")))) 
                                      (3 (astore_0)) 
                                      (4 (aload_0)) ;;at TAG_1
                                      (5 (invokestatic (methodCP "isOwnerAppContextDisposed" "java.awt.SequencedEvent" ((class "java.awt.SequencedEvent")) boolean))) 
                                      (8 (ifeq 22))  ;;to TAG_0
                                      (11 (aload_0)) 
                                      (12 (invokevirtual (methodCP "dispose" "java.awt.SequencedEvent" () void))) 
                                      (15 (invokestatic (methodCP "getFirst" "java.awt.SequencedEvent" () (class "java.awt.SequencedEvent")))) 
                                      (18 (astore_0)) 
                                      (19 (goto 4)) ;;to TAG_1
                                      (22 (aload_0)) ;;at TAG_0
                                      (23 (areturn)) 
                                      (endofcode 24))
                                   (Exceptions )
                                   (StackMap )))
                        (method "dispose"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *final* )
                              (code
                                   (max_stack . 3) (max_locals . 4) (code_length . 166)
                                   (parsedcode
                                      (0 (ldc_w )) 
                                      (3 (dup)) 
                                      (4 (astore_1)) 
                                      (5 (monitorenter)) 
                                      (6 (aload_0)) ;;at TAG_8
                                      (7 (getfield (fieldCP "disposed" "java.awt.SequencedEvent" boolean))) 
                                      (10 (ifeq 16)) ;;to TAG_0
                                      (13 (aload_1)) 
                                      (14 (monitorexit)) 
                                      (15 (return)) ;;at TAG_9
                                      (16 (invokestatic (methodCP "getCurrentKeyboardFocusManager" "java.awt.KeyboardFocusManager" () (class "java.awt.KeyboardFocusManager")))) ;;at TAG_0
                                      (19 (invokevirtual (methodCP "getCurrentSequencedEvent" "java.awt.KeyboardFocusManager" () (class "java.awt.SequencedEvent")))) 
                                      (22 (aload_0)) 
                                      (23 (if_acmpne 33))  ;;to TAG_1
                                      (26 (invokestatic (methodCP "getCurrentKeyboardFocusManager" "java.awt.KeyboardFocusManager" () (class "java.awt.KeyboardFocusManager")))) 
                                      (29 (aconst_null)) 
                                      (30 (invokevirtual (methodCP "setCurrentSequencedEvent" "java.awt.KeyboardFocusManager" ((class "java.awt.SequencedEvent")) void))) 
                                      (33 (aload_0)) ;;at TAG_1
                                      (34 (iconst_1)) 
                                      (35 (putfield (fieldCP "disposed" "java.awt.SequencedEvent" boolean))) 
                                      (38 (aload_1)) 
                                      (39 (monitorexit)) 
                                      (40 (goto 48)) ;;to TAG_2;;at TAG_11
                                      (43 (astore_2)) ;;at TAG_10
                                      (44 (aload_1)) 
                                      (45 (monitorexit)) 
                                      (46 (aload_2)) ;;at TAG_12
                                      (47 (athrow)) 
                                      (48 (aload_0)) ;;at TAG_2
                                      (49 (getfield (fieldCP "appContext" "java.awt.SequencedEvent" (class "sun.awt.AppContext")))) 
                                      (52 (ifnull 69)) ;;to TAG_3
                                      (55 (aload_0)) 
                                      (56 (getfield (fieldCP "appContext" "java.awt.SequencedEvent" (class "sun.awt.AppContext")))) 
                                      (59 (new (class "java.awt.SentEvent"))) 
                                      (62 (dup)) 
                                      (63 (invokespecial (methodCP "<init>" "java.awt.SentEvent" () void))) 
                                      (66 (invokestatic (methodCP "postEvent" "sun.awt.SunToolkit" ((class "sun.awt.AppContext") (class "java.awt.AWTEvent")) void))) 
                                      (69 (aconst_null)) ;;at TAG_3
                                      (70 (astore_1)) 
                                      (71 (ldc_w )) 
                                      (74 (dup)) 
                                      (75 (astore_2)) 
                                      (76 (monitorenter)) 
                                      (77 (ldc_w )) ;;at TAG_13
                                      (80 (invokevirtual (methodCP "notifyAll" "java.lang.Object" () void))) 
                                      (83 (getstatic (fieldCP "list" "java.awt.SequencedEvent" (class "java.util.LinkedList")))) 
                                      (86 (invokevirtual (methodCP "getFirst" "java.util.LinkedList" () (class "java.lang.Object")))) 
                                      (89 (aload_0)) 
                                      (90 (if_acmpne 122)) ;;to TAG_4
                                      (93 (getstatic (fieldCP "list" "java.awt.SequencedEvent" (class "java.util.LinkedList")))) 
                                      (96 (invokevirtual (methodCP "removeFirst" "java.util.LinkedList" () (class "java.lang.Object")))) 
                                      (99 (pop)) 
                                      (100 (getstatic (fieldCP "list" "java.awt.SequencedEvent" (class "java.util.LinkedList")))) 
                                      (103 (invokevirtual (methodCP "isEmpty" "java.util.LinkedList" () boolean))) 
                                      (106 (ifne 130)) ;;to TAG_5
                                      (109 (getstatic (fieldCP "list" "java.awt.SequencedEvent" (class "java.util.LinkedList")))) 
                                      (112 (invokevirtual (methodCP "getFirst" "java.util.LinkedList" () (class "java.lang.Object")))) 
                                      (115 (checkcast (class "java.awt.SequencedEvent"))) 
                                      (118 (astore_1)) 
                                      (119 (goto 130)) ;;to TAG_5
                                      (122 (getstatic (fieldCP "list" "java.awt.SequencedEvent" (class "java.util.LinkedList")))) ;;at TAG_4
                                      (125 (aload_0)) 
                                      (126 (invokevirtual (methodCP "remove" "java.util.LinkedList" ((class "java.lang.Object")) boolean))) 
                                      (129 (pop)) 
                                      (130 (aload_2)) ;;at TAG_5
                                      (131 (monitorexit)) 
                                      (132 (goto 140)) ;;to TAG_6;;at TAG_14
                                      (135 (astore_3)) ;;at TAG_15
                                      (136 (aload_2)) 
                                      (137 (monitorexit)) 
                                      (138 (aload_3)) ;;at TAG_16
                                      (139 (athrow)) 
                                      (140 (aload_1)) ;;at TAG_6
                                      (141 (ifnull 165)) ;;to TAG_7
                                      (144 (aload_1)) 
                                      (145 (getfield (fieldCP "appContext" "java.awt.SequencedEvent" (class "sun.awt.AppContext")))) 
                                      (148 (ifnull 165)) ;;to TAG_7
                                      (151 (aload_1)) 
                                      (152 (getfield (fieldCP "appContext" "java.awt.SequencedEvent" (class "sun.awt.AppContext")))) 
                                      (155 (new (class "java.awt.SentEvent"))) 
                                      (158 (dup)) 
                                      (159 (invokespecial (methodCP "<init>" "java.awt.SentEvent" () void))) 
                                      (162 (invokestatic (methodCP "postEvent" "sun.awt.SunToolkit" ((class "sun.awt.AppContext") (class "java.awt.AWTEvent")) void))) 
                                      (165 (return)) ;;at TAG_7
                                      (endofcode 166))
                                   (Exceptions 
                                     (handler 6 15  43 (class "java.lang.Throwable"))
                                     (handler 16 40  43 (class "java.lang.Throwable"))
                                     (handler 43 46  43 (class "java.lang.Throwable"))
                                     (handler 77 132  135 (class "java.lang.Throwable"))
                                     (handler 135 138  135 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 11)
                                   (parsedcode
                                      (0 (new (class "java.util.LinkedList")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.util.LinkedList" () void)))
                                      (7 (putstatic (fieldCP "list" "java.awt.SequencedEvent" (class "java.util.LinkedList"))))
                                      (10 (return))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.awt.ActiveEvent")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *SequencedEvent-class-table*
  (make-static-class-decls 
   *java.awt.SequencedEvent*))

(defconst *package-name-map* 
  ("java.awt.SequencedEvent" . "java.awt"))

