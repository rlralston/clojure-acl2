; WaitDispatchSupport$1-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:30 CDT 2014.
;

(defconst *java.awt.WaitDispatchSupport$1*
 (make-class-def
      '(class "java.awt.WaitDispatchSupport$1"
            "java.lang.Object"
            (constant_pool
                        (STRING  "evaluate(): blockingEDT=")
                        (STRING  ", blockingCT="))
            (fields
                        (field "this$0" (class "java.awt.WaitDispatchSupport") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.awt.WaitDispatchSupport"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.awt.WaitDispatchSupport$1" (class "java.awt.WaitDispatchSupport"))))
                                      (5 (aload_0))
                                      (6 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "evaluate"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 142)
                                   (parsedcode
                                      (0 (invokestatic (methodCP "access$000" "java.awt.WaitDispatchSupport" () (class "sun.util.logging.PlatformLogger")))) 
                                      (3 (sipush 300)) 
                                      (6 (invokevirtual (methodCP "isLoggable" "sun.util.logging.PlatformLogger" (int) boolean))) 
                                      (9 (ifeq 64)) ;;to TAG_0
                                      (12 (invokestatic (methodCP "access$000" "java.awt.WaitDispatchSupport" () (class "sun.util.logging.PlatformLogger")))) 
                                      (15 (new (class "java.lang.StringBuilder"))) 
                                      (18 (dup)) 
                                      (19 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (22 (ldc 0)) ;;STRING:: "evaluate(): blockingEDT="
                                      (24 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (27 (aload_0)) 
                                      (28 (getfield (fieldCP "this$0" "java.awt.WaitDispatchSupport$1" (class "java.awt.WaitDispatchSupport")))) 
                                      (31 (invokestatic (methodCP "access$100" "java.awt.WaitDispatchSupport" ((class "java.awt.WaitDispatchSupport")) (class "java.util.concurrent.atomic.AtomicBoolean")))) 
                                      (34 (invokevirtual (methodCP "get" "java.util.concurrent.atomic.AtomicBoolean" () boolean))) 
                                      (37 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (boolean) (class "java.lang.StringBuilder")))) 
                                      (40 (ldc 1)) ;;STRING:: ", blockingCT="
                                      (42 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (45 (aload_0)) 
                                      (46 (getfield (fieldCP "this$0" "java.awt.WaitDispatchSupport$1" (class "java.awt.WaitDispatchSupport")))) 
                                      (49 (invokestatic (methodCP "access$200" "java.awt.WaitDispatchSupport" ((class "java.awt.WaitDispatchSupport")) (class "java.util.concurrent.atomic.AtomicBoolean")))) 
                                      (52 (invokevirtual (methodCP "get" "java.util.concurrent.atomic.AtomicBoolean" () boolean))) 
                                      (55 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (boolean) (class "java.lang.StringBuilder")))) 
                                      (58 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (61 (invokevirtual (methodCP "finest" "sun.util.logging.PlatformLogger" ((class "java.lang.String")) void))) 
                                      (64 (aload_0)) ;;at TAG_0
                                      (65 (getfield (fieldCP "this$0" "java.awt.WaitDispatchSupport$1" (class "java.awt.WaitDispatchSupport")))) 
                                      (68 (invokestatic (methodCP "access$300" "java.awt.WaitDispatchSupport" ((class "java.awt.WaitDispatchSupport")) (class "java.awt.Conditional")))) 
                                      (71 (ifnull 89)) ;;to TAG_1
                                      (74 (aload_0)) 
                                      (75 (getfield (fieldCP "this$0" "java.awt.WaitDispatchSupport$1" (class "java.awt.WaitDispatchSupport")))) 
                                      (78 (invokestatic (methodCP "access$300" "java.awt.WaitDispatchSupport" ((class "java.awt.WaitDispatchSupport")) (class "java.awt.Conditional")))) 
                                      (81 (invokeinterface (methodCP "evaluate" "java.awt.Conditional" () boolean) 1)) 
                                      (86 (goto 90))  ;;to TAG_2
                                      (89 (iconst_1)) ;;at TAG_1
                                      (90 (istore_1)) ;;at TAG_2
                                      (91 (aload_0)) 
                                      (92 (getfield (fieldCP "this$0" "java.awt.WaitDispatchSupport$1" (class "java.awt.WaitDispatchSupport")))) 
                                      (95 (invokestatic (methodCP "access$100" "java.awt.WaitDispatchSupport" ((class "java.awt.WaitDispatchSupport")) (class "java.util.concurrent.atomic.AtomicBoolean")))) 
                                      (98 (invokevirtual (methodCP "get" "java.util.concurrent.atomic.AtomicBoolean" () boolean))) 
                                      (101 (ifeq 108)) ;;to TAG_3
                                      (104 (iload_1)) 
                                      (105 (ifne 140)) ;;to TAG_4
                                      (108 (aload_0)) ;;at TAG_3
                                      (109 (getfield (fieldCP "this$0" "java.awt.WaitDispatchSupport$1" (class "java.awt.WaitDispatchSupport")))) 
                                      (112 (invokestatic (methodCP "access$400" "java.awt.WaitDispatchSupport" ((class "java.awt.WaitDispatchSupport")) (class "java.util.TimerTask")))) 
                                      (115 (ifnull 138)) ;;to TAG_5
                                      (118 (aload_0)) 
                                      (119 (getfield (fieldCP "this$0" "java.awt.WaitDispatchSupport$1" (class "java.awt.WaitDispatchSupport")))) 
                                      (122 (invokestatic (methodCP "access$400" "java.awt.WaitDispatchSupport" ((class "java.awt.WaitDispatchSupport")) (class "java.util.TimerTask")))) 
                                      (125 (invokevirtual (methodCP "cancel" "java.util.TimerTask" () boolean))) 
                                      (128 (pop)) 
                                      (129 (aload_0)) 
                                      (130 (getfield (fieldCP "this$0" "java.awt.WaitDispatchSupport$1" (class "java.awt.WaitDispatchSupport")))) 
                                      (133 (aconst_null)) 
                                      (134 (invokestatic (methodCP "access$402" "java.awt.WaitDispatchSupport" ((class "java.awt.WaitDispatchSupport") (class "java.util.TimerTask")) (class "java.util.TimerTask")))) 
                                      (137 (pop)) 
                                      (138 (iconst_0)) ;;at TAG_5
                                      (139 (ireturn)) 
                                      (140 (iconst_1)) ;;at TAG_4
                                      (141 (ireturn)) 
                                      (endofcode 142))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.awt.Conditional")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "InnerClasses")))))


(defconst *WaitDispatchSupport$1-class-table*
  (make-static-class-decls 
   *java.awt.WaitDispatchSupport$1*))

(defconst *package-name-map* 
  ("java.awt.WaitDispatchSupport$1" . "java.awt"))
