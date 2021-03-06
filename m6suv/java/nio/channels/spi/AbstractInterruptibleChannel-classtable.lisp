; AbstractInterruptibleChannel-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:38 CDT 2014.
;

(defconst *java.nio.channels.spi.AbstractInterruptibleChannel*
 (make-class-def
      '(class "java.nio.channels.spi.AbstractInterruptibleChannel"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "closeLock" (class "java.lang.Object") (accessflags  *class*  *final*  *private* ) -1)
                        (field "open" boolean (accessflags  *class*  *private*  *volatile* ) -1)
                        (field "interruptor" (class "sun.nio.ch.Interruptible") (accessflags  *class*  *private* ) -1)
                        (field "interrupted" (class "java.lang.Thread") (accessflags  *class*  *private*  *volatile* ) -1))
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 21)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (new (class "java.lang.Object")))
                                      (8 (dup))
                                      (9 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (12 (putfield (fieldCP "closeLock" "java.nio.channels.spi.AbstractInterruptibleChannel" (class "java.lang.Object"))))
                                      (15 (aload_0))
                                      (16 (iconst_1))
                                      (17 (putfield (fieldCP "open" "java.nio.channels.spi.AbstractInterruptibleChannel" boolean)))
                                      (20 (return))
                                      (endofcode 21))
                                   (Exceptions )
                                   (StackMap )))
                        (method "close"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 37)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "closeLock" "java.nio.channels.spi.AbstractInterruptibleChannel" (class "java.lang.Object")))) 
                                      (4 (dup)) 
                                      (5 (astore_1)) 
                                      (6 (monitorenter)) 
                                      (7 (aload_0)) ;;at TAG_2
                                      (8 (getfield (fieldCP "open" "java.nio.channels.spi.AbstractInterruptibleChannel" boolean))) 
                                      (11 (ifne 17)) ;;to TAG_0
                                      (14 (aload_1)) 
                                      (15 (monitorexit)) 
                                      (16 (return)) ;;at TAG_3
                                      (17 (aload_0)) ;;at TAG_0
                                      (18 (iconst_0)) 
                                      (19 (putfield (fieldCP "open" "java.nio.channels.spi.AbstractInterruptibleChannel" boolean))) 
                                      (22 (aload_0)) 
                                      (23 (invokevirtual (methodCP "implCloseChannel" "java.nio.channels.spi.AbstractInterruptibleChannel" () void))) 
                                      (26 (aload_1)) 
                                      (27 (monitorexit)) 
                                      (28 (goto 36)) ;;to TAG_1;;at TAG_5
                                      (31 (astore_2)) ;;at TAG_4
                                      (32 (aload_1)) 
                                      (33 (monitorexit)) 
                                      (34 (aload_2)) ;;at TAG_6
                                      (35 (athrow)) 
                                      (36 (return)) ;;at TAG_1
                                      (endofcode 37))
                                   (Exceptions 
                                     (handler 7 16  31 (class "java.lang.Throwable"))
                                     (handler 17 28  31 (class "java.lang.Throwable"))
                                     (handler 31 34  31 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "implCloseChannel"
                              (parameters )
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *protected* )
                              (code))
                        (method "isOpen"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "open" "java.nio.channels.spi.AbstractInterruptibleChannel" boolean)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "begin"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *final*  *protected* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 48)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "interruptor" "java.nio.channels.spi.AbstractInterruptibleChannel" (class "sun.nio.ch.Interruptible")))) 
                                      (4 (ifnonnull 19))  ;;to TAG_0
                                      (7 (aload_0)) 
                                      (8 (new (class "java.nio.channels.spi.AbstractInterruptibleChannel$1"))) 
                                      (11 (dup)) 
                                      (12 (aload_0)) 
                                      (13 (invokespecial (methodCP "<init>" "java.nio.channels.spi.AbstractInterruptibleChannel$1" ((class "java.nio.channels.spi.AbstractInterruptibleChannel")) void))) 
                                      (16 (putfield (fieldCP "interruptor" "java.nio.channels.spi.AbstractInterruptibleChannel" (class "sun.nio.ch.Interruptible")))) 
                                      (19 (aload_0)) ;;at TAG_0
                                      (20 (getfield (fieldCP "interruptor" "java.nio.channels.spi.AbstractInterruptibleChannel" (class "sun.nio.ch.Interruptible")))) 
                                      (23 (invokestatic (methodCP "blockedOn" "java.nio.channels.spi.AbstractInterruptibleChannel" ((class "sun.nio.ch.Interruptible")) void))) 
                                      (26 (invokestatic (methodCP "currentThread" "java.lang.Thread" () (class "java.lang.Thread")))) 
                                      (29 (astore_1)) 
                                      (30 (aload_1)) 
                                      (31 (invokevirtual (methodCP "isInterrupted" "java.lang.Thread" () boolean))) 
                                      (34 (ifeq 47)) ;;to TAG_1
                                      (37 (aload_0)) 
                                      (38 (getfield (fieldCP "interruptor" "java.nio.channels.spi.AbstractInterruptibleChannel" (class "sun.nio.ch.Interruptible")))) 
                                      (41 (aload_1)) 
                                      (42 (invokeinterface (methodCP "interrupt" "sun.nio.ch.Interruptible" ((class "java.lang.Thread")) void) 2)) 
                                      (47 (return)) ;;at TAG_1
                                      (endofcode 48))
                                   (Exceptions )
                                   (StackMap )))
                        (method "end"
                              (parameters boolean)
                              (returntype . void)
                              (accessflags  *class*  *final*  *protected* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 50)
                                   (parsedcode
                                      (0 (aconst_null)) 
                                      (1 (invokestatic (methodCP "blockedOn" "java.nio.channels.spi.AbstractInterruptibleChannel" ((class "sun.nio.ch.Interruptible")) void))) 
                                      (4 (aload_0)) 
                                      (5 (getfield (fieldCP "interrupted" "java.nio.channels.spi.AbstractInterruptibleChannel" (class "java.lang.Thread")))) 
                                      (8 (astore_2)) 
                                      (9 (aload_2)) 
                                      (10 (ifnull 30))  ;;to TAG_0
                                      (13 (aload_2)) 
                                      (14 (invokestatic (methodCP "currentThread" "java.lang.Thread" () (class "java.lang.Thread")))) 
                                      (17 (if_acmpne 30))  ;;to TAG_0
                                      (20 (aconst_null)) 
                                      (21 (astore_2)) 
                                      (22 (new (class "java.nio.channels.ClosedByInterruptException"))) 
                                      (25 (dup)) 
                                      (26 (invokespecial (methodCP "<init>" "java.nio.channels.ClosedByInterruptException" () void))) 
                                      (29 (athrow)) 
                                      (30 (iload_1)) ;;at TAG_0
                                      (31 (ifne 49)) ;;to TAG_1
                                      (34 (aload_0)) 
                                      (35 (getfield (fieldCP "open" "java.nio.channels.spi.AbstractInterruptibleChannel" boolean))) 
                                      (38 (ifne 49)) ;;to TAG_1
                                      (41 (new (class "java.nio.channels.AsynchronousCloseException"))) 
                                      (44 (dup)) 
                                      (45 (invokespecial (methodCP "<init>" "java.nio.channels.AsynchronousCloseException" () void))) 
                                      (48 (athrow)) 
                                      (49 (return)) ;;at TAG_1
                                      (endofcode 50))
                                   (Exceptions )
                                   (StackMap )))
                        (method "blockedOn"
                              (parameters (class "sun.nio.ch.Interruptible"))
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 13)
                                   (parsedcode
                                      (0 (invokestatic
					(methodCP "getJavaLangAccess" "sun.misc.SharedSecrets" () (class "sun.misc.JavaLangAccess"))))
                                      (3 (invokestatic
					(methodCP "currentThread" "java.lang.Thread" () (class "java.lang.Thread"))))
                                      (6 (aload_0))
                                      (7 (invokeinterface
					(methodCP "blockedOn" "sun.misc.JavaLangAccess" ((class "java.lang.Thread") (class "sun.nio.ch.Interruptible")) void) 3))
                                      (12 (return))
                                      (endofcode 13))
                                   (Exceptions )
                                   (StackMap )))
                        (method "access$000"
                              (parameters (class "java.nio.channels.spi.AbstractInterruptibleChannel"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "closeLock" "java.nio.channels.spi.AbstractInterruptibleChannel" (class "java.lang.Object"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "access$100"
                              (parameters (class "java.nio.channels.spi.AbstractInterruptibleChannel"))
                              (returntype . boolean)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "open" "java.nio.channels.spi.AbstractInterruptibleChannel" boolean)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "access$102"
                              (parameters (class "java.nio.channels.spi.AbstractInterruptibleChannel") boolean)
                              (returntype . boolean)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 7)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (iload_1))
                                      (2 (dup_x1))
                                      (3 (putfield (fieldCP "open" "java.nio.channels.spi.AbstractInterruptibleChannel" boolean)))
                                      (6 (ireturn))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap )))
                        (method "access$202"
                              (parameters (class "java.nio.channels.spi.AbstractInterruptibleChannel") (class "java.lang.Thread"))
                              (returntype . (class "java.lang.Thread"))
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 7)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (dup_x1))
                                      (3 (putfield (fieldCP "interrupted" "java.nio.channels.spi.AbstractInterruptibleChannel" (class "java.lang.Thread"))))
                                      (6 (areturn))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.nio.channels.Channel" "java.nio.channels.InterruptibleChannel")
            (accessflags  *abstract*  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *AbstractInterruptibleChannel-class-table*
  (make-static-class-decls 
   *java.nio.channels.spi.AbstractInterruptibleChannel*))

(defconst *package-name-map* 
  ("java.nio.channels.spi.AbstractInterruptibleChannel" . "java.nio.channels.spi"))

