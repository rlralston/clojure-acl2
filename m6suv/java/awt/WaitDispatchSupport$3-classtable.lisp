; WaitDispatchSupport$3-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:30 CDT 2014.
;

(defconst *java.awt.WaitDispatchSupport$3*
 (make-class-def
      '(class "java.awt.WaitDispatchSupport$3"
            "java.util.TimerTask"
            (constant_pool)
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
                                      (2 (putfield (fieldCP "this$0" "java.awt.WaitDispatchSupport$3" (class "java.awt.WaitDispatchSupport"))))
                                      (5 (aload_0))
                                      (6 (invokespecial
					(methodCP "<init>" "java.util.TimerTask" () void)))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "run"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 23)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "this$0" "java.awt.WaitDispatchSupport$3" (class "java.awt.WaitDispatchSupport")))) 
                                      (4 (invokestatic (methodCP "access$100" "java.awt.WaitDispatchSupport" ((class "java.awt.WaitDispatchSupport")) (class "java.util.concurrent.atomic.AtomicBoolean")))) 
                                      (7 (iconst_1)) 
                                      (8 (iconst_0)) 
                                      (9 (invokevirtual (methodCP "compareAndSet" "java.util.concurrent.atomic.AtomicBoolean" (boolean boolean) boolean))) 
                                      (12 (ifeq 22))  ;;to TAG_0
                                      (15 (aload_0)) 
                                      (16 (getfield (fieldCP "this$0" "java.awt.WaitDispatchSupport$3" (class "java.awt.WaitDispatchSupport")))) 
                                      (19 (invokestatic (methodCP "access$800" "java.awt.WaitDispatchSupport" ((class "java.awt.WaitDispatchSupport")) void))) 
                                      (22 (return)) ;;at TAG_0
                                      (endofcode 23))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "InnerClasses")))))


(defconst *WaitDispatchSupport$3-class-table*
  (make-static-class-decls 
   *java.awt.WaitDispatchSupport$3*))

(defconst *package-name-map* 
  ("java.awt.WaitDispatchSupport$3" . "java.awt"))

