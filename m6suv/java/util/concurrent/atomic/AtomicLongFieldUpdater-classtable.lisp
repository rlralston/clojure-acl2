; AtomicLongFieldUpdater-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:43 CDT 2014.
;

(defconst *java.util.concurrent.atomic.AtomicLongFieldUpdater*
 (make-class-def
      '(class "java.util.concurrent.atomic.AtomicLongFieldUpdater"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "newUpdater"
                              (parameters (class "java.lang.Class") (class "java.lang.String"))
                              (returntype . (class "java.util.concurrent.atomic.AtomicLongFieldUpdater"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 26)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "VM_SUPPORTS_LONG_CAS" "java.util.concurrent.atomic.AtomicLong" boolean))) 
                                      (3 (ifeq 16))  ;;to TAG_0
                                      (6 (new (class "java.util.concurrent.atomic.AtomicLongFieldUpdater$CASUpdater"))) 
                                      (9 (dup)) 
                                      (10 (aload_0)) 
                                      (11 (aload_1)) 
                                      (12 (invokespecial (methodCP "<init>" "java.util.concurrent.atomic.AtomicLongFieldUpdater$CASUpdater" ((class "java.lang.Class") (class "java.lang.String")) void))) 
                                      (15 (areturn)) 
                                      (16 (new (class "java.util.concurrent.atomic.AtomicLongFieldUpdater$LockedUpdater"))) ;;at TAG_0
                                      (19 (dup)) 
                                      (20 (aload_0)) 
                                      (21 (aload_1)) 
                                      (22 (invokespecial (methodCP "<init>" "java.util.concurrent.atomic.AtomicLongFieldUpdater$LockedUpdater" ((class "java.lang.Class") (class "java.lang.String")) void))) 
                                      (25 (areturn)) 
                                      (endofcode 26))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *protected* )
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
                        (method "compareAndSet"
                              (parameters (class "java.lang.Object") long long)
                              (returntype . boolean)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "weakCompareAndSet"
                              (parameters (class "java.lang.Object") long long)
                              (returntype . boolean)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "set"
                              (parameters (class "java.lang.Object") long)
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "lazySet"
                              (parameters (class "java.lang.Object") long)
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "get"
                              (parameters (class "java.lang.Object"))
                              (returntype . long)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getAndSet"
                              (parameters (class "java.lang.Object") long)
                              (returntype . long)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 6) (code_length . 24)
                                   (parsedcode
                                      (0 (aload_0)) ;;at TAG_1
                                      (1 (aload_1)) 
                                      (2 (invokevirtual (methodCP "get" "java.util.concurrent.atomic.AtomicLongFieldUpdater" ((class "java.lang.Object")) long))) 
                                      (5 (lstore 4)) 
                                      (7 (aload_0)) 
                                      (8 (aload_1)) 
                                      (9 (lload 4)) 
                                      (11 (lload_2)) 
                                      (12 (invokevirtual (methodCP "compareAndSet" "java.util.concurrent.atomic.AtomicLongFieldUpdater" ((class "java.lang.Object") long long) boolean))) 
                                      (15 (ifeq 21))  ;;to TAG_0
                                      (18 (lload 4)) 
                                      (20 (lreturn)) 
                                      (21 (goto 0)) ;;to TAG_1;;at TAG_0
                                      (endofcode 24))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getAndIncrement"
                              (parameters (class "java.lang.Object"))
                              (returntype . long)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 6) (code_length . 27)
                                   (parsedcode
                                      (0 (aload_0)) ;;at TAG_1
                                      (1 (aload_1)) 
                                      (2 (invokevirtual (methodCP "get" "java.util.concurrent.atomic.AtomicLongFieldUpdater" ((class "java.lang.Object")) long))) 
                                      (5 (lstore_2)) 
                                      (6 (lload_2)) 
                                      (7 (lconst_1)) 
                                      (8 (ladd)) 
                                      (9 (lstore 4)) 
                                      (11 (aload_0)) 
                                      (12 (aload_1)) 
                                      (13 (lload_2)) 
                                      (14 (lload 4)) 
                                      (16 (invokevirtual (methodCP "compareAndSet" "java.util.concurrent.atomic.AtomicLongFieldUpdater" ((class "java.lang.Object") long long) boolean))) 
                                      (19 (ifeq 24))  ;;to TAG_0
                                      (22 (lload_2)) 
                                      (23 (lreturn)) 
                                      (24 (goto 0)) ;;to TAG_1;;at TAG_0
                                      (endofcode 27))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getAndDecrement"
                              (parameters (class "java.lang.Object"))
                              (returntype . long)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 6) (code_length . 27)
                                   (parsedcode
                                      (0 (aload_0)) ;;at TAG_1
                                      (1 (aload_1)) 
                                      (2 (invokevirtual (methodCP "get" "java.util.concurrent.atomic.AtomicLongFieldUpdater" ((class "java.lang.Object")) long))) 
                                      (5 (lstore_2)) 
                                      (6 (lload_2)) 
                                      (7 (lconst_1)) 
                                      (8 (lsub)) 
                                      (9 (lstore 4)) 
                                      (11 (aload_0)) 
                                      (12 (aload_1)) 
                                      (13 (lload_2)) 
                                      (14 (lload 4)) 
                                      (16 (invokevirtual (methodCP "compareAndSet" "java.util.concurrent.atomic.AtomicLongFieldUpdater" ((class "java.lang.Object") long long) boolean))) 
                                      (19 (ifeq 24))  ;;to TAG_0
                                      (22 (lload_2)) 
                                      (23 (lreturn)) 
                                      (24 (goto 0)) ;;to TAG_1;;at TAG_0
                                      (endofcode 27))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getAndAdd"
                              (parameters (class "java.lang.Object") long)
                              (returntype . long)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 8) (code_length . 31)
                                   (parsedcode
                                      (0 (aload_0)) ;;at TAG_1
                                      (1 (aload_1)) 
                                      (2 (invokevirtual (methodCP "get" "java.util.concurrent.atomic.AtomicLongFieldUpdater" ((class "java.lang.Object")) long))) 
                                      (5 (lstore 4)) 
                                      (7 (lload 4)) 
                                      (9 (lload_2)) 
                                      (10 (ladd)) 
                                      (11 (lstore 6)) 
                                      (13 (aload_0)) 
                                      (14 (aload_1)) 
                                      (15 (lload 4)) 
                                      (17 (lload 6)) 
                                      (19 (invokevirtual (methodCP "compareAndSet" "java.util.concurrent.atomic.AtomicLongFieldUpdater" ((class "java.lang.Object") long long) boolean))) 
                                      (22 (ifeq 28))  ;;to TAG_0
                                      (25 (lload 4)) 
                                      (27 (lreturn)) 
                                      (28 (goto 0)) ;;to TAG_1;;at TAG_0
                                      (endofcode 31))
                                   (Exceptions )
                                   (StackMap )))
                        (method "incrementAndGet"
                              (parameters (class "java.lang.Object"))
                              (returntype . long)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 6) (code_length . 28)
                                   (parsedcode
                                      (0 (aload_0)) ;;at TAG_1
                                      (1 (aload_1)) 
                                      (2 (invokevirtual (methodCP "get" "java.util.concurrent.atomic.AtomicLongFieldUpdater" ((class "java.lang.Object")) long))) 
                                      (5 (lstore_2)) 
                                      (6 (lload_2)) 
                                      (7 (lconst_1)) 
                                      (8 (ladd)) 
                                      (9 (lstore 4)) 
                                      (11 (aload_0)) 
                                      (12 (aload_1)) 
                                      (13 (lload_2)) 
                                      (14 (lload 4)) 
                                      (16 (invokevirtual (methodCP "compareAndSet" "java.util.concurrent.atomic.AtomicLongFieldUpdater" ((class "java.lang.Object") long long) boolean))) 
                                      (19 (ifeq 25))  ;;to TAG_0
                                      (22 (lload 4)) 
                                      (24 (lreturn)) 
                                      (25 (goto 0)) ;;to TAG_1;;at TAG_0
                                      (endofcode 28))
                                   (Exceptions )
                                   (StackMap )))
                        (method "decrementAndGet"
                              (parameters (class "java.lang.Object"))
                              (returntype . long)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 6) (code_length . 28)
                                   (parsedcode
                                      (0 (aload_0)) ;;at TAG_1
                                      (1 (aload_1)) 
                                      (2 (invokevirtual (methodCP "get" "java.util.concurrent.atomic.AtomicLongFieldUpdater" ((class "java.lang.Object")) long))) 
                                      (5 (lstore_2)) 
                                      (6 (lload_2)) 
                                      (7 (lconst_1)) 
                                      (8 (lsub)) 
                                      (9 (lstore 4)) 
                                      (11 (aload_0)) 
                                      (12 (aload_1)) 
                                      (13 (lload_2)) 
                                      (14 (lload 4)) 
                                      (16 (invokevirtual (methodCP "compareAndSet" "java.util.concurrent.atomic.AtomicLongFieldUpdater" ((class "java.lang.Object") long long) boolean))) 
                                      (19 (ifeq 25))  ;;to TAG_0
                                      (22 (lload 4)) 
                                      (24 (lreturn)) 
                                      (25 (goto 0)) ;;to TAG_1;;at TAG_0
                                      (endofcode 28))
                                   (Exceptions )
                                   (StackMap )))
                        (method "addAndGet"
                              (parameters (class "java.lang.Object") long)
                              (returntype . long)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 8) (code_length . 31)
                                   (parsedcode
                                      (0 (aload_0)) ;;at TAG_1
                                      (1 (aload_1)) 
                                      (2 (invokevirtual (methodCP "get" "java.util.concurrent.atomic.AtomicLongFieldUpdater" ((class "java.lang.Object")) long))) 
                                      (5 (lstore 4)) 
                                      (7 (lload 4)) 
                                      (9 (lload_2)) 
                                      (10 (ladd)) 
                                      (11 (lstore 6)) 
                                      (13 (aload_0)) 
                                      (14 (aload_1)) 
                                      (15 (lload 4)) 
                                      (17 (lload 6)) 
                                      (19 (invokevirtual (methodCP "compareAndSet" "java.util.concurrent.atomic.AtomicLongFieldUpdater" ((class "java.lang.Object") long long) boolean))) 
                                      (22 (ifeq 28))  ;;to TAG_0
                                      (25 (lload 6)) 
                                      (27 (lreturn)) 
                                      (28 (goto 0)) ;;to TAG_1;;at TAG_0
                                      (endofcode 31))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *abstract*  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *AtomicLongFieldUpdater-class-table*
  (make-static-class-decls 
   *java.util.concurrent.atomic.AtomicLongFieldUpdater*))

(defconst *package-name-map* 
  ("java.util.concurrent.atomic.AtomicLongFieldUpdater" . "java.util.concurrent.atomic"))

