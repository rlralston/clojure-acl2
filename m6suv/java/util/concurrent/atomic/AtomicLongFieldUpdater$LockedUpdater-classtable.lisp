; AtomicLongFieldUpdater$LockedUpdater-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:43 CDT 2014.
;

(defconst *java.util.concurrent.atomic.AtomicLongFieldUpdater$LockedUpdater*
 (make-class-def
      '(class "java.util.concurrent.atomic.AtomicLongFieldUpdater$LockedUpdater"
            "java.util.concurrent.atomic.AtomicLongFieldUpdater"
            (constant_pool
                        (STRING  "Must be long type")
                        (STRING  "Must be volatile type")
                        (STRING  "Class ")
                        (STRING  " can not access a protected member of class ")
                        (STRING  " using an instance of "))
            (fields
                        (field "unsafe" (class "sun.misc.Unsafe") (accessflags  *class*  *final*  *private*  *static* ) -1)
                        (field "offset" long (accessflags  *class*  *final*  *private* ) -1)
                        (field "tclass" (class "java.lang.Class") (accessflags  *class*  *final*  *private* ) -1)
                        (field "cclass" (class "java.lang.Class") (accessflags  *class*  *final*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.lang.Class") (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 4) (max_locals . 7) (code_length . 141)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokespecial (methodCP "<init>" "java.util.concurrent.atomic.AtomicLongFieldUpdater" () void))) 
                                      (4 (aconst_null)) 
                                      (5 (astore_3)) 
                                      (6 (aconst_null)) 
                                      (7 (astore 4)) 
                                      (9 (iconst_0)) 
                                      (10 (istore 5)) 
                                      (12 (aload_1)) ;;at TAG_5
                                      (13 (aload_2)) 
                                      (14 (invokevirtual (methodCP "getDeclaredField" "java.lang.Class" ((class "java.lang.String")) (class "java.lang.reflect.Field")))) 
                                      (17 (astore_3)) 
                                      (18 (iconst_3)) 
                                      (19 (invokestatic (methodCP "getCallerClass" "sun.reflect.Reflection" (int) (class "java.lang.Class")))) 
                                      (22 (astore 4)) 
                                      (24 (aload_3)) 
                                      (25 (invokevirtual (methodCP "getModifiers" "java.lang.reflect.Field" () int))) 
                                      (28 (istore 5)) 
                                      (30 (aload 4)) 
                                      (32 (aload_1)) 
                                      (33 (aconst_null)) 
                                      (34 (iload 5)) 
                                      (36 (invokestatic (methodCP "ensureMemberAccess" "sun.reflect.misc.ReflectUtil" ((class "java.lang.Class") (class "java.lang.Class") (class "java.lang.Object") int) void))) 
                                      (39 (aload_1)) 
                                      (40 (invokestatic (methodCP "checkPackageAccess" "sun.reflect.misc.ReflectUtil" ((class "java.lang.Class")) void))) 
                                      (43 (goto 58)) ;;to TAG_0;;at TAG_6
                                      (46 (astore 6)) ;;at TAG_7
                                      (48 (new (class "java.lang.RuntimeException"))) 
                                      (51 (dup)) 
                                      (52 (aload 6)) 
                                      (54 (invokespecial (methodCP "<init>" "java.lang.RuntimeException" ((class "java.lang.Throwable")) void))) 
                                      (57 (athrow)) 
                                      (58 (aload_3)) ;;at TAG_0
                                      (59 (invokevirtual (methodCP "getType" "java.lang.reflect.Field" () (class "java.lang.Class")))) 
                                      (62 (astore 6)) 
                                      (64 (aload 6)) 
                                      (66 (getstatic (fieldCP "TYPE" "java.lang.Long" (class "java.lang.Class")))) 
                                      (69 (if_acmpeq 82)) ;;to TAG_1
                                      (72 (new (class "java.lang.IllegalArgumentException"))) 
                                      (75 (dup)) 
                                      (76 (ldc 0)) ;;STRING:: "Must be long type"
                                      (78 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (81 (athrow)) 
                                      (82 (iload 5)) ;;at TAG_1
                                      (84 (invokestatic (methodCP "isVolatile" "java.lang.reflect.Modifier" (int) boolean))) 
                                      (87 (ifne 100))  ;;to TAG_2
                                      (90 (new (class "java.lang.IllegalArgumentException"))) 
                                      (93 (dup)) 
                                      (94 (ldc 1)) ;;STRING:: "Must be volatile type"
                                      (96 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (99 (athrow)) 
                                      (100 (aload_0)) ;;at TAG_2
                                      (101 (iload 5)) 
                                      (103 (invokestatic (methodCP "isProtected" "java.lang.reflect.Modifier" (int) boolean))) 
                                      (106 (ifeq 120)) ;;to TAG_3
                                      (109 (aload 4)) 
                                      (111 (aload_1)) 
                                      (112 (if_acmpeq 120)) ;;to TAG_3
                                      (115 (aload 4)) 
                                      (117 (goto 121)) ;;to TAG_4
                                      (120 (aconst_null)) ;;at TAG_3
                                      (121 (putfield (fieldCP "cclass" "java.util.concurrent.atomic.AtomicLongFieldUpdater$LockedUpdater" (class "java.lang.Class")))) ;;at TAG_4
                                      (124 (aload_0)) 
                                      (125 (aload_1)) 
                                      (126 (putfield (fieldCP "tclass" "java.util.concurrent.atomic.AtomicLongFieldUpdater$LockedUpdater" (class "java.lang.Class")))) 
                                      (129 (aload_0)) 
                                      (130 (getstatic (fieldCP "unsafe" "java.util.concurrent.atomic.AtomicLongFieldUpdater$LockedUpdater" (class "sun.misc.Unsafe")))) 
                                      (133 (aload_3)) 
                                      (134 (invokevirtual (methodCP "objectFieldOffset" "sun.misc.Unsafe" ((class "java.lang.reflect.Field")) long))) 
                                      (137 (putfield (fieldCP "offset" "java.util.concurrent.atomic.AtomicLongFieldUpdater$LockedUpdater" long))) 
                                      (140 (return)) 
                                      (endofcode 141))
                                   (Exceptions 
                                     (handler 12 43  46 (class "java.lang.Exception")))
                                   (StackMap )))
                        (method "fullCheck"
                              (parameters (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 32)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "tclass" "java.util.concurrent.atomic.AtomicLongFieldUpdater$LockedUpdater" (class "java.lang.Class")))) 
                                      (4 (aload_1)) 
                                      (5 (invokevirtual (methodCP "isInstance" "java.lang.Class" ((class "java.lang.Object")) boolean))) 
                                      (8 (ifne 19))  ;;to TAG_0
                                      (11 (new (class "java.lang.ClassCastException"))) 
                                      (14 (dup)) 
                                      (15 (invokespecial (methodCP "<init>" "java.lang.ClassCastException" () void))) 
                                      (18 (athrow)) 
                                      (19 (aload_0)) ;;at TAG_0
                                      (20 (getfield (fieldCP "cclass" "java.util.concurrent.atomic.AtomicLongFieldUpdater$LockedUpdater" (class "java.lang.Class")))) 
                                      (23 (ifnull 31)) ;;to TAG_1
                                      (26 (aload_0)) 
                                      (27 (aload_1)) 
                                      (28 (invokespecial (methodCP "ensureProtectedAccess" "java.util.concurrent.atomic.AtomicLongFieldUpdater$LockedUpdater" ((class "java.lang.Object")) void))) 
                                      (31 (return)) ;;at TAG_1
                                      (endofcode 32))
                                   (Exceptions )
                                   (StackMap )))
                        (method "compareAndSet"
                              (parameters (class "java.lang.Object") long long)
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 10) (code_length . 83)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (ifnull 22)) ;;to TAG_0
                                      (4 (aload_1)) 
                                      (5 (invokevirtual (methodCP "getClass" "java.lang.Object" () (class "java.lang.Class")))) 
                                      (8 (aload_0)) 
                                      (9 (getfield (fieldCP "tclass" "java.util.concurrent.atomic.AtomicLongFieldUpdater$LockedUpdater" (class "java.lang.Class")))) 
                                      (12 (if_acmpne 22)) ;;to TAG_0
                                      (15 (aload_0)) 
                                      (16 (getfield (fieldCP "cclass" "java.util.concurrent.atomic.AtomicLongFieldUpdater$LockedUpdater" (class "java.lang.Class")))) 
                                      (19 (ifnull 27)) ;;to TAG_1
                                      (22 (aload_0)) ;;at TAG_0
                                      (23 (aload_1)) 
                                      (24 (invokespecial (methodCP "fullCheck" "java.util.concurrent.atomic.AtomicLongFieldUpdater$LockedUpdater" ((class "java.lang.Object")) void))) 
                                      (27 (aload_0)) ;;at TAG_1
                                      (28 (dup)) 
                                      (29 (astore 6)) 
                                      (31 (monitorenter)) 
                                      (32 (getstatic (fieldCP "unsafe" "java.util.concurrent.atomic.AtomicLongFieldUpdater$LockedUpdater" (class "sun.misc.Unsafe")))) ;;at TAG_3
                                      (35 (aload_1)) 
                                      (36 (aload_0)) 
                                      (37 (getfield (fieldCP "offset" "java.util.concurrent.atomic.AtomicLongFieldUpdater$LockedUpdater" long))) 
                                      (40 (invokevirtual (methodCP "getLong" "sun.misc.Unsafe" ((class "java.lang.Object") long) long))) 
                                      (43 (lstore 7)) 
                                      (45 (lload 7)) 
                                      (47 (lload_2)) 
                                      (48 (lcmp)) 
                                      (49 (ifeq 57))  ;;to TAG_2
                                      (52 (iconst_0)) 
                                      (53 (aload 6)) 
                                      (55 (monitorexit)) 
                                      (56 (ireturn)) ;;at TAG_4
                                      (57 (getstatic (fieldCP "unsafe" "java.util.concurrent.atomic.AtomicLongFieldUpdater$LockedUpdater" (class "sun.misc.Unsafe")))) ;;at TAG_2
                                      (60 (aload_1)) 
                                      (61 (aload_0)) 
                                      (62 (getfield (fieldCP "offset" "java.util.concurrent.atomic.AtomicLongFieldUpdater$LockedUpdater" long))) 
                                      (65 (lload 4)) 
                                      (67 (invokevirtual (methodCP "putLong" "sun.misc.Unsafe" ((class "java.lang.Object") long long) void))) 
                                      (70 (iconst_1)) 
                                      (71 (aload 6)) 
                                      (73 (monitorexit)) 
                                      (74 (ireturn)) ;;at TAG_6
                                      (75 (astore 9)) ;;at TAG_5
                                      (77 (aload 6)) 
                                      (79 (monitorexit)) 
                                      (80 (aload 9)) ;;at TAG_7
                                      (82 (athrow)) 
                                      (endofcode 83))
                                   (Exceptions 
                                     (handler 32 56  75 (class "java.lang.Throwable"))
                                     (handler 57 74  75 (class "java.lang.Throwable"))
                                     (handler 75 80  75 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "weakCompareAndSet"
                              (parameters (class "java.lang.Object") long long)
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 6) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (lload_2))
                                      (3 (lload 4))
                                      (5 (invokevirtual
					(methodCP "compareAndSet" "java.util.concurrent.atomic.AtomicLongFieldUpdater$LockedUpdater" ((class "java.lang.Object") long long) boolean)))
                                      (8 (ireturn))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "set"
                              (parameters (class "java.lang.Object") long)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 6) (code_length . 59)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (ifnull 22)) ;;to TAG_0
                                      (4 (aload_1)) 
                                      (5 (invokevirtual (methodCP "getClass" "java.lang.Object" () (class "java.lang.Class")))) 
                                      (8 (aload_0)) 
                                      (9 (getfield (fieldCP "tclass" "java.util.concurrent.atomic.AtomicLongFieldUpdater$LockedUpdater" (class "java.lang.Class")))) 
                                      (12 (if_acmpne 22)) ;;to TAG_0
                                      (15 (aload_0)) 
                                      (16 (getfield (fieldCP "cclass" "java.util.concurrent.atomic.AtomicLongFieldUpdater$LockedUpdater" (class "java.lang.Class")))) 
                                      (19 (ifnull 27)) ;;to TAG_1
                                      (22 (aload_0)) ;;at TAG_0
                                      (23 (aload_1)) 
                                      (24 (invokespecial (methodCP "fullCheck" "java.util.concurrent.atomic.AtomicLongFieldUpdater$LockedUpdater" ((class "java.lang.Object")) void))) 
                                      (27 (aload_0)) ;;at TAG_1
                                      (28 (dup)) 
                                      (29 (astore 4)) 
                                      (31 (monitorenter)) 
                                      (32 (getstatic (fieldCP "unsafe" "java.util.concurrent.atomic.AtomicLongFieldUpdater$LockedUpdater" (class "sun.misc.Unsafe")))) ;;at TAG_3
                                      (35 (aload_1)) 
                                      (36 (aload_0)) 
                                      (37 (getfield (fieldCP "offset" "java.util.concurrent.atomic.AtomicLongFieldUpdater$LockedUpdater" long))) 
                                      (40 (lload_2)) 
                                      (41 (invokevirtual (methodCP "putLong" "sun.misc.Unsafe" ((class "java.lang.Object") long long) void))) 
                                      (44 (aload 4)) 
                                      (46 (monitorexit)) 
                                      (47 (goto 58))  ;;to TAG_2;;at TAG_4
                                      (50 (astore 5)) ;;at TAG_5
                                      (52 (aload 4)) 
                                      (54 (monitorexit)) 
                                      (55 (aload 5)) ;;at TAG_6
                                      (57 (athrow)) 
                                      (58 (return)) ;;at TAG_2
                                      (endofcode 59))
                                   (Exceptions 
                                     (handler 32 47  50 (class "java.lang.Throwable"))
                                     (handler 50 55  50 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "lazySet"
                              (parameters (class "java.lang.Object") long)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 4) (code_length . 7)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (lload_2))
                                      (3 (invokevirtual
					(methodCP "set" "java.util.concurrent.atomic.AtomicLongFieldUpdater$LockedUpdater" ((class "java.lang.Object") long) void)))
                                      (6 (return))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap )))
                        (method "get"
                              (parameters (class "java.lang.Object"))
                              (returntype . long)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 4) (code_length . 50)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (ifnull 22)) ;;to TAG_0
                                      (4 (aload_1)) 
                                      (5 (invokevirtual (methodCP "getClass" "java.lang.Object" () (class "java.lang.Class")))) 
                                      (8 (aload_0)) 
                                      (9 (getfield (fieldCP "tclass" "java.util.concurrent.atomic.AtomicLongFieldUpdater$LockedUpdater" (class "java.lang.Class")))) 
                                      (12 (if_acmpne 22)) ;;to TAG_0
                                      (15 (aload_0)) 
                                      (16 (getfield (fieldCP "cclass" "java.util.concurrent.atomic.AtomicLongFieldUpdater$LockedUpdater" (class "java.lang.Class")))) 
                                      (19 (ifnull 27)) ;;to TAG_1
                                      (22 (aload_0)) ;;at TAG_0
                                      (23 (aload_1)) 
                                      (24 (invokespecial (methodCP "fullCheck" "java.util.concurrent.atomic.AtomicLongFieldUpdater$LockedUpdater" ((class "java.lang.Object")) void))) 
                                      (27 (aload_0)) ;;at TAG_1
                                      (28 (dup)) 
                                      (29 (astore_2)) 
                                      (30 (monitorenter)) 
                                      (31 (getstatic (fieldCP "unsafe" "java.util.concurrent.atomic.AtomicLongFieldUpdater$LockedUpdater" (class "sun.misc.Unsafe")))) ;;at TAG_2
                                      (34 (aload_1)) 
                                      (35 (aload_0)) 
                                      (36 (getfield (fieldCP "offset" "java.util.concurrent.atomic.AtomicLongFieldUpdater$LockedUpdater" long))) 
                                      (39 (invokevirtual (methodCP "getLong" "sun.misc.Unsafe" ((class "java.lang.Object") long) long))) 
                                      (42 (aload_2)) 
                                      (43 (monitorexit)) 
                                      (44 (lreturn)) ;;at TAG_3
                                      (45 (astore_3)) ;;at TAG_4
                                      (46 (aload_2)) 
                                      (47 (monitorexit)) 
                                      (48 (aload_3)) ;;at TAG_5
                                      (49 (athrow)) 
                                      (endofcode 50))
                                   (Exceptions 
                                     (handler 31 44  45 (class "java.lang.Throwable"))
                                     (handler 45 48  45 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "ensureProtectedAccess"
                              (parameters (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 6) (max_locals . 2) (code_length . 82)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "cclass" "java.util.concurrent.atomic.AtomicLongFieldUpdater$LockedUpdater" (class "java.lang.Class")))) 
                                      (4 (aload_1)) 
                                      (5 (invokevirtual (methodCP "isInstance" "java.lang.Class" ((class "java.lang.Object")) boolean))) 
                                      (8 (ifeq 12))  ;;to TAG_0
                                      (11 (return)) 
                                      (12 (new (class "java.lang.RuntimeException"))) ;;at TAG_0
                                      (15 (dup)) 
                                      (16 (new (class "java.lang.IllegalAccessException"))) 
                                      (19 (dup)) 
                                      (20 (new (class "java.lang.StringBuilder"))) 
                                      (23 (dup)) 
                                      (24 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (27 (ldc 2)) ;;STRING:: "Class "
                                      (29 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (32 (aload_0)) 
                                      (33 (getfield (fieldCP "cclass" "java.util.concurrent.atomic.AtomicLongFieldUpdater$LockedUpdater" (class "java.lang.Class")))) 
                                      (36 (invokevirtual (methodCP "getName" "java.lang.Class" () (class "java.lang.String")))) 
                                      (39 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (42 (ldc 3)) ;;STRING:: " can not access a protected member of class "
                                      (44 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (47 (aload_0)) 
                                      (48 (getfield (fieldCP "tclass" "java.util.concurrent.atomic.AtomicLongFieldUpdater$LockedUpdater" (class "java.lang.Class")))) 
                                      (51 (invokevirtual (methodCP "getName" "java.lang.Class" () (class "java.lang.String")))) 
                                      (54 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (57 (ldc 4)) ;;STRING:: " using an instance of "
                                      (59 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (62 (aload_1)) 
                                      (63 (invokevirtual (methodCP "getClass" "java.lang.Object" () (class "java.lang.Class")))) 
                                      (66 (invokevirtual (methodCP "getName" "java.lang.Class" () (class "java.lang.String")))) 
                                      (69 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (72 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (75 (invokespecial (methodCP "<init>" "java.lang.IllegalAccessException" ((class "java.lang.String")) void))) 
                                      (78 (invokespecial (methodCP "<init>" "java.lang.RuntimeException" ((class "java.lang.Throwable")) void))) 
                                      (81 (athrow)) 
                                      (endofcode 82))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 0) (code_length . 7)
                                   (parsedcode
                                      (0 (invokestatic
					(methodCP "getUnsafe" "sun.misc.Unsafe" () (class "sun.misc.Unsafe"))))
                                      (3 (putstatic (fieldCP "unsafe" "java.util.concurrent.atomic.AtomicLongFieldUpdater$LockedUpdater" (class "sun.misc.Unsafe"))))
                                      (6 (return))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *AtomicLongFieldUpdater$LockedUpdater-class-table*
  (make-static-class-decls 
   *java.util.concurrent.atomic.AtomicLongFieldUpdater$LockedUpdater*))

(defconst *package-name-map* 
  ("java.util.concurrent.atomic.AtomicLongFieldUpdater$LockedUpdater" . "java.util.concurrent.atomic"))

