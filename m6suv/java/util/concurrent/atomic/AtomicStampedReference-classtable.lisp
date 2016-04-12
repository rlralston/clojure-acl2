; AtomicStampedReference-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:43 CDT 2014.
;

(defconst *java.util.concurrent.atomic.AtomicStampedReference*
 (make-class-def
      '(class "java.util.concurrent.atomic.AtomicStampedReference"
            "java.lang.Object"
            (constant_pool
                        (STRING  "pair"))
            (fields
                        (field "pair" (class "java.util.concurrent.atomic.AtomicStampedReference$Pair") (accessflags  *class*  *private*  *volatile* ) -1)
                        (field "UNSAFE" (class "sun.misc.Unsafe") (accessflags  *class*  *final*  *private*  *static* ) -1)
                        (field "pairOffset" long (accessflags  *class*  *final*  *private*  *static* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.lang.Object") int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 14)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (iload_2))
                                      (7 (invokestatic
					(methodCP "of" "java.util.concurrent.atomic.AtomicStampedReference$Pair" ((class "java.lang.Object") int) (class "java.util.concurrent.atomic.AtomicStampedReference$Pair"))))
                                      (10 (putfield (fieldCP "pair" "java.util.concurrent.atomic.AtomicStampedReference" (class "java.util.concurrent.atomic.AtomicStampedReference$Pair"))))
                                      (13 (return))
                                      (endofcode 14))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getReference"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "pair" "java.util.concurrent.atomic.AtomicStampedReference" (class "java.util.concurrent.atomic.AtomicStampedReference$Pair"))))
                                      (4 (getfield (fieldCP "reference" "java.util.concurrent.atomic.AtomicStampedReference$Pair" (class "java.lang.Object"))))
                                      (7 (areturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getStamp"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "pair" "java.util.concurrent.atomic.AtomicStampedReference" (class "java.util.concurrent.atomic.AtomicStampedReference$Pair"))))
                                      (4 (getfield (fieldCP "stamp" "java.util.concurrent.atomic.AtomicStampedReference$Pair" int)))
                                      (7 (ireturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "get"
                              (parameters (array int))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 17)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "pair" "java.util.concurrent.atomic.AtomicStampedReference" (class "java.util.concurrent.atomic.AtomicStampedReference$Pair"))))
                                      (4 (astore_2))
                                      (5 (aload_1))
                                      (6 (iconst_0))
                                      (7 (aload_2))
                                      (8 (getfield (fieldCP "stamp" "java.util.concurrent.atomic.AtomicStampedReference$Pair" int)))
                                      (11 (iastore))
                                      (12 (aload_2))
                                      (13 (getfield (fieldCP "reference" "java.util.concurrent.atomic.AtomicStampedReference$Pair" (class "java.lang.Object"))))
                                      (16 (areturn))
                                      (endofcode 17))
                                   (Exceptions )
                                   (StackMap )))
                        (method "weakCompareAndSet"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") int int)
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 5) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (aload_2))
                                      (3 (iload_3))
                                      (4 (iload 4))
                                      (6 (invokevirtual
					(methodCP "compareAndSet" "java.util.concurrent.atomic.AtomicStampedReference" ((class "java.lang.Object") (class "java.lang.Object") int int) boolean)))
                                      (9 (ireturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "compareAndSet"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") int int)
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 6) (code_length . 64)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "pair" "java.util.concurrent.atomic.AtomicStampedReference" (class "java.util.concurrent.atomic.AtomicStampedReference$Pair")))) 
                                      (4 (astore 5)) 
                                      (6 (aload_1)) 
                                      (7 (aload 5)) 
                                      (9 (getfield (fieldCP "reference" "java.util.concurrent.atomic.AtomicStampedReference$Pair" (class "java.lang.Object")))) 
                                      (12 (if_acmpne 62)) ;;to TAG_0
                                      (15 (iload_3)) 
                                      (16 (aload 5)) 
                                      (18 (getfield (fieldCP "stamp" "java.util.concurrent.atomic.AtomicStampedReference$Pair" int))) 
                                      (21 (if_icmpne 62)) ;;to TAG_0
                                      (24 (aload_2)) 
                                      (25 (aload 5)) 
                                      (27 (getfield (fieldCP "reference" "java.util.concurrent.atomic.AtomicStampedReference$Pair" (class "java.lang.Object")))) 
                                      (30 (if_acmpne 43)) ;;to TAG_1
                                      (33 (iload 4)) 
                                      (35 (aload 5)) 
                                      (37 (getfield (fieldCP "stamp" "java.util.concurrent.atomic.AtomicStampedReference$Pair" int))) 
                                      (40 (if_icmpeq 58))  ;;to TAG_2
                                      (43 (aload_0)) ;;at TAG_1
                                      (44 (aload 5)) 
                                      (46 (aload_2)) 
                                      (47 (iload 4)) 
                                      (49 (invokestatic (methodCP "of" "java.util.concurrent.atomic.AtomicStampedReference$Pair" ((class "java.lang.Object") int) (class "java.util.concurrent.atomic.AtomicStampedReference$Pair")))) 
                                      (52 (invokespecial (methodCP "casPair" "java.util.concurrent.atomic.AtomicStampedReference" ((class "java.util.concurrent.atomic.AtomicStampedReference$Pair") (class "java.util.concurrent.atomic.AtomicStampedReference$Pair")) boolean))) 
                                      (55 (ifeq 62)) ;;to TAG_0
                                      (58 (iconst_1)) ;;at TAG_2
                                      (59 (goto 63)) ;;to TAG_3
                                      (62 (iconst_0)) ;;at TAG_0
                                      (63 (ireturn)) ;;at TAG_3
                                      (endofcode 64))
                                   (Exceptions )
                                   (StackMap )))
                        (method "set"
                              (parameters (class "java.lang.Object") int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 4) (code_length . 31)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "pair" "java.util.concurrent.atomic.AtomicStampedReference" (class "java.util.concurrent.atomic.AtomicStampedReference$Pair")))) 
                                      (4 (astore_3)) 
                                      (5 (aload_1)) 
                                      (6 (aload_3)) 
                                      (7 (getfield (fieldCP "reference" "java.util.concurrent.atomic.AtomicStampedReference$Pair" (class "java.lang.Object")))) 
                                      (10 (if_acmpne 21))  ;;to TAG_0
                                      (13 (iload_2)) 
                                      (14 (aload_3)) 
                                      (15 (getfield (fieldCP "stamp" "java.util.concurrent.atomic.AtomicStampedReference$Pair" int))) 
                                      (18 (if_icmpeq 30)) ;;to TAG_1
                                      (21 (aload_0)) ;;at TAG_0
                                      (22 (aload_1)) 
                                      (23 (iload_2)) 
                                      (24 (invokestatic (methodCP "of" "java.util.concurrent.atomic.AtomicStampedReference$Pair" ((class "java.lang.Object") int) (class "java.util.concurrent.atomic.AtomicStampedReference$Pair")))) 
                                      (27 (putfield (fieldCP "pair" "java.util.concurrent.atomic.AtomicStampedReference" (class "java.util.concurrent.atomic.AtomicStampedReference$Pair")))) 
                                      (30 (return)) ;;at TAG_1
                                      (endofcode 31))
                                   (Exceptions )
                                   (StackMap )))
                        (method "attemptStamp"
                              (parameters (class "java.lang.Object") int)
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 4) (code_length . 40)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "pair" "java.util.concurrent.atomic.AtomicStampedReference" (class "java.util.concurrent.atomic.AtomicStampedReference$Pair")))) 
                                      (4 (astore_3)) 
                                      (5 (aload_1)) 
                                      (6 (aload_3)) 
                                      (7 (getfield (fieldCP "reference" "java.util.concurrent.atomic.AtomicStampedReference$Pair" (class "java.lang.Object")))) 
                                      (10 (if_acmpne 38)) ;;to TAG_0
                                      (13 (iload_2)) 
                                      (14 (aload_3)) 
                                      (15 (getfield (fieldCP "stamp" "java.util.concurrent.atomic.AtomicStampedReference$Pair" int))) 
                                      (18 (if_icmpeq 34)) ;;to TAG_1
                                      (21 (aload_0)) 
                                      (22 (aload_3)) 
                                      (23 (aload_1)) 
                                      (24 (iload_2)) 
                                      (25 (invokestatic (methodCP "of" "java.util.concurrent.atomic.AtomicStampedReference$Pair" ((class "java.lang.Object") int) (class "java.util.concurrent.atomic.AtomicStampedReference$Pair")))) 
                                      (28 (invokespecial (methodCP "casPair" "java.util.concurrent.atomic.AtomicStampedReference" ((class "java.util.concurrent.atomic.AtomicStampedReference$Pair") (class "java.util.concurrent.atomic.AtomicStampedReference$Pair")) boolean))) 
                                      (31 (ifeq 38)) ;;to TAG_0
                                      (34 (iconst_1)) ;;at TAG_1
                                      (35 (goto 39))  ;;to TAG_2
                                      (38 (iconst_0)) ;;at TAG_0
                                      (39 (ireturn)) ;;at TAG_2
                                      (endofcode 40))
                                   (Exceptions )
                                   (StackMap )))
                        (method "casPair"
                              (parameters (class "java.util.concurrent.atomic.AtomicStampedReference$Pair") (class "java.util.concurrent.atomic.AtomicStampedReference$Pair"))
                              (returntype . boolean)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 6) (max_locals . 3) (code_length . 13)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "UNSAFE" "java.util.concurrent.atomic.AtomicStampedReference" (class "sun.misc.Unsafe"))))
                                      (3 (aload_0))
                                      (4 (getstatic (fieldCP "pairOffset" "java.util.concurrent.atomic.AtomicStampedReference" long)))
                                      (7 (aload_1))
                                      (8 (aload_2))
                                      (9 (invokevirtual
					(methodCP "compareAndSwapObject" "sun.misc.Unsafe" ((class "java.lang.Object") long (class "java.lang.Object") (class "java.lang.Object")) boolean)))
                                      (12 (ireturn))
                                      (endofcode 13))
                                   (Exceptions )
                                   (StackMap )))
                        (method "objectFieldOffset"
                              (parameters (class "sun.misc.Unsafe") (class "java.lang.String") (class "java.lang.Class"))
                              (returntype . long)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 5) (code_length . 31)
                                   (parsedcode
                                      (0 (aload_0)) ;;at TAG_0
                                      (1 (aload_2)) 
                                      (2 (aload_1)) 
                                      (3 (invokevirtual (methodCP "getDeclaredField" "java.lang.Class" ((class "java.lang.String")) (class "java.lang.reflect.Field")))) 
                                      (6 (invokevirtual (methodCP "objectFieldOffset" "sun.misc.Unsafe" ((class "java.lang.reflect.Field")) long))) 
                                      (9 (lreturn)) ;;at TAG_1
                                      (10 (astore_3)) ;;at TAG_2
                                      (11 (new (class "java.lang.NoSuchFieldError"))) 
                                      (14 (dup)) 
                                      (15 (aload_1)) 
                                      (16 (invokespecial (methodCP "<init>" "java.lang.NoSuchFieldError" ((class "java.lang.String")) void))) 
                                      (19 (astore 4)) 
                                      (21 (aload 4)) 
                                      (23 (aload_3)) 
                                      (24 (invokevirtual (methodCP "initCause" "java.lang.NoSuchFieldError" ((class "java.lang.Throwable")) (class "java.lang.Throwable")))) 
                                      (27 (pop)) 
                                      (28 (aload 4)) 
                                      (30 (athrow)) 
                                      (endofcode 31))
                                   (Exceptions 
                                     (handler 0 9  10 (class "java.lang.NoSuchFieldException")))
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 0) (code_length . 21)
                                   (parsedcode
                                      (0 (invokestatic
					(methodCP "getUnsafe" "sun.misc.Unsafe" () (class "sun.misc.Unsafe"))))
                                      (3 (putstatic (fieldCP "UNSAFE" "java.util.concurrent.atomic.AtomicStampedReference" (class "sun.misc.Unsafe"))))
                                      (6 (getstatic (fieldCP "UNSAFE" "java.util.concurrent.atomic.AtomicStampedReference" (class "sun.misc.Unsafe"))))
                                      (9 (ldc 0))         ;;STRING:: "pair"
                                      (11 (ldc_w ))
                                      (14 (invokestatic
					(methodCP "objectFieldOffset" "java.util.concurrent.atomic.AtomicStampedReference" ((class "sun.misc.Unsafe") (class "java.lang.String") (class "java.lang.Class")) long)))
                                      (17 (putstatic (fieldCP "pairOffset" "java.util.concurrent.atomic.AtomicStampedReference" long)))
                                      (20 (return))
                                      (endofcode 21))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *AtomicStampedReference-class-table*
  (make-static-class-decls 
   *java.util.concurrent.atomic.AtomicStampedReference*))

(defconst *package-name-map* 
  ("java.util.concurrent.atomic.AtomicStampedReference" . "java.util.concurrent.atomic"))
