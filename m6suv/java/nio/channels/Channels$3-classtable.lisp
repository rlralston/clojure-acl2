; Channels$3-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:38 CDT 2014.
;

(defconst *java.nio.channels.Channels$3*
 (make-class-def
      '(class "java.nio.channels.Channels$3"
            "java.io.OutputStream"
            (constant_pool)
            (fields
                        (field "bb" (class "java.nio.ByteBuffer") (accessflags  *class*  *private* ) -1)
                        (field "bs" (array byte) (accessflags  *class*  *private* ) -1)
                        (field "b1" (array byte) (accessflags  *class*  *private* ) -1)
                        (field "val$ch" (class "java.nio.channels.AsynchronousByteChannel") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.nio.channels.AsynchronousByteChannel"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 25)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "val$ch" "java.nio.channels.Channels$3" (class "java.nio.channels.AsynchronousByteChannel"))))
                                      (5 (aload_0))
                                      (6 (invokespecial
					(methodCP "<init>" "java.io.OutputStream" () void)))
                                      (9 (aload_0))
                                      (10 (aconst_null))
                                      (11 (putfield (fieldCP "bb" "java.nio.channels.Channels$3" (class "java.nio.ByteBuffer"))))
                                      (14 (aload_0))
                                      (15 (aconst_null))
                                      (16 (putfield (fieldCP "bs" "java.nio.channels.Channels$3" (array byte))))
                                      (19 (aload_0))
                                      (20 (aconst_null))
                                      (21 (putfield (fieldCP "b1" "java.nio.channels.Channels$3" (array byte))))
                                      (24 (return))
                                      (endofcode 25))
                                   (Exceptions )
                                   (StackMap )))
                        (method "write"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *class*  *public*  *super*  *synchronized* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 31)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "b1" "java.nio.channels.Channels$3" (array byte)))) 
                                      (4 (ifnonnull 14))  ;;to TAG_0
                                      (7 (aload_0)) 
                                      (8 (iconst_1)) 
                                      (9 (newarray BYTE)) 
                                      (11 (putfield (fieldCP "b1" "java.nio.channels.Channels$3" (array byte)))) 
                                      (14 (aload_0)) ;;at TAG_0
                                      (15 (getfield (fieldCP "b1" "java.nio.channels.Channels$3" (array byte)))) 
                                      (18 (iconst_0)) 
                                      (19 (iload_1)) 
                                      (20 (i2b)) 
                                      (21 (bastore)) 
                                      (22 (aload_0)) 
                                      (23 (aload_0)) 
                                      (24 (getfield (fieldCP "b1" "java.nio.channels.Channels$3" (array byte)))) 
                                      (27 (invokevirtual (methodCP "write" "java.nio.channels.Channels$3" ((array byte)) void))) 
                                      (30 (return)) 
                                      (endofcode 31))
                                   (Exceptions )
                                   (StackMap )))
                        (method "write"
                              (parameters (array byte) int int)
                              (returntype . void)
                              (accessflags  *class*  *public*  *super*  *synchronized* )
                              (code
                                   (max_stack . 3) (max_locals . 8) (code_length . 182)
                                   (parsedcode
                                      (0 (iload_2)) 
                                      (1 (iflt 28)) ;;to TAG_0
                                      (4 (iload_2)) 
                                      (5 (aload_1)) 
                                      (6 (arraylength)) 
                                      (7 (if_icmpgt 28)) ;;to TAG_0
                                      (10 (iload_3)) 
                                      (11 (iflt 28)) ;;to TAG_0
                                      (14 (iload_2)) 
                                      (15 (iload_3)) 
                                      (16 (iadd)) 
                                      (17 (aload_1)) 
                                      (18 (arraylength)) 
                                      (19 (if_icmpgt 28)) ;;to TAG_0
                                      (22 (iload_2)) 
                                      (23 (iload_3)) 
                                      (24 (iadd)) 
                                      (25 (ifge 36))  ;;to TAG_1
                                      (28 (new (class "java.lang.IndexOutOfBoundsException"))) ;;at TAG_0
                                      (31 (dup)) 
                                      (32 (invokespecial (methodCP "<init>" "java.lang.IndexOutOfBoundsException" () void))) 
                                      (35 (athrow)) 
                                      (36 (iload_3)) ;;at TAG_1
                                      (37 (ifne 41)) ;;to TAG_2
                                      (40 (return)) 
                                      (41 (aload_0)) ;;at TAG_2
                                      (42 (getfield (fieldCP "bs" "java.nio.channels.Channels$3" (array byte)))) 
                                      (45 (aload_1)) 
                                      (46 (if_acmpne 56)) ;;to TAG_3
                                      (49 (aload_0)) 
                                      (50 (getfield (fieldCP "bb" "java.nio.channels.Channels$3" (class "java.nio.ByteBuffer")))) 
                                      (53 (goto 60)) ;;to TAG_4
                                      (56 (aload_1)) ;;at TAG_3
                                      (57 (invokestatic (methodCP "wrap" "java.nio.ByteBuffer" ((array byte)) (class "java.nio.ByteBuffer")))) 
                                      (60 (astore 4)) ;;at TAG_4
                                      (62 (aload 4)) 
                                      (64 (iload_2)) 
                                      (65 (iload_3)) 
                                      (66 (iadd)) 
                                      (67 (aload 4)) 
                                      (69 (invokevirtual (methodCP "capacity" "java.nio.ByteBuffer" () int))) 
                                      (72 (invokestatic (methodCP "min" "java.lang.Math" (int int) int))) 
                                      (75 (invokevirtual (methodCP "limit" "java.nio.ByteBuffer" (int) (class "java.nio.Buffer")))) 
                                      (78 (pop)) 
                                      (79 (aload 4)) 
                                      (81 (iload_2)) 
                                      (82 (invokevirtual (methodCP "position" "java.nio.ByteBuffer" (int) (class "java.nio.Buffer")))) 
                                      (85 (pop)) 
                                      (86 (aload_0)) 
                                      (87 (aload 4)) 
                                      (89 (putfield (fieldCP "bb" "java.nio.channels.Channels$3" (class "java.nio.ByteBuffer")))) 
                                      (92 (aload_0)) 
                                      (93 (aload_1)) 
                                      (94 (putfield (fieldCP "bs" "java.nio.channels.Channels$3" (array byte)))) 
                                      (97 (iconst_0)) 
                                      (98 (istore 5)) 
                                      (100 (aload 4)) ;;at TAG_6
                                      (102 (invokevirtual (methodCP "remaining" "java.nio.ByteBuffer" () int))) 
                                      (105 (ifle 151)) ;;to TAG_5
                                      (108 (aload_0)) ;;at TAG_9
                                      (109 (getfield (fieldCP "val$ch" "java.nio.channels.Channels$3" (class "java.nio.channels.AsynchronousByteChannel")))) 
                                      (112 (aload 4)) 
                                      (114 (invokeinterface (methodCP "write" "java.nio.channels.AsynchronousByteChannel" ((class "java.nio.ByteBuffer")) (class "java.util.concurrent.Future")) 2)) 
                                      (119 (invokeinterface (methodCP "get" "java.util.concurrent.Future" () (class "java.lang.Object")) 1)) 
                                      (124 (pop)) 
                                      (125 (goto 100)) ;;to TAG_6;;at TAG_10
                                      (128 (astore 6)) ;;at TAG_11
                                      (130 (new (class "java.io.IOException"))) 
                                      (133 (dup)) 
                                      (134 (aload 6)) 
                                      (136 (invokevirtual (methodCP "getCause" "java.util.concurrent.ExecutionException" () (class "java.lang.Throwable")))) 
                                      (139 (invokespecial (methodCP "<init>" "java.io.IOException" ((class "java.lang.Throwable")) void))) 
                                      (142 (athrow)) 
                                      (143 (astore 6)) ;;at TAG_12
                                      (145 (iconst_1)) 
                                      (146 (istore 5)) 
                                      (148 (goto 100)) ;;to TAG_6
                                      (151 (iload 5)) ;;at TAG_5
                                      (153 (ifeq 181)) ;;to TAG_7
                                      (156 (invokestatic (methodCP "currentThread" "java.lang.Thread" () (class "java.lang.Thread")))) 
                                      (159 (invokevirtual (methodCP "interrupt" "java.lang.Thread" () void))) 
                                      (162 (goto 181)) ;;to TAG_7
                                      (165 (astore 7)) ;;at TAG_13
                                      (167 (iload 5)) ;;at TAG_14
                                      (169 (ifeq 178)) ;;to TAG_8
                                      (172 (invokestatic (methodCP "currentThread" "java.lang.Thread" () (class "java.lang.Thread")))) 
                                      (175 (invokevirtual (methodCP "interrupt" "java.lang.Thread" () void))) 
                                      (178 (aload 7)) ;;at TAG_8
                                      (180 (athrow)) 
                                      (181 (return)) ;;at TAG_7
                                      (endofcode 182))
                                   (Exceptions 
                                     (handler 108 125  128 (class "java.util.concurrent.ExecutionException"))
                                     (handler 108 125  143 (class "java.lang.InterruptedException"))
                                     (handler 100 151  165 (class "java.lang.Throwable"))
                                     (handler 165 167  165 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "close"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "val$ch" "java.nio.channels.Channels$3" (class "java.nio.channels.AsynchronousByteChannel"))))
                                      (4 (invokeinterface
					(methodCP "close" "java.nio.channels.AsynchronousByteChannel" () void) 1))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "InnerClasses")))))


(defconst *Channels$3-class-table*
  (make-static-class-decls 
   *java.nio.channels.Channels$3*))

(defconst *package-name-map* 
  ("java.nio.channels.Channels$3" . "java.nio.channels"))

