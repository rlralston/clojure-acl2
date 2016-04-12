; Channels$ReadableByteChannelImpl-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:38 CDT 2014.
;

(defconst *java.nio.channels.Channels$ReadableByteChannelImpl*
 (make-class-def
      '(class "java.nio.channels.Channels$ReadableByteChannelImpl"
            "java.nio.channels.spi.AbstractInterruptibleChannel"
            (constant_pool
                        (INT 8192))
            (fields
                        (field "in" (class "java.io.InputStream") (accessflags  *class* ) -1)
                        (field "TRANSFER_SIZE" int (accessflags  *class*  *final*  *private*  *static* ) 0)
                        (field "buf" (array byte) (accessflags  *class*  *private* ) -1)
                        (field "open" boolean (accessflags  *class*  *private* ) -1)
                        (field "readLock" (class "java.lang.Object") (accessflags  *class*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.io.InputStream"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 33)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.nio.channels.spi.AbstractInterruptibleChannel" () void)))
                                      (4 (aload_0))
                                      (5 (iconst_0))
                                      (6 (newarray BYTE))
                                      (8 (putfield (fieldCP "buf" "java.nio.channels.Channels$ReadableByteChannelImpl" (array byte))))
                                      (11 (aload_0))
                                      (12 (iconst_1))
                                      (13 (putfield (fieldCP "open" "java.nio.channels.Channels$ReadableByteChannelImpl" boolean)))
                                      (16 (aload_0))
                                      (17 (new (class "java.lang.Object")))
                                      (20 (dup))
                                      (21 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (24 (putfield (fieldCP "readLock" "java.nio.channels.Channels$ReadableByteChannelImpl" (class "java.lang.Object"))))
                                      (27 (aload_0))
                                      (28 (aload_1))
                                      (29 (putfield (fieldCP "in" "java.nio.channels.Channels$ReadableByteChannelImpl" (class "java.io.InputStream"))))
                                      (32 (return))
                                      (endofcode 33))
                                   (Exceptions )
                                   (StackMap )))
                        (method "read"
                              (parameters (class "java.nio.ByteBuffer"))
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 9) (code_length . 180)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (invokevirtual (methodCP "remaining" "java.nio.ByteBuffer" () int))) 
                                      (4 (istore_2)) 
                                      (5 (iconst_0)) 
                                      (6 (istore_3)) 
                                      (7 (iconst_0)) 
                                      (8 (istore 4)) 
                                      (10 (aload_0)) 
                                      (11 (getfield (fieldCP "readLock" "java.nio.channels.Channels$ReadableByteChannelImpl" (class "java.lang.Object")))) 
                                      (14 (dup)) 
                                      (15 (astore 5)) 
                                      (17 (monitorenter)) 
                                      (18 (iload_3)) ;;at TAG_9
                                      (19 (iload_2)) 
                                      (20 (if_icmpge 153)) ;;to TAG_0
                                      (23 (iload_2)) 
                                      (24 (iload_3)) 
                                      (25 (isub)) 
                                      (26 (sipush 8192)) 
                                      (29 (invokestatic (methodCP "min" "java.lang.Math" (int int) int))) 
                                      (32 (istore 6)) 
                                      (34 (aload_0)) 
                                      (35 (getfield (fieldCP "buf" "java.nio.channels.Channels$ReadableByteChannelImpl" (array byte)))) 
                                      (38 (arraylength)) 
                                      (39 (iload 6)) 
                                      (41 (if_icmpge 52)) ;;to TAG_1
                                      (44 (aload_0)) 
                                      (45 (iload 6)) 
                                      (47 (newarray BYTE)) 
                                      (49 (putfield (fieldCP "buf" "java.nio.channels.Channels$ReadableByteChannelImpl" (array byte)))) 
                                      (52 (iload_3)) ;;at TAG_1
                                      (53 (ifle 69)) ;;to TAG_2
                                      (56 (aload_0)) 
                                      (57 (getfield (fieldCP "in" "java.nio.channels.Channels$ReadableByteChannelImpl" (class "java.io.InputStream")))) 
                                      (60 (invokevirtual (methodCP "available" "java.io.InputStream" () int))) 
                                      (63 (ifgt 69)) ;;to TAG_2
                                      (66 (goto 153)) ;;to TAG_0
                                      (69 (aload_0)) ;;at TAG_2
                                      (70 (invokevirtual (methodCP "begin" "java.nio.channels.Channels$ReadableByteChannelImpl" () void))) 
                                      (73 (aload_0)) 
                                      (74 (getfield (fieldCP "in" "java.nio.channels.Channels$ReadableByteChannelImpl" (class "java.io.InputStream")))) 
                                      (77 (aload_0)) 
                                      (78 (getfield (fieldCP "buf" "java.nio.channels.Channels$ReadableByteChannelImpl" (array byte)))) 
                                      (81 (iconst_0)) 
                                      (82 (iload 6)) 
                                      (84 (invokevirtual (methodCP "read" "java.io.InputStream" ((array byte) int int) int))) 
                                      (87 (istore 4)) 
                                      (89 (aload_0)) ;;at TAG_11
                                      (90 (iload 4)) 
                                      (92 (ifle 99)) ;;to TAG_3
                                      (95 (iconst_1)) 
                                      (96 (goto 100)) ;;to TAG_4
                                      (99 (iconst_0)) ;;at TAG_3
                                      (100 (invokevirtual (methodCP "end" "java.nio.channels.Channels$ReadableByteChannelImpl" (boolean) void))) ;;at TAG_4
                                      (103 (goto 125)) ;;to TAG_5
                                      (106 (astore 7)) ;;at TAG_12
                                      (108 (aload_0)) ;;at TAG_13
                                      (109 (iload 4)) 
                                      (111 (ifle 118)) ;;to TAG_6
                                      (114 (iconst_1)) 
                                      (115 (goto 119)) ;;to TAG_7
                                      (118 (iconst_0)) ;;at TAG_6
                                      (119 (invokevirtual (methodCP "end" "java.nio.channels.Channels$ReadableByteChannelImpl" (boolean) void))) ;;at TAG_7
                                      (122 (aload 7)) 
                                      (124 (athrow)) 
                                      (125 (iload 4)) ;;at TAG_5
                                      (127 (ifge 133)) ;;to TAG_8
                                      (130 (goto 153)) ;;to TAG_0
                                      (133 (iload_3)) ;;at TAG_8
                                      (134 (iload 4)) 
                                      (136 (iadd)) 
                                      (137 (istore_3)) 
                                      (138 (aload_1)) 
                                      (139 (aload_0)) 
                                      (140 (getfield (fieldCP "buf" "java.nio.channels.Channels$ReadableByteChannelImpl" (array byte)))) 
                                      (143 (iconst_0)) 
                                      (144 (iload 4)) 
                                      (146 (invokevirtual (methodCP "put" "java.nio.ByteBuffer" ((array byte) int int) (class "java.nio.ByteBuffer")))) 
                                      (149 (pop)) 
                                      (150 (goto 18)) ;;to TAG_9
                                      (153 (iload 4)) ;;at TAG_0
                                      (155 (ifge 167)) ;;to TAG_10
                                      (158 (iload_3)) 
                                      (159 (ifne 167)) ;;to TAG_10
                                      (162 (iconst_m1)) 
                                      (163 (aload 5)) 
                                      (165 (monitorexit)) 
                                      (166 (ireturn)) ;;at TAG_14
                                      (167 (iload_3)) ;;at TAG_10
                                      (168 (aload 5)) 
                                      (170 (monitorexit)) 
                                      (171 (ireturn)) ;;at TAG_16
                                      (172 (astore 8)) ;;at TAG_15
                                      (174 (aload 5)) 
                                      (176 (monitorexit)) 
                                      (177 (aload 8)) ;;at TAG_17
                                      (179 (athrow)) 
                                      (endofcode 180))
                                   (Exceptions 
                                     (handler 69 89  106 (class "java.lang.Throwable"))
                                     (handler 106 108  106 (class "java.lang.Throwable"))
                                     (handler 18 166  172 (class "java.lang.Throwable"))
                                     (handler 167 171  172 (class "java.lang.Throwable"))
                                     (handler 172 177  172 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "implCloseChannel"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 13)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "in" "java.nio.channels.Channels$ReadableByteChannelImpl" (class "java.io.InputStream"))))
                                      (4 (invokevirtual
					(methodCP "close" "java.io.InputStream" () void)))
                                      (7 (aload_0))
                                      (8 (iconst_0))
                                      (9 (putfield (fieldCP "open" "java.nio.channels.Channels$ReadableByteChannelImpl" boolean)))
                                      (12 (return))
                                      (endofcode 13))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.nio.channels.ReadableByteChannel")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *Channels$ReadableByteChannelImpl-class-table*
  (make-static-class-decls 
   *java.nio.channels.Channels$ReadableByteChannelImpl*))

(defconst *package-name-map* 
  ("java.nio.channels.Channels$ReadableByteChannelImpl" . "java.nio.channels"))
