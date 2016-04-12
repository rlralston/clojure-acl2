; GZIPInputStream-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:48 CDT 2014.
;

(defconst *java.util.zip.GZIPInputStream*
 (make-class-def
      '(class "java.util.zip.GZIPInputStream"
            "java.util.zip.InflaterInputStream"
            (constant_pool
                        (INT 35615)
                        (INT 1)
                        (INT 2)
                        (INT 4)
                        (INT 8)
                        (INT 16)
                        (STRING  "Stream closed")
                        (STRING  "Not in GZIP format")
                        (STRING  "Unsupported compression method")
                        (INT 65535)
                        (STRING  "Corrupt GZIP header")
                        (LONG 4294967295)
                        (STRING  "Corrupt GZIP trailer")
                        (STRING  ".read() returned value out of range -1..255: "))
            (fields
                        (field "crc" (class "java.util.zip.CRC32") (accessflags  *class*  *protected* ) -1)
                        (field "eos" boolean (accessflags  *class*  *protected* ) -1)
                        (field "closed" boolean (accessflags  *class*  *private* ) -1)
                        (field "GZIP_MAGIC" int (accessflags  *class*  *final*  *public*  *static* ) 0)
                        (field "FTEXT" int (accessflags  *class*  *final*  *private*  *static* ) 1)
                        (field "FHCRC" int (accessflags  *class*  *final*  *private*  *static* ) 2)
                        (field "FEXTRA" int (accessflags  *class*  *final*  *private*  *static* ) 3)
                        (field "FNAME" int (accessflags  *class*  *final*  *private*  *static* ) 4)
                        (field "FCOMMENT" int (accessflags  *class*  *final*  *private*  *static* ) 5)
                        (field "tmpbuf" (array byte) (accessflags  *class*  *private* ) -1))
            (methods
                        (method "ensureOpen"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 18)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "closed" "java.util.zip.GZIPInputStream" boolean))) 
                                      (4 (ifeq 17))  ;;to TAG_0
                                      (7 (new (class "java.io.IOException"))) 
                                      (10 (dup)) 
                                      (11 (ldc 6)) ;;STRING:: "Stream closed"
                                      (13 (invokespecial (methodCP "<init>" "java.io.IOException" ((class "java.lang.String")) void))) 
                                      (16 (athrow)) 
                                      (17 (return)) ;;at TAG_0
                                      (endofcode 18))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.io.InputStream") int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 3) (code_length . 51)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (new (class "java.util.zip.Inflater")))
                                      (5 (dup))
                                      (6 (iconst_1))
                                      (7 (invokespecial
					(methodCP "<init>" "java.util.zip.Inflater" (boolean) void)))
                                      (10 (iload_2))
                                      (11 (invokespecial
					(methodCP "<init>" "java.util.zip.InflaterInputStream" ((class "java.io.InputStream") (class "java.util.zip.Inflater") int) void)))
                                      (14 (aload_0))
                                      (15 (new (class "java.util.zip.CRC32")))
                                      (18 (dup))
                                      (19 (invokespecial
					(methodCP "<init>" "java.util.zip.CRC32" () void)))
                                      (22 (putfield (fieldCP "crc" "java.util.zip.GZIPInputStream" (class "java.util.zip.CRC32"))))
                                      (25 (aload_0))
                                      (26 (iconst_0))
                                      (27 (putfield (fieldCP "closed" "java.util.zip.GZIPInputStream" boolean)))
                                      (30 (aload_0))
                                      (31 (sipush 128))
                                      (34 (newarray BYTE))
                                      (36 (putfield (fieldCP "tmpbuf" "java.util.zip.GZIPInputStream" (array byte))))
                                      (39 (aload_0))
                                      (40 (iconst_1))
                                      (41 (putfield (fieldCP "usesDefaultInflater" "java.util.zip.GZIPInputStream" boolean)))
                                      (44 (aload_0))
                                      (45 (aload_1))
                                      (46 (invokespecial
					(methodCP "readHeader" "java.util.zip.GZIPInputStream" ((class "java.io.InputStream")) int)))
                                      (49 (pop))
                                      (50 (return))
                                      (endofcode 51))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.io.InputStream"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (sipush 512))
                                      (5 (invokespecial
					(methodCP "<init>" "java.util.zip.GZIPInputStream" ((class "java.io.InputStream") int) void)))
                                      (8 (return))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "read"
                              (parameters (array byte) int int)
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 5) (code_length . 65)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokespecial (methodCP "ensureOpen" "java.util.zip.GZIPInputStream" () void))) 
                                      (4 (aload_0)) 
                                      (5 (getfield (fieldCP "eos" "java.util.zip.GZIPInputStream" boolean))) 
                                      (8 (ifeq 13)) ;;to TAG_0
                                      (11 (iconst_m1)) 
                                      (12 (ireturn)) 
                                      (13 (aload_0)) ;;at TAG_0
                                      (14 (aload_1)) 
                                      (15 (iload_2)) 
                                      (16 (iload_3)) 
                                      (17 (invokespecial (methodCP "read" "java.util.zip.InflaterInputStream" ((array byte) int int) int))) 
                                      (20 (istore 4)) 
                                      (22 (iload 4)) 
                                      (24 (iconst_m1)) 
                                      (25 (if_icmpne 51)) ;;to TAG_1
                                      (28 (aload_0)) 
                                      (29 (invokespecial (methodCP "readTrailer" "java.util.zip.GZIPInputStream" () boolean))) 
                                      (32 (ifeq 43))  ;;to TAG_2
                                      (35 (aload_0)) 
                                      (36 (iconst_1)) 
                                      (37 (putfield (fieldCP "eos" "java.util.zip.GZIPInputStream" boolean))) 
                                      (40 (goto 62)) ;;to TAG_3
                                      (43 (aload_0)) ;;at TAG_2
                                      (44 (aload_1)) 
                                      (45 (iload_2)) 
                                      (46 (iload_3)) 
                                      (47 (invokevirtual (methodCP "read" "java.util.zip.GZIPInputStream" ((array byte) int int) int))) 
                                      (50 (ireturn)) 
                                      (51 (aload_0)) ;;at TAG_1
                                      (52 (getfield (fieldCP "crc" "java.util.zip.GZIPInputStream" (class "java.util.zip.CRC32")))) 
                                      (55 (aload_1)) 
                                      (56 (iload_2)) 
                                      (57 (iload 4)) 
                                      (59 (invokevirtual (methodCP "update" "java.util.zip.CRC32" ((array byte) int int) void))) 
                                      (62 (iload 4)) ;;at TAG_3
                                      (64 (ireturn)) 
                                      (endofcode 65))
                                   (Exceptions )
                                   (StackMap )))
                        (method "close"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 22)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "closed" "java.util.zip.GZIPInputStream" boolean))) 
                                      (4 (ifne 21))  ;;to TAG_0
                                      (7 (aload_0)) 
                                      (8 (invokespecial (methodCP "close" "java.util.zip.InflaterInputStream" () void))) 
                                      (11 (aload_0)) 
                                      (12 (iconst_1)) 
                                      (13 (putfield (fieldCP "eos" "java.util.zip.GZIPInputStream" boolean))) 
                                      (16 (aload_0)) 
                                      (17 (iconst_1)) 
                                      (18 (putfield (fieldCP "closed" "java.util.zip.GZIPInputStream" boolean))) 
                                      (21 (return)) ;;at TAG_0
                                      (endofcode 22))
                                   (Exceptions )
                                   (StackMap )))
                        (method "readHeader"
                              (parameters (class "java.io.InputStream"))
                              (returntype . int)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 4) (max_locals . 6) (code_length . 200)
                                   (parsedcode
                                      (0 (new (class "java.util.zip.CheckedInputStream"))) 
                                      (3 (dup)) 
                                      (4 (aload_1)) 
                                      (5 (aload_0)) 
                                      (6 (getfield (fieldCP "crc" "java.util.zip.GZIPInputStream" (class "java.util.zip.CRC32")))) 
                                      (9 (invokespecial (methodCP "<init>" "java.util.zip.CheckedInputStream" ((class "java.io.InputStream") (class "java.util.zip.Checksum")) void))) 
                                      (12 (astore_2)) 
                                      (13 (aload_0)) 
                                      (14 (getfield (fieldCP "crc" "java.util.zip.GZIPInputStream" (class "java.util.zip.CRC32")))) 
                                      (17 (invokevirtual (methodCP "reset" "java.util.zip.CRC32" () void))) 
                                      (20 (aload_0)) 
                                      (21 (aload_2)) 
                                      (22 (invokespecial (methodCP "readUShort" "java.util.zip.GZIPInputStream" ((class "java.io.InputStream")) int))) 
                                      (25 (ldc 0)) ;;INT:: "35615"
                                      (27 (if_icmpeq 40)) ;;to TAG_0
                                      (30 (new (class "java.util.zip.ZipException"))) 
                                      (33 (dup)) 
                                      (34 (ldc 7)) ;;STRING:: "Not in GZIP format"
                                      (36 (invokespecial (methodCP "<init>" "java.util.zip.ZipException" ((class "java.lang.String")) void))) 
                                      (39 (athrow)) 
                                      (40 (aload_0)) ;;at TAG_0
                                      (41 (aload_2)) 
                                      (42 (invokespecial (methodCP "readUByte" "java.util.zip.GZIPInputStream" ((class "java.io.InputStream")) int))) 
                                      (45 (bipush 8)) 
                                      (47 (if_icmpeq 60))  ;;to TAG_1
                                      (50 (new (class "java.util.zip.ZipException"))) 
                                      (53 (dup)) 
                                      (54 (ldc 8)) ;;STRING:: "Unsupported compression method"
                                      (56 (invokespecial (methodCP "<init>" "java.util.zip.ZipException" ((class "java.lang.String")) void))) 
                                      (59 (athrow)) 
                                      (60 (aload_0)) ;;at TAG_1
                                      (61 (aload_2)) 
                                      (62 (invokespecial (methodCP "readUByte" "java.util.zip.GZIPInputStream" ((class "java.io.InputStream")) int))) 
                                      (65 (istore_3)) 
                                      (66 (aload_0)) 
                                      (67 (aload_2)) 
                                      (68 (bipush 6)) 
                                      (70 (invokespecial (methodCP "skipBytes" "java.util.zip.GZIPInputStream" ((class "java.io.InputStream") int) void))) 
                                      (73 (bipush 10)) 
                                      (75 (istore 4)) 
                                      (77 (iload_3)) 
                                      (78 (iconst_4)) 
                                      (79 (iand)) 
                                      (80 (iconst_4)) 
                                      (81 (if_icmpne 107)) ;;to TAG_2
                                      (84 (aload_0)) 
                                      (85 (aload_2)) 
                                      (86 (invokespecial (methodCP "readUShort" "java.util.zip.GZIPInputStream" ((class "java.io.InputStream")) int))) 
                                      (89 (istore 5)) 
                                      (91 (aload_0)) 
                                      (92 (aload_2)) 
                                      (93 (iload 5)) 
                                      (95 (invokespecial (methodCP "skipBytes" "java.util.zip.GZIPInputStream" ((class "java.io.InputStream") int) void))) 
                                      (98 (iload 4)) 
                                      (100 (iload 5)) 
                                      (102 (iconst_2)) 
                                      (103 (iadd)) 
                                      (104 (iadd)) 
                                      (105 (istore 4)) 
                                      (107 (iload_3)) ;;at TAG_2
                                      (108 (bipush 8)) 
                                      (110 (iand)) 
                                      (111 (bipush 8)) 
                                      (113 (if_icmpne 127)) ;;to TAG_3
                                      (116 (iinc 4 1)) ;;at TAG_4
                                      (119 (aload_0)) 
                                      (120 (aload_2)) 
                                      (121 (invokespecial (methodCP "readUByte" "java.util.zip.GZIPInputStream" ((class "java.io.InputStream")) int))) 
                                      (124 (ifne 116)) ;;to TAG_4
                                      (127 (iload_3)) ;;at TAG_3
                                      (128 (bipush 16)) 
                                      (130 (iand)) 
                                      (131 (bipush 16)) 
                                      (133 (if_icmpne 147)) ;;to TAG_5
                                      (136 (iinc 4 1)) ;;at TAG_6
                                      (139 (aload_0)) 
                                      (140 (aload_2)) 
                                      (141 (invokespecial (methodCP "readUByte" "java.util.zip.GZIPInputStream" ((class "java.io.InputStream")) int))) 
                                      (144 (ifne 136)) ;;to TAG_6
                                      (147 (iload_3)) ;;at TAG_5
                                      (148 (iconst_2)) 
                                      (149 (iand)) 
                                      (150 (iconst_2)) 
                                      (151 (if_icmpne 190)) ;;to TAG_7
                                      (154 (aload_0)) 
                                      (155 (getfield (fieldCP "crc" "java.util.zip.GZIPInputStream" (class "java.util.zip.CRC32")))) 
                                      (158 (invokevirtual (methodCP "getValue" "java.util.zip.CRC32" () long))) 
                                      (161 (l2i)) 
                                      (162 (ldc 9)) ;;INT:: "65535"
                                      (164 (iand)) 
                                      (165 (istore 5)) 
                                      (167 (aload_0)) 
                                      (168 (aload_2)) 
                                      (169 (invokespecial (methodCP "readUShort" "java.util.zip.GZIPInputStream" ((class "java.io.InputStream")) int))) 
                                      (172 (iload 5)) 
                                      (174 (if_icmpeq 187)) ;;to TAG_8
                                      (177 (new (class "java.util.zip.ZipException"))) 
                                      (180 (dup)) 
                                      (181 (ldc 10)) ;;STRING:: "Corrupt GZIP header"
                                      (183 (invokespecial (methodCP "<init>" "java.util.zip.ZipException" ((class "java.lang.String")) void))) 
                                      (186 (athrow)) 
                                      (187 (iinc 4 2)) ;;at TAG_8
                                      (190 (aload_0)) ;;at TAG_7
                                      (191 (getfield (fieldCP "crc" "java.util.zip.GZIPInputStream" (class "java.util.zip.CRC32")))) 
                                      (194 (invokevirtual (methodCP "reset" "java.util.zip.CRC32" () void))) 
                                      (197 (iload 4)) 
                                      (199 (ireturn)) 
                                      (endofcode 200))
                                   (Exceptions )
                                   (StackMap )))
                        (method "readTrailer"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 7) (max_locals . 5) (code_length . 162)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "in" "java.util.zip.GZIPInputStream" (class "java.io.InputStream")))) 
                                      (4 (astore_1)) 
                                      (5 (aload_0)) 
                                      (6 (getfield (fieldCP "inf" "java.util.zip.GZIPInputStream" (class "java.util.zip.Inflater")))) 
                                      (9 (invokevirtual (methodCP "getRemaining" "java.util.zip.Inflater" () int))) 
                                      (12 (istore_2)) 
                                      (13 (iload_2)) 
                                      (14 (ifle 44)) ;;to TAG_0
                                      (17 (new (class "java.io.SequenceInputStream"))) 
                                      (20 (dup)) 
                                      (21 (new (class "java.io.ByteArrayInputStream"))) 
                                      (24 (dup)) 
                                      (25 (aload_0)) 
                                      (26 (getfield (fieldCP "buf" "java.util.zip.GZIPInputStream" (array byte)))) 
                                      (29 (aload_0)) 
                                      (30 (getfield (fieldCP "len" "java.util.zip.GZIPInputStream" int))) 
                                      (33 (iload_2)) 
                                      (34 (isub)) 
                                      (35 (iload_2)) 
                                      (36 (invokespecial (methodCP "<init>" "java.io.ByteArrayInputStream" ((array byte) int int) void))) 
                                      (39 (aload_1)) 
                                      (40 (invokespecial (methodCP "<init>" "java.io.SequenceInputStream" ((class "java.io.InputStream") (class "java.io.InputStream")) void))) 
                                      (43 (astore_1)) 
                                      (44 (aload_0)) ;;at TAG_0
                                      (45 (aload_1)) 
                                      (46 (invokespecial (methodCP "readUInt" "java.util.zip.GZIPInputStream" ((class "java.io.InputStream")) long))) 
                                      (49 (aload_0)) 
                                      (50 (getfield (fieldCP "crc" "java.util.zip.GZIPInputStream" (class "java.util.zip.CRC32")))) 
                                      (53 (invokevirtual (methodCP "getValue" "java.util.zip.CRC32" () long))) 
                                      (56 (lcmp)) 
                                      (57 (ifne 80))  ;;to TAG_1
                                      (60 (aload_0)) 
                                      (61 (aload_1)) 
                                      (62 (invokespecial (methodCP "readUInt" "java.util.zip.GZIPInputStream" ((class "java.io.InputStream")) long))) 
                                      (65 (aload_0)) 
                                      (66 (getfield (fieldCP "inf" "java.util.zip.GZIPInputStream" (class "java.util.zip.Inflater")))) 
                                      (69 (invokevirtual (methodCP "getBytesWritten" "java.util.zip.Inflater" () long))) 
                                      (72 (ldc2_w 11)) ;; LONG:: "4294967295"
                                      (75 (land)) 
                                      (76 (lcmp)) 
                                      (77 (ifeq 90)) ;;to TAG_2
                                      (80 (new (class "java.util.zip.ZipException"))) ;;at TAG_1
                                      (83 (dup)) 
                                      (84 (ldc 12)) ;;STRING:: "Corrupt GZIP trailer"
                                      (86 (invokespecial (methodCP "<init>" "java.util.zip.ZipException" ((class "java.lang.String")) void))) 
                                      (89 (athrow)) 
                                      (90 (aload_0)) ;;at TAG_2
                                      (91 (getfield (fieldCP "in" "java.util.zip.GZIPInputStream" (class "java.io.InputStream")))) 
                                      (94 (invokevirtual (methodCP "available" "java.io.InputStream" () int))) 
                                      (97 (ifgt 106)) ;;to TAG_3
                                      (100 (iload_2)) 
                                      (101 (bipush 26)) 
                                      (103 (if_icmple 160)) ;;to TAG_4
                                      (106 (bipush 8)) ;;at TAG_3
                                      (108 (istore_3)) 
                                      (109 (iload_3)) ;;at TAG_7
                                      (110 (aload_0)) 
                                      (111 (aload_1)) 
                                      (112 (invokespecial (methodCP "readHeader" "java.util.zip.GZIPInputStream" ((class "java.io.InputStream")) int))) 
                                      (115 (iadd)) 
                                      (116 (istore_3)) 
                                      (117 (goto 124)) ;;to TAG_5;;at TAG_8
                                      (120 (astore 4)) ;;at TAG_9
                                      (122 (iconst_1)) 
                                      (123 (ireturn)) 
                                      (124 (aload_0)) ;;at TAG_5
                                      (125 (getfield (fieldCP "inf" "java.util.zip.GZIPInputStream" (class "java.util.zip.Inflater")))) 
                                      (128 (invokevirtual (methodCP "reset" "java.util.zip.Inflater" () void))) 
                                      (131 (iload_2)) 
                                      (132 (iload_3)) 
                                      (133 (if_icmple 158)) ;;to TAG_6
                                      (136 (aload_0)) 
                                      (137 (getfield (fieldCP "inf" "java.util.zip.GZIPInputStream" (class "java.util.zip.Inflater")))) 
                                      (140 (aload_0)) 
                                      (141 (getfield (fieldCP "buf" "java.util.zip.GZIPInputStream" (array byte)))) 
                                      (144 (aload_0)) 
                                      (145 (getfield (fieldCP "len" "java.util.zip.GZIPInputStream" int))) 
                                      (148 (iload_2)) 
                                      (149 (isub)) 
                                      (150 (iload_3)) 
                                      (151 (iadd)) 
                                      (152 (iload_2)) 
                                      (153 (iload_3)) 
                                      (154 (isub)) 
                                      (155 (invokevirtual (methodCP "setInput" "java.util.zip.Inflater" ((array byte) int int) void))) 
                                      (158 (iconst_0)) ;;at TAG_6
                                      (159 (ireturn)) 
                                      (160 (iconst_1)) ;;at TAG_4
                                      (161 (ireturn)) 
                                      (endofcode 162))
                                   (Exceptions 
                                     (handler 109 117  120 (class "java.io.IOException")))
                                   (StackMap )))
                        (method "readUInt"
                              (parameters (class "java.io.InputStream"))
                              (returntype . long)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 4) (max_locals . 4) (code_length . 19)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokespecial
					(methodCP "readUShort" "java.util.zip.GZIPInputStream" ((class "java.io.InputStream")) int)))
                                      (5 (i2l))
                                      (6 (lstore_2))
                                      (7 (aload_0))
                                      (8 (aload_1))
                                      (9 (invokespecial
					(methodCP "readUShort" "java.util.zip.GZIPInputStream" ((class "java.io.InputStream")) int)))
                                      (12 (i2l))
                                      (13 (bipush 16))
                                      (15 (lshl))
                                      (16 (lload_2))
                                      (17 (lor))
                                      (18 (lreturn))
                                      (endofcode 19))
                                   (Exceptions )
                                   (StackMap )))
                        (method "readUShort"
                              (parameters (class "java.io.InputStream"))
                              (returntype . int)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 17)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokespecial
					(methodCP "readUByte" "java.util.zip.GZIPInputStream" ((class "java.io.InputStream")) int)))
                                      (5 (istore_2))
                                      (6 (aload_0))
                                      (7 (aload_1))
                                      (8 (invokespecial
					(methodCP "readUByte" "java.util.zip.GZIPInputStream" ((class "java.io.InputStream")) int)))
                                      (11 (bipush 8))
                                      (13 (ishl))
                                      (14 (iload_2))
                                      (15 (ior))
                                      (16 (ireturn))
                                      (endofcode 17))
                                   (Exceptions )
                                   (StackMap )))
                        (method "readUByte"
                              (parameters (class "java.io.InputStream"))
                              (returntype . int)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 72)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (invokevirtual (methodCP "read" "java.io.InputStream" () int))) 
                                      (4 (istore_2)) 
                                      (5 (iload_2)) 
                                      (6 (iconst_m1)) 
                                      (7 (if_icmpne 18)) ;;to TAG_0
                                      (10 (new (class "java.io.EOFException"))) 
                                      (13 (dup)) 
                                      (14 (invokespecial (methodCP "<init>" "java.io.EOFException" () void))) 
                                      (17 (athrow)) 
                                      (18 (iload_2)) ;;at TAG_0
                                      (19 (iconst_m1)) 
                                      (20 (if_icmplt 30)) ;;to TAG_1
                                      (23 (iload_2)) 
                                      (24 (sipush 255)) 
                                      (27 (if_icmple 70))  ;;to TAG_2
                                      (30 (new (class "java.io.IOException"))) ;;at TAG_1
                                      (33 (dup)) 
                                      (34 (new (class "java.lang.StringBuilder"))) 
                                      (37 (dup)) 
                                      (38 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (41 (aload_0)) 
                                      (42 (getfield (fieldCP "in" "java.util.zip.GZIPInputStream" (class "java.io.InputStream")))) 
                                      (45 (invokevirtual (methodCP "getClass" "java.lang.Object" () (class "java.lang.Class")))) 
                                      (48 (invokevirtual (methodCP "getName" "java.lang.Class" () (class "java.lang.String")))) 
                                      (51 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (54 (ldc 13)) ;;STRING:: ".read() returned value out of range -1..255: "
                                      (56 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (59 (iload_2)) 
                                      (60 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (int) (class "java.lang.StringBuilder")))) 
                                      (63 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (66 (invokespecial (methodCP "<init>" "java.io.IOException" ((class "java.lang.String")) void))) 
                                      (69 (athrow)) 
                                      (70 (iload_2)) ;;at TAG_2
                                      (71 (ireturn)) 
                                      (endofcode 72))
                                   (Exceptions )
                                   (StackMap )))
                        (method "skipBytes"
                              (parameters (class "java.io.InputStream") int)
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 5) (max_locals . 4) (code_length . 53)
                                   (parsedcode
                                      (0 (iload_2)) ;;at TAG_4
                                      (1 (ifle 52)) ;;to TAG_0
                                      (4 (aload_1)) 
                                      (5 (aload_0)) 
                                      (6 (getfield (fieldCP "tmpbuf" "java.util.zip.GZIPInputStream" (array byte)))) 
                                      (9 (iconst_0)) 
                                      (10 (iload_2)) 
                                      (11 (aload_0)) 
                                      (12 (getfield (fieldCP "tmpbuf" "java.util.zip.GZIPInputStream" (array byte)))) 
                                      (15 (arraylength)) 
                                      (16 (if_icmpge 23)) ;;to TAG_1
                                      (19 (iload_2)) 
                                      (20 (goto 28))  ;;to TAG_2
                                      (23 (aload_0)) ;;at TAG_1
                                      (24 (getfield (fieldCP "tmpbuf" "java.util.zip.GZIPInputStream" (array byte)))) 
                                      (27 (arraylength)) 
                                      (28 (invokevirtual (methodCP "read" "java.io.InputStream" ((array byte) int int) int))) ;;at TAG_2
                                      (31 (istore_3)) 
                                      (32 (iload_3)) 
                                      (33 (iconst_m1)) 
                                      (34 (if_icmpne 45)) ;;to TAG_3
                                      (37 (new (class "java.io.EOFException"))) 
                                      (40 (dup)) 
                                      (41 (invokespecial (methodCP "<init>" "java.io.EOFException" () void))) 
                                      (44 (athrow)) 
                                      (45 (iload_2)) ;;at TAG_3
                                      (46 (iload_3)) 
                                      (47 (isub)) 
                                      (48 (istore_2)) 
                                      (49 (goto 0)) ;;to TAG_4
                                      (52 (return)) ;;at TAG_0
                                      (endofcode 53))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *GZIPInputStream-class-table*
  (make-static-class-decls 
   *java.util.zip.GZIPInputStream*))

(defconst *package-name-map* 
  ("java.util.zip.GZIPInputStream" . "java.util.zip"))

