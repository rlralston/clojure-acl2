; ZipFile$ZipFileInflaterInputStream-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:48 CDT 2014.
;

(defconst *java.util.zip.ZipFile$ZipFileInflaterInputStream*
 (make-class-def
      '(class "java.util.zip.ZipFile$ZipFileInflaterInputStream"
            "java.util.zip.InflaterInputStream"
            (constant_pool
                        (STRING  "Unexpected end of ZLIB input stream")
                        (LONG 2147483647)
                        (INT 2147483647))
            (fields
                        (field "closeRequested" boolean (accessflags  *class*  *private*  *volatile* ) -1)
                        (field "eof" boolean (accessflags  *class*  *private* ) -1)
                        (field "zfin" (class "java.util.zip.ZipFile$ZipFileInputStream") (accessflags  *class*  *final*  *private* ) -1)
                        (field "this$0" (class "java.util.zip.ZipFile") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.zip.ZipFile") (class "java.util.zip.ZipFile$ZipFileInputStream") (class "java.util.zip.Inflater") int)
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 4) (max_locals . 5) (code_length . 29)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.util.zip.ZipFile$ZipFileInflaterInputStream" (class "java.util.zip.ZipFile"))))
                                      (5 (aload_0))
                                      (6 (aload_2))
                                      (7 (aload_3))
                                      (8 (iload 4))
                                      (10 (invokespecial
					(methodCP "<init>" "java.util.zip.InflaterInputStream" ((class "java.io.InputStream") (class "java.util.zip.Inflater") int) void)))
                                      (13 (aload_0))
                                      (14 (iconst_0))
                                      (15 (putfield (fieldCP "closeRequested" "java.util.zip.ZipFile$ZipFileInflaterInputStream" boolean)))
                                      (18 (aload_0))
                                      (19 (iconst_0))
                                      (20 (putfield (fieldCP "eof" "java.util.zip.ZipFile$ZipFileInflaterInputStream" boolean)))
                                      (23 (aload_0))
                                      (24 (aload_2))
                                      (25 (putfield (fieldCP "zfin" "java.util.zip.ZipFile$ZipFileInflaterInputStream" (class "java.util.zip.ZipFile$ZipFileInputStream"))))
                                      (28 (return))
                                      (endofcode 29))
                                   (Exceptions )
                                   (StackMap )))
                        (method "close"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 67)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "closeRequested" "java.util.zip.ZipFile$ZipFileInflaterInputStream" boolean))) 
                                      (4 (ifeq 8)) ;;to TAG_0
                                      (7 (return)) 
                                      (8 (aload_0)) ;;at TAG_0
                                      (9 (iconst_1)) 
                                      (10 (putfield (fieldCP "closeRequested" "java.util.zip.ZipFile$ZipFileInflaterInputStream" boolean))) 
                                      (13 (aload_0)) 
                                      (14 (invokespecial (methodCP "close" "java.util.zip.InflaterInputStream" () void))) 
                                      (17 (aload_0)) 
                                      (18 (getfield (fieldCP "this$0" "java.util.zip.ZipFile$ZipFileInflaterInputStream" (class "java.util.zip.ZipFile")))) 
                                      (21 (invokestatic (methodCP "access$000" "java.util.zip.ZipFile" ((class "java.util.zip.ZipFile")) (class "java.util.Map")))) 
                                      (24 (dup)) 
                                      (25 (astore_2)) 
                                      (26 (monitorenter)) 
                                      (27 (aload_0)) ;;at TAG_3
                                      (28 (getfield (fieldCP "this$0" "java.util.zip.ZipFile$ZipFileInflaterInputStream" (class "java.util.zip.ZipFile")))) 
                                      (31 (invokestatic (methodCP "access$000" "java.util.zip.ZipFile" ((class "java.util.zip.ZipFile")) (class "java.util.Map")))) 
                                      (34 (aload_0)) 
                                      (35 (invokeinterface (methodCP "remove" "java.util.Map" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (40 (checkcast (class "java.util.zip.Inflater"))) 
                                      (43 (astore_1)) 
                                      (44 (aload_2)) 
                                      (45 (monitorexit)) 
                                      (46 (goto 54)) ;;to TAG_1;;at TAG_4
                                      (49 (astore_3)) ;;at TAG_5
                                      (50 (aload_2)) 
                                      (51 (monitorexit)) 
                                      (52 (aload_3)) ;;at TAG_6
                                      (53 (athrow)) 
                                      (54 (aload_1)) ;;at TAG_1
                                      (55 (ifnull 66))  ;;to TAG_2
                                      (58 (aload_0)) 
                                      (59 (getfield (fieldCP "this$0" "java.util.zip.ZipFile$ZipFileInflaterInputStream" (class "java.util.zip.ZipFile")))) 
                                      (62 (aload_1)) 
                                      (63 (invokestatic (methodCP "access$100" "java.util.zip.ZipFile" ((class "java.util.zip.ZipFile") (class "java.util.zip.Inflater")) void))) 
                                      (66 (return)) ;;at TAG_2
                                      (endofcode 67))
                                   (Exceptions 
                                     (handler 27 46  49 (class "java.lang.Throwable"))
                                     (handler 49 52  49 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "fill"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 5) (max_locals . 1) (code_length . 80)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "eof" "java.util.zip.ZipFile$ZipFileInflaterInputStream" boolean))) 
                                      (4 (ifeq 17))  ;;to TAG_0
                                      (7 (new (class "java.io.EOFException"))) 
                                      (10 (dup)) 
                                      (11 (ldc 0)) ;;STRING:: "Unexpected end of ZLIB input stream"
                                      (13 (invokespecial (methodCP "<init>" "java.io.EOFException" ((class "java.lang.String")) void))) 
                                      (16 (athrow)) 
                                      (17 (aload_0)) ;;at TAG_0
                                      (18 (aload_0)) 
                                      (19 (getfield (fieldCP "in" "java.util.zip.ZipFile$ZipFileInflaterInputStream" (class "java.io.InputStream")))) 
                                      (22 (aload_0)) 
                                      (23 (getfield (fieldCP "buf" "java.util.zip.ZipFile$ZipFileInflaterInputStream" (array byte)))) 
                                      (26 (iconst_0)) 
                                      (27 (aload_0)) 
                                      (28 (getfield (fieldCP "buf" "java.util.zip.ZipFile$ZipFileInflaterInputStream" (array byte)))) 
                                      (31 (arraylength)) 
                                      (32 (invokevirtual (methodCP "read" "java.io.InputStream" ((array byte) int int) int))) 
                                      (35 (putfield (fieldCP "len" "java.util.zip.ZipFile$ZipFileInflaterInputStream" int))) 
                                      (38 (aload_0)) 
                                      (39 (getfield (fieldCP "len" "java.util.zip.ZipFile$ZipFileInflaterInputStream" int))) 
                                      (42 (iconst_m1)) 
                                      (43 (if_icmpne 63)) ;;to TAG_1
                                      (46 (aload_0)) 
                                      (47 (getfield (fieldCP "buf" "java.util.zip.ZipFile$ZipFileInflaterInputStream" (array byte)))) 
                                      (50 (iconst_0)) 
                                      (51 (iconst_0)) 
                                      (52 (bastore)) 
                                      (53 (aload_0)) 
                                      (54 (iconst_1)) 
                                      (55 (putfield (fieldCP "len" "java.util.zip.ZipFile$ZipFileInflaterInputStream" int))) 
                                      (58 (aload_0)) 
                                      (59 (iconst_1)) 
                                      (60 (putfield (fieldCP "eof" "java.util.zip.ZipFile$ZipFileInflaterInputStream" boolean))) 
                                      (63 (aload_0)) ;;at TAG_1
                                      (64 (getfield (fieldCP "inf" "java.util.zip.ZipFile$ZipFileInflaterInputStream" (class "java.util.zip.Inflater")))) 
                                      (67 (aload_0)) 
                                      (68 (getfield (fieldCP "buf" "java.util.zip.ZipFile$ZipFileInflaterInputStream" (array byte)))) 
                                      (71 (iconst_0)) 
                                      (72 (aload_0)) 
                                      (73 (getfield (fieldCP "len" "java.util.zip.ZipFile$ZipFileInflaterInputStream" int))) 
                                      (76 (invokevirtual (methodCP "setInput" "java.util.zip.Inflater" ((array byte) int int) void))) 
                                      (79 (return)) 
                                      (endofcode 80))
                                   (Exceptions )
                                   (StackMap )))
                        (method "available"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 41)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "closeRequested" "java.util.zip.ZipFile$ZipFileInflaterInputStream" boolean))) 
                                      (4 (ifeq 9)) ;;to TAG_0
                                      (7 (iconst_0)) 
                                      (8 (ireturn)) 
                                      (9 (aload_0)) ;;at TAG_0
                                      (10 (getfield (fieldCP "zfin" "java.util.zip.ZipFile$ZipFileInflaterInputStream" (class "java.util.zip.ZipFile$ZipFileInputStream")))) 
                                      (13 (invokevirtual (methodCP "size" "java.util.zip.ZipFile$ZipFileInputStream" () long))) 
                                      (16 (aload_0)) 
                                      (17 (getfield (fieldCP "inf" "java.util.zip.ZipFile$ZipFileInflaterInputStream" (class "java.util.zip.Inflater")))) 
                                      (20 (invokevirtual (methodCP "getBytesWritten" "java.util.zip.Inflater" () long))) 
                                      (23 (lsub)) 
                                      (24 (lstore_1)) 
                                      (25 (lload_1)) 
                                      (26 (ldc2_w 1)) ;; LONG:: "2147483647"
                                      (29 (lcmp)) 
                                      (30 (ifle 38)) ;;to TAG_1
                                      (33 (ldc 2)) ;;INT:: "2147483647"
                                      (35 (goto 40))  ;;to TAG_2
                                      (38 (lload_1)) ;;at TAG_1
                                      (39 (l2i)) 
                                      (40 (ireturn)) ;;at TAG_2
                                      (endofcode 41))
                                   (Exceptions )
                                   (StackMap )))
                        (method "finalize"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokevirtual
					(methodCP "close" "java.util.zip.ZipFile$ZipFileInflaterInputStream" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *ZipFile$ZipFileInflaterInputStream-class-table*
  (make-static-class-decls 
   *java.util.zip.ZipFile$ZipFileInflaterInputStream*))

(defconst *package-name-map* 
  ("java.util.zip.ZipFile$ZipFileInflaterInputStream" . "java.util.zip"))

