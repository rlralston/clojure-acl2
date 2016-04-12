; DeflaterOutputStream-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:48 CDT 2014.
;

(defconst *java.util.zip.DeflaterOutputStream*
 (make-class-def
      '(class "java.util.zip.DeflaterOutputStream"
            "java.io.FilterOutputStream"
            (constant_pool
                        (STRING  "buffer size <= 0")
                        (STRING  "write beyond end of stream"))
            (fields
                        (field "def" (class "java.util.zip.Deflater") (accessflags  *class*  *protected* ) -1)
                        (field "buf" (array byte) (accessflags  *class*  *protected* ) -1)
                        (field "closed" boolean (accessflags  *class*  *private* ) -1)
                        (field "syncFlush" boolean (accessflags  *class*  *final*  *private* ) -1)
                        (field "usesDefaultDeflater" boolean (accessflags  *class* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.io.OutputStream") (class "java.util.zip.Deflater") int boolean)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 5) (code_length . 64)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (aload_1)) 
                                      (2 (invokespecial (methodCP "<init>" "java.io.FilterOutputStream" ((class "java.io.OutputStream")) void))) 
                                      (5 (aload_0)) 
                                      (6 (iconst_0)) 
                                      (7 (putfield (fieldCP "closed" "java.util.zip.DeflaterOutputStream" boolean))) 
                                      (10 (aload_0)) 
                                      (11 (iconst_0)) 
                                      (12 (putfield (fieldCP "usesDefaultDeflater" "java.util.zip.DeflaterOutputStream" boolean))) 
                                      (15 (aload_1)) 
                                      (16 (ifnull 23)) ;;to TAG_0
                                      (19 (aload_2)) 
                                      (20 (ifnonnull 31)) ;;to TAG_1
                                      (23 (new (class "java.lang.NullPointerException"))) ;;at TAG_0
                                      (26 (dup)) 
                                      (27 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" () void))) 
                                      (30 (athrow)) 
                                      (31 (iload_3)) ;;at TAG_1
                                      (32 (ifgt 45))  ;;to TAG_2
                                      (35 (new (class "java.lang.IllegalArgumentException"))) 
                                      (38 (dup)) 
                                      (39 (ldc 0)) ;;STRING:: "buffer size <= 0"
                                      (41 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (44 (athrow)) 
                                      (45 (aload_0)) ;;at TAG_2
                                      (46 (aload_2)) 
                                      (47 (putfield (fieldCP "def" "java.util.zip.DeflaterOutputStream" (class "java.util.zip.Deflater")))) 
                                      (50 (aload_0)) 
                                      (51 (iload_3)) 
                                      (52 (newarray BYTE)) 
                                      (54 (putfield (fieldCP "buf" "java.util.zip.DeflaterOutputStream" (array byte)))) 
                                      (57 (aload_0)) 
                                      (58 (iload 4)) 
                                      (60 (putfield (fieldCP "syncFlush" "java.util.zip.DeflaterOutputStream" boolean))) 
                                      (63 (return)) 
                                      (endofcode 64))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.io.OutputStream") (class "java.util.zip.Deflater") int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 4) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (aload_2))
                                      (3 (iload_3))
                                      (4 (iconst_0))
                                      (5 (invokespecial
					(methodCP "<init>" "java.util.zip.DeflaterOutputStream" ((class "java.io.OutputStream") (class "java.util.zip.Deflater") int boolean) void)))
                                      (8 (return))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.io.OutputStream") (class "java.util.zip.Deflater") boolean)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 4) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (aload_2))
                                      (3 (sipush 512))
                                      (6 (iload_3))
                                      (7 (invokespecial
					(methodCP "<init>" "java.util.zip.DeflaterOutputStream" ((class "java.io.OutputStream") (class "java.util.zip.Deflater") int boolean) void)))
                                      (10 (return))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.io.OutputStream") (class "java.util.zip.Deflater"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 3) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (aload_2))
                                      (3 (sipush 512))
                                      (6 (iconst_0))
                                      (7 (invokespecial
					(methodCP "<init>" "java.util.zip.DeflaterOutputStream" ((class "java.io.OutputStream") (class "java.util.zip.Deflater") int boolean) void)))
                                      (10 (return))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.io.OutputStream") boolean)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 3) (code_length . 22)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (new (class "java.util.zip.Deflater")))
                                      (5 (dup))
                                      (6 (invokespecial
					(methodCP "<init>" "java.util.zip.Deflater" () void)))
                                      (9 (sipush 512))
                                      (12 (iload_2))
                                      (13 (invokespecial
					(methodCP "<init>" "java.util.zip.DeflaterOutputStream" ((class "java.io.OutputStream") (class "java.util.zip.Deflater") int boolean) void)))
                                      (16 (aload_0))
                                      (17 (iconst_1))
                                      (18 (putfield (fieldCP "usesDefaultDeflater" "java.util.zip.DeflaterOutputStream" boolean)))
                                      (21 (return))
                                      (endofcode 22))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.io.OutputStream"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 12)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (iconst_0))
                                      (3 (invokespecial
					(methodCP "<init>" "java.util.zip.DeflaterOutputStream" ((class "java.io.OutputStream") boolean) void)))
                                      (6 (aload_0))
                                      (7 (iconst_1))
                                      (8 (putfield (fieldCP "usesDefaultDeflater" "java.util.zip.DeflaterOutputStream" boolean)))
                                      (11 (return))
                                      (endofcode 12))
                                   (Exceptions )
                                   (StackMap )))
                        (method "write"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 21)
                                   (parsedcode
                                      (0 (iconst_1))
                                      (1 (newarray BYTE))
                                      (3 (astore_2))
                                      (4 (aload_2))
                                      (5 (iconst_0))
                                      (6 (iload_1))
                                      (7 (sipush 255))
                                      (10 (iand))
                                      (11 (i2b))
                                      (12 (bastore))
                                      (13 (aload_0))
                                      (14 (aload_2))
                                      (15 (iconst_0))
                                      (16 (iconst_1))
                                      (17 (invokevirtual
					(methodCP "write" "java.util.zip.DeflaterOutputStream" ((array byte) int int) void)))
                                      (20 (return))
                                      (endofcode 21))
                                   (Exceptions )
                                   (StackMap )))
                        (method "write"
                              (parameters (array byte) int int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 4) (code_length . 88)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "def" "java.util.zip.DeflaterOutputStream" (class "java.util.zip.Deflater")))) 
                                      (4 (invokevirtual (methodCP "finished" "java.util.zip.Deflater" () boolean))) 
                                      (7 (ifeq 20)) ;;to TAG_0
                                      (10 (new (class "java.io.IOException"))) 
                                      (13 (dup)) 
                                      (14 (ldc 1)) ;;STRING:: "write beyond end of stream"
                                      (16 (invokespecial (methodCP "<init>" "java.io.IOException" ((class "java.lang.String")) void))) 
                                      (19 (athrow)) 
                                      (20 (iload_2)) ;;at TAG_0
                                      (21 (iload_3)) 
                                      (22 (ior)) 
                                      (23 (iload_2)) 
                                      (24 (iload_3)) 
                                      (25 (iadd)) 
                                      (26 (ior)) 
                                      (27 (aload_1)) 
                                      (28 (arraylength)) 
                                      (29 (iload_2)) 
                                      (30 (iload_3)) 
                                      (31 (iadd)) 
                                      (32 (isub)) 
                                      (33 (ior)) 
                                      (34 (ifge 45)) ;;to TAG_1
                                      (37 (new (class "java.lang.IndexOutOfBoundsException"))) 
                                      (40 (dup)) 
                                      (41 (invokespecial (methodCP "<init>" "java.lang.IndexOutOfBoundsException" () void))) 
                                      (44 (athrow)) 
                                      (45 (iload_3)) ;;at TAG_1
                                      (46 (ifne 50))  ;;to TAG_2
                                      (49 (return)) 
                                      (50 (aload_0)) ;;at TAG_2
                                      (51 (getfield (fieldCP "def" "java.util.zip.DeflaterOutputStream" (class "java.util.zip.Deflater")))) 
                                      (54 (invokevirtual (methodCP "finished" "java.util.zip.Deflater" () boolean))) 
                                      (57 (ifne 87)) ;;to TAG_3
                                      (60 (aload_0)) 
                                      (61 (getfield (fieldCP "def" "java.util.zip.DeflaterOutputStream" (class "java.util.zip.Deflater")))) 
                                      (64 (aload_1)) 
                                      (65 (iload_2)) 
                                      (66 (iload_3)) 
                                      (67 (invokevirtual (methodCP "setInput" "java.util.zip.Deflater" ((array byte) int int) void))) 
                                      (70 (aload_0)) ;;at TAG_4
                                      (71 (getfield (fieldCP "def" "java.util.zip.DeflaterOutputStream" (class "java.util.zip.Deflater")))) 
                                      (74 (invokevirtual (methodCP "needsInput" "java.util.zip.Deflater" () boolean))) 
                                      (77 (ifne 87)) ;;to TAG_3
                                      (80 (aload_0)) 
                                      (81 (invokevirtual (methodCP "deflate" "java.util.zip.DeflaterOutputStream" () void))) 
                                      (84 (goto 70)) ;;to TAG_4
                                      (87 (return)) ;;at TAG_3
                                      (endofcode 88))
                                   (Exceptions )
                                   (StackMap )))
                        (method "finish"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 35)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "def" "java.util.zip.DeflaterOutputStream" (class "java.util.zip.Deflater")))) 
                                      (4 (invokevirtual (methodCP "finished" "java.util.zip.Deflater" () boolean))) 
                                      (7 (ifne 34))  ;;to TAG_0
                                      (10 (aload_0)) 
                                      (11 (getfield (fieldCP "def" "java.util.zip.DeflaterOutputStream" (class "java.util.zip.Deflater")))) 
                                      (14 (invokevirtual (methodCP "finish" "java.util.zip.Deflater" () void))) 
                                      (17 (aload_0)) ;;at TAG_1
                                      (18 (getfield (fieldCP "def" "java.util.zip.DeflaterOutputStream" (class "java.util.zip.Deflater")))) 
                                      (21 (invokevirtual (methodCP "finished" "java.util.zip.Deflater" () boolean))) 
                                      (24 (ifne 34))  ;;to TAG_0
                                      (27 (aload_0)) 
                                      (28 (invokevirtual (methodCP "deflate" "java.util.zip.DeflaterOutputStream" () void))) 
                                      (31 (goto 17)) ;;to TAG_1
                                      (34 (return)) ;;at TAG_0
                                      (endofcode 35))
                                   (Exceptions )
                                   (StackMap )))
                        (method "close"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 38)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "closed" "java.util.zip.DeflaterOutputStream" boolean))) 
                                      (4 (ifne 37))  ;;to TAG_0
                                      (7 (aload_0)) 
                                      (8 (invokevirtual (methodCP "finish" "java.util.zip.DeflaterOutputStream" () void))) 
                                      (11 (aload_0)) 
                                      (12 (getfield (fieldCP "usesDefaultDeflater" "java.util.zip.DeflaterOutputStream" boolean))) 
                                      (15 (ifeq 25)) ;;to TAG_1
                                      (18 (aload_0)) 
                                      (19 (getfield (fieldCP "def" "java.util.zip.DeflaterOutputStream" (class "java.util.zip.Deflater")))) 
                                      (22 (invokevirtual (methodCP "end" "java.util.zip.Deflater" () void))) 
                                      (25 (aload_0)) ;;at TAG_1
                                      (26 (getfield (fieldCP "out" "java.util.zip.DeflaterOutputStream" (class "java.io.OutputStream")))) 
                                      (29 (invokevirtual (methodCP "close" "java.io.OutputStream" () void))) 
                                      (32 (aload_0)) 
                                      (33 (iconst_1)) 
                                      (34 (putfield (fieldCP "closed" "java.util.zip.DeflaterOutputStream" boolean))) 
                                      (37 (return)) ;;at TAG_0
                                      (endofcode 38))
                                   (Exceptions )
                                   (StackMap )))
                        (method "deflate"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 36)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "def" "java.util.zip.DeflaterOutputStream" (class "java.util.zip.Deflater")))) 
                                      (4 (aload_0)) 
                                      (5 (getfield (fieldCP "buf" "java.util.zip.DeflaterOutputStream" (array byte)))) 
                                      (8 (iconst_0)) 
                                      (9 (aload_0)) 
                                      (10 (getfield (fieldCP "buf" "java.util.zip.DeflaterOutputStream" (array byte)))) 
                                      (13 (arraylength)) 
                                      (14 (invokevirtual (methodCP "deflate" "java.util.zip.Deflater" ((array byte) int int) int))) 
                                      (17 (istore_1)) 
                                      (18 (iload_1)) 
                                      (19 (ifle 35))  ;;to TAG_0
                                      (22 (aload_0)) 
                                      (23 (getfield (fieldCP "out" "java.util.zip.DeflaterOutputStream" (class "java.io.OutputStream")))) 
                                      (26 (aload_0)) 
                                      (27 (getfield (fieldCP "buf" "java.util.zip.DeflaterOutputStream" (array byte)))) 
                                      (30 (iconst_0)) 
                                      (31 (iload_1)) 
                                      (32 (invokevirtual (methodCP "write" "java.io.OutputStream" ((array byte) int int) void))) 
                                      (35 (return)) ;;at TAG_0
                                      (endofcode 36))
                                   (Exceptions )
                                   (StackMap )))
                        (method "flush"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 2) (code_length . 75)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "syncFlush" "java.util.zip.DeflaterOutputStream" boolean))) 
                                      (4 (ifeq 67))  ;;to TAG_0
                                      (7 (aload_0)) 
                                      (8 (getfield (fieldCP "def" "java.util.zip.DeflaterOutputStream" (class "java.util.zip.Deflater")))) 
                                      (11 (invokevirtual (methodCP "finished" "java.util.zip.Deflater" () boolean))) 
                                      (14 (ifne 67))  ;;to TAG_0
                                      (17 (iconst_0)) 
                                      (18 (istore_1)) 
                                      (19 (aload_0)) ;;at TAG_1
                                      (20 (getfield (fieldCP "def" "java.util.zip.DeflaterOutputStream" (class "java.util.zip.Deflater")))) 
                                      (23 (aload_0)) 
                                      (24 (getfield (fieldCP "buf" "java.util.zip.DeflaterOutputStream" (array byte)))) 
                                      (27 (iconst_0)) 
                                      (28 (aload_0)) 
                                      (29 (getfield (fieldCP "buf" "java.util.zip.DeflaterOutputStream" (array byte)))) 
                                      (32 (arraylength)) 
                                      (33 (iconst_2)) 
                                      (34 (invokevirtual (methodCP "deflate" "java.util.zip.Deflater" ((array byte) int int int) int))) 
                                      (37 (dup)) 
                                      (38 (istore_1)) 
                                      (39 (ifle 67))  ;;to TAG_0
                                      (42 (aload_0)) 
                                      (43 (getfield (fieldCP "out" "java.util.zip.DeflaterOutputStream" (class "java.io.OutputStream")))) 
                                      (46 (aload_0)) 
                                      (47 (getfield (fieldCP "buf" "java.util.zip.DeflaterOutputStream" (array byte)))) 
                                      (50 (iconst_0)) 
                                      (51 (iload_1)) 
                                      (52 (invokevirtual (methodCP "write" "java.io.OutputStream" ((array byte) int int) void))) 
                                      (55 (iload_1)) 
                                      (56 (aload_0)) 
                                      (57 (getfield (fieldCP "buf" "java.util.zip.DeflaterOutputStream" (array byte)))) 
                                      (60 (arraylength)) 
                                      (61 (if_icmpge 19)) ;;to TAG_1
                                      (64 (goto 67))  ;;to TAG_0
                                      (67 (aload_0)) ;;at TAG_0
                                      (68 (getfield (fieldCP "out" "java.util.zip.DeflaterOutputStream" (class "java.io.OutputStream")))) 
                                      (71 (invokevirtual (methodCP "flush" "java.io.OutputStream" () void))) 
                                      (74 (return)) 
                                      (endofcode 75))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *DeflaterOutputStream-class-table*
  (make-static-class-decls 
   *java.util.zip.DeflaterOutputStream*))

(defconst *package-name-map* 
  ("java.util.zip.DeflaterOutputStream" . "java.util.zip"))
