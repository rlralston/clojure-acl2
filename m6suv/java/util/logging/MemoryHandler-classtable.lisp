; MemoryHandler-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:46 CDT 2014.
;

(defconst *java.util.logging.MemoryHandler*
 (make-class-def
      '(class "java.util.logging.MemoryHandler"
            "java.util.logging.Handler"
            (constant_pool
                        (INT 1000)
                        (STRING  ".push")
                        (STRING  ".size")
                        (STRING  ".level")
                        (STRING  ".filter")
                        (STRING  ".formatter")
                        (STRING  "???")
                        (STRING  "java.util.logging.MemoryHandler.target")
                        (STRING  "MemoryHandler can\nt load handler \"")
                        (STRING  "\""))
            (fields
                        (field "DEFAULT_SIZE" int (accessflags  *class*  *final*  *private*  *static* ) 0)
                        (field "pushLevel" (class "java.util.logging.Level") (accessflags  *class*  *private* ) -1)
                        (field "size" int (accessflags  *class*  *private* ) -1)
                        (field "target" (class "java.util.logging.Handler") (accessflags  *class*  *private* ) -1)
                        (field "buffer" (array (class "java.util.logging.LogRecord")) (accessflags  *class*  *private* ) -1)
                        (field "start" int (accessflags  *class* ) -1)
                        (field "count" int (accessflags  *class* ) -1))
            (methods
                        (method "configure"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 5) (max_locals . 3) (code_length . 179)
                                   (parsedcode
                                      (0 (invokestatic (methodCP "getLogManager" "java.util.logging.LogManager" () (class "java.util.logging.LogManager")))) 
                                      (3 (astore_1)) 
                                      (4 (aload_0)) 
                                      (5 (invokevirtual (methodCP "getClass" "java.lang.Object" () (class "java.lang.Class")))) 
                                      (8 (invokevirtual (methodCP "getName" "java.lang.Class" () (class "java.lang.String")))) 
                                      (11 (astore_2)) 
                                      (12 (aload_0)) 
                                      (13 (aload_1)) 
                                      (14 (new (class "java.lang.StringBuilder"))) 
                                      (17 (dup)) 
                                      (18 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (21 (aload_2)) 
                                      (22 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (25 (ldc 1)) ;;STRING:: ".push"
                                      (27 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (30 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (33 (getstatic (fieldCP "SEVERE" "java.util.logging.Level" (class "java.util.logging.Level")))) 
                                      (36 (invokevirtual (methodCP "getLevelProperty" "java.util.logging.LogManager" ((class "java.lang.String") (class "java.util.logging.Level")) (class "java.util.logging.Level")))) 
                                      (39 (putfield (fieldCP "pushLevel" "java.util.logging.MemoryHandler" (class "java.util.logging.Level")))) 
                                      (42 (aload_0)) 
                                      (43 (aload_1)) 
                                      (44 (new (class "java.lang.StringBuilder"))) 
                                      (47 (dup)) 
                                      (48 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (51 (aload_2)) 
                                      (52 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (55 (ldc 2)) ;;STRING:: ".size"
                                      (57 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (60 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (63 (sipush 1000)) 
                                      (66 (invokevirtual (methodCP "getIntProperty" "java.util.logging.LogManager" ((class "java.lang.String") int) int))) 
                                      (69 (putfield (fieldCP "size" "java.util.logging.MemoryHandler" int))) 
                                      (72 (aload_0)) 
                                      (73 (getfield (fieldCP "size" "java.util.logging.MemoryHandler" int))) 
                                      (76 (ifgt 86))  ;;to TAG_0
                                      (79 (aload_0)) 
                                      (80 (sipush 1000)) 
                                      (83 (putfield (fieldCP "size" "java.util.logging.MemoryHandler" int))) 
                                      (86 (aload_0)) ;;at TAG_0
                                      (87 (aload_1)) 
                                      (88 (new (class "java.lang.StringBuilder"))) 
                                      (91 (dup)) 
                                      (92 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (95 (aload_2)) 
                                      (96 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (99 (ldc 3)) ;;STRING:: ".level"
                                      (101 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (104 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (107 (getstatic (fieldCP "ALL" "java.util.logging.Level" (class "java.util.logging.Level")))) 
                                      (110 (invokevirtual (methodCP "getLevelProperty" "java.util.logging.LogManager" ((class "java.lang.String") (class "java.util.logging.Level")) (class "java.util.logging.Level")))) 
                                      (113 (invokevirtual (methodCP "setLevel" "java.util.logging.MemoryHandler" ((class "java.util.logging.Level")) void))) 
                                      (116 (aload_0)) 
                                      (117 (aload_1)) 
                                      (118 (new (class "java.lang.StringBuilder"))) 
                                      (121 (dup)) 
                                      (122 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (125 (aload_2)) 
                                      (126 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (129 (ldc 4)) ;;STRING:: ".filter"
                                      (131 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (134 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (137 (aconst_null)) 
                                      (138 (invokevirtual (methodCP "getFilterProperty" "java.util.logging.LogManager" ((class "java.lang.String") (class "java.util.logging.Filter")) (class "java.util.logging.Filter")))) 
                                      (141 (invokevirtual (methodCP "setFilter" "java.util.logging.MemoryHandler" ((class "java.util.logging.Filter")) void))) 
                                      (144 (aload_0)) 
                                      (145 (aload_1)) 
                                      (146 (new (class "java.lang.StringBuilder"))) 
                                      (149 (dup)) 
                                      (150 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (153 (aload_2)) 
                                      (154 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (157 (ldc 5)) ;;STRING:: ".formatter"
                                      (159 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (162 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (165 (new (class "java.util.logging.SimpleFormatter"))) 
                                      (168 (dup)) 
                                      (169 (invokespecial (methodCP "<init>" "java.util.logging.SimpleFormatter" () void))) 
                                      (172 (invokevirtual (methodCP "getFormatterProperty" "java.util.logging.LogManager" ((class "java.lang.String") (class "java.util.logging.Formatter")) (class "java.util.logging.Formatter")))) 
                                      (175 (invokevirtual (methodCP "setFormatter" "java.util.logging.MemoryHandler" ((class "java.util.logging.Formatter")) void))) 
                                      (178 (return)) 
                                      (endofcode 179))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 4) (code_length . 93)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokespecial (methodCP "<init>" "java.util.logging.Handler" () void))) 
                                      (4 (aload_0)) 
                                      (5 (iconst_0)) 
                                      (6 (putfield (fieldCP "sealed" "java.util.logging.MemoryHandler" boolean))) 
                                      (9 (aload_0)) 
                                      (10 (invokespecial (methodCP "configure" "java.util.logging.MemoryHandler" () void))) 
                                      (13 (aload_0)) 
                                      (14 (iconst_1)) 
                                      (15 (putfield (fieldCP "sealed" "java.util.logging.MemoryHandler" boolean))) 
                                      (18 (ldc 6)) ;;STRING:: "???"
                                      (20 (astore_1)) 
                                      (21 (invokestatic (methodCP "getLogManager" "java.util.logging.LogManager" () (class "java.util.logging.LogManager")))) ;;at TAG_1
                                      (24 (astore_2)) 
                                      (25 (aload_2)) 
                                      (26 (ldc 7)) ;;STRING:: "java.util.logging.MemoryHandler.target"
                                      (28 (invokevirtual (methodCP "getProperty" "java.util.logging.LogManager" ((class "java.lang.String")) (class "java.lang.String")))) 
                                      (31 (astore_1)) 
                                      (32 (invokestatic (methodCP "getSystemClassLoader" "java.lang.ClassLoader" () (class "java.lang.ClassLoader")))) 
                                      (35 (aload_1)) 
                                      (36 (invokevirtual (methodCP "loadClass" "java.lang.ClassLoader" ((class "java.lang.String")) (class "java.lang.Class")))) 
                                      (39 (astore_3)) 
                                      (40 (aload_0)) 
                                      (41 (aload_3)) 
                                      (42 (invokevirtual (methodCP "newInstance" "java.lang.Class" () (class "java.lang.Object")))) 
                                      (45 (checkcast (class "java.util.logging.Handler"))) 
                                      (48 (putfield (fieldCP "target" "java.util.logging.MemoryHandler" (class "java.util.logging.Handler")))) 
                                      (51 (goto 88)) ;;to TAG_0;;at TAG_2
                                      (54 (astore_2)) ;;at TAG_3
                                      (55 (new (class "java.lang.RuntimeException"))) 
                                      (58 (dup)) 
                                      (59 (new (class "java.lang.StringBuilder"))) 
                                      (62 (dup)) 
                                      (63 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (66 (ldc 8)) ;;STRING:: "MemoryHandler can\nt load handler \""
                                      (68 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (71 (aload_1)) 
                                      (72 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (75 (ldc 9)) ;;STRING:: "\""
                                      (77 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (80 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (83 (aload_2)) 
                                      (84 (invokespecial (methodCP "<init>" "java.lang.RuntimeException" ((class "java.lang.String") (class "java.lang.Throwable")) void))) 
                                      (87 (athrow)) 
                                      (88 (aload_0)) ;;at TAG_0
                                      (89 (invokespecial (methodCP "init" "java.util.logging.MemoryHandler" () void))) 
                                      (92 (return)) 
                                      (endofcode 93))
                                   (Exceptions 
                                     (handler 21 51  54 (class "java.lang.Exception")))
                                   (StackMap )))
                        (method "init"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 22)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_0))
                                      (2 (getfield (fieldCP "size" "java.util.logging.MemoryHandler" int)))
                                      (5 (anewarray (class "java.util.logging.LogRecord")))
                                      (8 (putfield (fieldCP "buffer" "java.util.logging.MemoryHandler" (array (class "java.util.logging.LogRecord")))))
                                      (11 (aload_0))
                                      (12 (iconst_0))
                                      (13 (putfield (fieldCP "start" "java.util.logging.MemoryHandler" int)))
                                      (16 (aload_0))
                                      (17 (iconst_0))
                                      (18 (putfield (fieldCP "count" "java.util.logging.MemoryHandler" int)))
                                      (21 (return))
                                      (endofcode 22))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.util.logging.Handler") int (class "java.util.logging.Level"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 66)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokespecial (methodCP "<init>" "java.util.logging.Handler" () void))) 
                                      (4 (aload_1)) 
                                      (5 (ifnull 12)) ;;to TAG_0
                                      (8 (aload_3)) 
                                      (9 (ifnonnull 20)) ;;to TAG_1
                                      (12 (new (class "java.lang.NullPointerException"))) ;;at TAG_0
                                      (15 (dup)) 
                                      (16 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" () void))) 
                                      (19 (athrow)) 
                                      (20 (iload_2)) ;;at TAG_1
                                      (21 (ifgt 32))  ;;to TAG_2
                                      (24 (new (class "java.lang.IllegalArgumentException"))) 
                                      (27 (dup)) 
                                      (28 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" () void))) 
                                      (31 (athrow)) 
                                      (32 (aload_0)) ;;at TAG_2
                                      (33 (iconst_0)) 
                                      (34 (putfield (fieldCP "sealed" "java.util.logging.MemoryHandler" boolean))) 
                                      (37 (aload_0)) 
                                      (38 (invokespecial (methodCP "configure" "java.util.logging.MemoryHandler" () void))) 
                                      (41 (aload_0)) 
                                      (42 (iconst_1)) 
                                      (43 (putfield (fieldCP "sealed" "java.util.logging.MemoryHandler" boolean))) 
                                      (46 (aload_0)) 
                                      (47 (aload_1)) 
                                      (48 (putfield (fieldCP "target" "java.util.logging.MemoryHandler" (class "java.util.logging.Handler")))) 
                                      (51 (aload_0)) 
                                      (52 (aload_3)) 
                                      (53 (putfield (fieldCP "pushLevel" "java.util.logging.MemoryHandler" (class "java.util.logging.Level")))) 
                                      (56 (aload_0)) 
                                      (57 (iload_2)) 
                                      (58 (putfield (fieldCP "size" "java.util.logging.MemoryHandler" int))) 
                                      (61 (aload_0)) 
                                      (62 (invokespecial (methodCP "init" "java.util.logging.MemoryHandler" () void))) 
                                      (65 (return)) 
                                      (endofcode 66))
                                   (Exceptions )
                                   (StackMap )))
                        (method "publish"
                              (parameters (class "java.util.logging.LogRecord"))
                              (returntype . void)
                              (accessflags  *class*  *public*  *super*  *synchronized* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 103)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (aload_1)) 
                                      (2 (invokevirtual (methodCP "isLoggable" "java.util.logging.MemoryHandler" ((class "java.util.logging.LogRecord")) boolean))) 
                                      (5 (ifne 9)) ;;to TAG_0
                                      (8 (return)) 
                                      (9 (aload_0)) ;;at TAG_0
                                      (10 (getfield (fieldCP "start" "java.util.logging.MemoryHandler" int))) 
                                      (13 (aload_0)) 
                                      (14 (getfield (fieldCP "count" "java.util.logging.MemoryHandler" int))) 
                                      (17 (iadd)) 
                                      (18 (aload_0)) 
                                      (19 (getfield (fieldCP "buffer" "java.util.logging.MemoryHandler" (array (class "java.util.logging.LogRecord"))))) 
                                      (22 (arraylength)) 
                                      (23 (irem)) 
                                      (24 (istore_2)) 
                                      (25 (aload_0)) 
                                      (26 (getfield (fieldCP "buffer" "java.util.logging.MemoryHandler" (array (class "java.util.logging.LogRecord"))))) 
                                      (29 (iload_2)) 
                                      (30 (aload_1)) 
                                      (31 (aastore)) 
                                      (32 (aload_0)) 
                                      (33 (getfield (fieldCP "count" "java.util.logging.MemoryHandler" int))) 
                                      (36 (aload_0)) 
                                      (37 (getfield (fieldCP "buffer" "java.util.logging.MemoryHandler" (array (class "java.util.logging.LogRecord"))))) 
                                      (40 (arraylength)) 
                                      (41 (if_icmpge 57)) ;;to TAG_1
                                      (44 (aload_0)) 
                                      (45 (dup)) 
                                      (46 (getfield (fieldCP "count" "java.util.logging.MemoryHandler" int))) 
                                      (49 (iconst_1)) 
                                      (50 (iadd)) 
                                      (51 (putfield (fieldCP "count" "java.util.logging.MemoryHandler" int))) 
                                      (54 (goto 81))  ;;to TAG_2
                                      (57 (aload_0)) ;;at TAG_1
                                      (58 (dup)) 
                                      (59 (getfield (fieldCP "start" "java.util.logging.MemoryHandler" int))) 
                                      (62 (iconst_1)) 
                                      (63 (iadd)) 
                                      (64 (putfield (fieldCP "start" "java.util.logging.MemoryHandler" int))) 
                                      (67 (aload_0)) 
                                      (68 (dup)) 
                                      (69 (getfield (fieldCP "start" "java.util.logging.MemoryHandler" int))) 
                                      (72 (aload_0)) 
                                      (73 (getfield (fieldCP "buffer" "java.util.logging.MemoryHandler" (array (class "java.util.logging.LogRecord"))))) 
                                      (76 (arraylength)) 
                                      (77 (irem)) 
                                      (78 (putfield (fieldCP "start" "java.util.logging.MemoryHandler" int))) 
                                      (81 (aload_1)) ;;at TAG_2
                                      (82 (invokevirtual (methodCP "getLevel" "java.util.logging.LogRecord" () (class "java.util.logging.Level")))) 
                                      (85 (invokevirtual (methodCP "intValue" "java.util.logging.Level" () int))) 
                                      (88 (aload_0)) 
                                      (89 (getfield (fieldCP "pushLevel" "java.util.logging.MemoryHandler" (class "java.util.logging.Level")))) 
                                      (92 (invokevirtual (methodCP "intValue" "java.util.logging.Level" () int))) 
                                      (95 (if_icmplt 102)) ;;to TAG_3
                                      (98 (aload_0)) 
                                      (99 (invokevirtual (methodCP "push" "java.util.logging.MemoryHandler" () void))) 
                                      (102 (return)) ;;at TAG_3
                                      (endofcode 103))
                                   (Exceptions )
                                   (StackMap )))
                        (method "push"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *super*  *synchronized* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 55)
                                   (parsedcode
                                      (0 (iconst_0)) 
                                      (1 (istore_1)) 
                                      (2 (iload_1)) ;;at TAG_1
                                      (3 (aload_0)) 
                                      (4 (getfield (fieldCP "count" "java.util.logging.MemoryHandler" int))) 
                                      (7 (if_icmpge 44))  ;;to TAG_0
                                      (10 (aload_0)) 
                                      (11 (getfield (fieldCP "start" "java.util.logging.MemoryHandler" int))) 
                                      (14 (iload_1)) 
                                      (15 (iadd)) 
                                      (16 (aload_0)) 
                                      (17 (getfield (fieldCP "buffer" "java.util.logging.MemoryHandler" (array (class "java.util.logging.LogRecord"))))) 
                                      (20 (arraylength)) 
                                      (21 (irem)) 
                                      (22 (istore_2)) 
                                      (23 (aload_0)) 
                                      (24 (getfield (fieldCP "buffer" "java.util.logging.MemoryHandler" (array (class "java.util.logging.LogRecord"))))) 
                                      (27 (iload_2)) 
                                      (28 (aaload)) 
                                      (29 (astore_3)) 
                                      (30 (aload_0)) 
                                      (31 (getfield (fieldCP "target" "java.util.logging.MemoryHandler" (class "java.util.logging.Handler")))) 
                                      (34 (aload_3)) 
                                      (35 (invokevirtual (methodCP "publish" "java.util.logging.Handler" ((class "java.util.logging.LogRecord")) void))) 
                                      (38 (iinc 1 1)) 
                                      (41 (goto 2)) ;;to TAG_1
                                      (44 (aload_0)) ;;at TAG_0
                                      (45 (iconst_0)) 
                                      (46 (putfield (fieldCP "start" "java.util.logging.MemoryHandler" int))) 
                                      (49 (aload_0)) 
                                      (50 (iconst_0)) 
                                      (51 (putfield (fieldCP "count" "java.util.logging.MemoryHandler" int))) 
                                      (54 (return)) 
                                      (endofcode 55))
                                   (Exceptions )
                                   (StackMap )))
                        (method "flush"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "target" "java.util.logging.MemoryHandler" (class "java.util.logging.Handler"))))
                                      (4 (invokevirtual
					(methodCP "flush" "java.util.logging.Handler" () void)))
                                      (7 (return))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "close"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "target" "java.util.logging.MemoryHandler" (class "java.util.logging.Handler"))))
                                      (4 (invokevirtual
					(methodCP "close" "java.util.logging.Handler" () void)))
                                      (7 (aload_0))
                                      (8 (getstatic (fieldCP "OFF" "java.util.logging.Level" (class "java.util.logging.Level"))))
                                      (11 (invokevirtual
					(methodCP "setLevel" "java.util.logging.MemoryHandler" ((class "java.util.logging.Level")) void)))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "setPushLevel"
                              (parameters (class "java.util.logging.Level"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 26)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (ifnonnull 12))  ;;to TAG_0
                                      (4 (new (class "java.lang.NullPointerException"))) 
                                      (7 (dup)) 
                                      (8 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" () void))) 
                                      (11 (athrow)) 
                                      (12 (invokestatic (methodCP "getLogManager" "java.util.logging.LogManager" () (class "java.util.logging.LogManager")))) ;;at TAG_0
                                      (15 (astore_2)) 
                                      (16 (aload_0)) 
                                      (17 (invokevirtual (methodCP "checkAccess" "java.util.logging.MemoryHandler" () void))) 
                                      (20 (aload_0)) 
                                      (21 (aload_1)) 
                                      (22 (putfield (fieldCP "pushLevel" "java.util.logging.MemoryHandler" (class "java.util.logging.Level")))) 
                                      (25 (return)) 
                                      (endofcode 26))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getPushLevel"
                              (parameters )
                              (returntype . (class "java.util.logging.Level"))
                              (accessflags  *class*  *public*  *super*  *synchronized* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "pushLevel" "java.util.logging.MemoryHandler" (class "java.util.logging.Level"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isLoggable"
                              (parameters (class "java.util.logging.LogRecord"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokespecial
					(methodCP "isLoggable" "java.util.logging.Handler" ((class "java.util.logging.LogRecord")) boolean)))
                                      (5 (ireturn))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *MemoryHandler-class-table*
  (make-static-class-decls 
   *java.util.logging.MemoryHandler*))

(defconst *package-name-map* 
  ("java.util.logging.MemoryHandler" . "java.util.logging"))

