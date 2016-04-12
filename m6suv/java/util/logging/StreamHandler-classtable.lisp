; StreamHandler-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:46 CDT 2014.
;

(defconst *java.util.logging.StreamHandler*
 (make-class-def
      '(class "java.util.logging.StreamHandler"
            "java.util.logging.Handler"
            (constant_pool
                        (STRING  ".level")
                        (STRING  ".filter")
                        (STRING  ".formatter")
                        (STRING  ".encoding")
                        (STRING  "Unexpected exception "))
            (fields
                        (field "manager" (class "java.util.logging.LogManager") (accessflags  *class*  *private* ) -1)
                        (field "output" (class "java.io.OutputStream") (accessflags  *class*  *private* ) -1)
                        (field "doneHeader" boolean (accessflags  *class*  *private* ) -1)
                        (field "writer" (class "java.io.Writer") (accessflags  *class*  *private* ) -1))
            (methods
                        (method "configure"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 5) (max_locals . 5) (code_length . 147)
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
                                      (25 (ldc 0)) ;;STRING:: ".level"
                                      (27 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (30 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (33 (getstatic (fieldCP "INFO" "java.util.logging.Level" (class "java.util.logging.Level")))) 
                                      (36 (invokevirtual (methodCP "getLevelProperty" "java.util.logging.LogManager" ((class "java.lang.String") (class "java.util.logging.Level")) (class "java.util.logging.Level")))) 
                                      (39 (invokevirtual (methodCP "setLevel" "java.util.logging.StreamHandler" ((class "java.util.logging.Level")) void))) 
                                      (42 (aload_0)) 
                                      (43 (aload_1)) 
                                      (44 (new (class "java.lang.StringBuilder"))) 
                                      (47 (dup)) 
                                      (48 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (51 (aload_2)) 
                                      (52 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (55 (ldc 1)) ;;STRING:: ".filter"
                                      (57 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (60 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (63 (aconst_null)) 
                                      (64 (invokevirtual (methodCP "getFilterProperty" "java.util.logging.LogManager" ((class "java.lang.String") (class "java.util.logging.Filter")) (class "java.util.logging.Filter")))) 
                                      (67 (invokevirtual (methodCP "setFilter" "java.util.logging.StreamHandler" ((class "java.util.logging.Filter")) void))) 
                                      (70 (aload_0)) 
                                      (71 (aload_1)) 
                                      (72 (new (class "java.lang.StringBuilder"))) 
                                      (75 (dup)) 
                                      (76 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (79 (aload_2)) 
                                      (80 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (83 (ldc 2)) ;;STRING:: ".formatter"
                                      (85 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (88 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (91 (new (class "java.util.logging.SimpleFormatter"))) 
                                      (94 (dup)) 
                                      (95 (invokespecial (methodCP "<init>" "java.util.logging.SimpleFormatter" () void))) 
                                      (98 (invokevirtual (methodCP "getFormatterProperty" "java.util.logging.LogManager" ((class "java.lang.String") (class "java.util.logging.Formatter")) (class "java.util.logging.Formatter")))) 
                                      (101 (invokevirtual (methodCP "setFormatter" "java.util.logging.StreamHandler" ((class "java.util.logging.Formatter")) void))) 
                                      (104 (aload_0)) ;;at TAG_1
                                      (105 (aload_1)) 
                                      (106 (new (class "java.lang.StringBuilder"))) 
                                      (109 (dup)) 
                                      (110 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (113 (aload_2)) 
                                      (114 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (117 (ldc 3)) ;;STRING:: ".encoding"
                                      (119 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (122 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (125 (aconst_null)) 
                                      (126 (invokevirtual (methodCP "getStringProperty" "java.util.logging.LogManager" ((class "java.lang.String") (class "java.lang.String")) (class "java.lang.String")))) 
                                      (129 (invokevirtual (methodCP "setEncoding" "java.util.logging.StreamHandler" ((class "java.lang.String")) void))) 
                                      (132 (goto 146)) ;;to TAG_0;;at TAG_2
                                      (135 (astore_3)) ;;at TAG_3
                                      (136 (aload_0)) ;;at TAG_4
                                      (137 (aconst_null)) 
                                      (138 (invokevirtual (methodCP "setEncoding" "java.util.logging.StreamHandler" ((class "java.lang.String")) void))) 
                                      (141 (goto 146)) ;;to TAG_0;;at TAG_5
                                      (144 (astore 4)) ;;at TAG_6
                                      (146 (return)) ;;at TAG_0
                                      (endofcode 147))
                                   (Exceptions 
                                     (handler 104 132  135 (class "java.lang.Exception"))
                                     (handler 136 141  144 (class "java.lang.Exception")))
                                   (StackMap )))
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 26)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.util.logging.Handler" () void)))
                                      (4 (aload_0))
                                      (5 (invokestatic
					(methodCP "getLogManager" "java.util.logging.LogManager" () (class "java.util.logging.LogManager"))))
                                      (8 (putfield (fieldCP "manager" "java.util.logging.StreamHandler" (class "java.util.logging.LogManager"))))
                                      (11 (aload_0))
                                      (12 (iconst_0))
                                      (13 (putfield (fieldCP "sealed" "java.util.logging.StreamHandler" boolean)))
                                      (16 (aload_0))
                                      (17 (invokespecial
					(methodCP "configure" "java.util.logging.StreamHandler" () void)))
                                      (20 (aload_0))
                                      (21 (iconst_1))
                                      (22 (putfield (fieldCP "sealed" "java.util.logging.StreamHandler" boolean)))
                                      (25 (return))
                                      (endofcode 26))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.io.OutputStream") (class "java.util.logging.Formatter"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 36)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.util.logging.Handler" () void)))
                                      (4 (aload_0))
                                      (5 (invokestatic
					(methodCP "getLogManager" "java.util.logging.LogManager" () (class "java.util.logging.LogManager"))))
                                      (8 (putfield (fieldCP "manager" "java.util.logging.StreamHandler" (class "java.util.logging.LogManager"))))
                                      (11 (aload_0))
                                      (12 (iconst_0))
                                      (13 (putfield (fieldCP "sealed" "java.util.logging.StreamHandler" boolean)))
                                      (16 (aload_0))
                                      (17 (invokespecial
					(methodCP "configure" "java.util.logging.StreamHandler" () void)))
                                      (20 (aload_0))
                                      (21 (aload_2))
                                      (22 (invokevirtual
					(methodCP "setFormatter" "java.util.logging.StreamHandler" ((class "java.util.logging.Formatter")) void)))
                                      (25 (aload_0))
                                      (26 (aload_1))
                                      (27 (invokevirtual
					(methodCP "setOutputStream" "java.util.logging.StreamHandler" ((class "java.io.OutputStream")) void)))
                                      (30 (aload_0))
                                      (31 (iconst_1))
                                      (32 (putfield (fieldCP "sealed" "java.util.logging.StreamHandler" boolean)))
                                      (35 (return))
                                      (endofcode 36))
                                   (Exceptions )
                                   (StackMap )))
                        (method "setOutputStream"
                              (parameters (class "java.io.OutputStream"))
                              (returntype . void)
                              (accessflags  *class*  *protected*  *super*  *synchronized* )
                              (code
                                   (max_stack . 5) (max_locals . 4) (code_length . 101)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (ifnonnull 12)) ;;to TAG_0
                                      (4 (new (class "java.lang.NullPointerException"))) 
                                      (7 (dup)) 
                                      (8 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" () void))) 
                                      (11 (athrow)) 
                                      (12 (aload_0)) ;;at TAG_0
                                      (13 (invokespecial (methodCP "flushAndClose" "java.util.logging.StreamHandler" () void))) 
                                      (16 (aload_0)) 
                                      (17 (aload_1)) 
                                      (18 (putfield (fieldCP "output" "java.util.logging.StreamHandler" (class "java.io.OutputStream")))) 
                                      (21 (aload_0)) 
                                      (22 (iconst_0)) 
                                      (23 (putfield (fieldCP "doneHeader" "java.util.logging.StreamHandler" boolean))) 
                                      (26 (aload_0)) 
                                      (27 (invokevirtual (methodCP "getEncoding" "java.util.logging.StreamHandler" () (class "java.lang.String")))) 
                                      (30 (astore_2)) 
                                      (31 (aload_2)) 
                                      (32 (ifnonnull 53)) ;;to TAG_1
                                      (35 (aload_0)) 
                                      (36 (new (class "java.io.OutputStreamWriter"))) 
                                      (39 (dup)) 
                                      (40 (aload_0)) 
                                      (41 (getfield (fieldCP "output" "java.util.logging.StreamHandler" (class "java.io.OutputStream")))) 
                                      (44 (invokespecial (methodCP "<init>" "java.io.OutputStreamWriter" ((class "java.io.OutputStream")) void))) 
                                      (47 (putfield (fieldCP "writer" "java.util.logging.StreamHandler" (class "java.io.Writer")))) 
                                      (50 (goto 100))  ;;to TAG_2
                                      (53 (aload_0)) ;;at TAG_1
                                      (54 (new (class "java.io.OutputStreamWriter"))) 
                                      (57 (dup)) 
                                      (58 (aload_0)) 
                                      (59 (getfield (fieldCP "output" "java.util.logging.StreamHandler" (class "java.io.OutputStream")))) 
                                      (62 (aload_2)) 
                                      (63 (invokespecial (methodCP "<init>" "java.io.OutputStreamWriter" ((class "java.io.OutputStream") (class "java.lang.String")) void))) 
                                      (66 (putfield (fieldCP "writer" "java.util.logging.StreamHandler" (class "java.io.Writer")))) 
                                      (69 (goto 100))  ;;to TAG_2;;at TAG_3
                                      (72 (astore_3)) ;;at TAG_4
                                      (73 (new (class "java.lang.Error"))) 
                                      (76 (dup)) 
                                      (77 (new (class "java.lang.StringBuilder"))) 
                                      (80 (dup)) 
                                      (81 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (84 (ldc 4)) ;;STRING:: "Unexpected exception "
                                      (86 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (89 (aload_3)) 
                                      (90 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.Object")) (class "java.lang.StringBuilder")))) 
                                      (93 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (96 (invokespecial (methodCP "<init>" "java.lang.Error" ((class "java.lang.String")) void))) 
                                      (99 (athrow)) 
                                      (100 (return)) ;;at TAG_2
                                      (endofcode 101))
                                   (Exceptions 
                                     (handler 53 69  72 (class "java.io.UnsupportedEncodingException")))
                                   (StackMap )))
                        (method "setEncoding"
                              (parameters (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 2) (code_length . 56)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (aload_1)) 
                                      (2 (invokespecial (methodCP "setEncoding" "java.util.logging.Handler" ((class "java.lang.String")) void))) 
                                      (5 (aload_0)) 
                                      (6 (getfield (fieldCP "output" "java.util.logging.StreamHandler" (class "java.io.OutputStream")))) 
                                      (9 (ifnonnull 13)) ;;to TAG_0
                                      (12 (return)) 
                                      (13 (aload_0)) ;;at TAG_0
                                      (14 (invokevirtual (methodCP "flush" "java.util.logging.StreamHandler" () void))) 
                                      (17 (aload_1)) 
                                      (18 (ifnonnull 39)) ;;to TAG_1
                                      (21 (aload_0)) 
                                      (22 (new (class "java.io.OutputStreamWriter"))) 
                                      (25 (dup)) 
                                      (26 (aload_0)) 
                                      (27 (getfield (fieldCP "output" "java.util.logging.StreamHandler" (class "java.io.OutputStream")))) 
                                      (30 (invokespecial (methodCP "<init>" "java.io.OutputStreamWriter" ((class "java.io.OutputStream")) void))) 
                                      (33 (putfield (fieldCP "writer" "java.util.logging.StreamHandler" (class "java.io.Writer")))) 
                                      (36 (goto 55))  ;;to TAG_2
                                      (39 (aload_0)) ;;at TAG_1
                                      (40 (new (class "java.io.OutputStreamWriter"))) 
                                      (43 (dup)) 
                                      (44 (aload_0)) 
                                      (45 (getfield (fieldCP "output" "java.util.logging.StreamHandler" (class "java.io.OutputStream")))) 
                                      (48 (aload_1)) 
                                      (49 (invokespecial (methodCP "<init>" "java.io.OutputStreamWriter" ((class "java.io.OutputStream") (class "java.lang.String")) void))) 
                                      (52 (putfield (fieldCP "writer" "java.util.logging.StreamHandler" (class "java.io.Writer")))) 
                                      (55 (return)) ;;at TAG_2
                                      (endofcode 56))
                                   (Exceptions )
                                   (StackMap )))
                        (method "publish"
                              (parameters (class "java.util.logging.LogRecord"))
                              (returntype . void)
                              (accessflags  *class*  *public*  *super*  *synchronized* )
                              (code
                                   (max_stack . 4) (max_locals . 4) (code_length . 77)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (aload_1)) 
                                      (2 (invokevirtual (methodCP "isLoggable" "java.util.logging.StreamHandler" ((class "java.util.logging.LogRecord")) boolean))) 
                                      (5 (ifne 9)) ;;to TAG_0
                                      (8 (return)) 
                                      (9 (aload_0)) ;;at TAG_0
                                      (10 (invokevirtual (methodCP "getFormatter" "java.util.logging.StreamHandler" () (class "java.util.logging.Formatter")))) 
                                      (13 (aload_1)) 
                                      (14 (invokevirtual (methodCP "format" "java.util.logging.Formatter" ((class "java.util.logging.LogRecord")) (class "java.lang.String")))) 
                                      (17 (astore_2)) 
                                      (18 (goto 30)) ;;to TAG_1;;at TAG_4
                                      (21 (astore_3)) ;;at TAG_5
                                      (22 (aload_0)) 
                                      (23 (aconst_null)) 
                                      (24 (aload_3)) 
                                      (25 (iconst_5)) 
                                      (26 (invokevirtual (methodCP "reportError" "java.util.logging.StreamHandler" ((class "java.lang.String") (class "java.lang.Exception") int) void))) 
                                      (29 (return)) 
                                      (30 (aload_0)) ;;at TAG_1
                                      (31 (getfield (fieldCP "doneHeader" "java.util.logging.StreamHandler" boolean))) 
                                      (34 (ifne 57))  ;;to TAG_2
                                      (37 (aload_0)) 
                                      (38 (getfield (fieldCP "writer" "java.util.logging.StreamHandler" (class "java.io.Writer")))) 
                                      (41 (aload_0)) 
                                      (42 (invokevirtual (methodCP "getFormatter" "java.util.logging.StreamHandler" () (class "java.util.logging.Formatter")))) 
                                      (45 (aload_0)) 
                                      (46 (invokevirtual (methodCP "getHead" "java.util.logging.Formatter" ((class "java.util.logging.Handler")) (class "java.lang.String")))) 
                                      (49 (invokevirtual (methodCP "write" "java.io.Writer" ((class "java.lang.String")) void))) 
                                      (52 (aload_0)) 
                                      (53 (iconst_1)) 
                                      (54 (putfield (fieldCP "doneHeader" "java.util.logging.StreamHandler" boolean))) 
                                      (57 (aload_0)) ;;at TAG_2
                                      (58 (getfield (fieldCP "writer" "java.util.logging.StreamHandler" (class "java.io.Writer")))) 
                                      (61 (aload_2)) 
                                      (62 (invokevirtual (methodCP "write" "java.io.Writer" ((class "java.lang.String")) void))) 
                                      (65 (goto 76)) ;;to TAG_3;;at TAG_6
                                      (68 (astore_3)) ;;at TAG_7
                                      (69 (aload_0)) 
                                      (70 (aconst_null)) 
                                      (71 (aload_3)) 
                                      (72 (iconst_1)) 
                                      (73 (invokevirtual (methodCP "reportError" "java.util.logging.StreamHandler" ((class "java.lang.String") (class "java.lang.Exception") int) void))) 
                                      (76 (return)) ;;at TAG_3
                                      (endofcode 77))
                                   (Exceptions 
                                     (handler 9 18  21 (class "java.lang.Exception"))
                                     (handler 30 65  68 (class "java.lang.Exception")))
                                   (StackMap )))
                        (method "isLoggable"
                              (parameters (class "java.util.logging.LogRecord"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 19)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "writer" "java.util.logging.StreamHandler" (class "java.io.Writer")))) 
                                      (4 (ifnull 11))  ;;to TAG_0
                                      (7 (aload_1)) 
                                      (8 (ifnonnull 13)) ;;to TAG_1
                                      (11 (iconst_0)) ;;at TAG_0
                                      (12 (ireturn)) 
                                      (13 (aload_0)) ;;at TAG_1
                                      (14 (aload_1)) 
                                      (15 (invokespecial (methodCP "isLoggable" "java.util.logging.Handler" ((class "java.util.logging.LogRecord")) boolean))) 
                                      (18 (ireturn)) 
                                      (endofcode 19))
                                   (Exceptions )
                                   (StackMap )))
                        (method "flush"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *super*  *synchronized* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 26)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "writer" "java.util.logging.StreamHandler" (class "java.io.Writer")))) 
                                      (4 (ifnull 25)) ;;to TAG_0
                                      (7 (aload_0)) ;;at TAG_1
                                      (8 (getfield (fieldCP "writer" "java.util.logging.StreamHandler" (class "java.io.Writer")))) 
                                      (11 (invokevirtual (methodCP "flush" "java.io.Writer" () void))) 
                                      (14 (goto 25)) ;;to TAG_0;;at TAG_2
                                      (17 (astore_1)) ;;at TAG_3
                                      (18 (aload_0)) 
                                      (19 (aconst_null)) 
                                      (20 (aload_1)) 
                                      (21 (iconst_2)) 
                                      (22 (invokevirtual (methodCP "reportError" "java.util.logging.StreamHandler" ((class "java.lang.String") (class "java.lang.Exception") int) void))) 
                                      (25 (return)) ;;at TAG_0
                                      (endofcode 26))
                                   (Exceptions 
                                     (handler 7 14  17 (class "java.lang.Exception")))
                                   (StackMap )))
                        (method "flushAndClose"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *private*  *super*  *synchronized* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 89)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "checkAccess" "java.util.logging.StreamHandler" () void))) 
                                      (4 (aload_0)) 
                                      (5 (getfield (fieldCP "writer" "java.util.logging.StreamHandler" (class "java.io.Writer")))) 
                                      (8 (ifnull 88)) ;;to TAG_0
                                      (11 (aload_0)) ;;at TAG_3
                                      (12 (getfield (fieldCP "doneHeader" "java.util.logging.StreamHandler" boolean))) 
                                      (15 (ifne 38)) ;;to TAG_1
                                      (18 (aload_0)) 
                                      (19 (getfield (fieldCP "writer" "java.util.logging.StreamHandler" (class "java.io.Writer")))) 
                                      (22 (aload_0)) 
                                      (23 (invokevirtual (methodCP "getFormatter" "java.util.logging.StreamHandler" () (class "java.util.logging.Formatter")))) 
                                      (26 (aload_0)) 
                                      (27 (invokevirtual (methodCP "getHead" "java.util.logging.Formatter" ((class "java.util.logging.Handler")) (class "java.lang.String")))) 
                                      (30 (invokevirtual (methodCP "write" "java.io.Writer" ((class "java.lang.String")) void))) 
                                      (33 (aload_0)) 
                                      (34 (iconst_1)) 
                                      (35 (putfield (fieldCP "doneHeader" "java.util.logging.StreamHandler" boolean))) 
                                      (38 (aload_0)) ;;at TAG_1
                                      (39 (getfield (fieldCP "writer" "java.util.logging.StreamHandler" (class "java.io.Writer")))) 
                                      (42 (aload_0)) 
                                      (43 (invokevirtual (methodCP "getFormatter" "java.util.logging.StreamHandler" () (class "java.util.logging.Formatter")))) 
                                      (46 (aload_0)) 
                                      (47 (invokevirtual (methodCP "getTail" "java.util.logging.Formatter" ((class "java.util.logging.Handler")) (class "java.lang.String")))) 
                                      (50 (invokevirtual (methodCP "write" "java.io.Writer" ((class "java.lang.String")) void))) 
                                      (53 (aload_0)) 
                                      (54 (getfield (fieldCP "writer" "java.util.logging.StreamHandler" (class "java.io.Writer")))) 
                                      (57 (invokevirtual (methodCP "flush" "java.io.Writer" () void))) 
                                      (60 (aload_0)) 
                                      (61 (getfield (fieldCP "writer" "java.util.logging.StreamHandler" (class "java.io.Writer")))) 
                                      (64 (invokevirtual (methodCP "close" "java.io.Writer" () void))) 
                                      (67 (goto 78))  ;;to TAG_2;;at TAG_4
                                      (70 (astore_1)) ;;at TAG_5
                                      (71 (aload_0)) 
                                      (72 (aconst_null)) 
                                      (73 (aload_1)) 
                                      (74 (iconst_3)) 
                                      (75 (invokevirtual (methodCP "reportError" "java.util.logging.StreamHandler" ((class "java.lang.String") (class "java.lang.Exception") int) void))) 
                                      (78 (aload_0)) ;;at TAG_2
                                      (79 (aconst_null)) 
                                      (80 (putfield (fieldCP "writer" "java.util.logging.StreamHandler" (class "java.io.Writer")))) 
                                      (83 (aload_0)) 
                                      (84 (aconst_null)) 
                                      (85 (putfield (fieldCP "output" "java.util.logging.StreamHandler" (class "java.io.OutputStream")))) 
                                      (88 (return)) ;;at TAG_0
                                      (endofcode 89))
                                   (Exceptions 
                                     (handler 11 67  70 (class "java.lang.Exception")))
                                   (StackMap )))
                        (method "close"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *super*  *synchronized* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "flushAndClose" "java.util.logging.StreamHandler" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *StreamHandler-class-table*
  (make-static-class-decls 
   *java.util.logging.StreamHandler*))

(defconst *package-name-map* 
  ("java.util.logging.StreamHandler" . "java.util.logging"))

