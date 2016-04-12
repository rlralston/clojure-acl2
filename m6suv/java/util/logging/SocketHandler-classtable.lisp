; SocketHandler-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:46 CDT 2014.
;

(defconst *java.util.logging.SocketHandler*
 (make-class-def
      '(class "java.util.logging.SocketHandler"
            "java.util.logging.StreamHandler"
            (constant_pool
                        (STRING  ".level")
                        (STRING  ".filter")
                        (STRING  ".formatter")
                        (STRING  ".encoding")
                        (STRING  ".port")
                        (STRING  ".host")
                        (STRING  "SocketHandler: connect failed to ")
                        (STRING  ":")
                        (STRING  "Bad port: ")
                        (STRING  "Null host name: "))
            (fields
                        (field "sock" (class "java.net.Socket") (accessflags  *class*  *private* ) -1)
                        (field "host" (class "java.lang.String") (accessflags  *class*  *private* ) -1)
                        (field "port" int (accessflags  *class*  *private* ) -1)
                        (field "portProperty" (class "java.lang.String") (accessflags  *class*  *private* ) -1))
            (methods
                        (method "configure"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 5) (max_locals . 5) (code_length . 203)
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
                                      (33 (getstatic (fieldCP "ALL" "java.util.logging.Level" (class "java.util.logging.Level")))) 
                                      (36 (invokevirtual (methodCP "getLevelProperty" "java.util.logging.LogManager" ((class "java.lang.String") (class "java.util.logging.Level")) (class "java.util.logging.Level")))) 
                                      (39 (invokevirtual (methodCP "setLevel" "java.util.logging.SocketHandler" ((class "java.util.logging.Level")) void))) 
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
                                      (67 (invokevirtual (methodCP "setFilter" "java.util.logging.SocketHandler" ((class "java.util.logging.Filter")) void))) 
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
                                      (91 (new (class "java.util.logging.XMLFormatter"))) 
                                      (94 (dup)) 
                                      (95 (invokespecial (methodCP "<init>" "java.util.logging.XMLFormatter" () void))) 
                                      (98 (invokevirtual (methodCP "getFormatterProperty" "java.util.logging.LogManager" ((class "java.lang.String") (class "java.util.logging.Formatter")) (class "java.util.logging.Formatter")))) 
                                      (101 (invokevirtual (methodCP "setFormatter" "java.util.logging.SocketHandler" ((class "java.util.logging.Formatter")) void))) 
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
                                      (129 (invokevirtual (methodCP "setEncoding" "java.util.logging.SocketHandler" ((class "java.lang.String")) void))) 
                                      (132 (goto 146)) ;;to TAG_0;;at TAG_2
                                      (135 (astore_3)) ;;at TAG_3
                                      (136 (aload_0)) ;;at TAG_4
                                      (137 (aconst_null)) 
                                      (138 (invokevirtual (methodCP "setEncoding" "java.util.logging.SocketHandler" ((class "java.lang.String")) void))) 
                                      (141 (goto 146)) ;;to TAG_0;;at TAG_5
                                      (144 (astore 4)) ;;at TAG_6
                                      (146 (aload_0)) ;;at TAG_0
                                      (147 (aload_1)) 
                                      (148 (new (class "java.lang.StringBuilder"))) 
                                      (151 (dup)) 
                                      (152 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (155 (aload_2)) 
                                      (156 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (159 (ldc 4)) ;;STRING:: ".port"
                                      (161 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (164 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (167 (iconst_0)) 
                                      (168 (invokevirtual (methodCP "getIntProperty" "java.util.logging.LogManager" ((class "java.lang.String") int) int))) 
                                      (171 (putfield (fieldCP "port" "java.util.logging.SocketHandler" int))) 
                                      (174 (aload_0)) 
                                      (175 (aload_1)) 
                                      (176 (new (class "java.lang.StringBuilder"))) 
                                      (179 (dup)) 
                                      (180 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (183 (aload_2)) 
                                      (184 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (187 (ldc 5)) ;;STRING:: ".host"
                                      (189 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (192 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (195 (aconst_null)) 
                                      (196 (invokevirtual (methodCP "getStringProperty" "java.util.logging.LogManager" ((class "java.lang.String") (class "java.lang.String")) (class "java.lang.String")))) 
                                      (199 (putfield (fieldCP "host" "java.util.logging.SocketHandler" (class "java.lang.String")))) 
                                      (202 (return)) 
                                      (endofcode 203))
                                   (Exceptions 
                                     (handler 104 132  135 (class "java.lang.Exception"))
                                     (handler 136 141  144 (class "java.lang.Exception")))
                                   (StackMap )))
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 69)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokespecial (methodCP "<init>" "java.util.logging.StreamHandler" () void))) 
                                      (4 (aload_0)) 
                                      (5 (iconst_0)) 
                                      (6 (putfield (fieldCP "sealed" "java.util.logging.SocketHandler" boolean))) 
                                      (9 (aload_0)) 
                                      (10 (invokespecial (methodCP "configure" "java.util.logging.SocketHandler" () void))) 
                                      (13 (aload_0)) ;;at TAG_1
                                      (14 (invokespecial (methodCP "connect" "java.util.logging.SocketHandler" () void))) 
                                      (17 (goto 63)) ;;to TAG_0;;at TAG_2
                                      (20 (astore_1)) ;;at TAG_3
                                      (21 (getstatic (fieldCP "err" "java.lang.System" (class "java.io.PrintStream")))) 
                                      (24 (new (class "java.lang.StringBuilder"))) 
                                      (27 (dup)) 
                                      (28 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (31 (ldc 6)) ;;STRING:: "SocketHandler: connect failed to "
                                      (33 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (36 (aload_0)) 
                                      (37 (getfield (fieldCP "host" "java.util.logging.SocketHandler" (class "java.lang.String")))) 
                                      (40 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (43 (ldc 7)) ;;STRING:: ":"
                                      (45 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (48 (aload_0)) 
                                      (49 (getfield (fieldCP "port" "java.util.logging.SocketHandler" int))) 
                                      (52 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (int) (class "java.lang.StringBuilder")))) 
                                      (55 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (58 (invokevirtual (methodCP "println" "java.io.PrintStream" ((class "java.lang.String")) void))) 
                                      (61 (aload_1)) 
                                      (62 (athrow)) 
                                      (63 (aload_0)) ;;at TAG_0
                                      (64 (iconst_1)) 
                                      (65 (putfield (fieldCP "sealed" "java.util.logging.SocketHandler" boolean))) 
                                      (68 (return)) 
                                      (endofcode 69))
                                   (Exceptions 
                                     (handler 13 17  20 (class "java.io.IOException")))
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.String") int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 33)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.util.logging.StreamHandler" () void)))
                                      (4 (aload_0))
                                      (5 (iconst_0))
                                      (6 (putfield (fieldCP "sealed" "java.util.logging.SocketHandler" boolean)))
                                      (9 (aload_0))
                                      (10 (invokespecial
					(methodCP "configure" "java.util.logging.SocketHandler" () void)))
                                      (13 (aload_0))
                                      (14 (iconst_1))
                                      (15 (putfield (fieldCP "sealed" "java.util.logging.SocketHandler" boolean)))
                                      (18 (aload_0))
                                      (19 (iload_2))
                                      (20 (putfield (fieldCP "port" "java.util.logging.SocketHandler" int)))
                                      (23 (aload_0))
                                      (24 (aload_1))
                                      (25 (putfield (fieldCP "host" "java.util.logging.SocketHandler" (class "java.lang.String"))))
                                      (28 (aload_0))
                                      (29 (invokespecial
					(methodCP "connect" "java.util.logging.SocketHandler" () void)))
                                      (32 (return))
                                      (endofcode 33))
                                   (Exceptions )
                                   (StackMap )))
                        (method "connect"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 5) (max_locals . 3) (code_length . 116)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "port" "java.util.logging.SocketHandler" int))) 
                                      (4 (ifne 37))  ;;to TAG_0
                                      (7 (new (class "java.lang.IllegalArgumentException"))) 
                                      (10 (dup)) 
                                      (11 (new (class "java.lang.StringBuilder"))) 
                                      (14 (dup)) 
                                      (15 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (18 (ldc 8)) ;;STRING:: "Bad port: "
                                      (20 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (23 (aload_0)) 
                                      (24 (getfield (fieldCP "port" "java.util.logging.SocketHandler" int))) 
                                      (27 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (int) (class "java.lang.StringBuilder")))) 
                                      (30 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (33 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (36 (athrow)) 
                                      (37 (aload_0)) ;;at TAG_0
                                      (38 (getfield (fieldCP "host" "java.util.logging.SocketHandler" (class "java.lang.String")))) 
                                      (41 (ifnonnull 74)) ;;to TAG_1
                                      (44 (new (class "java.lang.IllegalArgumentException"))) 
                                      (47 (dup)) 
                                      (48 (new (class "java.lang.StringBuilder"))) 
                                      (51 (dup)) 
                                      (52 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (55 (ldc 9)) ;;STRING:: "Null host name: "
                                      (57 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (60 (aload_0)) 
                                      (61 (getfield (fieldCP "host" "java.util.logging.SocketHandler" (class "java.lang.String")))) 
                                      (64 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (67 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (70 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (73 (athrow)) 
                                      (74 (aload_0)) ;;at TAG_1
                                      (75 (new (class "java.net.Socket"))) 
                                      (78 (dup)) 
                                      (79 (aload_0)) 
                                      (80 (getfield (fieldCP "host" "java.util.logging.SocketHandler" (class "java.lang.String")))) 
                                      (83 (aload_0)) 
                                      (84 (getfield (fieldCP "port" "java.util.logging.SocketHandler" int))) 
                                      (87 (invokespecial (methodCP "<init>" "java.net.Socket" ((class "java.lang.String") int) void))) 
                                      (90 (putfield (fieldCP "sock" "java.util.logging.SocketHandler" (class "java.net.Socket")))) 
                                      (93 (aload_0)) 
                                      (94 (getfield (fieldCP "sock" "java.util.logging.SocketHandler" (class "java.net.Socket")))) 
                                      (97 (invokevirtual (methodCP "getOutputStream" "java.net.Socket" () (class "java.io.OutputStream")))) 
                                      (100 (astore_1)) 
                                      (101 (new (class "java.io.BufferedOutputStream"))) 
                                      (104 (dup)) 
                                      (105 (aload_1)) 
                                      (106 (invokespecial (methodCP "<init>" "java.io.BufferedOutputStream" ((class "java.io.OutputStream")) void))) 
                                      (109 (astore_2)) 
                                      (110 (aload_0)) 
                                      (111 (aload_2)) 
                                      (112 (invokevirtual (methodCP "setOutputStream" "java.util.logging.SocketHandler" ((class "java.io.OutputStream")) void))) 
                                      (115 (return)) 
                                      (endofcode 116))
                                   (Exceptions )
                                   (StackMap )))
                        (method "close"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *super*  *synchronized* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 28)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokespecial (methodCP "close" "java.util.logging.StreamHandler" () void))) 
                                      (4 (aload_0)) 
                                      (5 (getfield (fieldCP "sock" "java.util.logging.SocketHandler" (class "java.net.Socket")))) 
                                      (8 (ifnull 22)) ;;to TAG_0
                                      (11 (aload_0)) ;;at TAG_1
                                      (12 (getfield (fieldCP "sock" "java.util.logging.SocketHandler" (class "java.net.Socket")))) 
                                      (15 (invokevirtual (methodCP "close" "java.net.Socket" () void))) 
                                      (18 (goto 22)) ;;to TAG_0;;at TAG_2
                                      (21 (astore_1)) ;;at TAG_3
                                      (22 (aload_0)) ;;at TAG_0
                                      (23 (aconst_null)) 
                                      (24 (putfield (fieldCP "sock" "java.util.logging.SocketHandler" (class "java.net.Socket")))) 
                                      (27 (return)) 
                                      (endofcode 28))
                                   (Exceptions 
                                     (handler 11 18  21 (class "java.io.IOException")))
                                   (StackMap )))
                        (method "publish"
                              (parameters (class "java.util.logging.LogRecord"))
                              (returntype . void)
                              (accessflags  *class*  *public*  *super*  *synchronized* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 19)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (aload_1)) 
                                      (2 (invokevirtual (methodCP "isLoggable" "java.util.logging.SocketHandler" ((class "java.util.logging.LogRecord")) boolean))) 
                                      (5 (ifne 9))  ;;to TAG_0
                                      (8 (return)) 
                                      (9 (aload_0)) ;;at TAG_0
                                      (10 (aload_1)) 
                                      (11 (invokespecial (methodCP "publish" "java.util.logging.StreamHandler" ((class "java.util.logging.LogRecord")) void))) 
                                      (14 (aload_0)) 
                                      (15 (invokevirtual (methodCP "flush" "java.util.logging.SocketHandler" () void))) 
                                      (18 (return)) 
                                      (endofcode 19))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *SocketHandler-class-table*
  (make-static-class-decls 
   *java.util.logging.SocketHandler*))

(defconst *package-name-map* 
  ("java.util.logging.SocketHandler" . "java.util.logging"))

