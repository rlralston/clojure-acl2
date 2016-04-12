; StringTokenizer-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:48 CDT 2014.
;

(defconst *java.util.StringTokenizer*
 (make-class-def
      '(class "java.util.StringTokenizer"
            "java.lang.Object"
            (constant_pool
                        (INT 55296)
                        (INT 57343)
                        (STRING  " \t\n\r\f"))
            (fields
                        (field "currentPosition" int (accessflags  *class*  *private* ) -1)
                        (field "newPosition" int (accessflags  *class*  *private* ) -1)
                        (field "maxPosition" int (accessflags  *class*  *private* ) -1)
                        (field "str" (class "java.lang.String") (accessflags  *class*  *private* ) -1)
                        (field "delimiters" (class "java.lang.String") (accessflags  *class*  *private* ) -1)
                        (field "retDelims" boolean (accessflags  *class*  *private* ) -1)
                        (field "delimsChanged" boolean (accessflags  *class*  *private* ) -1)
                        (field "maxDelimCodePoint" int (accessflags  *class*  *private* ) -1)
                        (field "hasSurrogates" boolean (accessflags  *class*  *private* ) -1)
                        (field "delimiterCodePoints" (array int) (accessflags  *class*  *private* ) -1))
            (methods
                        (method "setMaxDelimCodePoint"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 3) (max_locals . 6) (code_length . 156)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "delimiters" "java.util.StringTokenizer" (class "java.lang.String")))) 
                                      (4 (ifnonnull 13)) ;;to TAG_0
                                      (7 (aload_0)) 
                                      (8 (iconst_0)) 
                                      (9 (putfield (fieldCP "maxDelimCodePoint" "java.util.StringTokenizer" int))) 
                                      (12 (return)) 
                                      (13 (iconst_0)) ;;at TAG_0
                                      (14 (istore_1)) 
                                      (15 (iconst_0)) 
                                      (16 (istore_3)) 
                                      (17 (iconst_0)) 
                                      (18 (istore 4)) 
                                      (20 (iload 4)) ;;at TAG_4
                                      (22 (aload_0)) 
                                      (23 (getfield (fieldCP "delimiters" "java.util.StringTokenizer" (class "java.lang.String")))) 
                                      (26 (invokevirtual (methodCP "length" "java.lang.String" () int))) 
                                      (29 (if_icmpge 91)) ;;to TAG_1
                                      (32 (aload_0)) 
                                      (33 (getfield (fieldCP "delimiters" "java.util.StringTokenizer" (class "java.lang.String")))) 
                                      (36 (iload 4)) 
                                      (38 (invokevirtual (methodCP "charAt" "java.lang.String" (int) char))) 
                                      (41 (istore_2)) 
                                      (42 (iload_2)) 
                                      (43 (ldc 0)) ;;INT:: "55296"
                                      (45 (if_icmplt 69))  ;;to TAG_2
                                      (48 (iload_2)) 
                                      (49 (ldc 1)) ;;INT:: "57343"
                                      (51 (if_icmpgt 69))  ;;to TAG_2
                                      (54 (aload_0)) 
                                      (55 (getfield (fieldCP "delimiters" "java.util.StringTokenizer" (class "java.lang.String")))) 
                                      (58 (iload 4)) 
                                      (60 (invokevirtual (methodCP "codePointAt" "java.lang.String" (int) int))) 
                                      (63 (istore_2)) 
                                      (64 (aload_0)) 
                                      (65 (iconst_1)) 
                                      (66 (putfield (fieldCP "hasSurrogates" "java.util.StringTokenizer" boolean))) 
                                      (69 (iload_1)) ;;at TAG_2
                                      (70 (iload_2)) 
                                      (71 (if_icmpge 76)) ;;to TAG_3
                                      (74 (iload_2)) 
                                      (75 (istore_1)) 
                                      (76 (iinc 3 1)) ;;at TAG_3
                                      (79 (iload 4)) 
                                      (81 (iload_2)) 
                                      (82 (invokestatic (methodCP "charCount" "java.lang.Character" (int) int))) 
                                      (85 (iadd)) 
                                      (86 (istore 4)) 
                                      (88 (goto 20)) ;;to TAG_4
                                      (91 (aload_0)) ;;at TAG_1
                                      (92 (iload_1)) 
                                      (93 (putfield (fieldCP "maxDelimCodePoint" "java.util.StringTokenizer" int))) 
                                      (96 (aload_0)) 
                                      (97 (getfield (fieldCP "hasSurrogates" "java.util.StringTokenizer" boolean))) 
                                      (100 (ifeq 155)) ;;to TAG_5
                                      (103 (aload_0)) 
                                      (104 (iload_3)) 
                                      (105 (newarray INT)) 
                                      (107 (putfield (fieldCP "delimiterCodePoints" "java.util.StringTokenizer" (array int)))) 
                                      (110 (iconst_0)) 
                                      (111 (istore 4)) 
                                      (113 (iconst_0)) 
                                      (114 (istore 5)) 
                                      (116 (iload 4)) ;;at TAG_6
                                      (118 (iload_3)) 
                                      (119 (if_icmpge 155)) ;;to TAG_5
                                      (122 (aload_0)) 
                                      (123 (getfield (fieldCP "delimiters" "java.util.StringTokenizer" (class "java.lang.String")))) 
                                      (126 (iload 5)) 
                                      (128 (invokevirtual (methodCP "codePointAt" "java.lang.String" (int) int))) 
                                      (131 (istore_2)) 
                                      (132 (aload_0)) 
                                      (133 (getfield (fieldCP "delimiterCodePoints" "java.util.StringTokenizer" (array int)))) 
                                      (136 (iload 4)) 
                                      (138 (iload_2)) 
                                      (139 (iastore)) 
                                      (140 (iinc 4 1)) 
                                      (143 (iload 5)) 
                                      (145 (iload_2)) 
                                      (146 (invokestatic (methodCP "charCount" "java.lang.Character" (int) int))) 
                                      (149 (iadd)) 
                                      (150 (istore 5)) 
                                      (152 (goto 116)) ;;to TAG_6
                                      (155 (return)) ;;at TAG_5
                                      (endofcode 156))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.String") (class "java.lang.String") boolean)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 52)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (iconst_0))
                                      (6 (putfield (fieldCP "hasSurrogates" "java.util.StringTokenizer" boolean)))
                                      (9 (aload_0))
                                      (10 (iconst_0))
                                      (11 (putfield (fieldCP "currentPosition" "java.util.StringTokenizer" int)))
                                      (14 (aload_0))
                                      (15 (iconst_m1))
                                      (16 (putfield (fieldCP "newPosition" "java.util.StringTokenizer" int)))
                                      (19 (aload_0))
                                      (20 (iconst_0))
                                      (21 (putfield (fieldCP "delimsChanged" "java.util.StringTokenizer" boolean)))
                                      (24 (aload_0))
                                      (25 (aload_1))
                                      (26 (putfield (fieldCP "str" "java.util.StringTokenizer" (class "java.lang.String"))))
                                      (29 (aload_0))
                                      (30 (aload_1))
                                      (31 (invokevirtual
					(methodCP "length" "java.lang.String" () int)))
                                      (34 (putfield (fieldCP "maxPosition" "java.util.StringTokenizer" int)))
                                      (37 (aload_0))
                                      (38 (aload_2))
                                      (39 (putfield (fieldCP "delimiters" "java.util.StringTokenizer" (class "java.lang.String"))))
                                      (42 (aload_0))
                                      (43 (iload_3))
                                      (44 (putfield (fieldCP "retDelims" "java.util.StringTokenizer" boolean)))
                                      (47 (aload_0))
                                      (48 (invokespecial
					(methodCP "setMaxDelimCodePoint" "java.util.StringTokenizer" () void)))
                                      (51 (return))
                                      (endofcode 52))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.String") (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (aload_2))
                                      (3 (iconst_0))
                                      (4 (invokespecial
					(methodCP "<init>" "java.util.StringTokenizer" ((class "java.lang.String") (class "java.lang.String") boolean) void)))
                                      (7 (return))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (ldc 2))         ;;STRING:: " \t\n\r\f"
                                      (4 (iconst_0))
                                      (5 (invokespecial
					(methodCP "<init>" "java.util.StringTokenizer" ((class "java.lang.String") (class "java.lang.String") boolean) void)))
                                      (8 (return))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "skipDelimiters"
                              (parameters int)
                              (returntype . int)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 116)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "delimiters" "java.util.StringTokenizer" (class "java.lang.String")))) 
                                      (4 (ifnonnull 15)) ;;to TAG_0
                                      (7 (new (class "java.lang.NullPointerException"))) 
                                      (10 (dup)) 
                                      (11 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" () void))) 
                                      (14 (athrow)) 
                                      (15 (iload_1)) ;;at TAG_0
                                      (16 (istore_2)) 
                                      (17 (aload_0)) ;;at TAG_4
                                      (18 (getfield (fieldCP "retDelims" "java.util.StringTokenizer" boolean))) 
                                      (21 (ifne 114)) ;;to TAG_1
                                      (24 (iload_2)) 
                                      (25 (aload_0)) 
                                      (26 (getfield (fieldCP "maxPosition" "java.util.StringTokenizer" int))) 
                                      (29 (if_icmpge 114)) ;;to TAG_1
                                      (32 (aload_0)) 
                                      (33 (getfield (fieldCP "hasSurrogates" "java.util.StringTokenizer" boolean))) 
                                      (36 (ifne 76))  ;;to TAG_2
                                      (39 (aload_0)) 
                                      (40 (getfield (fieldCP "str" "java.util.StringTokenizer" (class "java.lang.String")))) 
                                      (43 (iload_2)) 
                                      (44 (invokevirtual (methodCP "charAt" "java.lang.String" (int) char))) 
                                      (47 (istore_3)) 
                                      (48 (iload_3)) 
                                      (49 (aload_0)) 
                                      (50 (getfield (fieldCP "maxDelimCodePoint" "java.util.StringTokenizer" int))) 
                                      (53 (if_icmpgt 114)) ;;to TAG_1
                                      (56 (aload_0)) 
                                      (57 (getfield (fieldCP "delimiters" "java.util.StringTokenizer" (class "java.lang.String")))) 
                                      (60 (iload_3)) 
                                      (61 (invokevirtual (methodCP "indexOf" "java.lang.String" (int) int))) 
                                      (64 (ifge 70)) ;;to TAG_3
                                      (67 (goto 114)) ;;to TAG_1
                                      (70 (iinc 2 1)) ;;at TAG_3
                                      (73 (goto 17)) ;;to TAG_4
                                      (76 (aload_0)) ;;at TAG_2
                                      (77 (getfield (fieldCP "str" "java.util.StringTokenizer" (class "java.lang.String")))) 
                                      (80 (iload_2)) 
                                      (81 (invokevirtual (methodCP "codePointAt" "java.lang.String" (int) int))) 
                                      (84 (istore_3)) 
                                      (85 (iload_3)) 
                                      (86 (aload_0)) 
                                      (87 (getfield (fieldCP "maxDelimCodePoint" "java.util.StringTokenizer" int))) 
                                      (90 (if_icmpgt 114)) ;;to TAG_1
                                      (93 (aload_0)) 
                                      (94 (iload_3)) 
                                      (95 (invokespecial (methodCP "isDelimiter" "java.util.StringTokenizer" (int) boolean))) 
                                      (98 (ifne 104)) ;;to TAG_5
                                      (101 (goto 114)) ;;to TAG_1
                                      (104 (iload_2)) ;;at TAG_5
                                      (105 (iload_3)) 
                                      (106 (invokestatic (methodCP "charCount" "java.lang.Character" (int) int))) 
                                      (109 (iadd)) 
                                      (110 (istore_2)) 
                                      (111 (goto 17)) ;;to TAG_4
                                      (114 (iload_2)) ;;at TAG_1
                                      (115 (ireturn)) 
                                      (endofcode 116))
                                   (Exceptions )
                                   (StackMap )))
                        (method "scanToken"
                              (parameters int)
                              (returntype . int)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 179)
                                   (parsedcode
                                      (0 (iload_1)) 
                                      (1 (istore_2)) 
                                      (2 (iload_2)) ;;at TAG_3
                                      (3 (aload_0)) 
                                      (4 (getfield (fieldCP "maxPosition" "java.util.StringTokenizer" int))) 
                                      (7 (if_icmpge 92)) ;;to TAG_0
                                      (10 (aload_0)) 
                                      (11 (getfield (fieldCP "hasSurrogates" "java.util.StringTokenizer" boolean))) 
                                      (14 (ifne 54)) ;;to TAG_1
                                      (17 (aload_0)) 
                                      (18 (getfield (fieldCP "str" "java.util.StringTokenizer" (class "java.lang.String")))) 
                                      (21 (iload_2)) 
                                      (22 (invokevirtual (methodCP "charAt" "java.lang.String" (int) char))) 
                                      (25 (istore_3)) 
                                      (26 (iload_3)) 
                                      (27 (aload_0)) 
                                      (28 (getfield (fieldCP "maxDelimCodePoint" "java.util.StringTokenizer" int))) 
                                      (31 (if_icmpgt 48))  ;;to TAG_2
                                      (34 (aload_0)) 
                                      (35 (getfield (fieldCP "delimiters" "java.util.StringTokenizer" (class "java.lang.String")))) 
                                      (38 (iload_3)) 
                                      (39 (invokevirtual (methodCP "indexOf" "java.lang.String" (int) int))) 
                                      (42 (iflt 48))  ;;to TAG_2
                                      (45 (goto 92)) ;;to TAG_0
                                      (48 (iinc 2 1)) ;;at TAG_2
                                      (51 (goto 2)) ;;to TAG_3
                                      (54 (aload_0)) ;;at TAG_1
                                      (55 (getfield (fieldCP "str" "java.util.StringTokenizer" (class "java.lang.String")))) 
                                      (58 (iload_2)) 
                                      (59 (invokevirtual (methodCP "codePointAt" "java.lang.String" (int) int))) 
                                      (62 (istore_3)) 
                                      (63 (iload_3)) 
                                      (64 (aload_0)) 
                                      (65 (getfield (fieldCP "maxDelimCodePoint" "java.util.StringTokenizer" int))) 
                                      (68 (if_icmpgt 82)) ;;to TAG_4
                                      (71 (aload_0)) 
                                      (72 (iload_3)) 
                                      (73 (invokespecial (methodCP "isDelimiter" "java.util.StringTokenizer" (int) boolean))) 
                                      (76 (ifeq 82)) ;;to TAG_4
                                      (79 (goto 92)) ;;to TAG_0
                                      (82 (iload_2)) ;;at TAG_4
                                      (83 (iload_3)) 
                                      (84 (invokestatic (methodCP "charCount" "java.lang.Character" (int) int))) 
                                      (87 (iadd)) 
                                      (88 (istore_2)) 
                                      (89 (goto 2)) ;;to TAG_3
                                      (92 (aload_0)) ;;at TAG_0
                                      (93 (getfield (fieldCP "retDelims" "java.util.StringTokenizer" boolean))) 
                                      (96 (ifeq 177)) ;;to TAG_5
                                      (99 (iload_1)) 
                                      (100 (iload_2)) 
                                      (101 (if_icmpne 177)) ;;to TAG_5
                                      (104 (aload_0)) 
                                      (105 (getfield (fieldCP "hasSurrogates" "java.util.StringTokenizer" boolean))) 
                                      (108 (ifne 145)) ;;to TAG_6
                                      (111 (aload_0)) 
                                      (112 (getfield (fieldCP "str" "java.util.StringTokenizer" (class "java.lang.String")))) 
                                      (115 (iload_2)) 
                                      (116 (invokevirtual (methodCP "charAt" "java.lang.String" (int) char))) 
                                      (119 (istore_3)) 
                                      (120 (iload_3)) 
                                      (121 (aload_0)) 
                                      (122 (getfield (fieldCP "maxDelimCodePoint" "java.util.StringTokenizer" int))) 
                                      (125 (if_icmpgt 142)) ;;to TAG_7
                                      (128 (aload_0)) 
                                      (129 (getfield (fieldCP "delimiters" "java.util.StringTokenizer" (class "java.lang.String")))) 
                                      (132 (iload_3)) 
                                      (133 (invokevirtual (methodCP "indexOf" "java.lang.String" (int) int))) 
                                      (136 (iflt 142)) ;;to TAG_7
                                      (139 (iinc 2 1)) 
                                      (142 (goto 177)) ;;to TAG_5;;at TAG_7
                                      (145 (aload_0)) ;;at TAG_6
                                      (146 (getfield (fieldCP "str" "java.util.StringTokenizer" (class "java.lang.String")))) 
                                      (149 (iload_2)) 
                                      (150 (invokevirtual (methodCP "codePointAt" "java.lang.String" (int) int))) 
                                      (153 (istore_3)) 
                                      (154 (iload_3)) 
                                      (155 (aload_0)) 
                                      (156 (getfield (fieldCP "maxDelimCodePoint" "java.util.StringTokenizer" int))) 
                                      (159 (if_icmpgt 177)) ;;to TAG_5
                                      (162 (aload_0)) 
                                      (163 (iload_3)) 
                                      (164 (invokespecial (methodCP "isDelimiter" "java.util.StringTokenizer" (int) boolean))) 
                                      (167 (ifeq 177)) ;;to TAG_5
                                      (170 (iload_2)) 
                                      (171 (iload_3)) 
                                      (172 (invokestatic (methodCP "charCount" "java.lang.Character" (int) int))) 
                                      (175 (iadd)) 
                                      (176 (istore_2)) 
                                      (177 (iload_2)) ;;at TAG_5
                                      (178 (ireturn)) 
                                      (endofcode 179))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isDelimiter"
                              (parameters int)
                              (returntype . boolean)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 31)
                                   (parsedcode
                                      (0 (iconst_0)) 
                                      (1 (istore_2)) 
                                      (2 (iload_2)) ;;at TAG_2
                                      (3 (aload_0)) 
                                      (4 (getfield (fieldCP "delimiterCodePoints" "java.util.StringTokenizer" (array int)))) 
                                      (7 (arraylength)) 
                                      (8 (if_icmpge 29)) ;;to TAG_0
                                      (11 (aload_0)) 
                                      (12 (getfield (fieldCP "delimiterCodePoints" "java.util.StringTokenizer" (array int)))) 
                                      (15 (iload_2)) 
                                      (16 (iaload)) 
                                      (17 (iload_1)) 
                                      (18 (if_icmpne 23)) ;;to TAG_1
                                      (21 (iconst_1)) 
                                      (22 (ireturn)) 
                                      (23 (iinc 2 1)) ;;at TAG_1
                                      (26 (goto 2))  ;;to TAG_2
                                      (29 (iconst_0)) ;;at TAG_0
                                      (30 (ireturn)) 
                                      (endofcode 31))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hasMoreTokens"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 29)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (aload_0)) 
                                      (2 (aload_0)) 
                                      (3 (getfield (fieldCP "currentPosition" "java.util.StringTokenizer" int))) 
                                      (6 (invokespecial (methodCP "skipDelimiters" "java.util.StringTokenizer" (int) int))) 
                                      (9 (putfield (fieldCP "newPosition" "java.util.StringTokenizer" int))) 
                                      (12 (aload_0)) 
                                      (13 (getfield (fieldCP "newPosition" "java.util.StringTokenizer" int))) 
                                      (16 (aload_0)) 
                                      (17 (getfield (fieldCP "maxPosition" "java.util.StringTokenizer" int))) 
                                      (20 (if_icmpge 27))  ;;to TAG_0
                                      (23 (iconst_1)) 
                                      (24 (goto 28)) ;;to TAG_1
                                      (27 (iconst_0)) ;;at TAG_0
                                      (28 (ireturn)) ;;at TAG_1
                                      (endofcode 29))
                                   (Exceptions )
                                   (StackMap )))
                        (method "nextToken"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 92)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (aload_0)) 
                                      (2 (getfield (fieldCP "newPosition" "java.util.StringTokenizer" int))) 
                                      (5 (iflt 22)) ;;to TAG_0
                                      (8 (aload_0)) 
                                      (9 (getfield (fieldCP "delimsChanged" "java.util.StringTokenizer" boolean))) 
                                      (12 (ifne 22)) ;;to TAG_0
                                      (15 (aload_0)) 
                                      (16 (getfield (fieldCP "newPosition" "java.util.StringTokenizer" int))) 
                                      (19 (goto 30)) ;;to TAG_1
                                      (22 (aload_0)) ;;at TAG_0
                                      (23 (aload_0)) 
                                      (24 (getfield (fieldCP "currentPosition" "java.util.StringTokenizer" int))) 
                                      (27 (invokespecial (methodCP "skipDelimiters" "java.util.StringTokenizer" (int) int))) 
                                      (30 (putfield (fieldCP "currentPosition" "java.util.StringTokenizer" int))) ;;at TAG_1
                                      (33 (aload_0)) 
                                      (34 (iconst_0)) 
                                      (35 (putfield (fieldCP "delimsChanged" "java.util.StringTokenizer" boolean))) 
                                      (38 (aload_0)) 
                                      (39 (iconst_m1)) 
                                      (40 (putfield (fieldCP "newPosition" "java.util.StringTokenizer" int))) 
                                      (43 (aload_0)) 
                                      (44 (getfield (fieldCP "currentPosition" "java.util.StringTokenizer" int))) 
                                      (47 (aload_0)) 
                                      (48 (getfield (fieldCP "maxPosition" "java.util.StringTokenizer" int))) 
                                      (51 (if_icmplt 62))  ;;to TAG_2
                                      (54 (new (class "java.util.NoSuchElementException"))) 
                                      (57 (dup)) 
                                      (58 (invokespecial (methodCP "<init>" "java.util.NoSuchElementException" () void))) 
                                      (61 (athrow)) 
                                      (62 (aload_0)) ;;at TAG_2
                                      (63 (getfield (fieldCP "currentPosition" "java.util.StringTokenizer" int))) 
                                      (66 (istore_1)) 
                                      (67 (aload_0)) 
                                      (68 (aload_0)) 
                                      (69 (aload_0)) 
                                      (70 (getfield (fieldCP "currentPosition" "java.util.StringTokenizer" int))) 
                                      (73 (invokespecial (methodCP "scanToken" "java.util.StringTokenizer" (int) int))) 
                                      (76 (putfield (fieldCP "currentPosition" "java.util.StringTokenizer" int))) 
                                      (79 (aload_0)) 
                                      (80 (getfield (fieldCP "str" "java.util.StringTokenizer" (class "java.lang.String")))) 
                                      (83 (iload_1)) 
                                      (84 (aload_0)) 
                                      (85 (getfield (fieldCP "currentPosition" "java.util.StringTokenizer" int))) 
                                      (88 (invokevirtual (methodCP "substring" "java.lang.String" (int int) (class "java.lang.String")))) 
                                      (91 (areturn)) 
                                      (endofcode 92))
                                   (Exceptions )
                                   (StackMap )))
                        (method "nextToken"
                              (parameters (class "java.lang.String"))
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 19)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "delimiters" "java.util.StringTokenizer" (class "java.lang.String"))))
                                      (5 (aload_0))
                                      (6 (iconst_1))
                                      (7 (putfield (fieldCP "delimsChanged" "java.util.StringTokenizer" boolean)))
                                      (10 (aload_0))
                                      (11 (invokespecial
					(methodCP "setMaxDelimCodePoint" "java.util.StringTokenizer" () void)))
                                      (14 (aload_0))
                                      (15 (invokevirtual
					(methodCP "nextToken" "java.util.StringTokenizer" () (class "java.lang.String"))))
                                      (18 (areturn))
                                      (endofcode 19))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hasMoreElements"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokevirtual
					(methodCP "hasMoreTokens" "java.util.StringTokenizer" () boolean)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "nextElement"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokevirtual
					(methodCP "nextToken" "java.util.StringTokenizer" () (class "java.lang.String"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "countTokens"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 46)
                                   (parsedcode
                                      (0 (iconst_0)) 
                                      (1 (istore_1)) 
                                      (2 (aload_0)) 
                                      (3 (getfield (fieldCP "currentPosition" "java.util.StringTokenizer" int))) 
                                      (6 (istore_2)) 
                                      (7 (iload_2)) ;;at TAG_2
                                      (8 (aload_0)) 
                                      (9 (getfield (fieldCP "maxPosition" "java.util.StringTokenizer" int))) 
                                      (12 (if_icmpge 44)) ;;to TAG_0
                                      (15 (aload_0)) 
                                      (16 (iload_2)) 
                                      (17 (invokespecial (methodCP "skipDelimiters" "java.util.StringTokenizer" (int) int))) 
                                      (20 (istore_2)) 
                                      (21 (iload_2)) 
                                      (22 (aload_0)) 
                                      (23 (getfield (fieldCP "maxPosition" "java.util.StringTokenizer" int))) 
                                      (26 (if_icmplt 32)) ;;to TAG_1
                                      (29 (goto 44)) ;;to TAG_0
                                      (32 (aload_0)) ;;at TAG_1
                                      (33 (iload_2)) 
                                      (34 (invokespecial (methodCP "scanToken" "java.util.StringTokenizer" (int) int))) 
                                      (37 (istore_2)) 
                                      (38 (iinc 1 1)) 
                                      (41 (goto 7))  ;;to TAG_2
                                      (44 (iload_1)) ;;at TAG_0
                                      (45 (ireturn)) 
                                      (endofcode 46))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.util.Enumeration")
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")))))


(defconst *StringTokenizer-class-table*
  (make-static-class-decls 
   *java.util.StringTokenizer*))

(defconst *package-name-map* 
  ("java.util.StringTokenizer" . "java.util"))

