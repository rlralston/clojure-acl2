; HeapCharBuffer-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:39 CDT 2014.
;

(defconst *java.nio.HeapCharBuffer*
 (make-class-def
      '(class "java.nio.HeapCharBuffer"
            "java.nio.CharBuffer"
            (constant_pool)
            (fields)
            (methods
                        (method "<init>"
                              (parameters int int)
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 7) (max_locals . 3) (code_length . 13)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (iconst_m1))
                                      (2 (iconst_0))
                                      (3 (iload_2))
                                      (4 (iload_1))
                                      (5 (iload_1))
                                      (6 (newarray CHAR))
                                      (8 (iconst_0))
                                      (9 (invokespecial
					(methodCP "<init>" "java.nio.CharBuffer" (int int int int (array char) int) void)))
                                      (12 (return))
                                      (endofcode 13))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (array char) int int)
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 7) (max_locals . 4) (code_length . 14)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (iconst_m1))
                                      (2 (iload_2))
                                      (3 (iload_2))
                                      (4 (iload_3))
                                      (5 (iadd))
                                      (6 (aload_1))
                                      (7 (arraylength))
                                      (8 (aload_1))
                                      (9 (iconst_0))
                                      (10 (invokespecial
					(methodCP "<init>" "java.nio.CharBuffer" (int int int int (array char) int) void)))
                                      (13 (return))
                                      (endofcode 14))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (array char) int int int int int)
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 7) (max_locals . 7) (code_length . 14)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (iload_2))
                                      (2 (iload_3))
                                      (3 (iload 4))
                                      (5 (iload 5))
                                      (7 (aload_1))
                                      (8 (iload 6))
                                      (10 (invokespecial
					(methodCP "<init>" "java.nio.CharBuffer" (int int int int (array char) int) void)))
                                      (13 (return))
                                      (endofcode 14))
                                   (Exceptions )
                                   (StackMap )))
                        (method "slice"
                              (parameters )
                              (returntype . (class "java.nio.CharBuffer"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 9) (max_locals . 1) (code_length . 31)
                                   (parsedcode
                                      (0 (new (class "java.nio.HeapCharBuffer")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "hb" "java.nio.HeapCharBuffer" (array char))))
                                      (8 (iconst_m1))
                                      (9 (iconst_0))
                                      (10 (aload_0))
                                      (11 (invokevirtual
					(methodCP "remaining" "java.nio.HeapCharBuffer" () int)))
                                      (14 (aload_0))
                                      (15 (invokevirtual
					(methodCP "remaining" "java.nio.HeapCharBuffer" () int)))
                                      (18 (aload_0))
                                      (19 (invokevirtual
					(methodCP "position" "java.nio.HeapCharBuffer" () int)))
                                      (22 (aload_0))
                                      (23 (getfield (fieldCP "offset" "java.nio.HeapCharBuffer" int)))
                                      (26 (iadd))
                                      (27 (invokespecial
					(methodCP "<init>" "java.nio.HeapCharBuffer" ((array char) int int int int int) void)))
                                      (30 (areturn))
                                      (endofcode 31))
                                   (Exceptions )
                                   (StackMap )))
                        (method "duplicate"
                              (parameters )
                              (returntype . (class "java.nio.CharBuffer"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 8) (max_locals . 1) (code_length . 32)
                                   (parsedcode
                                      (0 (new (class "java.nio.HeapCharBuffer")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "hb" "java.nio.HeapCharBuffer" (array char))))
                                      (8 (aload_0))
                                      (9 (invokevirtual
					(methodCP "markValue" "java.nio.HeapCharBuffer" () int)))
                                      (12 (aload_0))
                                      (13 (invokevirtual
					(methodCP "position" "java.nio.HeapCharBuffer" () int)))
                                      (16 (aload_0))
                                      (17 (invokevirtual
					(methodCP "limit" "java.nio.HeapCharBuffer" () int)))
                                      (20 (aload_0))
                                      (21 (invokevirtual
					(methodCP "capacity" "java.nio.HeapCharBuffer" () int)))
                                      (24 (aload_0))
                                      (25 (getfield (fieldCP "offset" "java.nio.HeapCharBuffer" int)))
                                      (28 (invokespecial
					(methodCP "<init>" "java.nio.HeapCharBuffer" ((array char) int int int int int) void)))
                                      (31 (areturn))
                                      (endofcode 32))
                                   (Exceptions )
                                   (StackMap )))
                        (method "asReadOnlyBuffer"
                              (parameters )
                              (returntype . (class "java.nio.CharBuffer"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 8) (max_locals . 1) (code_length . 32)
                                   (parsedcode
                                      (0 (new (class "java.nio.HeapCharBufferR")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "hb" "java.nio.HeapCharBuffer" (array char))))
                                      (8 (aload_0))
                                      (9 (invokevirtual
					(methodCP "markValue" "java.nio.HeapCharBuffer" () int)))
                                      (12 (aload_0))
                                      (13 (invokevirtual
					(methodCP "position" "java.nio.HeapCharBuffer" () int)))
                                      (16 (aload_0))
                                      (17 (invokevirtual
					(methodCP "limit" "java.nio.HeapCharBuffer" () int)))
                                      (20 (aload_0))
                                      (21 (invokevirtual
					(methodCP "capacity" "java.nio.HeapCharBuffer" () int)))
                                      (24 (aload_0))
                                      (25 (getfield (fieldCP "offset" "java.nio.HeapCharBuffer" int)))
                                      (28 (invokespecial
					(methodCP "<init>" "java.nio.HeapCharBufferR" ((array char) int int int int int) void)))
                                      (31 (areturn))
                                      (endofcode 32))
                                   (Exceptions )
                                   (StackMap )))
                        (method "ix"
                              (parameters int)
                              (returntype . int)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 7)
                                   (parsedcode
                                      (0 (iload_1))
                                      (1 (aload_0))
                                      (2 (getfield (fieldCP "offset" "java.nio.HeapCharBuffer" int)))
                                      (5 (iadd))
                                      (6 (ireturn))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap )))
                        (method "get"
                              (parameters )
                              (returntype . char)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 14)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "hb" "java.nio.HeapCharBuffer" (array char))))
                                      (4 (aload_0))
                                      (5 (aload_0))
                                      (6 (invokevirtual
					(methodCP "nextGetIndex" "java.nio.HeapCharBuffer" () int)))
                                      (9 (invokevirtual
					(methodCP "ix" "java.nio.HeapCharBuffer" (int) int)))
                                      (12 (caload))
                                      (13 (ireturn))
                                      (endofcode 14))
                                   (Exceptions )
                                   (StackMap )))
                        (method "get"
                              (parameters int)
                              (returntype . char)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "hb" "java.nio.HeapCharBuffer" (array char))))
                                      (4 (aload_0))
                                      (5 (aload_0))
                                      (6 (iload_1))
                                      (7 (invokevirtual
					(methodCP "checkIndex" "java.nio.HeapCharBuffer" (int) int)))
                                      (10 (invokevirtual
					(methodCP "ix" "java.nio.HeapCharBuffer" (int) int)))
                                      (13 (caload))
                                      (14 (ireturn))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "get"
                              (parameters (array char) int int)
                              (returntype . (class "java.nio.CharBuffer"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 4) (code_length . 54)
                                   (parsedcode
                                      (0 (iload_2)) 
                                      (1 (iload_3)) 
                                      (2 (aload_1)) 
                                      (3 (arraylength)) 
                                      (4 (invokestatic (methodCP "checkBounds" "java.nio.HeapCharBuffer" (int int int) void))) 
                                      (7 (iload_3)) 
                                      (8 (aload_0)) 
                                      (9 (invokevirtual (methodCP "remaining" "java.nio.HeapCharBuffer" () int))) 
                                      (12 (if_icmple 23))  ;;to TAG_0
                                      (15 (new (class "java.nio.BufferUnderflowException"))) 
                                      (18 (dup)) 
                                      (19 (invokespecial (methodCP "<init>" "java.nio.BufferUnderflowException" () void))) 
                                      (22 (athrow)) 
                                      (23 (aload_0)) ;;at TAG_0
                                      (24 (getfield (fieldCP "hb" "java.nio.HeapCharBuffer" (array char)))) 
                                      (27 (aload_0)) 
                                      (28 (aload_0)) 
                                      (29 (invokevirtual (methodCP "position" "java.nio.HeapCharBuffer" () int))) 
                                      (32 (invokevirtual (methodCP "ix" "java.nio.HeapCharBuffer" (int) int))) 
                                      (35 (aload_1)) 
                                      (36 (iload_2)) 
                                      (37 (iload_3)) 
                                      (38 (invokestatic (methodCP "arraycopy" "java.lang.System" ((class "java.lang.Object") int (class "java.lang.Object") int int) void))) 
                                      (41 (aload_0)) 
                                      (42 (aload_0)) 
                                      (43 (invokevirtual (methodCP "position" "java.nio.HeapCharBuffer" () int))) 
                                      (46 (iload_3)) 
                                      (47 (iadd)) 
                                      (48 (invokevirtual (methodCP "position" "java.nio.HeapCharBuffer" (int) (class "java.nio.Buffer")))) 
                                      (51 (pop)) 
                                      (52 (aload_0)) 
                                      (53 (areturn)) 
                                      (endofcode 54))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isDirect"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 2)
                                   (parsedcode
                                      (0 (iconst_0))
                                      (1 (ireturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isReadOnly"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 2)
                                   (parsedcode
                                      (0 (iconst_0))
                                      (1 (ireturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap )))
                        (method "put"
                              (parameters char)
                              (returntype . (class "java.nio.CharBuffer"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 16)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "hb" "java.nio.HeapCharBuffer" (array char))))
                                      (4 (aload_0))
                                      (5 (aload_0))
                                      (6 (invokevirtual
					(methodCP "nextPutIndex" "java.nio.HeapCharBuffer" () int)))
                                      (9 (invokevirtual
					(methodCP "ix" "java.nio.HeapCharBuffer" (int) int)))
                                      (12 (iload_1))
                                      (13 (castore))
                                      (14 (aload_0))
                                      (15 (areturn))
                                      (endofcode 16))
                                   (Exceptions )
                                   (StackMap )))
                        (method "put"
                              (parameters int char)
                              (returntype . (class "java.nio.CharBuffer"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 17)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "hb" "java.nio.HeapCharBuffer" (array char))))
                                      (4 (aload_0))
                                      (5 (aload_0))
                                      (6 (iload_1))
                                      (7 (invokevirtual
					(methodCP "checkIndex" "java.nio.HeapCharBuffer" (int) int)))
                                      (10 (invokevirtual
					(methodCP "ix" "java.nio.HeapCharBuffer" (int) int)))
                                      (13 (iload_2))
                                      (14 (castore))
                                      (15 (aload_0))
                                      (16 (areturn))
                                      (endofcode 17))
                                   (Exceptions )
                                   (StackMap )))
                        (method "put"
                              (parameters (array char) int int)
                              (returntype . (class "java.nio.CharBuffer"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 4) (code_length . 54)
                                   (parsedcode
                                      (0 (iload_2)) 
                                      (1 (iload_3)) 
                                      (2 (aload_1)) 
                                      (3 (arraylength)) 
                                      (4 (invokestatic (methodCP "checkBounds" "java.nio.HeapCharBuffer" (int int int) void))) 
                                      (7 (iload_3)) 
                                      (8 (aload_0)) 
                                      (9 (invokevirtual (methodCP "remaining" "java.nio.HeapCharBuffer" () int))) 
                                      (12 (if_icmple 23))  ;;to TAG_0
                                      (15 (new (class "java.nio.BufferOverflowException"))) 
                                      (18 (dup)) 
                                      (19 (invokespecial (methodCP "<init>" "java.nio.BufferOverflowException" () void))) 
                                      (22 (athrow)) 
                                      (23 (aload_1)) ;;at TAG_0
                                      (24 (iload_2)) 
                                      (25 (aload_0)) 
                                      (26 (getfield (fieldCP "hb" "java.nio.HeapCharBuffer" (array char)))) 
                                      (29 (aload_0)) 
                                      (30 (aload_0)) 
                                      (31 (invokevirtual (methodCP "position" "java.nio.HeapCharBuffer" () int))) 
                                      (34 (invokevirtual (methodCP "ix" "java.nio.HeapCharBuffer" (int) int))) 
                                      (37 (iload_3)) 
                                      (38 (invokestatic (methodCP "arraycopy" "java.lang.System" ((class "java.lang.Object") int (class "java.lang.Object") int int) void))) 
                                      (41 (aload_0)) 
                                      (42 (aload_0)) 
                                      (43 (invokevirtual (methodCP "position" "java.nio.HeapCharBuffer" () int))) 
                                      (46 (iload_3)) 
                                      (47 (iadd)) 
                                      (48 (invokevirtual (methodCP "position" "java.nio.HeapCharBuffer" (int) (class "java.nio.Buffer")))) 
                                      (51 (pop)) 
                                      (52 (aload_0)) 
                                      (53 (areturn)) 
                                      (endofcode 54))
                                   (Exceptions )
                                   (StackMap )))
                        (method "put"
                              (parameters (class "java.nio.CharBuffer"))
                              (returntype . (class "java.nio.CharBuffer"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 4) (code_length . 167)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (instanceof (class "java.nio.HeapCharBuffer"))) 
                                      (4 (ifeq 99)) ;;to TAG_0
                                      (7 (aload_1)) 
                                      (8 (aload_0)) 
                                      (9 (if_acmpne 20)) ;;to TAG_1
                                      (12 (new (class "java.lang.IllegalArgumentException"))) 
                                      (15 (dup)) 
                                      (16 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" () void))) 
                                      (19 (athrow)) 
                                      (20 (aload_1)) ;;at TAG_1
                                      (21 (checkcast (class "java.nio.HeapCharBuffer"))) 
                                      (24 (astore_2)) 
                                      (25 (aload_2)) 
                                      (26 (invokevirtual (methodCP "remaining" "java.nio.HeapCharBuffer" () int))) 
                                      (29 (istore_3)) 
                                      (30 (iload_3)) 
                                      (31 (aload_0)) 
                                      (32 (invokevirtual (methodCP "remaining" "java.nio.HeapCharBuffer" () int))) 
                                      (35 (if_icmple 46))  ;;to TAG_2
                                      (38 (new (class "java.nio.BufferOverflowException"))) 
                                      (41 (dup)) 
                                      (42 (invokespecial (methodCP "<init>" "java.nio.BufferOverflowException" () void))) 
                                      (45 (athrow)) 
                                      (46 (aload_2)) ;;at TAG_2
                                      (47 (getfield (fieldCP "hb" "java.nio.HeapCharBuffer" (array char)))) 
                                      (50 (aload_2)) 
                                      (51 (aload_2)) 
                                      (52 (invokevirtual (methodCP "position" "java.nio.HeapCharBuffer" () int))) 
                                      (55 (invokevirtual (methodCP "ix" "java.nio.HeapCharBuffer" (int) int))) 
                                      (58 (aload_0)) 
                                      (59 (getfield (fieldCP "hb" "java.nio.HeapCharBuffer" (array char)))) 
                                      (62 (aload_0)) 
                                      (63 (aload_0)) 
                                      (64 (invokevirtual (methodCP "position" "java.nio.HeapCharBuffer" () int))) 
                                      (67 (invokevirtual (methodCP "ix" "java.nio.HeapCharBuffer" (int) int))) 
                                      (70 (iload_3)) 
                                      (71 (invokestatic (methodCP "arraycopy" "java.lang.System" ((class "java.lang.Object") int (class "java.lang.Object") int int) void))) 
                                      (74 (aload_2)) 
                                      (75 (aload_2)) 
                                      (76 (invokevirtual (methodCP "position" "java.nio.HeapCharBuffer" () int))) 
                                      (79 (iload_3)) 
                                      (80 (iadd)) 
                                      (81 (invokevirtual (methodCP "position" "java.nio.HeapCharBuffer" (int) (class "java.nio.Buffer")))) 
                                      (84 (pop)) 
                                      (85 (aload_0)) 
                                      (86 (aload_0)) 
                                      (87 (invokevirtual (methodCP "position" "java.nio.HeapCharBuffer" () int))) 
                                      (90 (iload_3)) 
                                      (91 (iadd)) 
                                      (92 (invokevirtual (methodCP "position" "java.nio.HeapCharBuffer" (int) (class "java.nio.Buffer")))) 
                                      (95 (pop)) 
                                      (96 (goto 165)) ;;to TAG_3
                                      (99 (aload_1)) ;;at TAG_0
                                      (100 (invokevirtual (methodCP "isDirect" "java.nio.CharBuffer" () boolean))) 
                                      (103 (ifeq 159)) ;;to TAG_4
                                      (106 (aload_1)) 
                                      (107 (invokevirtual (methodCP "remaining" "java.nio.CharBuffer" () int))) 
                                      (110 (istore_2)) 
                                      (111 (iload_2)) 
                                      (112 (aload_0)) 
                                      (113 (invokevirtual (methodCP "remaining" "java.nio.HeapCharBuffer" () int))) 
                                      (116 (if_icmple 127)) ;;to TAG_5
                                      (119 (new (class "java.nio.BufferOverflowException"))) 
                                      (122 (dup)) 
                                      (123 (invokespecial (methodCP "<init>" "java.nio.BufferOverflowException" () void))) 
                                      (126 (athrow)) 
                                      (127 (aload_1)) ;;at TAG_5
                                      (128 (aload_0)) 
                                      (129 (getfield (fieldCP "hb" "java.nio.HeapCharBuffer" (array char)))) 
                                      (132 (aload_0)) 
                                      (133 (aload_0)) 
                                      (134 (invokevirtual (methodCP "position" "java.nio.HeapCharBuffer" () int))) 
                                      (137 (invokevirtual (methodCP "ix" "java.nio.HeapCharBuffer" (int) int))) 
                                      (140 (iload_2)) 
                                      (141 (invokevirtual (methodCP "get" "java.nio.CharBuffer" ((array char) int int) (class "java.nio.CharBuffer")))) 
                                      (144 (pop)) 
                                      (145 (aload_0)) 
                                      (146 (aload_0)) 
                                      (147 (invokevirtual (methodCP "position" "java.nio.HeapCharBuffer" () int))) 
                                      (150 (iload_2)) 
                                      (151 (iadd)) 
                                      (152 (invokevirtual (methodCP "position" "java.nio.HeapCharBuffer" (int) (class "java.nio.Buffer")))) 
                                      (155 (pop)) 
                                      (156 (goto 165)) ;;to TAG_3
                                      (159 (aload_0)) ;;at TAG_4
                                      (160 (aload_1)) 
                                      (161 (invokespecial (methodCP "put" "java.nio.CharBuffer" ((class "java.nio.CharBuffer")) (class "java.nio.CharBuffer")))) 
                                      (164 (pop)) 
                                      (165 (aload_0)) ;;at TAG_3
                                      (166 (areturn)) 
                                      (endofcode 167))
                                   (Exceptions )
                                   (StackMap )))
                        (method "compact"
                              (parameters )
                              (returntype . (class "java.nio.CharBuffer"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 1) (code_length . 52)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "hb" "java.nio.HeapCharBuffer" (array char))))
                                      (4 (aload_0))
                                      (5 (aload_0))
                                      (6 (invokevirtual
					(methodCP "position" "java.nio.HeapCharBuffer" () int)))
                                      (9 (invokevirtual
					(methodCP "ix" "java.nio.HeapCharBuffer" (int) int)))
                                      (12 (aload_0))
                                      (13 (getfield (fieldCP "hb" "java.nio.HeapCharBuffer" (array char))))
                                      (16 (aload_0))
                                      (17 (iconst_0))
                                      (18 (invokevirtual
					(methodCP "ix" "java.nio.HeapCharBuffer" (int) int)))
                                      (21 (aload_0))
                                      (22 (invokevirtual
					(methodCP "remaining" "java.nio.HeapCharBuffer" () int)))
                                      (25 (invokestatic
					(methodCP "arraycopy" "java.lang.System" ((class "java.lang.Object") int (class "java.lang.Object") int int) void)))
                                      (28 (aload_0))
                                      (29 (aload_0))
                                      (30 (invokevirtual
					(methodCP "remaining" "java.nio.HeapCharBuffer" () int)))
                                      (33 (invokevirtual
					(methodCP "position" "java.nio.HeapCharBuffer" (int) (class "java.nio.Buffer"))))
                                      (36 (pop))
                                      (37 (aload_0))
                                      (38 (aload_0))
                                      (39 (invokevirtual
					(methodCP "capacity" "java.nio.HeapCharBuffer" () int)))
                                      (42 (invokevirtual
					(methodCP "limit" "java.nio.HeapCharBuffer" (int) (class "java.nio.Buffer"))))
                                      (45 (pop))
                                      (46 (aload_0))
                                      (47 (invokevirtual
					(methodCP "discardMark" "java.nio.HeapCharBuffer" () void)))
                                      (50 (aload_0))
                                      (51 (areturn))
                                      (endofcode 52))
                                   (Exceptions )
                                   (StackMap )))
                        (method "toString"
                              (parameters int int)
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class* )
                              (code
                                   (max_stack . 6) (max_locals . 4) (code_length . 30)
                                   (parsedcode
                                      (0 (new (class "java.lang.String"))) ;;at TAG_0
                                      (3 (dup)) 
                                      (4 (aload_0)) 
                                      (5 (getfield (fieldCP "hb" "java.nio.HeapCharBuffer" (array char)))) 
                                      (8 (iload_1)) 
                                      (9 (aload_0)) 
                                      (10 (getfield (fieldCP "offset" "java.nio.HeapCharBuffer" int))) 
                                      (13 (iadd)) 
                                      (14 (iload_2)) 
                                      (15 (iload_1)) 
                                      (16 (isub)) 
                                      (17 (invokespecial (methodCP "<init>" "java.lang.String" ((array char) int int) void))) 
                                      (20 (areturn)) ;;at TAG_1
                                      (21 (astore_3)) ;;at TAG_2
                                      (22 (new (class "java.lang.IndexOutOfBoundsException"))) 
                                      (25 (dup)) 
                                      (26 (invokespecial (methodCP "<init>" "java.lang.IndexOutOfBoundsException" () void))) 
                                      (29 (athrow)) 
                                      (endofcode 30))
                                   (Exceptions 
                                     (handler 0 20  21 (class "java.lang.StringIndexOutOfBoundsException")))
                                   (StackMap )))
                        (method "subSequence"
                              (parameters int int)
                              (returntype . (class "java.nio.CharBuffer"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 8) (max_locals . 4) (code_length . 57)
                                   (parsedcode
                                      (0 (iload_1)) 
                                      (1 (iflt 17))  ;;to TAG_0
                                      (4 (iload_2)) 
                                      (5 (aload_0)) 
                                      (6 (invokevirtual (methodCP "length" "java.nio.HeapCharBuffer" () int))) 
                                      (9 (if_icmpgt 17))  ;;to TAG_0
                                      (12 (iload_1)) 
                                      (13 (iload_2)) 
                                      (14 (if_icmple 25)) ;;to TAG_1
                                      (17 (new (class "java.lang.IndexOutOfBoundsException"))) ;;at TAG_0
                                      (20 (dup)) 
                                      (21 (invokespecial (methodCP "<init>" "java.lang.IndexOutOfBoundsException" () void))) 
                                      (24 (athrow)) 
                                      (25 (aload_0)) ;;at TAG_1
                                      (26 (invokevirtual (methodCP "position" "java.nio.HeapCharBuffer" () int))) 
                                      (29 (istore_3)) 
                                      (30 (new (class "java.nio.HeapCharBuffer"))) 
                                      (33 (dup)) 
                                      (34 (aload_0)) 
                                      (35 (getfield (fieldCP "hb" "java.nio.HeapCharBuffer" (array char)))) 
                                      (38 (iconst_m1)) 
                                      (39 (iload_3)) 
                                      (40 (iload_1)) 
                                      (41 (iadd)) 
                                      (42 (iload_3)) 
                                      (43 (iload_2)) 
                                      (44 (iadd)) 
                                      (45 (aload_0)) 
                                      (46 (invokevirtual (methodCP "capacity" "java.nio.HeapCharBuffer" () int))) 
                                      (49 (aload_0)) 
                                      (50 (getfield (fieldCP "offset" "java.nio.HeapCharBuffer" int))) 
                                      (53 (invokespecial (methodCP "<init>" "java.nio.HeapCharBuffer" ((array char) int int int int int) void))) 
                                      (56 (areturn)) 
                                      (endofcode 57))
                                   (Exceptions )
                                   (StackMap )))
                        (method "order"
                              (parameters )
                              (returntype . (class "java.nio.ByteOrder"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 4)
                                   (parsedcode
                                      (0 (invokestatic
					(methodCP "nativeOrder" "java.nio.ByteOrder" () (class "java.nio.ByteOrder"))))
                                      (3 (areturn))
                                      (endofcode 4))
                                   (Exceptions )
                                   (StackMap )))
                        (method "subSequence"
                              (parameters int int)
                              (returntype . (class "java.lang.CharSequence"))
                              (accessflags  *class*  *public*  *volatile* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 7)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (iload_1))
                                      (2 (iload_2))
                                      (3 (invokevirtual
					(methodCP "subSequence" "java.nio.HeapCharBuffer" (int int) (class "java.nio.CharBuffer"))))
                                      (6 (areturn))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *HeapCharBuffer-class-table*
  (make-static-class-decls 
   *java.nio.HeapCharBuffer*))

(defconst *package-name-map* 
  ("java.nio.HeapCharBuffer" . "java.nio"))

