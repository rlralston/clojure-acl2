; ByteBufferAsCharBufferB-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:38 CDT 2014.
;

(defconst *java.nio.ByteBufferAsCharBufferB*
 (make-class-def
      '(class "java.nio.ByteBufferAsCharBufferB"
            "java.nio.CharBuffer"
            (constant_pool)
            (fields
                        (field "bb" (class "java.nio.ByteBuffer") (accessflags  *class*  *final*  *protected* ) -1)
                        (field "offset" int (accessflags  *class*  *final*  *protected* ) -1)
                        (field "$assertionsDisabled" boolean (accessflags  *class*  *final*  *static* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.nio.ByteBuffer"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 6) (max_locals . 4) (code_length . 64)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (iconst_m1)) 
                                      (2 (iconst_0)) 
                                      (3 (aload_1)) 
                                      (4 (invokevirtual (methodCP "remaining" "java.nio.ByteBuffer" () int))) 
                                      (7 (iconst_1)) 
                                      (8 (ishr)) 
                                      (9 (aload_1)) 
                                      (10 (invokevirtual (methodCP "remaining" "java.nio.ByteBuffer" () int))) 
                                      (13 (iconst_1)) 
                                      (14 (ishr)) 
                                      (15 (invokespecial (methodCP "<init>" "java.nio.CharBuffer" (int int int int) void))) 
                                      (18 (aload_0)) 
                                      (19 (aload_1)) 
                                      (20 (putfield (fieldCP "bb" "java.nio.ByteBufferAsCharBufferB" (class "java.nio.ByteBuffer")))) 
                                      (23 (aload_0)) 
                                      (24 (invokevirtual (methodCP "capacity" "java.nio.ByteBufferAsCharBufferB" () int))) 
                                      (27 (istore_2)) 
                                      (28 (aload_0)) 
                                      (29 (iload_2)) 
                                      (30 (invokevirtual (methodCP "limit" "java.nio.ByteBufferAsCharBufferB" (int) (class "java.nio.Buffer")))) 
                                      (33 (pop)) 
                                      (34 (aload_0)) 
                                      (35 (invokevirtual (methodCP "position" "java.nio.ByteBufferAsCharBufferB" () int))) 
                                      (38 (istore_3)) 
                                      (39 (getstatic (fieldCP "$assertionsDisabled" "java.nio.ByteBufferAsCharBufferB" boolean))) 
                                      (42 (ifne 58))  ;;to TAG_0
                                      (45 (iload_3)) 
                                      (46 (iload_2)) 
                                      (47 (if_icmple 58))  ;;to TAG_0
                                      (50 (new (class "java.lang.AssertionError"))) 
                                      (53 (dup)) 
                                      (54 (invokespecial (methodCP "<init>" "java.lang.AssertionError" () void))) 
                                      (57 (athrow)) 
                                      (58 (aload_0)) ;;at TAG_0
                                      (59 (iload_3)) 
                                      (60 (putfield (fieldCP "offset" "java.nio.ByteBufferAsCharBufferB" int))) 
                                      (63 (return)) 
                                      (endofcode 64))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.nio.ByteBuffer") int int int int int)
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 5) (max_locals . 7) (code_length . 22)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (iload_2))
                                      (2 (iload_3))
                                      (3 (iload 4))
                                      (5 (iload 5))
                                      (7 (invokespecial
					(methodCP "<init>" "java.nio.CharBuffer" (int int int int) void)))
                                      (10 (aload_0))
                                      (11 (aload_1))
                                      (12 (putfield (fieldCP "bb" "java.nio.ByteBufferAsCharBufferB" (class "java.nio.ByteBuffer"))))
                                      (15 (aload_0))
                                      (16 (iload 6))
                                      (18 (putfield (fieldCP "offset" "java.nio.ByteBufferAsCharBufferB" int)))
                                      (21 (return))
                                      (endofcode 22))
                                   (Exceptions )
                                   (StackMap )))
                        (method "slice"
                              (parameters )
                              (returntype . (class "java.nio.CharBuffer"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 8) (max_locals . 5) (code_length . 89)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "position" "java.nio.ByteBufferAsCharBufferB" () int))) 
                                      (4 (istore_1)) 
                                      (5 (aload_0)) 
                                      (6 (invokevirtual (methodCP "limit" "java.nio.ByteBufferAsCharBufferB" () int))) 
                                      (9 (istore_2)) 
                                      (10 (getstatic (fieldCP "$assertionsDisabled" "java.nio.ByteBufferAsCharBufferB" boolean))) 
                                      (13 (ifne 29)) ;;to TAG_0
                                      (16 (iload_1)) 
                                      (17 (iload_2)) 
                                      (18 (if_icmple 29)) ;;to TAG_0
                                      (21 (new (class "java.lang.AssertionError"))) 
                                      (24 (dup)) 
                                      (25 (invokespecial (methodCP "<init>" "java.lang.AssertionError" () void))) 
                                      (28 (athrow)) 
                                      (29 (iload_1)) ;;at TAG_0
                                      (30 (iload_2)) 
                                      (31 (if_icmpgt 40)) ;;to TAG_1
                                      (34 (iload_2)) 
                                      (35 (iload_1)) 
                                      (36 (isub)) 
                                      (37 (goto 41))  ;;to TAG_2
                                      (40 (iconst_0)) ;;at TAG_1
                                      (41 (istore_3)) ;;at TAG_2
                                      (42 (iload_1)) 
                                      (43 (iconst_1)) 
                                      (44 (ishl)) 
                                      (45 (aload_0)) 
                                      (46 (getfield (fieldCP "offset" "java.nio.ByteBufferAsCharBufferB" int))) 
                                      (49 (iadd)) 
                                      (50 (istore 4)) 
                                      (52 (getstatic (fieldCP "$assertionsDisabled" "java.nio.ByteBufferAsCharBufferB" boolean))) 
                                      (55 (ifne 71)) ;;to TAG_3
                                      (58 (iload 4)) 
                                      (60 (ifge 71)) ;;to TAG_3
                                      (63 (new (class "java.lang.AssertionError"))) 
                                      (66 (dup)) 
                                      (67 (invokespecial (methodCP "<init>" "java.lang.AssertionError" () void))) 
                                      (70 (athrow)) 
                                      (71 (new (class "java.nio.ByteBufferAsCharBufferB"))) ;;at TAG_3
                                      (74 (dup)) 
                                      (75 (aload_0)) 
                                      (76 (getfield (fieldCP "bb" "java.nio.ByteBufferAsCharBufferB" (class "java.nio.ByteBuffer")))) 
                                      (79 (iconst_m1)) 
                                      (80 (iconst_0)) 
                                      (81 (iload_3)) 
                                      (82 (iload_3)) 
                                      (83 (iload 4)) 
                                      (85 (invokespecial (methodCP "<init>" "java.nio.ByteBufferAsCharBufferB" ((class "java.nio.ByteBuffer") int int int int int) void))) 
                                      (88 (areturn)) 
                                      (endofcode 89))
                                   (Exceptions )
                                   (StackMap )))
                        (method "duplicate"
                              (parameters )
                              (returntype . (class "java.nio.CharBuffer"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 8) (max_locals . 1) (code_length . 32)
                                   (parsedcode
                                      (0 (new (class "java.nio.ByteBufferAsCharBufferB")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "bb" "java.nio.ByteBufferAsCharBufferB" (class "java.nio.ByteBuffer"))))
                                      (8 (aload_0))
                                      (9 (invokevirtual
					(methodCP "markValue" "java.nio.ByteBufferAsCharBufferB" () int)))
                                      (12 (aload_0))
                                      (13 (invokevirtual
					(methodCP "position" "java.nio.ByteBufferAsCharBufferB" () int)))
                                      (16 (aload_0))
                                      (17 (invokevirtual
					(methodCP "limit" "java.nio.ByteBufferAsCharBufferB" () int)))
                                      (20 (aload_0))
                                      (21 (invokevirtual
					(methodCP "capacity" "java.nio.ByteBufferAsCharBufferB" () int)))
                                      (24 (aload_0))
                                      (25 (getfield (fieldCP "offset" "java.nio.ByteBufferAsCharBufferB" int)))
                                      (28 (invokespecial
					(methodCP "<init>" "java.nio.ByteBufferAsCharBufferB" ((class "java.nio.ByteBuffer") int int int int int) void)))
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
                                      (0 (new (class "java.nio.ByteBufferAsCharBufferRB")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "bb" "java.nio.ByteBufferAsCharBufferB" (class "java.nio.ByteBuffer"))))
                                      (8 (aload_0))
                                      (9 (invokevirtual
					(methodCP "markValue" "java.nio.ByteBufferAsCharBufferB" () int)))
                                      (12 (aload_0))
                                      (13 (invokevirtual
					(methodCP "position" "java.nio.ByteBufferAsCharBufferB" () int)))
                                      (16 (aload_0))
                                      (17 (invokevirtual
					(methodCP "limit" "java.nio.ByteBufferAsCharBufferB" () int)))
                                      (20 (aload_0))
                                      (21 (invokevirtual
					(methodCP "capacity" "java.nio.ByteBufferAsCharBufferB" () int)))
                                      (24 (aload_0))
                                      (25 (getfield (fieldCP "offset" "java.nio.ByteBufferAsCharBufferB" int)))
                                      (28 (invokespecial
					(methodCP "<init>" "java.nio.ByteBufferAsCharBufferRB" ((class "java.nio.ByteBuffer") int int int int int) void)))
                                      (31 (areturn))
                                      (endofcode 32))
                                   (Exceptions )
                                   (StackMap )))
                        (method "ix"
                              (parameters int)
                              (returntype . int)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 9)
                                   (parsedcode
                                      (0 (iload_1))
                                      (1 (iconst_1))
                                      (2 (ishl))
                                      (3 (aload_0))
                                      (4 (getfield (fieldCP "offset" "java.nio.ByteBufferAsCharBufferB" int)))
                                      (7 (iadd))
                                      (8 (ireturn))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "get"
                              (parameters )
                              (returntype . char)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 16)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "bb" "java.nio.ByteBufferAsCharBufferB" (class "java.nio.ByteBuffer"))))
                                      (4 (aload_0))
                                      (5 (aload_0))
                                      (6 (invokevirtual
					(methodCP "nextGetIndex" "java.nio.ByteBufferAsCharBufferB" () int)))
                                      (9 (invokevirtual
					(methodCP "ix" "java.nio.ByteBufferAsCharBufferB" (int) int)))
                                      (12 (invokestatic
					(methodCP "getCharB" "java.nio.Bits" ((class "java.nio.ByteBuffer") int) char)))
                                      (15 (ireturn))
                                      (endofcode 16))
                                   (Exceptions )
                                   (StackMap )))
                        (method "get"
                              (parameters int)
                              (returntype . char)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 17)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "bb" "java.nio.ByteBufferAsCharBufferB" (class "java.nio.ByteBuffer"))))
                                      (4 (aload_0))
                                      (5 (aload_0))
                                      (6 (iload_1))
                                      (7 (invokevirtual
					(methodCP "checkIndex" "java.nio.ByteBufferAsCharBufferB" (int) int)))
                                      (10 (invokevirtual
					(methodCP "ix" "java.nio.ByteBufferAsCharBufferB" (int) int)))
                                      (13 (invokestatic
					(methodCP "getCharB" "java.nio.Bits" ((class "java.nio.ByteBuffer") int) char)))
                                      (16 (ireturn))
                                      (endofcode 17))
                                   (Exceptions )
                                   (StackMap )))
                        (method "put"
                              (parameters char)
                              (returntype . (class "java.nio.CharBuffer"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 18)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "bb" "java.nio.ByteBufferAsCharBufferB" (class "java.nio.ByteBuffer"))))
                                      (4 (aload_0))
                                      (5 (aload_0))
                                      (6 (invokevirtual
					(methodCP "nextPutIndex" "java.nio.ByteBufferAsCharBufferB" () int)))
                                      (9 (invokevirtual
					(methodCP "ix" "java.nio.ByteBufferAsCharBufferB" (int) int)))
                                      (12 (iload_1))
                                      (13 (invokestatic
					(methodCP "putCharB" "java.nio.Bits" ((class "java.nio.ByteBuffer") int char) void)))
                                      (16 (aload_0))
                                      (17 (areturn))
                                      (endofcode 18))
                                   (Exceptions )
                                   (StackMap )))
                        (method "put"
                              (parameters int char)
                              (returntype . (class "java.nio.CharBuffer"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 19)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "bb" "java.nio.ByteBufferAsCharBufferB" (class "java.nio.ByteBuffer"))))
                                      (4 (aload_0))
                                      (5 (aload_0))
                                      (6 (iload_1))
                                      (7 (invokevirtual
					(methodCP "checkIndex" "java.nio.ByteBufferAsCharBufferB" (int) int)))
                                      (10 (invokevirtual
					(methodCP "ix" "java.nio.ByteBufferAsCharBufferB" (int) int)))
                                      (13 (iload_2))
                                      (14 (invokestatic
					(methodCP "putCharB" "java.nio.Bits" ((class "java.nio.ByteBuffer") int char) void)))
                                      (17 (aload_0))
                                      (18 (areturn))
                                      (endofcode 19))
                                   (Exceptions )
                                   (StackMap )))
                        (method "compact"
                              (parameters )
                              (returntype . (class "java.nio.CharBuffer"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 6) (code_length . 116)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "position" "java.nio.ByteBufferAsCharBufferB" () int))) 
                                      (4 (istore_1)) 
                                      (5 (aload_0)) 
                                      (6 (invokevirtual (methodCP "limit" "java.nio.ByteBufferAsCharBufferB" () int))) 
                                      (9 (istore_2)) 
                                      (10 (getstatic (fieldCP "$assertionsDisabled" "java.nio.ByteBufferAsCharBufferB" boolean))) 
                                      (13 (ifne 29)) ;;to TAG_0
                                      (16 (iload_1)) 
                                      (17 (iload_2)) 
                                      (18 (if_icmple 29)) ;;to TAG_0
                                      (21 (new (class "java.lang.AssertionError"))) 
                                      (24 (dup)) 
                                      (25 (invokespecial (methodCP "<init>" "java.lang.AssertionError" () void))) 
                                      (28 (athrow)) 
                                      (29 (iload_1)) ;;at TAG_0
                                      (30 (iload_2)) 
                                      (31 (if_icmpgt 40)) ;;to TAG_1
                                      (34 (iload_2)) 
                                      (35 (iload_1)) 
                                      (36 (isub)) 
                                      (37 (goto 41))  ;;to TAG_2
                                      (40 (iconst_0)) ;;at TAG_1
                                      (41 (istore_3)) ;;at TAG_2
                                      (42 (aload_0)) 
                                      (43 (getfield (fieldCP "bb" "java.nio.ByteBufferAsCharBufferB" (class "java.nio.ByteBuffer")))) 
                                      (46 (invokevirtual (methodCP "duplicate" "java.nio.ByteBuffer" () (class "java.nio.ByteBuffer")))) 
                                      (49 (astore 4)) 
                                      (51 (aload 4)) 
                                      (53 (aload_0)) 
                                      (54 (iload_2)) 
                                      (55 (invokevirtual (methodCP "ix" "java.nio.ByteBufferAsCharBufferB" (int) int))) 
                                      (58 (invokevirtual (methodCP "limit" "java.nio.ByteBuffer" (int) (class "java.nio.Buffer")))) 
                                      (61 (pop)) 
                                      (62 (aload 4)) 
                                      (64 (aload_0)) 
                                      (65 (iconst_0)) 
                                      (66 (invokevirtual (methodCP "ix" "java.nio.ByteBufferAsCharBufferB" (int) int))) 
                                      (69 (invokevirtual (methodCP "position" "java.nio.ByteBuffer" (int) (class "java.nio.Buffer")))) 
                                      (72 (pop)) 
                                      (73 (aload 4)) 
                                      (75 (invokevirtual (methodCP "slice" "java.nio.ByteBuffer" () (class "java.nio.ByteBuffer")))) 
                                      (78 (astore 5)) 
                                      (80 (aload 5)) 
                                      (82 (iload_1)) 
                                      (83 (iconst_1)) 
                                      (84 (ishl)) 
                                      (85 (invokevirtual (methodCP "position" "java.nio.ByteBuffer" (int) (class "java.nio.Buffer")))) 
                                      (88 (pop)) 
                                      (89 (aload 5)) 
                                      (91 (invokevirtual (methodCP "compact" "java.nio.ByteBuffer" () (class "java.nio.ByteBuffer")))) 
                                      (94 (pop)) 
                                      (95 (aload_0)) 
                                      (96 (iload_3)) 
                                      (97 (invokevirtual (methodCP "position" "java.nio.ByteBufferAsCharBufferB" (int) (class "java.nio.Buffer")))) 
                                      (100 (pop)) 
                                      (101 (aload_0)) 
                                      (102 (aload_0)) 
                                      (103 (invokevirtual (methodCP "capacity" "java.nio.ByteBufferAsCharBufferB" () int))) 
                                      (106 (invokevirtual (methodCP "limit" "java.nio.ByteBufferAsCharBufferB" (int) (class "java.nio.Buffer")))) 
                                      (109 (pop)) 
                                      (110 (aload_0)) 
                                      (111 (invokevirtual (methodCP "discardMark" "java.nio.ByteBufferAsCharBufferB" () void))) 
                                      (114 (aload_0)) 
                                      (115 (areturn)) 
                                      (endofcode 116))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isDirect"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "bb" "java.nio.ByteBufferAsCharBufferB" (class "java.nio.ByteBuffer"))))
                                      (4 (invokevirtual
					(methodCP "isDirect" "java.nio.ByteBuffer" () boolean)))
                                      (7 (ireturn))
                                      (endofcode 8))
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
                        (method "toString"
                              (parameters int int)
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 7) (code_length . 84)
                                   (parsedcode
                                      (0 (iload_2)) 
                                      (1 (aload_0)) 
                                      (2 (invokevirtual (methodCP "limit" "java.nio.ByteBufferAsCharBufferB" () int))) 
                                      (5 (if_icmpgt 13)) ;;to TAG_0
                                      (8 (iload_1)) 
                                      (9 (iload_2)) 
                                      (10 (if_icmple 21)) ;;to TAG_1
                                      (13 (new (class "java.lang.IndexOutOfBoundsException"))) ;;at TAG_0
                                      (16 (dup)) 
                                      (17 (invokespecial (methodCP "<init>" "java.lang.IndexOutOfBoundsException" () void))) 
                                      (20 (athrow)) 
                                      (21 (iload_2)) ;;at TAG_1
                                      (22 (iload_1)) 
                                      (23 (isub)) 
                                      (24 (istore_3)) 
                                      (25 (iload_3)) 
                                      (26 (newarray CHAR)) 
                                      (28 (astore 4)) 
                                      (30 (aload 4)) 
                                      (32 (invokestatic (methodCP "wrap" "java.nio.CharBuffer" ((array char)) (class "java.nio.CharBuffer")))) 
                                      (35 (astore 5)) 
                                      (37 (aload_0)) 
                                      (38 (invokevirtual (methodCP "duplicate" "java.nio.ByteBufferAsCharBufferB" () (class "java.nio.CharBuffer")))) 
                                      (41 (astore 6)) 
                                      (43 (aload 6)) 
                                      (45 (iload_1)) 
                                      (46 (invokevirtual (methodCP "position" "java.nio.CharBuffer" (int) (class "java.nio.Buffer")))) 
                                      (49 (pop)) 
                                      (50 (aload 6)) 
                                      (52 (iload_2)) 
                                      (53 (invokevirtual (methodCP "limit" "java.nio.CharBuffer" (int) (class "java.nio.Buffer")))) 
                                      (56 (pop)) 
                                      (57 (aload 5)) 
                                      (59 (aload 6)) 
                                      (61 (invokevirtual (methodCP "put" "java.nio.CharBuffer" ((class "java.nio.CharBuffer")) (class "java.nio.CharBuffer")))) 
                                      (64 (pop)) 
                                      (65 (new (class "java.lang.String"))) 
                                      (68 (dup)) 
                                      (69 (aload 4)) 
                                      (71 (invokespecial (methodCP "<init>" "java.lang.String" ((array char)) void))) 
                                      (74 (areturn)) ;;at TAG_2
                                      (75 (astore_3)) ;;at TAG_3
                                      (76 (new (class "java.lang.IndexOutOfBoundsException"))) 
                                      (79 (dup)) 
                                      (80 (invokespecial (methodCP "<init>" "java.lang.IndexOutOfBoundsException" () void))) 
                                      (83 (athrow)) 
                                      (endofcode 84))
                                   (Exceptions 
                                     (handler 21 74  75 (class "java.lang.StringIndexOutOfBoundsException")))
                                   (StackMap )))
                        (method "subSequence"
                              (parameters int int)
                              (returntype . (class "java.nio.CharBuffer"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 8) (max_locals . 6) (code_length . 100)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "position" "java.nio.ByteBufferAsCharBufferB" () int))) 
                                      (4 (istore_3)) 
                                      (5 (aload_0)) 
                                      (6 (invokevirtual (methodCP "limit" "java.nio.ByteBufferAsCharBufferB" () int))) 
                                      (9 (istore 4)) 
                                      (11 (getstatic (fieldCP "$assertionsDisabled" "java.nio.ByteBufferAsCharBufferB" boolean))) 
                                      (14 (ifne 31)) ;;to TAG_0
                                      (17 (iload_3)) 
                                      (18 (iload 4)) 
                                      (20 (if_icmple 31)) ;;to TAG_0
                                      (23 (new (class "java.lang.AssertionError"))) 
                                      (26 (dup)) 
                                      (27 (invokespecial (methodCP "<init>" "java.lang.AssertionError" () void))) 
                                      (30 (athrow)) 
                                      (31 (iload_3)) ;;at TAG_0
                                      (32 (iload 4)) 
                                      (34 (if_icmpgt 41)) ;;to TAG_1
                                      (37 (iload_3)) 
                                      (38 (goto 43))  ;;to TAG_2
                                      (41 (iload 4)) ;;at TAG_1
                                      (43 (istore_3)) ;;at TAG_2
                                      (44 (iload 4)) 
                                      (46 (iload_3)) 
                                      (47 (isub)) 
                                      (48 (istore 5)) 
                                      (50 (iload_1)) 
                                      (51 (iflt 65)) ;;to TAG_3
                                      (54 (iload_2)) 
                                      (55 (iload 5)) 
                                      (57 (if_icmpgt 65)) ;;to TAG_3
                                      (60 (iload_1)) 
                                      (61 (iload_2)) 
                                      (62 (if_icmple 73)) ;;to TAG_4
                                      (65 (new (class "java.lang.IndexOutOfBoundsException"))) ;;at TAG_3
                                      (68 (dup)) 
                                      (69 (invokespecial (methodCP "<init>" "java.lang.IndexOutOfBoundsException" () void))) 
                                      (72 (athrow)) 
                                      (73 (new (class "java.nio.ByteBufferAsCharBufferB"))) ;;at TAG_4
                                      (76 (dup)) 
                                      (77 (aload_0)) 
                                      (78 (getfield (fieldCP "bb" "java.nio.ByteBufferAsCharBufferB" (class "java.nio.ByteBuffer")))) 
                                      (81 (iconst_m1)) 
                                      (82 (iload_3)) 
                                      (83 (iload_1)) 
                                      (84 (iadd)) 
                                      (85 (iload_3)) 
                                      (86 (iload_2)) 
                                      (87 (iadd)) 
                                      (88 (aload_0)) 
                                      (89 (invokevirtual (methodCP "capacity" "java.nio.ByteBufferAsCharBufferB" () int))) 
                                      (92 (aload_0)) 
                                      (93 (getfield (fieldCP "offset" "java.nio.ByteBufferAsCharBufferB" int))) 
                                      (96 (invokespecial (methodCP "<init>" "java.nio.ByteBufferAsCharBufferB" ((class "java.nio.ByteBuffer") int int int int int) void))) 
                                      (99 (areturn)) 
                                      (endofcode 100))
                                   (Exceptions )
                                   (StackMap )))
                        (method "order"
                              (parameters )
                              (returntype . (class "java.nio.ByteOrder"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 4)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "BIG_ENDIAN" "java.nio.ByteOrder" (class "java.nio.ByteOrder"))))
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
					(methodCP "subSequence" "java.nio.ByteBufferAsCharBufferB" (int int) (class "java.nio.CharBuffer"))))
                                      (6 (areturn))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 0) (code_length . 18)
                                   (parsedcode
                                      (0 (ldc_w )) 
                                      (3 (invokevirtual (methodCP "desiredAssertionStatus" "java.lang.Class" () boolean))) 
                                      (6 (ifne 13))  ;;to TAG_0
                                      (9 (iconst_1)) 
                                      (10 (goto 14)) ;;to TAG_1
                                      (13 (iconst_0)) ;;at TAG_0
                                      (14 (putstatic (fieldCP "$assertionsDisabled" "java.nio.ByteBufferAsCharBufferB" boolean))) ;;at TAG_1
                                      (17 (return)) 
                                      (endofcode 18))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *ByteBufferAsCharBufferB-class-table*
  (make-static-class-decls 
   *java.nio.ByteBufferAsCharBufferB*))

(defconst *package-name-map* 
  ("java.nio.ByteBufferAsCharBufferB" . "java.nio"))

