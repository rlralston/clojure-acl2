; ByteBufferAsShortBufferB-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:38 CDT 2014.
;

(defconst *java.nio.ByteBufferAsShortBufferB*
 (make-class-def
      '(class "java.nio.ByteBufferAsShortBufferB"
            "java.nio.ShortBuffer"
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
                                      (15 (invokespecial (methodCP "<init>" "java.nio.ShortBuffer" (int int int int) void))) 
                                      (18 (aload_0)) 
                                      (19 (aload_1)) 
                                      (20 (putfield (fieldCP "bb" "java.nio.ByteBufferAsShortBufferB" (class "java.nio.ByteBuffer")))) 
                                      (23 (aload_0)) 
                                      (24 (invokevirtual (methodCP "capacity" "java.nio.ByteBufferAsShortBufferB" () int))) 
                                      (27 (istore_2)) 
                                      (28 (aload_0)) 
                                      (29 (iload_2)) 
                                      (30 (invokevirtual (methodCP "limit" "java.nio.ByteBufferAsShortBufferB" (int) (class "java.nio.Buffer")))) 
                                      (33 (pop)) 
                                      (34 (aload_0)) 
                                      (35 (invokevirtual (methodCP "position" "java.nio.ByteBufferAsShortBufferB" () int))) 
                                      (38 (istore_3)) 
                                      (39 (getstatic (fieldCP "$assertionsDisabled" "java.nio.ByteBufferAsShortBufferB" boolean))) 
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
                                      (60 (putfield (fieldCP "offset" "java.nio.ByteBufferAsShortBufferB" int))) 
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
					(methodCP "<init>" "java.nio.ShortBuffer" (int int int int) void)))
                                      (10 (aload_0))
                                      (11 (aload_1))
                                      (12 (putfield (fieldCP "bb" "java.nio.ByteBufferAsShortBufferB" (class "java.nio.ByteBuffer"))))
                                      (15 (aload_0))
                                      (16 (iload 6))
                                      (18 (putfield (fieldCP "offset" "java.nio.ByteBufferAsShortBufferB" int)))
                                      (21 (return))
                                      (endofcode 22))
                                   (Exceptions )
                                   (StackMap )))
                        (method "slice"
                              (parameters )
                              (returntype . (class "java.nio.ShortBuffer"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 8) (max_locals . 5) (code_length . 89)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "position" "java.nio.ByteBufferAsShortBufferB" () int))) 
                                      (4 (istore_1)) 
                                      (5 (aload_0)) 
                                      (6 (invokevirtual (methodCP "limit" "java.nio.ByteBufferAsShortBufferB" () int))) 
                                      (9 (istore_2)) 
                                      (10 (getstatic (fieldCP "$assertionsDisabled" "java.nio.ByteBufferAsShortBufferB" boolean))) 
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
                                      (46 (getfield (fieldCP "offset" "java.nio.ByteBufferAsShortBufferB" int))) 
                                      (49 (iadd)) 
                                      (50 (istore 4)) 
                                      (52 (getstatic (fieldCP "$assertionsDisabled" "java.nio.ByteBufferAsShortBufferB" boolean))) 
                                      (55 (ifne 71)) ;;to TAG_3
                                      (58 (iload 4)) 
                                      (60 (ifge 71)) ;;to TAG_3
                                      (63 (new (class "java.lang.AssertionError"))) 
                                      (66 (dup)) 
                                      (67 (invokespecial (methodCP "<init>" "java.lang.AssertionError" () void))) 
                                      (70 (athrow)) 
                                      (71 (new (class "java.nio.ByteBufferAsShortBufferB"))) ;;at TAG_3
                                      (74 (dup)) 
                                      (75 (aload_0)) 
                                      (76 (getfield (fieldCP "bb" "java.nio.ByteBufferAsShortBufferB" (class "java.nio.ByteBuffer")))) 
                                      (79 (iconst_m1)) 
                                      (80 (iconst_0)) 
                                      (81 (iload_3)) 
                                      (82 (iload_3)) 
                                      (83 (iload 4)) 
                                      (85 (invokespecial (methodCP "<init>" "java.nio.ByteBufferAsShortBufferB" ((class "java.nio.ByteBuffer") int int int int int) void))) 
                                      (88 (areturn)) 
                                      (endofcode 89))
                                   (Exceptions )
                                   (StackMap )))
                        (method "duplicate"
                              (parameters )
                              (returntype . (class "java.nio.ShortBuffer"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 8) (max_locals . 1) (code_length . 32)
                                   (parsedcode
                                      (0 (new (class "java.nio.ByteBufferAsShortBufferB")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "bb" "java.nio.ByteBufferAsShortBufferB" (class "java.nio.ByteBuffer"))))
                                      (8 (aload_0))
                                      (9 (invokevirtual
					(methodCP "markValue" "java.nio.ByteBufferAsShortBufferB" () int)))
                                      (12 (aload_0))
                                      (13 (invokevirtual
					(methodCP "position" "java.nio.ByteBufferAsShortBufferB" () int)))
                                      (16 (aload_0))
                                      (17 (invokevirtual
					(methodCP "limit" "java.nio.ByteBufferAsShortBufferB" () int)))
                                      (20 (aload_0))
                                      (21 (invokevirtual
					(methodCP "capacity" "java.nio.ByteBufferAsShortBufferB" () int)))
                                      (24 (aload_0))
                                      (25 (getfield (fieldCP "offset" "java.nio.ByteBufferAsShortBufferB" int)))
                                      (28 (invokespecial
					(methodCP "<init>" "java.nio.ByteBufferAsShortBufferB" ((class "java.nio.ByteBuffer") int int int int int) void)))
                                      (31 (areturn))
                                      (endofcode 32))
                                   (Exceptions )
                                   (StackMap )))
                        (method "asReadOnlyBuffer"
                              (parameters )
                              (returntype . (class "java.nio.ShortBuffer"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 8) (max_locals . 1) (code_length . 32)
                                   (parsedcode
                                      (0 (new (class "java.nio.ByteBufferAsShortBufferRB")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "bb" "java.nio.ByteBufferAsShortBufferB" (class "java.nio.ByteBuffer"))))
                                      (8 (aload_0))
                                      (9 (invokevirtual
					(methodCP "markValue" "java.nio.ByteBufferAsShortBufferB" () int)))
                                      (12 (aload_0))
                                      (13 (invokevirtual
					(methodCP "position" "java.nio.ByteBufferAsShortBufferB" () int)))
                                      (16 (aload_0))
                                      (17 (invokevirtual
					(methodCP "limit" "java.nio.ByteBufferAsShortBufferB" () int)))
                                      (20 (aload_0))
                                      (21 (invokevirtual
					(methodCP "capacity" "java.nio.ByteBufferAsShortBufferB" () int)))
                                      (24 (aload_0))
                                      (25 (getfield (fieldCP "offset" "java.nio.ByteBufferAsShortBufferB" int)))
                                      (28 (invokespecial
					(methodCP "<init>" "java.nio.ByteBufferAsShortBufferRB" ((class "java.nio.ByteBuffer") int int int int int) void)))
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
                                      (4 (getfield (fieldCP "offset" "java.nio.ByteBufferAsShortBufferB" int)))
                                      (7 (iadd))
                                      (8 (ireturn))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "get"
                              (parameters )
                              (returntype . short)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 16)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "bb" "java.nio.ByteBufferAsShortBufferB" (class "java.nio.ByteBuffer"))))
                                      (4 (aload_0))
                                      (5 (aload_0))
                                      (6 (invokevirtual
					(methodCP "nextGetIndex" "java.nio.ByteBufferAsShortBufferB" () int)))
                                      (9 (invokevirtual
					(methodCP "ix" "java.nio.ByteBufferAsShortBufferB" (int) int)))
                                      (12 (invokestatic
					(methodCP "getShortB" "java.nio.Bits" ((class "java.nio.ByteBuffer") int) short)))
                                      (15 (ireturn))
                                      (endofcode 16))
                                   (Exceptions )
                                   (StackMap )))
                        (method "get"
                              (parameters int)
                              (returntype . short)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 17)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "bb" "java.nio.ByteBufferAsShortBufferB" (class "java.nio.ByteBuffer"))))
                                      (4 (aload_0))
                                      (5 (aload_0))
                                      (6 (iload_1))
                                      (7 (invokevirtual
					(methodCP "checkIndex" "java.nio.ByteBufferAsShortBufferB" (int) int)))
                                      (10 (invokevirtual
					(methodCP "ix" "java.nio.ByteBufferAsShortBufferB" (int) int)))
                                      (13 (invokestatic
					(methodCP "getShortB" "java.nio.Bits" ((class "java.nio.ByteBuffer") int) short)))
                                      (16 (ireturn))
                                      (endofcode 17))
                                   (Exceptions )
                                   (StackMap )))
                        (method "put"
                              (parameters short)
                              (returntype . (class "java.nio.ShortBuffer"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 18)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "bb" "java.nio.ByteBufferAsShortBufferB" (class "java.nio.ByteBuffer"))))
                                      (4 (aload_0))
                                      (5 (aload_0))
                                      (6 (invokevirtual
					(methodCP "nextPutIndex" "java.nio.ByteBufferAsShortBufferB" () int)))
                                      (9 (invokevirtual
					(methodCP "ix" "java.nio.ByteBufferAsShortBufferB" (int) int)))
                                      (12 (iload_1))
                                      (13 (invokestatic
					(methodCP "putShortB" "java.nio.Bits" ((class "java.nio.ByteBuffer") int short) void)))
                                      (16 (aload_0))
                                      (17 (areturn))
                                      (endofcode 18))
                                   (Exceptions )
                                   (StackMap )))
                        (method "put"
                              (parameters int short)
                              (returntype . (class "java.nio.ShortBuffer"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 19)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "bb" "java.nio.ByteBufferAsShortBufferB" (class "java.nio.ByteBuffer"))))
                                      (4 (aload_0))
                                      (5 (aload_0))
                                      (6 (iload_1))
                                      (7 (invokevirtual
					(methodCP "checkIndex" "java.nio.ByteBufferAsShortBufferB" (int) int)))
                                      (10 (invokevirtual
					(methodCP "ix" "java.nio.ByteBufferAsShortBufferB" (int) int)))
                                      (13 (iload_2))
                                      (14 (invokestatic
					(methodCP "putShortB" "java.nio.Bits" ((class "java.nio.ByteBuffer") int short) void)))
                                      (17 (aload_0))
                                      (18 (areturn))
                                      (endofcode 19))
                                   (Exceptions )
                                   (StackMap )))
                        (method "compact"
                              (parameters )
                              (returntype . (class "java.nio.ShortBuffer"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 6) (code_length . 116)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "position" "java.nio.ByteBufferAsShortBufferB" () int))) 
                                      (4 (istore_1)) 
                                      (5 (aload_0)) 
                                      (6 (invokevirtual (methodCP "limit" "java.nio.ByteBufferAsShortBufferB" () int))) 
                                      (9 (istore_2)) 
                                      (10 (getstatic (fieldCP "$assertionsDisabled" "java.nio.ByteBufferAsShortBufferB" boolean))) 
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
                                      (43 (getfield (fieldCP "bb" "java.nio.ByteBufferAsShortBufferB" (class "java.nio.ByteBuffer")))) 
                                      (46 (invokevirtual (methodCP "duplicate" "java.nio.ByteBuffer" () (class "java.nio.ByteBuffer")))) 
                                      (49 (astore 4)) 
                                      (51 (aload 4)) 
                                      (53 (aload_0)) 
                                      (54 (iload_2)) 
                                      (55 (invokevirtual (methodCP "ix" "java.nio.ByteBufferAsShortBufferB" (int) int))) 
                                      (58 (invokevirtual (methodCP "limit" "java.nio.ByteBuffer" (int) (class "java.nio.Buffer")))) 
                                      (61 (pop)) 
                                      (62 (aload 4)) 
                                      (64 (aload_0)) 
                                      (65 (iconst_0)) 
                                      (66 (invokevirtual (methodCP "ix" "java.nio.ByteBufferAsShortBufferB" (int) int))) 
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
                                      (97 (invokevirtual (methodCP "position" "java.nio.ByteBufferAsShortBufferB" (int) (class "java.nio.Buffer")))) 
                                      (100 (pop)) 
                                      (101 (aload_0)) 
                                      (102 (aload_0)) 
                                      (103 (invokevirtual (methodCP "capacity" "java.nio.ByteBufferAsShortBufferB" () int))) 
                                      (106 (invokevirtual (methodCP "limit" "java.nio.ByteBufferAsShortBufferB" (int) (class "java.nio.Buffer")))) 
                                      (109 (pop)) 
                                      (110 (aload_0)) 
                                      (111 (invokevirtual (methodCP "discardMark" "java.nio.ByteBufferAsShortBufferB" () void))) 
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
                                      (1 (getfield (fieldCP "bb" "java.nio.ByteBufferAsShortBufferB" (class "java.nio.ByteBuffer"))))
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
                                      (14 (putstatic (fieldCP "$assertionsDisabled" "java.nio.ByteBufferAsShortBufferB" boolean))) ;;at TAG_1
                                      (17 (return)) 
                                      (endofcode 18))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *ByteBufferAsShortBufferB-class-table*
  (make-static-class-decls 
   *java.nio.ByteBufferAsShortBufferB*))

(defconst *package-name-map* 
  ("java.nio.ByteBufferAsShortBufferB" . "java.nio"))

