; ByteBufferAsCharBufferRB-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:38 CDT 2014.
;

(defconst *java.nio.ByteBufferAsCharBufferRB*
 (make-class-def
      '(class "java.nio.ByteBufferAsCharBufferRB"
            "java.nio.ByteBufferAsCharBufferB"
            (constant_pool)
            (fields
                        (field "$assertionsDisabled" boolean (accessflags  *class*  *final*  *static* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.nio.ByteBuffer"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokespecial
					(methodCP "<init>" "java.nio.ByteBufferAsCharBufferB" ((class "java.nio.ByteBuffer")) void)))
                                      (5 (return))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.nio.ByteBuffer") int int int int int)
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 7) (max_locals . 7) (code_length . 14)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (iload_2))
                                      (3 (iload_3))
                                      (4 (iload 4))
                                      (6 (iload 5))
                                      (8 (iload 6))
                                      (10 (invokespecial
					(methodCP "<init>" "java.nio.ByteBufferAsCharBufferB" ((class "java.nio.ByteBuffer") int int int int int) void)))
                                      (13 (return))
                                      (endofcode 14))
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
                                      (1 (invokevirtual (methodCP "position" "java.nio.ByteBufferAsCharBufferRB" () int))) 
                                      (4 (istore_1)) 
                                      (5 (aload_0)) 
                                      (6 (invokevirtual (methodCP "limit" "java.nio.ByteBufferAsCharBufferRB" () int))) 
                                      (9 (istore_2)) 
                                      (10 (getstatic (fieldCP "$assertionsDisabled" "java.nio.ByteBufferAsCharBufferRB" boolean))) 
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
                                      (46 (getfield (fieldCP "offset" "java.nio.ByteBufferAsCharBufferRB" int))) 
                                      (49 (iadd)) 
                                      (50 (istore 4)) 
                                      (52 (getstatic (fieldCP "$assertionsDisabled" "java.nio.ByteBufferAsCharBufferRB" boolean))) 
                                      (55 (ifne 71)) ;;to TAG_3
                                      (58 (iload 4)) 
                                      (60 (ifge 71)) ;;to TAG_3
                                      (63 (new (class "java.lang.AssertionError"))) 
                                      (66 (dup)) 
                                      (67 (invokespecial (methodCP "<init>" "java.lang.AssertionError" () void))) 
                                      (70 (athrow)) 
                                      (71 (new (class "java.nio.ByteBufferAsCharBufferRB"))) ;;at TAG_3
                                      (74 (dup)) 
                                      (75 (aload_0)) 
                                      (76 (getfield (fieldCP "bb" "java.nio.ByteBufferAsCharBufferRB" (class "java.nio.ByteBuffer")))) 
                                      (79 (iconst_m1)) 
                                      (80 (iconst_0)) 
                                      (81 (iload_3)) 
                                      (82 (iload_3)) 
                                      (83 (iload 4)) 
                                      (85 (invokespecial (methodCP "<init>" "java.nio.ByteBufferAsCharBufferRB" ((class "java.nio.ByteBuffer") int int int int int) void))) 
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
                                      (0 (new (class "java.nio.ByteBufferAsCharBufferRB")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "bb" "java.nio.ByteBufferAsCharBufferRB" (class "java.nio.ByteBuffer"))))
                                      (8 (aload_0))
                                      (9 (invokevirtual
					(methodCP "markValue" "java.nio.ByteBufferAsCharBufferRB" () int)))
                                      (12 (aload_0))
                                      (13 (invokevirtual
					(methodCP "position" "java.nio.ByteBufferAsCharBufferRB" () int)))
                                      (16 (aload_0))
                                      (17 (invokevirtual
					(methodCP "limit" "java.nio.ByteBufferAsCharBufferRB" () int)))
                                      (20 (aload_0))
                                      (21 (invokevirtual
					(methodCP "capacity" "java.nio.ByteBufferAsCharBufferRB" () int)))
                                      (24 (aload_0))
                                      (25 (getfield (fieldCP "offset" "java.nio.ByteBufferAsCharBufferRB" int)))
                                      (28 (invokespecial
					(methodCP "<init>" "java.nio.ByteBufferAsCharBufferRB" ((class "java.nio.ByteBuffer") int int int int int) void)))
                                      (31 (areturn))
                                      (endofcode 32))
                                   (Exceptions )
                                   (StackMap )))
                        (method "asReadOnlyBuffer"
                              (parameters )
                              (returntype . (class "java.nio.CharBuffer"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokevirtual
					(methodCP "duplicate" "java.nio.ByteBufferAsCharBufferRB" () (class "java.nio.CharBuffer"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "put"
                              (parameters char)
                              (returntype . (class "java.nio.CharBuffer"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 8)
                                   (parsedcode
                                      (0 (new (class "java.nio.ReadOnlyBufferException")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.nio.ReadOnlyBufferException" () void)))
                                      (7 (athrow))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "put"
                              (parameters int char)
                              (returntype . (class "java.nio.CharBuffer"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 8)
                                   (parsedcode
                                      (0 (new (class "java.nio.ReadOnlyBufferException")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.nio.ReadOnlyBufferException" () void)))
                                      (7 (athrow))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "compact"
                              (parameters )
                              (returntype . (class "java.nio.CharBuffer"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (new (class "java.nio.ReadOnlyBufferException")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.nio.ReadOnlyBufferException" () void)))
                                      (7 (athrow))
                                      (endofcode 8))
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
                                      (1 (getfield (fieldCP "bb" "java.nio.ByteBufferAsCharBufferRB" (class "java.nio.ByteBuffer"))))
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
                                      (0 (iconst_1))
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
                                      (2 (invokevirtual (methodCP "limit" "java.nio.ByteBufferAsCharBufferRB" () int))) 
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
                                      (38 (invokevirtual (methodCP "duplicate" "java.nio.ByteBufferAsCharBufferRB" () (class "java.nio.CharBuffer")))) 
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
                                      (1 (invokevirtual (methodCP "position" "java.nio.ByteBufferAsCharBufferRB" () int))) 
                                      (4 (istore_3)) 
                                      (5 (aload_0)) 
                                      (6 (invokevirtual (methodCP "limit" "java.nio.ByteBufferAsCharBufferRB" () int))) 
                                      (9 (istore 4)) 
                                      (11 (getstatic (fieldCP "$assertionsDisabled" "java.nio.ByteBufferAsCharBufferRB" boolean))) 
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
                                      (73 (new (class "java.nio.ByteBufferAsCharBufferRB"))) ;;at TAG_4
                                      (76 (dup)) 
                                      (77 (aload_0)) 
                                      (78 (getfield (fieldCP "bb" "java.nio.ByteBufferAsCharBufferRB" (class "java.nio.ByteBuffer")))) 
                                      (81 (iconst_m1)) 
                                      (82 (iload_3)) 
                                      (83 (iload_1)) 
                                      (84 (iadd)) 
                                      (85 (iload_3)) 
                                      (86 (iload_2)) 
                                      (87 (iadd)) 
                                      (88 (aload_0)) 
                                      (89 (invokevirtual (methodCP "capacity" "java.nio.ByteBufferAsCharBufferRB" () int))) 
                                      (92 (aload_0)) 
                                      (93 (getfield (fieldCP "offset" "java.nio.ByteBufferAsCharBufferRB" int))) 
                                      (96 (invokespecial (methodCP "<init>" "java.nio.ByteBufferAsCharBufferRB" ((class "java.nio.ByteBuffer") int int int int int) void))) 
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
					(methodCP "subSequence" "java.nio.ByteBufferAsCharBufferRB" (int int) (class "java.nio.CharBuffer"))))
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
                                      (14 (putstatic (fieldCP "$assertionsDisabled" "java.nio.ByteBufferAsCharBufferRB" boolean))) ;;at TAG_1
                                      (17 (return)) 
                                      (endofcode 18))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *ByteBufferAsCharBufferRB-class-table*
  (make-static-class-decls 
   *java.nio.ByteBufferAsCharBufferRB*))

(defconst *package-name-map* 
  ("java.nio.ByteBufferAsCharBufferRB" . "java.nio"))

