; DirectShortBufferRS-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:39 CDT 2014.
;

(defconst *java.nio.DirectShortBufferRS*
 (make-class-def
      '(class "java.nio.DirectShortBufferRS"
            "java.nio.DirectShortBufferS"
            (constant_pool)
            (fields
                        (field "$assertionsDisabled" boolean (accessflags  *class*  *final*  *static* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "sun.nio.ch.DirectBuffer") int int int int int)
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
					(methodCP "<init>" "java.nio.DirectShortBufferS" ((class "sun.nio.ch.DirectBuffer") int int int int int) void)))
                                      (13 (return))
                                      (endofcode 14))
                                   (Exceptions )
                                   (StackMap )))
                        (method "slice"
                              (parameters )
                              (returntype . (class "java.nio.ShortBuffer"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 8) (max_locals . 5) (code_length . 81)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "position" "java.nio.DirectShortBufferRS" () int))) 
                                      (4 (istore_1)) 
                                      (5 (aload_0)) 
                                      (6 (invokevirtual (methodCP "limit" "java.nio.DirectShortBufferRS" () int))) 
                                      (9 (istore_2)) 
                                      (10 (getstatic (fieldCP "$assertionsDisabled" "java.nio.DirectShortBufferRS" boolean))) 
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
                                      (45 (istore 4)) 
                                      (47 (getstatic (fieldCP "$assertionsDisabled" "java.nio.DirectShortBufferRS" boolean))) 
                                      (50 (ifne 66)) ;;to TAG_3
                                      (53 (iload 4)) 
                                      (55 (ifge 66)) ;;to TAG_3
                                      (58 (new (class "java.lang.AssertionError"))) 
                                      (61 (dup)) 
                                      (62 (invokespecial (methodCP "<init>" "java.lang.AssertionError" () void))) 
                                      (65 (athrow)) 
                                      (66 (new (class "java.nio.DirectShortBufferRS"))) ;;at TAG_3
                                      (69 (dup)) 
                                      (70 (aload_0)) 
                                      (71 (iconst_m1)) 
                                      (72 (iconst_0)) 
                                      (73 (iload_3)) 
                                      (74 (iload_3)) 
                                      (75 (iload 4)) 
                                      (77 (invokespecial (methodCP "<init>" "java.nio.DirectShortBufferRS" ((class "sun.nio.ch.DirectBuffer") int int int int int) void))) 
                                      (80 (areturn)) 
                                      (endofcode 81))
                                   (Exceptions )
                                   (StackMap )))
                        (method "duplicate"
                              (parameters )
                              (returntype . (class "java.nio.ShortBuffer"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 8) (max_locals . 1) (code_length . 26)
                                   (parsedcode
                                      (0 (new (class "java.nio.DirectShortBufferRS")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (aload_0))
                                      (6 (invokevirtual
					(methodCP "markValue" "java.nio.DirectShortBufferRS" () int)))
                                      (9 (aload_0))
                                      (10 (invokevirtual
					(methodCP "position" "java.nio.DirectShortBufferRS" () int)))
                                      (13 (aload_0))
                                      (14 (invokevirtual
					(methodCP "limit" "java.nio.DirectShortBufferRS" () int)))
                                      (17 (aload_0))
                                      (18 (invokevirtual
					(methodCP "capacity" "java.nio.DirectShortBufferRS" () int)))
                                      (21 (iconst_0))
                                      (22 (invokespecial
					(methodCP "<init>" "java.nio.DirectShortBufferRS" ((class "sun.nio.ch.DirectBuffer") int int int int int) void)))
                                      (25 (areturn))
                                      (endofcode 26))
                                   (Exceptions )
                                   (StackMap )))
                        (method "asReadOnlyBuffer"
                              (parameters )
                              (returntype . (class "java.nio.ShortBuffer"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokevirtual
					(methodCP "duplicate" "java.nio.DirectShortBufferRS" () (class "java.nio.ShortBuffer"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "put"
                              (parameters short)
                              (returntype . (class "java.nio.ShortBuffer"))
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
                              (parameters int short)
                              (returntype . (class "java.nio.ShortBuffer"))
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
                        (method "put"
                              (parameters (class "java.nio.ShortBuffer"))
                              (returntype . (class "java.nio.ShortBuffer"))
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
                              (parameters (array short) int int)
                              (returntype . (class "java.nio.ShortBuffer"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 8)
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
                              (returntype . (class "java.nio.ShortBuffer"))
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
                                   (max_stack . 1) (max_locals . 1) (code_length . 2)
                                   (parsedcode
                                      (0 (iconst_1))
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
                                      (0 (iconst_1))
                                      (1 (ireturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap )))
                        (method "order"
                              (parameters )
                              (returntype . (class "java.nio.ByteOrder"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 19)
                                   (parsedcode
                                      (0 (invokestatic (methodCP "nativeOrder" "java.nio.ByteOrder" () (class "java.nio.ByteOrder")))) 
                                      (3 (getstatic (fieldCP "BIG_ENDIAN" "java.nio.ByteOrder" (class "java.nio.ByteOrder")))) 
                                      (6 (if_acmpne 15))  ;;to TAG_0
                                      (9 (getstatic (fieldCP "LITTLE_ENDIAN" "java.nio.ByteOrder" (class "java.nio.ByteOrder")))) 
                                      (12 (goto 18)) ;;to TAG_1
                                      (15 (getstatic (fieldCP "BIG_ENDIAN" "java.nio.ByteOrder" (class "java.nio.ByteOrder")))) ;;at TAG_0
                                      (18 (areturn)) ;;at TAG_1
                                      (endofcode 19))
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
                                      (14 (putstatic (fieldCP "$assertionsDisabled" "java.nio.DirectShortBufferRS" boolean))) ;;at TAG_1
                                      (17 (return)) 
                                      (endofcode 18))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "sun.nio.ch.DirectBuffer")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *DirectShortBufferRS-class-table*
  (make-static-class-decls 
   *java.nio.DirectShortBufferRS*))

(defconst *package-name-map* 
  ("java.nio.DirectShortBufferRS" . "java.nio"))
