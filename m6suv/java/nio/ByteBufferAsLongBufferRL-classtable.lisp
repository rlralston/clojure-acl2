; ByteBufferAsLongBufferRL-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:38 CDT 2014.
;

(defconst *java.nio.ByteBufferAsLongBufferRL*
 (make-class-def
      '(class "java.nio.ByteBufferAsLongBufferRL"
            "java.nio.ByteBufferAsLongBufferL"
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
					(methodCP "<init>" "java.nio.ByteBufferAsLongBufferL" ((class "java.nio.ByteBuffer")) void)))
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
					(methodCP "<init>" "java.nio.ByteBufferAsLongBufferL" ((class "java.nio.ByteBuffer") int int int int int) void)))
                                      (13 (return))
                                      (endofcode 14))
                                   (Exceptions )
                                   (StackMap )))
                        (method "slice"
                              (parameters )
                              (returntype . (class "java.nio.LongBuffer"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 8) (max_locals . 5) (code_length . 89)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "position" "java.nio.ByteBufferAsLongBufferRL" () int))) 
                                      (4 (istore_1)) 
                                      (5 (aload_0)) 
                                      (6 (invokevirtual (methodCP "limit" "java.nio.ByteBufferAsLongBufferRL" () int))) 
                                      (9 (istore_2)) 
                                      (10 (getstatic (fieldCP "$assertionsDisabled" "java.nio.ByteBufferAsLongBufferRL" boolean))) 
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
                                      (43 (iconst_3)) 
                                      (44 (ishl)) 
                                      (45 (aload_0)) 
                                      (46 (getfield (fieldCP "offset" "java.nio.ByteBufferAsLongBufferRL" int))) 
                                      (49 (iadd)) 
                                      (50 (istore 4)) 
                                      (52 (getstatic (fieldCP "$assertionsDisabled" "java.nio.ByteBufferAsLongBufferRL" boolean))) 
                                      (55 (ifne 71)) ;;to TAG_3
                                      (58 (iload 4)) 
                                      (60 (ifge 71)) ;;to TAG_3
                                      (63 (new (class "java.lang.AssertionError"))) 
                                      (66 (dup)) 
                                      (67 (invokespecial (methodCP "<init>" "java.lang.AssertionError" () void))) 
                                      (70 (athrow)) 
                                      (71 (new (class "java.nio.ByteBufferAsLongBufferRL"))) ;;at TAG_3
                                      (74 (dup)) 
                                      (75 (aload_0)) 
                                      (76 (getfield (fieldCP "bb" "java.nio.ByteBufferAsLongBufferRL" (class "java.nio.ByteBuffer")))) 
                                      (79 (iconst_m1)) 
                                      (80 (iconst_0)) 
                                      (81 (iload_3)) 
                                      (82 (iload_3)) 
                                      (83 (iload 4)) 
                                      (85 (invokespecial (methodCP "<init>" "java.nio.ByteBufferAsLongBufferRL" ((class "java.nio.ByteBuffer") int int int int int) void))) 
                                      (88 (areturn)) 
                                      (endofcode 89))
                                   (Exceptions )
                                   (StackMap )))
                        (method "duplicate"
                              (parameters )
                              (returntype . (class "java.nio.LongBuffer"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 8) (max_locals . 1) (code_length . 32)
                                   (parsedcode
                                      (0 (new (class "java.nio.ByteBufferAsLongBufferRL")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "bb" "java.nio.ByteBufferAsLongBufferRL" (class "java.nio.ByteBuffer"))))
                                      (8 (aload_0))
                                      (9 (invokevirtual
					(methodCP "markValue" "java.nio.ByteBufferAsLongBufferRL" () int)))
                                      (12 (aload_0))
                                      (13 (invokevirtual
					(methodCP "position" "java.nio.ByteBufferAsLongBufferRL" () int)))
                                      (16 (aload_0))
                                      (17 (invokevirtual
					(methodCP "limit" "java.nio.ByteBufferAsLongBufferRL" () int)))
                                      (20 (aload_0))
                                      (21 (invokevirtual
					(methodCP "capacity" "java.nio.ByteBufferAsLongBufferRL" () int)))
                                      (24 (aload_0))
                                      (25 (getfield (fieldCP "offset" "java.nio.ByteBufferAsLongBufferRL" int)))
                                      (28 (invokespecial
					(methodCP "<init>" "java.nio.ByteBufferAsLongBufferRL" ((class "java.nio.ByteBuffer") int int int int int) void)))
                                      (31 (areturn))
                                      (endofcode 32))
                                   (Exceptions )
                                   (StackMap )))
                        (method "asReadOnlyBuffer"
                              (parameters )
                              (returntype . (class "java.nio.LongBuffer"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokevirtual
					(methodCP "duplicate" "java.nio.ByteBufferAsLongBufferRL" () (class "java.nio.LongBuffer"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "put"
                              (parameters long)
                              (returntype . (class "java.nio.LongBuffer"))
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
                              (parameters int long)
                              (returntype . (class "java.nio.LongBuffer"))
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
                              (returntype . (class "java.nio.LongBuffer"))
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
                                      (1 (getfield (fieldCP "bb" "java.nio.ByteBufferAsLongBufferRL" (class "java.nio.ByteBuffer"))))
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
                        (method "order"
                              (parameters )
                              (returntype . (class "java.nio.ByteOrder"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 4)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "LITTLE_ENDIAN" "java.nio.ByteOrder" (class "java.nio.ByteOrder"))))
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
                                      (14 (putstatic (fieldCP "$assertionsDisabled" "java.nio.ByteBufferAsLongBufferRL" boolean))) ;;at TAG_1
                                      (17 (return)) 
                                      (endofcode 18))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *ByteBufferAsLongBufferRL-class-table*
  (make-static-class-decls 
   *java.nio.ByteBufferAsLongBufferRL*))

(defconst *package-name-map* 
  ("java.nio.ByteBufferAsLongBufferRL" . "java.nio"))

