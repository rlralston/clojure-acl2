; HeapShortBufferR-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:39 CDT 2014.
;

(defconst *java.nio.HeapShortBufferR*
 (make-class-def
      '(class "java.nio.HeapShortBufferR"
            "java.nio.HeapShortBuffer"
            (constant_pool)
            (fields)
            (methods
                        (method "<init>"
                              (parameters int int)
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 12)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (iload_1))
                                      (2 (iload_2))
                                      (3 (invokespecial
					(methodCP "<init>" "java.nio.HeapShortBuffer" (int int) void)))
                                      (6 (aload_0))
                                      (7 (iconst_1))
                                      (8 (putfield (fieldCP "isReadOnly" "java.nio.HeapShortBufferR" boolean)))
                                      (11 (return))
                                      (endofcode 12))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (array short) int int)
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 4) (max_locals . 4) (code_length . 13)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (iload_2))
                                      (3 (iload_3))
                                      (4 (invokespecial
					(methodCP "<init>" "java.nio.HeapShortBuffer" ((array short) int int) void)))
                                      (7 (aload_0))
                                      (8 (iconst_1))
                                      (9 (putfield (fieldCP "isReadOnly" "java.nio.HeapShortBufferR" boolean)))
                                      (12 (return))
                                      (endofcode 13))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (array short) int int int int int)
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 7) (max_locals . 7) (code_length . 19)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (iload_2))
                                      (3 (iload_3))
                                      (4 (iload 4))
                                      (6 (iload 5))
                                      (8 (iload 6))
                                      (10 (invokespecial
					(methodCP "<init>" "java.nio.HeapShortBuffer" ((array short) int int int int int) void)))
                                      (13 (aload_0))
                                      (14 (iconst_1))
                                      (15 (putfield (fieldCP "isReadOnly" "java.nio.HeapShortBufferR" boolean)))
                                      (18 (return))
                                      (endofcode 19))
                                   (Exceptions )
                                   (StackMap )))
                        (method "slice"
                              (parameters )
                              (returntype . (class "java.nio.ShortBuffer"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 9) (max_locals . 1) (code_length . 31)
                                   (parsedcode
                                      (0 (new (class "java.nio.HeapShortBufferR")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "hb" "java.nio.HeapShortBufferR" (array short))))
                                      (8 (iconst_m1))
                                      (9 (iconst_0))
                                      (10 (aload_0))
                                      (11 (invokevirtual
					(methodCP "remaining" "java.nio.HeapShortBufferR" () int)))
                                      (14 (aload_0))
                                      (15 (invokevirtual
					(methodCP "remaining" "java.nio.HeapShortBufferR" () int)))
                                      (18 (aload_0))
                                      (19 (invokevirtual
					(methodCP "position" "java.nio.HeapShortBufferR" () int)))
                                      (22 (aload_0))
                                      (23 (getfield (fieldCP "offset" "java.nio.HeapShortBufferR" int)))
                                      (26 (iadd))
                                      (27 (invokespecial
					(methodCP "<init>" "java.nio.HeapShortBufferR" ((array short) int int int int int) void)))
                                      (30 (areturn))
                                      (endofcode 31))
                                   (Exceptions )
                                   (StackMap )))
                        (method "duplicate"
                              (parameters )
                              (returntype . (class "java.nio.ShortBuffer"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 8) (max_locals . 1) (code_length . 32)
                                   (parsedcode
                                      (0 (new (class "java.nio.HeapShortBufferR")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "hb" "java.nio.HeapShortBufferR" (array short))))
                                      (8 (aload_0))
                                      (9 (invokevirtual
					(methodCP "markValue" "java.nio.HeapShortBufferR" () int)))
                                      (12 (aload_0))
                                      (13 (invokevirtual
					(methodCP "position" "java.nio.HeapShortBufferR" () int)))
                                      (16 (aload_0))
                                      (17 (invokevirtual
					(methodCP "limit" "java.nio.HeapShortBufferR" () int)))
                                      (20 (aload_0))
                                      (21 (invokevirtual
					(methodCP "capacity" "java.nio.HeapShortBufferR" () int)))
                                      (24 (aload_0))
                                      (25 (getfield (fieldCP "offset" "java.nio.HeapShortBufferR" int)))
                                      (28 (invokespecial
					(methodCP "<init>" "java.nio.HeapShortBufferR" ((array short) int int int int int) void)))
                                      (31 (areturn))
                                      (endofcode 32))
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
					(methodCP "duplicate" "java.nio.HeapShortBufferR" () (class "java.nio.ShortBuffer"))))
                                      (4 (areturn))
                                      (endofcode 5))
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
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *HeapShortBufferR-class-table*
  (make-static-class-decls 
   *java.nio.HeapShortBufferR*))

(defconst *package-name-map* 
  ("java.nio.HeapShortBufferR" . "java.nio"))
