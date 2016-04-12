; StringCharBuffer-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:39 CDT 2014.
;

(defconst *java.nio.StringCharBuffer*
 (make-class-def
      '(class "java.nio.StringCharBuffer"
            "java.nio.CharBuffer"
            (constant_pool)
            (fields
                        (field "str" (class "java.lang.CharSequence") (accessflags  *class* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.lang.CharSequence") int int)
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 5) (max_locals . 5) (code_length . 56)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (iconst_m1)) 
                                      (2 (iload_2)) 
                                      (3 (iload_3)) 
                                      (4 (aload_1)) 
                                      (5 (invokeinterface (methodCP "length" "java.lang.CharSequence" () int) 1)) 
                                      (10 (invokespecial (methodCP "<init>" "java.nio.CharBuffer" (int int int int) void))) 
                                      (13 (aload_1)) 
                                      (14 (invokeinterface (methodCP "length" "java.lang.CharSequence" () int) 1)) 
                                      (19 (istore 4)) 
                                      (21 (iload_2)) 
                                      (22 (iflt 42))  ;;to TAG_0
                                      (25 (iload_2)) 
                                      (26 (iload 4)) 
                                      (28 (if_icmpgt 42))  ;;to TAG_0
                                      (31 (iload_3)) 
                                      (32 (iload_2)) 
                                      (33 (if_icmplt 42))  ;;to TAG_0
                                      (36 (iload_3)) 
                                      (37 (iload 4)) 
                                      (39 (if_icmple 50)) ;;to TAG_1
                                      (42 (new (class "java.lang.IndexOutOfBoundsException"))) ;;at TAG_0
                                      (45 (dup)) 
                                      (46 (invokespecial (methodCP "<init>" "java.lang.IndexOutOfBoundsException" () void))) 
                                      (49 (athrow)) 
                                      (50 (aload_0)) ;;at TAG_1
                                      (51 (aload_1)) 
                                      (52 (putfield (fieldCP "str" "java.nio.StringCharBuffer" (class "java.lang.CharSequence")))) 
                                      (55 (return)) 
                                      (endofcode 56))
                                   (Exceptions )
                                   (StackMap )))
                        (method "slice"
                              (parameters )
                              (returntype . (class "java.nio.CharBuffer"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 9) (max_locals . 1) (code_length . 31)
                                   (parsedcode
                                      (0 (new (class "java.nio.StringCharBuffer")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "str" "java.nio.StringCharBuffer" (class "java.lang.CharSequence"))))
                                      (8 (iconst_m1))
                                      (9 (iconst_0))
                                      (10 (aload_0))
                                      (11 (invokevirtual
					(methodCP "remaining" "java.nio.StringCharBuffer" () int)))
                                      (14 (aload_0))
                                      (15 (invokevirtual
					(methodCP "remaining" "java.nio.StringCharBuffer" () int)))
                                      (18 (aload_0))
                                      (19 (getfield (fieldCP "offset" "java.nio.StringCharBuffer" int)))
                                      (22 (aload_0))
                                      (23 (invokevirtual
					(methodCP "position" "java.nio.StringCharBuffer" () int)))
                                      (26 (iadd))
                                      (27 (invokespecial
					(methodCP "<init>" "java.nio.StringCharBuffer" ((class "java.lang.CharSequence") int int int int int) void)))
                                      (30 (areturn))
                                      (endofcode 31))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.CharSequence") int int int int int)
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 7) (max_locals . 7) (code_length . 19)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (iload_2))
                                      (2 (iload_3))
                                      (3 (iload 4))
                                      (5 (iload 5))
                                      (7 (aconst_null))
                                      (8 (iload 6))
                                      (10 (invokespecial
					(methodCP "<init>" "java.nio.CharBuffer" (int int int int (array char) int) void)))
                                      (13 (aload_0))
                                      (14 (aload_1))
                                      (15 (putfield (fieldCP "str" "java.nio.StringCharBuffer" (class "java.lang.CharSequence"))))
                                      (18 (return))
                                      (endofcode 19))
                                   (Exceptions )
                                   (StackMap )))
                        (method "duplicate"
                              (parameters )
                              (returntype . (class "java.nio.CharBuffer"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 8) (max_locals . 1) (code_length . 32)
                                   (parsedcode
                                      (0 (new (class "java.nio.StringCharBuffer")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "str" "java.nio.StringCharBuffer" (class "java.lang.CharSequence"))))
                                      (8 (aload_0))
                                      (9 (invokevirtual
					(methodCP "markValue" "java.nio.StringCharBuffer" () int)))
                                      (12 (aload_0))
                                      (13 (invokevirtual
					(methodCP "position" "java.nio.StringCharBuffer" () int)))
                                      (16 (aload_0))
                                      (17 (invokevirtual
					(methodCP "limit" "java.nio.StringCharBuffer" () int)))
                                      (20 (aload_0))
                                      (21 (invokevirtual
					(methodCP "capacity" "java.nio.StringCharBuffer" () int)))
                                      (24 (aload_0))
                                      (25 (getfield (fieldCP "offset" "java.nio.StringCharBuffer" int)))
                                      (28 (invokespecial
					(methodCP "<init>" "java.nio.StringCharBuffer" ((class "java.lang.CharSequence") int int int int int) void)))
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
					(methodCP "duplicate" "java.nio.StringCharBuffer" () (class "java.nio.CharBuffer"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "get"
                              (parameters )
                              (returntype . char)
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 19)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "str" "java.nio.StringCharBuffer" (class "java.lang.CharSequence"))))
                                      (4 (aload_0))
                                      (5 (invokevirtual
					(methodCP "nextGetIndex" "java.nio.StringCharBuffer" () int)))
                                      (8 (aload_0))
                                      (9 (getfield (fieldCP "offset" "java.nio.StringCharBuffer" int)))
                                      (12 (iadd))
                                      (13 (invokeinterface
					(methodCP "charAt" "java.lang.CharSequence" (int) char) 2))
                                      (18 (ireturn))
                                      (endofcode 19))
                                   (Exceptions )
                                   (StackMap )))
                        (method "get"
                              (parameters int)
                              (returntype . char)
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 20)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "str" "java.nio.StringCharBuffer" (class "java.lang.CharSequence"))))
                                      (4 (aload_0))
                                      (5 (iload_1))
                                      (6 (invokevirtual
					(methodCP "checkIndex" "java.nio.StringCharBuffer" (int) int)))
                                      (9 (aload_0))
                                      (10 (getfield (fieldCP "offset" "java.nio.StringCharBuffer" int)))
                                      (13 (iadd))
                                      (14 (invokeinterface
					(methodCP "charAt" "java.lang.CharSequence" (int) char) 2))
                                      (19 (ireturn))
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap )))
                        (method "put"
                              (parameters char)
                              (returntype . (class "java.nio.CharBuffer"))
                              (accessflags  *class*  *final*  *public* )
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
                              (accessflags  *class*  *final*  *public* )
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
                              (accessflags  *class*  *final*  *public* )
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
                        (method "isReadOnly"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *final*  *public* )
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
                              (accessflags  *class*  *final* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 25)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "str" "java.nio.StringCharBuffer" (class "java.lang.CharSequence"))))
                                      (4 (invokeinterface
					(methodCP "toString" "java.lang.CharSequence" () (class "java.lang.String")) 1))
                                      (9 (iload_1))
                                      (10 (aload_0))
                                      (11 (getfield (fieldCP "offset" "java.nio.StringCharBuffer" int)))
                                      (14 (iadd))
                                      (15 (iload_2))
                                      (16 (aload_0))
                                      (17 (getfield (fieldCP "offset" "java.nio.StringCharBuffer" int)))
                                      (20 (iadd))
                                      (21 (invokevirtual
					(methodCP "substring" "java.lang.String" (int int) (class "java.lang.String"))))
                                      (24 (areturn))
                                      (endofcode 25))
                                   (Exceptions )
                                   (StackMap )))
                        (method "subSequence"
                              (parameters int int)
                              (returntype . (class "java.nio.CharBuffer"))
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 9) (max_locals . 4) (code_length . 51)
                                   (parsedcode
                                      (0 (aload_0)) ;;at TAG_0
                                      (1 (invokevirtual (methodCP "position" "java.nio.StringCharBuffer" () int))) 
                                      (4 (istore_3)) 
                                      (5 (new (class "java.nio.StringCharBuffer"))) 
                                      (8 (dup)) 
                                      (9 (aload_0)) 
                                      (10 (getfield (fieldCP "str" "java.nio.StringCharBuffer" (class "java.lang.CharSequence")))) 
                                      (13 (iconst_m1)) 
                                      (14 (iload_3)) 
                                      (15 (aload_0)) 
                                      (16 (iload_1)) 
                                      (17 (iload_3)) 
                                      (18 (invokevirtual (methodCP "checkIndex" "java.nio.StringCharBuffer" (int int) int))) 
                                      (21 (iadd)) 
                                      (22 (iload_3)) 
                                      (23 (aload_0)) 
                                      (24 (iload_2)) 
                                      (25 (iload_3)) 
                                      (26 (invokevirtual (methodCP "checkIndex" "java.nio.StringCharBuffer" (int int) int))) 
                                      (29 (iadd)) 
                                      (30 (aload_0)) 
                                      (31 (invokevirtual (methodCP "capacity" "java.nio.StringCharBuffer" () int))) 
                                      (34 (aload_0)) 
                                      (35 (getfield (fieldCP "offset" "java.nio.StringCharBuffer" int))) 
                                      (38 (invokespecial (methodCP "<init>" "java.nio.StringCharBuffer" ((class "java.lang.CharSequence") int int int int int) void))) 
                                      (41 (areturn)) ;;at TAG_1
                                      (42 (astore_3)) ;;at TAG_2
                                      (43 (new (class "java.lang.IndexOutOfBoundsException"))) 
                                      (46 (dup)) 
                                      (47 (invokespecial (methodCP "<init>" "java.lang.IndexOutOfBoundsException" () void))) 
                                      (50 (athrow)) 
                                      (endofcode 51))
                                   (Exceptions 
                                     (handler 0 41  42 (class "java.lang.IllegalArgumentException")))
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
					(methodCP "subSequence" "java.nio.StringCharBuffer" (int int) (class "java.nio.CharBuffer"))))
                                      (6 (areturn))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *StringCharBuffer-class-table*
  (make-static-class-decls 
   *java.nio.StringCharBuffer*))

(defconst *package-name-map* 
  ("java.nio.StringCharBuffer" . "java.nio"))

