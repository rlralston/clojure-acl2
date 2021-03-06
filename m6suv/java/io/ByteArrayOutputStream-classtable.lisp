; ByteArrayOutputStream-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:31 CDT 2014.
;

(defconst *java.io.ByteArrayOutputStream*
 (make-class-def
      '(class "java.io.ByteArrayOutputStream"
            "java.io.OutputStream"
            (constant_pool
                        (STRING  "Negative initial size: ")
                        (INT 2147483647))
            (fields
                        (field "buf" (array byte) (accessflags  *class*  *protected* ) -1)
                        (field "count" int (accessflags  *class*  *protected* ) -1))
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 7)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (bipush 32))
                                      (3 (invokespecial
					(methodCP "<init>" "java.io.ByteArrayOutputStream" (int) void)))
                                      (6 (return))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 43)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokespecial (methodCP "<init>" "java.io.OutputStream" () void))) 
                                      (4 (iload_1)) 
                                      (5 (ifge 35))  ;;to TAG_0
                                      (8 (new (class "java.lang.IllegalArgumentException"))) 
                                      (11 (dup)) 
                                      (12 (new (class "java.lang.StringBuilder"))) 
                                      (15 (dup)) 
                                      (16 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (19 (ldc 0)) ;;STRING:: "Negative initial size: "
                                      (21 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (24 (iload_1)) 
                                      (25 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (int) (class "java.lang.StringBuilder")))) 
                                      (28 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (31 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (34 (athrow)) 
                                      (35 (aload_0)) ;;at TAG_0
                                      (36 (iload_1)) 
                                      (37 (newarray BYTE)) 
                                      (39 (putfield (fieldCP "buf" "java.io.ByteArrayOutputStream" (array byte)))) 
                                      (42 (return)) 
                                      (endofcode 43))
                                   (Exceptions )
                                   (StackMap )))
                        (method "ensureCapacity"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 16)
                                   (parsedcode
                                      (0 (iload_1)) 
                                      (1 (aload_0)) 
                                      (2 (getfield (fieldCP "buf" "java.io.ByteArrayOutputStream" (array byte)))) 
                                      (5 (arraylength)) 
                                      (6 (isub)) 
                                      (7 (ifle 15))  ;;to TAG_0
                                      (10 (aload_0)) 
                                      (11 (iload_1)) 
                                      (12 (invokespecial (methodCP "grow" "java.io.ByteArrayOutputStream" (int) void))) 
                                      (15 (return)) ;;at TAG_0
                                      (endofcode 16))
                                   (Exceptions )
                                   (StackMap )))
                        (method "grow"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 3) (max_locals . 4) (code_length . 50)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "buf" "java.io.ByteArrayOutputStream" (array byte)))) 
                                      (4 (arraylength)) 
                                      (5 (istore_2)) 
                                      (6 (iload_2)) 
                                      (7 (iconst_1)) 
                                      (8 (ishl)) 
                                      (9 (istore_3)) 
                                      (10 (iload_3)) 
                                      (11 (iload_1)) 
                                      (12 (isub)) 
                                      (13 (ifge 18)) ;;to TAG_0
                                      (16 (iload_1)) 
                                      (17 (istore_3)) 
                                      (18 (iload_3)) ;;at TAG_0
                                      (19 (ifge 37)) ;;to TAG_1
                                      (22 (iload_1)) 
                                      (23 (ifge 34))  ;;to TAG_2
                                      (26 (new (class "java.lang.OutOfMemoryError"))) 
                                      (29 (dup)) 
                                      (30 (invokespecial (methodCP "<init>" "java.lang.OutOfMemoryError" () void))) 
                                      (33 (athrow)) 
                                      (34 (ldc 1)) ;;at TAG_2;;INT:: "2147483647"
                                      (36 (istore_3)) 
                                      (37 (aload_0)) ;;at TAG_1
                                      (38 (aload_0)) 
                                      (39 (getfield (fieldCP "buf" "java.io.ByteArrayOutputStream" (array byte)))) 
                                      (42 (iload_3)) 
                                      (43 (invokestatic (methodCP "copyOf" "java.util.Arrays" ((array byte) int) (array byte)))) 
                                      (46 (putfield (fieldCP "buf" "java.io.ByteArrayOutputStream" (array byte)))) 
                                      (49 (return)) 
                                      (endofcode 50))
                                   (Exceptions )
                                   (StackMap )))
                        (method "write"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *class*  *public*  *super*  *synchronized* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 32)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_0))
                                      (2 (getfield (fieldCP "count" "java.io.ByteArrayOutputStream" int)))
                                      (5 (iconst_1))
                                      (6 (iadd))
                                      (7 (invokespecial
					(methodCP "ensureCapacity" "java.io.ByteArrayOutputStream" (int) void)))
                                      (10 (aload_0))
                                      (11 (getfield (fieldCP "buf" "java.io.ByteArrayOutputStream" (array byte))))
                                      (14 (aload_0))
                                      (15 (getfield (fieldCP "count" "java.io.ByteArrayOutputStream" int)))
                                      (18 (iload_1))
                                      (19 (i2b))
                                      (20 (bastore))
                                      (21 (aload_0))
                                      (22 (dup))
                                      (23 (getfield (fieldCP "count" "java.io.ByteArrayOutputStream" int)))
                                      (26 (iconst_1))
                                      (27 (iadd))
                                      (28 (putfield (fieldCP "count" "java.io.ByteArrayOutputStream" int)))
                                      (31 (return))
                                      (endofcode 32))
                                   (Exceptions )
                                   (StackMap )))
                        (method "write"
                              (parameters (array byte) int int)
                              (returntype . void)
                              (accessflags  *class*  *public*  *super*  *synchronized* )
                              (code
                                   (max_stack . 5) (max_locals . 4) (code_length . 66)
                                   (parsedcode
                                      (0 (iload_2)) 
                                      (1 (iflt 23))  ;;to TAG_0
                                      (4 (iload_2)) 
                                      (5 (aload_1)) 
                                      (6 (arraylength)) 
                                      (7 (if_icmpgt 23))  ;;to TAG_0
                                      (10 (iload_3)) 
                                      (11 (iflt 23))  ;;to TAG_0
                                      (14 (iload_2)) 
                                      (15 (iload_3)) 
                                      (16 (iadd)) 
                                      (17 (aload_1)) 
                                      (18 (arraylength)) 
                                      (19 (isub)) 
                                      (20 (ifle 31)) ;;to TAG_1
                                      (23 (new (class "java.lang.IndexOutOfBoundsException"))) ;;at TAG_0
                                      (26 (dup)) 
                                      (27 (invokespecial (methodCP "<init>" "java.lang.IndexOutOfBoundsException" () void))) 
                                      (30 (athrow)) 
                                      (31 (aload_0)) ;;at TAG_1
                                      (32 (aload_0)) 
                                      (33 (getfield (fieldCP "count" "java.io.ByteArrayOutputStream" int))) 
                                      (36 (iload_3)) 
                                      (37 (iadd)) 
                                      (38 (invokespecial (methodCP "ensureCapacity" "java.io.ByteArrayOutputStream" (int) void))) 
                                      (41 (aload_1)) 
                                      (42 (iload_2)) 
                                      (43 (aload_0)) 
                                      (44 (getfield (fieldCP "buf" "java.io.ByteArrayOutputStream" (array byte)))) 
                                      (47 (aload_0)) 
                                      (48 (getfield (fieldCP "count" "java.io.ByteArrayOutputStream" int))) 
                                      (51 (iload_3)) 
                                      (52 (invokestatic (methodCP "arraycopy" "java.lang.System" ((class "java.lang.Object") int (class "java.lang.Object") int int) void))) 
                                      (55 (aload_0)) 
                                      (56 (dup)) 
                                      (57 (getfield (fieldCP "count" "java.io.ByteArrayOutputStream" int))) 
                                      (60 (iload_3)) 
                                      (61 (iadd)) 
                                      (62 (putfield (fieldCP "count" "java.io.ByteArrayOutputStream" int))) 
                                      (65 (return)) 
                                      (endofcode 66))
                                   (Exceptions )
                                   (StackMap )))
                        (method "writeTo"
                              (parameters (class "java.io.OutputStream"))
                              (returntype . void)
                              (accessflags  *class*  *public*  *super*  *synchronized* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 14)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (aload_0))
                                      (2 (getfield (fieldCP "buf" "java.io.ByteArrayOutputStream" (array byte))))
                                      (5 (iconst_0))
                                      (6 (aload_0))
                                      (7 (getfield (fieldCP "count" "java.io.ByteArrayOutputStream" int)))
                                      (10 (invokevirtual
					(methodCP "write" "java.io.OutputStream" ((array byte) int int) void)))
                                      (13 (return))
                                      (endofcode 14))
                                   (Exceptions )
                                   (StackMap )))
                        (method "reset"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *super*  *synchronized* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (iconst_0))
                                      (2 (putfield (fieldCP "count" "java.io.ByteArrayOutputStream" int)))
                                      (5 (return))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "toByteArray"
                              (parameters )
                              (returntype . (array byte))
                              (accessflags  *class*  *public*  *super*  *synchronized* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 12)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "buf" "java.io.ByteArrayOutputStream" (array byte))))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "count" "java.io.ByteArrayOutputStream" int)))
                                      (8 (invokestatic
					(methodCP "copyOf" "java.util.Arrays" ((array byte) int) (array byte))))
                                      (11 (areturn))
                                      (endofcode 12))
                                   (Exceptions )
                                   (StackMap )))
                        (method "size"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public*  *super*  *synchronized* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "count" "java.io.ByteArrayOutputStream" int)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "toString"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public*  *super*  *synchronized* )
                              (code
                                   (max_stack . 5) (max_locals . 1) (code_length . 17)
                                   (parsedcode
                                      (0 (new (class "java.lang.String")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "buf" "java.io.ByteArrayOutputStream" (array byte))))
                                      (8 (iconst_0))
                                      (9 (aload_0))
                                      (10 (getfield (fieldCP "count" "java.io.ByteArrayOutputStream" int)))
                                      (13 (invokespecial
					(methodCP "<init>" "java.lang.String" ((array byte) int int) void)))
                                      (16 (areturn))
                                      (endofcode 17))
                                   (Exceptions )
                                   (StackMap )))
                        (method "toString"
                              (parameters (class "java.lang.String"))
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public*  *super*  *synchronized* )
                              (code
                                   (max_stack . 6) (max_locals . 2) (code_length . 18)
                                   (parsedcode
                                      (0 (new (class "java.lang.String")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "buf" "java.io.ByteArrayOutputStream" (array byte))))
                                      (8 (iconst_0))
                                      (9 (aload_0))
                                      (10 (getfield (fieldCP "count" "java.io.ByteArrayOutputStream" int)))
                                      (13 (aload_1))
                                      (14 (invokespecial
					(methodCP "<init>" "java.lang.String" ((array byte) int int (class "java.lang.String")) void)))
                                      (17 (areturn))
                                      (endofcode 18))
                                   (Exceptions )
                                   (StackMap )))
                        (method "toString"
                              (parameters int)
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public*  *super*  *synchronized* )
                              (code
                                   (max_stack . 6) (max_locals . 2) (code_length . 18)
                                   (parsedcode
                                      (0 (new (class "java.lang.String")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "buf" "java.io.ByteArrayOutputStream" (array byte))))
                                      (8 (iload_1))
                                      (9 (iconst_0))
                                      (10 (aload_0))
                                      (11 (getfield (fieldCP "count" "java.io.ByteArrayOutputStream" int)))
                                      (14 (invokespecial
					(methodCP "<init>" "java.lang.String" ((array byte) int int int) void)))
                                      (17 (areturn))
                                      (endofcode 18))
                                   (Exceptions )
                                   (StackMap )))
                        (method "close"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 0) (max_locals . 1) (code_length . 1)
                                   (parsedcode
                                      (0 (return))
                                      (endofcode 1))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *ByteArrayOutputStream-class-table*
  (make-static-class-decls 
   *java.io.ByteArrayOutputStream*))

(defconst *package-name-map* 
  ("java.io.ByteArrayOutputStream" . "java.io"))

