; ByteArrayInputStream-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:31 CDT 2014.
;

(defconst *java.io.ByteArrayInputStream*
 (make-class-def
      '(class "java.io.ByteArrayInputStream"
            "java.io.InputStream"
            (constant_pool)
            (fields
                        (field "buf" (array byte) (accessflags  *class*  *protected* ) -1)
                        (field "pos" int (accessflags  *class*  *protected* ) -1)
                        (field "mark" int (accessflags  *class*  *protected* ) -1)
                        (field "count" int (accessflags  *class*  *protected* ) -1))
            (methods
                        (method "<init>"
                              (parameters (array byte))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 26)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.io.InputStream" () void)))
                                      (4 (aload_0))
                                      (5 (iconst_0))
                                      (6 (putfield (fieldCP "mark" "java.io.ByteArrayInputStream" int)))
                                      (9 (aload_0))
                                      (10 (aload_1))
                                      (11 (putfield (fieldCP "buf" "java.io.ByteArrayInputStream" (array byte))))
                                      (14 (aload_0))
                                      (15 (iconst_0))
                                      (16 (putfield (fieldCP "pos" "java.io.ByteArrayInputStream" int)))
                                      (19 (aload_0))
                                      (20 (aload_1))
                                      (21 (arraylength))
                                      (22 (putfield (fieldCP "count" "java.io.ByteArrayInputStream" int)))
                                      (25 (return))
                                      (endofcode 26))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (array byte) int int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 4) (code_length . 37)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.io.InputStream" () void)))
                                      (4 (aload_0))
                                      (5 (iconst_0))
                                      (6 (putfield (fieldCP "mark" "java.io.ByteArrayInputStream" int)))
                                      (9 (aload_0))
                                      (10 (aload_1))
                                      (11 (putfield (fieldCP "buf" "java.io.ByteArrayInputStream" (array byte))))
                                      (14 (aload_0))
                                      (15 (iload_2))
                                      (16 (putfield (fieldCP "pos" "java.io.ByteArrayInputStream" int)))
                                      (19 (aload_0))
                                      (20 (iload_2))
                                      (21 (iload_3))
                                      (22 (iadd))
                                      (23 (aload_1))
                                      (24 (arraylength))
                                      (25 (invokestatic
					(methodCP "min" "java.lang.Math" (int int) int)))
                                      (28 (putfield (fieldCP "count" "java.io.ByteArrayInputStream" int)))
                                      (31 (aload_0))
                                      (32 (iload_2))
                                      (33 (putfield (fieldCP "mark" "java.io.ByteArrayInputStream" int)))
                                      (36 (return))
                                      (endofcode 37))
                                   (Exceptions )
                                   (StackMap )))
                        (method "read"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public*  *super*  *synchronized* )
                              (code
                                   (max_stack . 5) (max_locals . 1) (code_length . 36)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "pos" "java.io.ByteArrayInputStream" int))) 
                                      (4 (aload_0)) 
                                      (5 (getfield (fieldCP "count" "java.io.ByteArrayInputStream" int))) 
                                      (8 (if_icmpge 34))  ;;to TAG_0
                                      (11 (aload_0)) 
                                      (12 (getfield (fieldCP "buf" "java.io.ByteArrayInputStream" (array byte)))) 
                                      (15 (aload_0)) 
                                      (16 (dup)) 
                                      (17 (getfield (fieldCP "pos" "java.io.ByteArrayInputStream" int))) 
                                      (20 (dup_x1)) 
                                      (21 (iconst_1)) 
                                      (22 (iadd)) 
                                      (23 (putfield (fieldCP "pos" "java.io.ByteArrayInputStream" int))) 
                                      (26 (baload)) 
                                      (27 (sipush 255)) 
                                      (30 (iand)) 
                                      (31 (goto 35)) ;;to TAG_1
                                      (34 (iconst_m1)) ;;at TAG_0
                                      (35 (ireturn)) ;;at TAG_1
                                      (endofcode 36))
                                   (Exceptions )
                                   (StackMap )))
                        (method "read"
                              (parameters (array byte) int int)
                              (returntype . int)
                              (accessflags  *class*  *public*  *super*  *synchronized* )
                              (code
                                   (max_stack . 5) (max_locals . 5) (code_length . 101)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (ifnonnull 12)) ;;to TAG_0
                                      (4 (new (class "java.lang.NullPointerException"))) 
                                      (7 (dup)) 
                                      (8 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" () void))) 
                                      (11 (athrow)) 
                                      (12 (iload_2)) ;;at TAG_0
                                      (13 (iflt 28)) ;;to TAG_1
                                      (16 (iload_3)) 
                                      (17 (iflt 28)) ;;to TAG_1
                                      (20 (iload_3)) 
                                      (21 (aload_1)) 
                                      (22 (arraylength)) 
                                      (23 (iload_2)) 
                                      (24 (isub)) 
                                      (25 (if_icmple 36))  ;;to TAG_2
                                      (28 (new (class "java.lang.IndexOutOfBoundsException"))) ;;at TAG_1
                                      (31 (dup)) 
                                      (32 (invokespecial (methodCP "<init>" "java.lang.IndexOutOfBoundsException" () void))) 
                                      (35 (athrow)) 
                                      (36 (aload_0)) ;;at TAG_2
                                      (37 (getfield (fieldCP "pos" "java.io.ByteArrayInputStream" int))) 
                                      (40 (aload_0)) 
                                      (41 (getfield (fieldCP "count" "java.io.ByteArrayInputStream" int))) 
                                      (44 (if_icmplt 49)) ;;to TAG_3
                                      (47 (iconst_m1)) 
                                      (48 (ireturn)) 
                                      (49 (aload_0)) ;;at TAG_3
                                      (50 (getfield (fieldCP "count" "java.io.ByteArrayInputStream" int))) 
                                      (53 (aload_0)) 
                                      (54 (getfield (fieldCP "pos" "java.io.ByteArrayInputStream" int))) 
                                      (57 (isub)) 
                                      (58 (istore 4)) 
                                      (60 (iload_3)) 
                                      (61 (iload 4)) 
                                      (63 (if_icmple 69)) ;;to TAG_4
                                      (66 (iload 4)) 
                                      (68 (istore_3)) 
                                      (69 (iload_3)) ;;at TAG_4
                                      (70 (ifgt 75)) ;;to TAG_5
                                      (73 (iconst_0)) 
                                      (74 (ireturn)) 
                                      (75 (aload_0)) ;;at TAG_5
                                      (76 (getfield (fieldCP "buf" "java.io.ByteArrayInputStream" (array byte)))) 
                                      (79 (aload_0)) 
                                      (80 (getfield (fieldCP "pos" "java.io.ByteArrayInputStream" int))) 
                                      (83 (aload_1)) 
                                      (84 (iload_2)) 
                                      (85 (iload_3)) 
                                      (86 (invokestatic (methodCP "arraycopy" "java.lang.System" ((class "java.lang.Object") int (class "java.lang.Object") int int) void))) 
                                      (89 (aload_0)) 
                                      (90 (dup)) 
                                      (91 (getfield (fieldCP "pos" "java.io.ByteArrayInputStream" int))) 
                                      (94 (iload_3)) 
                                      (95 (iadd)) 
                                      (96 (putfield (fieldCP "pos" "java.io.ByteArrayInputStream" int))) 
                                      (99 (iload_3)) 
                                      (100 (ireturn)) 
                                      (endofcode 101))
                                   (Exceptions )
                                   (StackMap )))
                        (method "skip"
                              (parameters long)
                              (returntype . long)
                              (accessflags  *class*  *public*  *super*  *synchronized* )
                              (code
                                   (max_stack . 5) (max_locals . 5) (code_length . 43)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "count" "java.io.ByteArrayInputStream" int))) 
                                      (4 (aload_0)) 
                                      (5 (getfield (fieldCP "pos" "java.io.ByteArrayInputStream" int))) 
                                      (8 (isub)) 
                                      (9 (i2l)) 
                                      (10 (lstore_3)) 
                                      (11 (lload_1)) 
                                      (12 (lload_3)) 
                                      (13 (lcmp)) 
                                      (14 (ifge 29)) ;;to TAG_0
                                      (17 (lload_1)) 
                                      (18 (lconst_0)) 
                                      (19 (lcmp)) 
                                      (20 (ifge 27)) ;;to TAG_1
                                      (23 (lconst_0)) 
                                      (24 (goto 28))  ;;to TAG_2
                                      (27 (lload_1)) ;;at TAG_1
                                      (28 (lstore_3)) ;;at TAG_2
                                      (29 (aload_0)) ;;at TAG_0
                                      (30 (dup)) 
                                      (31 (getfield (fieldCP "pos" "java.io.ByteArrayInputStream" int))) 
                                      (34 (i2l)) 
                                      (35 (lload_3)) 
                                      (36 (ladd)) 
                                      (37 (l2i)) 
                                      (38 (putfield (fieldCP "pos" "java.io.ByteArrayInputStream" int))) 
                                      (41 (lload_3)) 
                                      (42 (lreturn)) 
                                      (endofcode 43))
                                   (Exceptions )
                                   (StackMap )))
                        (method "available"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public*  *super*  *synchronized* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "count" "java.io.ByteArrayInputStream" int)))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "pos" "java.io.ByteArrayInputStream" int)))
                                      (8 (isub))
                                      (9 (ireturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "markSupported"
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
                        (method "mark"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_0))
                                      (2 (getfield (fieldCP "pos" "java.io.ByteArrayInputStream" int)))
                                      (5 (putfield (fieldCP "mark" "java.io.ByteArrayInputStream" int)))
                                      (8 (return))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "reset"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *super*  *synchronized* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_0))
                                      (2 (getfield (fieldCP "mark" "java.io.ByteArrayInputStream" int)))
                                      (5 (putfield (fieldCP "pos" "java.io.ByteArrayInputStream" int)))
                                      (8 (return))
                                      (endofcode 9))
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


(defconst *ByteArrayInputStream-class-table*
  (make-static-class-decls 
   *java.io.ByteArrayInputStream*))

(defconst *package-name-map* 
  ("java.io.ByteArrayInputStream" . "java.io"))

