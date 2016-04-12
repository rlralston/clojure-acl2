; SubList-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:48 CDT 2014.
;

(defconst *java.util.SubList*
 (make-class-def
      '(class "java.util.SubList"
            "java.util.AbstractList"
            (constant_pool
                        (STRING  "fromIndex = ")
                        (STRING  "toIndex = ")
                        (STRING  "fromIndex(")
                        (STRING  ") > toIndex(")
                        (STRING  ")")
                        (STRING  "Index: ")
                        (STRING  ", Size: "))
            (fields
                        (field "l" (class "java.util.AbstractList") (accessflags  *class*  *final*  *private* ) -1)
                        (field "offset" int (accessflags  *class*  *final*  *private* ) -1)
                        (field "size" int (accessflags  *class*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.AbstractList") int int)
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 4) (max_locals . 4) (code_length . 145)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokespecial (methodCP "<init>" "java.util.AbstractList" () void))) 
                                      (4 (iload_2)) 
                                      (5 (ifge 35)) ;;to TAG_0
                                      (8 (new (class "java.lang.IndexOutOfBoundsException"))) 
                                      (11 (dup)) 
                                      (12 (new (class "java.lang.StringBuilder"))) 
                                      (15 (dup)) 
                                      (16 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (19 (ldc 0)) ;;STRING:: "fromIndex = "
                                      (21 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (24 (iload_2)) 
                                      (25 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (int) (class "java.lang.StringBuilder")))) 
                                      (28 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (31 (invokespecial (methodCP "<init>" "java.lang.IndexOutOfBoundsException" ((class "java.lang.String")) void))) 
                                      (34 (athrow)) 
                                      (35 (iload_3)) ;;at TAG_0
                                      (36 (aload_1)) 
                                      (37 (invokevirtual (methodCP "size" "java.util.AbstractList" () int))) 
                                      (40 (if_icmple 70)) ;;to TAG_1
                                      (43 (new (class "java.lang.IndexOutOfBoundsException"))) 
                                      (46 (dup)) 
                                      (47 (new (class "java.lang.StringBuilder"))) 
                                      (50 (dup)) 
                                      (51 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (54 (ldc 1)) ;;STRING:: "toIndex = "
                                      (56 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (59 (iload_3)) 
                                      (60 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (int) (class "java.lang.StringBuilder")))) 
                                      (63 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (66 (invokespecial (methodCP "<init>" "java.lang.IndexOutOfBoundsException" ((class "java.lang.String")) void))) 
                                      (69 (athrow)) 
                                      (70 (iload_2)) ;;at TAG_1
                                      (71 (iload_3)) 
                                      (72 (if_icmple 116))  ;;to TAG_2
                                      (75 (new (class "java.lang.IllegalArgumentException"))) 
                                      (78 (dup)) 
                                      (79 (new (class "java.lang.StringBuilder"))) 
                                      (82 (dup)) 
                                      (83 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (86 (ldc 2)) ;;STRING:: "fromIndex("
                                      (88 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (91 (iload_2)) 
                                      (92 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (int) (class "java.lang.StringBuilder")))) 
                                      (95 (ldc 3)) ;;STRING:: ") > toIndex("
                                      (97 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (100 (iload_3)) 
                                      (101 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (int) (class "java.lang.StringBuilder")))) 
                                      (104 (ldc 4)) ;;STRING:: ")"
                                      (106 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (109 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (112 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (115 (athrow)) 
                                      (116 (aload_0)) ;;at TAG_2
                                      (117 (aload_1)) 
                                      (118 (putfield (fieldCP "l" "java.util.SubList" (class "java.util.AbstractList")))) 
                                      (121 (aload_0)) 
                                      (122 (iload_2)) 
                                      (123 (putfield (fieldCP "offset" "java.util.SubList" int))) 
                                      (126 (aload_0)) 
                                      (127 (iload_3)) 
                                      (128 (iload_2)) 
                                      (129 (isub)) 
                                      (130 (putfield (fieldCP "size" "java.util.SubList" int))) 
                                      (133 (aload_0)) 
                                      (134 (aload_0)) 
                                      (135 (getfield (fieldCP "l" "java.util.SubList" (class "java.util.AbstractList")))) 
                                      (138 (getfield (fieldCP "modCount" "java.util.AbstractList" int))) 
                                      (141 (putfield (fieldCP "modCount" "java.util.SubList" int))) 
                                      (144 (return)) 
                                      (endofcode 145))
                                   (Exceptions )
                                   (StackMap )))
                        (method "set"
                              (parameters int (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 24)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (iload_1))
                                      (2 (invokespecial
					(methodCP "rangeCheck" "java.util.SubList" (int) void)))
                                      (5 (aload_0))
                                      (6 (invokespecial
					(methodCP "checkForComodification" "java.util.SubList" () void)))
                                      (9 (aload_0))
                                      (10 (getfield (fieldCP "l" "java.util.SubList" (class "java.util.AbstractList"))))
                                      (13 (iload_1))
                                      (14 (aload_0))
                                      (15 (getfield (fieldCP "offset" "java.util.SubList" int)))
                                      (18 (iadd))
                                      (19 (aload_2))
                                      (20 (invokevirtual
					(methodCP "set" "java.util.AbstractList" (int (class "java.lang.Object")) (class "java.lang.Object"))))
                                      (23 (areturn))
                                      (endofcode 24))
                                   (Exceptions )
                                   (StackMap )))
                        (method "get"
                              (parameters int)
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 23)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (iload_1))
                                      (2 (invokespecial
					(methodCP "rangeCheck" "java.util.SubList" (int) void)))
                                      (5 (aload_0))
                                      (6 (invokespecial
					(methodCP "checkForComodification" "java.util.SubList" () void)))
                                      (9 (aload_0))
                                      (10 (getfield (fieldCP "l" "java.util.SubList" (class "java.util.AbstractList"))))
                                      (13 (iload_1))
                                      (14 (aload_0))
                                      (15 (getfield (fieldCP "offset" "java.util.SubList" int)))
                                      (18 (iadd))
                                      (19 (invokevirtual
					(methodCP "get" "java.util.AbstractList" (int) (class "java.lang.Object"))))
                                      (22 (areturn))
                                      (endofcode 23))
                                   (Exceptions )
                                   (StackMap )))
                        (method "size"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "checkForComodification" "java.util.SubList" () void)))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "size" "java.util.SubList" int)))
                                      (8 (ireturn))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "add"
                              (parameters int (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 45)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (iload_1))
                                      (2 (invokespecial
					(methodCP "rangeCheckForAdd" "java.util.SubList" (int) void)))
                                      (5 (aload_0))
                                      (6 (invokespecial
					(methodCP "checkForComodification" "java.util.SubList" () void)))
                                      (9 (aload_0))
                                      (10 (getfield (fieldCP "l" "java.util.SubList" (class "java.util.AbstractList"))))
                                      (13 (iload_1))
                                      (14 (aload_0))
                                      (15 (getfield (fieldCP "offset" "java.util.SubList" int)))
                                      (18 (iadd))
                                      (19 (aload_2))
                                      (20 (invokevirtual
					(methodCP "add" "java.util.AbstractList" (int (class "java.lang.Object")) void)))
                                      (23 (aload_0))
                                      (24 (aload_0))
                                      (25 (getfield (fieldCP "l" "java.util.SubList" (class "java.util.AbstractList"))))
                                      (28 (getfield (fieldCP "modCount" "java.util.AbstractList" int)))
                                      (31 (putfield (fieldCP "modCount" "java.util.SubList" int)))
                                      (34 (aload_0))
                                      (35 (dup))
                                      (36 (getfield (fieldCP "size" "java.util.SubList" int)))
                                      (39 (iconst_1))
                                      (40 (iadd))
                                      (41 (putfield (fieldCP "size" "java.util.SubList" int)))
                                      (44 (return))
                                      (endofcode 45))
                                   (Exceptions )
                                   (StackMap )))
                        (method "remove"
                              (parameters int)
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 46)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (iload_1))
                                      (2 (invokespecial
					(methodCP "rangeCheck" "java.util.SubList" (int) void)))
                                      (5 (aload_0))
                                      (6 (invokespecial
					(methodCP "checkForComodification" "java.util.SubList" () void)))
                                      (9 (aload_0))
                                      (10 (getfield (fieldCP "l" "java.util.SubList" (class "java.util.AbstractList"))))
                                      (13 (iload_1))
                                      (14 (aload_0))
                                      (15 (getfield (fieldCP "offset" "java.util.SubList" int)))
                                      (18 (iadd))
                                      (19 (invokevirtual
					(methodCP "remove" "java.util.AbstractList" (int) (class "java.lang.Object"))))
                                      (22 (astore_2))
                                      (23 (aload_0))
                                      (24 (aload_0))
                                      (25 (getfield (fieldCP "l" "java.util.SubList" (class "java.util.AbstractList"))))
                                      (28 (getfield (fieldCP "modCount" "java.util.AbstractList" int)))
                                      (31 (putfield (fieldCP "modCount" "java.util.SubList" int)))
                                      (34 (aload_0))
                                      (35 (dup))
                                      (36 (getfield (fieldCP "size" "java.util.SubList" int)))
                                      (39 (iconst_1))
                                      (40 (isub))
                                      (41 (putfield (fieldCP "size" "java.util.SubList" int)))
                                      (44 (aload_2))
                                      (45 (areturn))
                                      (endofcode 46))
                                   (Exceptions )
                                   (StackMap )))
                        (method "removeRange"
                              (parameters int int)
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 47)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "checkForComodification" "java.util.SubList" () void)))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "l" "java.util.SubList" (class "java.util.AbstractList"))))
                                      (8 (iload_1))
                                      (9 (aload_0))
                                      (10 (getfield (fieldCP "offset" "java.util.SubList" int)))
                                      (13 (iadd))
                                      (14 (iload_2))
                                      (15 (aload_0))
                                      (16 (getfield (fieldCP "offset" "java.util.SubList" int)))
                                      (19 (iadd))
                                      (20 (invokevirtual
					(methodCP "removeRange" "java.util.AbstractList" (int int) void)))
                                      (23 (aload_0))
                                      (24 (aload_0))
                                      (25 (getfield (fieldCP "l" "java.util.SubList" (class "java.util.AbstractList"))))
                                      (28 (getfield (fieldCP "modCount" "java.util.AbstractList" int)))
                                      (31 (putfield (fieldCP "modCount" "java.util.SubList" int)))
                                      (34 (aload_0))
                                      (35 (dup))
                                      (36 (getfield (fieldCP "size" "java.util.SubList" int)))
                                      (39 (iload_2))
                                      (40 (iload_1))
                                      (41 (isub))
                                      (42 (isub))
                                      (43 (putfield (fieldCP "size" "java.util.SubList" int)))
                                      (46 (return))
                                      (endofcode 47))
                                   (Exceptions )
                                   (StackMap )))
                        (method "addAll"
                              (parameters (class "java.util.Collection"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_0))
                                      (2 (getfield (fieldCP "size" "java.util.SubList" int)))
                                      (5 (aload_1))
                                      (6 (invokevirtual
					(methodCP "addAll" "java.util.SubList" (int (class "java.util.Collection")) boolean)))
                                      (9 (ireturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "addAll"
                              (parameters int (class "java.util.Collection"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 4) (code_length . 60)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (iload_1)) 
                                      (2 (invokespecial (methodCP "rangeCheckForAdd" "java.util.SubList" (int) void))) 
                                      (5 (aload_2)) 
                                      (6 (invokeinterface (methodCP "size" "java.util.Collection" () int) 1)) 
                                      (11 (istore_3)) 
                                      (12 (iload_3)) 
                                      (13 (ifne 18))  ;;to TAG_0
                                      (16 (iconst_0)) 
                                      (17 (ireturn)) 
                                      (18 (aload_0)) ;;at TAG_0
                                      (19 (invokespecial (methodCP "checkForComodification" "java.util.SubList" () void))) 
                                      (22 (aload_0)) 
                                      (23 (getfield (fieldCP "l" "java.util.SubList" (class "java.util.AbstractList")))) 
                                      (26 (aload_0)) 
                                      (27 (getfield (fieldCP "offset" "java.util.SubList" int))) 
                                      (30 (iload_1)) 
                                      (31 (iadd)) 
                                      (32 (aload_2)) 
                                      (33 (invokevirtual (methodCP "addAll" "java.util.AbstractList" (int (class "java.util.Collection")) boolean))) 
                                      (36 (pop)) 
                                      (37 (aload_0)) 
                                      (38 (aload_0)) 
                                      (39 (getfield (fieldCP "l" "java.util.SubList" (class "java.util.AbstractList")))) 
                                      (42 (getfield (fieldCP "modCount" "java.util.AbstractList" int))) 
                                      (45 (putfield (fieldCP "modCount" "java.util.SubList" int))) 
                                      (48 (aload_0)) 
                                      (49 (dup)) 
                                      (50 (getfield (fieldCP "size" "java.util.SubList" int))) 
                                      (53 (iload_3)) 
                                      (54 (iadd)) 
                                      (55 (putfield (fieldCP "size" "java.util.SubList" int))) 
                                      (58 (iconst_1)) 
                                      (59 (ireturn)) 
                                      (endofcode 60))
                                   (Exceptions )
                                   (StackMap )))
                        (method "iterator"
                              (parameters )
                              (returntype . (class "java.util.Iterator"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokevirtual
					(methodCP "listIterator" "java.util.SubList" () (class "java.util.ListIterator"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "listIterator"
                              (parameters int)
                              (returntype . (class "java.util.ListIterator"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 19)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "checkForComodification" "java.util.SubList" () void)))
                                      (4 (aload_0))
                                      (5 (iload_1))
                                      (6 (invokespecial
					(methodCP "rangeCheckForAdd" "java.util.SubList" (int) void)))
                                      (9 (new (class "java.util.SubList$1")))
                                      (12 (dup))
                                      (13 (aload_0))
                                      (14 (iload_1))
                                      (15 (invokespecial
					(methodCP "<init>" "java.util.SubList$1" ((class "java.util.SubList") int) void)))
                                      (18 (areturn))
                                      (endofcode 19))
                                   (Exceptions )
                                   (StackMap )))
                        (method "subList"
                              (parameters int int)
                              (returntype . (class "java.util.List"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 3) (code_length . 11)
                                   (parsedcode
                                      (0 (new (class "java.util.SubList")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (iload_1))
                                      (6 (iload_2))
                                      (7 (invokespecial
					(methodCP "<init>" "java.util.SubList" ((class "java.util.AbstractList") int int) void)))
                                      (10 (areturn))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "rangeCheck"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 26)
                                   (parsedcode
                                      (0 (iload_1)) 
                                      (1 (iflt 12))  ;;to TAG_0
                                      (4 (iload_1)) 
                                      (5 (aload_0)) 
                                      (6 (getfield (fieldCP "size" "java.util.SubList" int))) 
                                      (9 (if_icmplt 25)) ;;to TAG_1
                                      (12 (new (class "java.lang.IndexOutOfBoundsException"))) ;;at TAG_0
                                      (15 (dup)) 
                                      (16 (aload_0)) 
                                      (17 (iload_1)) 
                                      (18 (invokespecial (methodCP "outOfBoundsMsg" "java.util.SubList" (int) (class "java.lang.String")))) 
                                      (21 (invokespecial (methodCP "<init>" "java.lang.IndexOutOfBoundsException" ((class "java.lang.String")) void))) 
                                      (24 (athrow)) 
                                      (25 (return)) ;;at TAG_1
                                      (endofcode 26))
                                   (Exceptions )
                                   (StackMap )))
                        (method "rangeCheckForAdd"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 26)
                                   (parsedcode
                                      (0 (iload_1)) 
                                      (1 (iflt 12))  ;;to TAG_0
                                      (4 (iload_1)) 
                                      (5 (aload_0)) 
                                      (6 (getfield (fieldCP "size" "java.util.SubList" int))) 
                                      (9 (if_icmple 25)) ;;to TAG_1
                                      (12 (new (class "java.lang.IndexOutOfBoundsException"))) ;;at TAG_0
                                      (15 (dup)) 
                                      (16 (aload_0)) 
                                      (17 (iload_1)) 
                                      (18 (invokespecial (methodCP "outOfBoundsMsg" "java.util.SubList" (int) (class "java.lang.String")))) 
                                      (21 (invokespecial (methodCP "<init>" "java.lang.IndexOutOfBoundsException" ((class "java.lang.String")) void))) 
                                      (24 (athrow)) 
                                      (25 (return)) ;;at TAG_1
                                      (endofcode 26))
                                   (Exceptions )
                                   (StackMap )))
                        (method "outOfBoundsMsg"
                              (parameters int)
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 32)
                                   (parsedcode
                                      (0 (new (class "java.lang.StringBuilder")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.lang.StringBuilder" () void)))
                                      (7 (ldc 5))         ;;STRING:: "Index: "
                                      (9 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (12 (iload_1))
                                      (13 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" (int) (class "java.lang.StringBuilder"))))
                                      (16 (ldc 6))        ;;STRING:: ", Size: "
                                      (18 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (21 (aload_0))
                                      (22 (getfield (fieldCP "size" "java.util.SubList" int)))
                                      (25 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" (int) (class "java.lang.StringBuilder"))))
                                      (28 (invokevirtual
					(methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String"))))
                                      (31 (areturn))
                                      (endofcode 32))
                                   (Exceptions )
                                   (StackMap )))
                        (method "checkForComodification"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 23)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "modCount" "java.util.SubList" int))) 
                                      (4 (aload_0)) 
                                      (5 (getfield (fieldCP "l" "java.util.SubList" (class "java.util.AbstractList")))) 
                                      (8 (getfield (fieldCP "modCount" "java.util.AbstractList" int))) 
                                      (11 (if_icmpeq 22))  ;;to TAG_0
                                      (14 (new (class "java.util.ConcurrentModificationException"))) 
                                      (17 (dup)) 
                                      (18 (invokespecial (methodCP "<init>" "java.util.ConcurrentModificationException" () void))) 
                                      (21 (athrow)) 
                                      (22 (return)) ;;at TAG_0
                                      (endofcode 23))
                                   (Exceptions )
                                   (StackMap )))
                        (method "access$000"
                              (parameters (class "java.util.SubList"))
                              (returntype . int)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "offset" "java.util.SubList" int)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "access$100"
                              (parameters (class "java.util.SubList"))
                              (returntype . (class "java.util.AbstractList"))
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "l" "java.util.SubList" (class "java.util.AbstractList"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "access$200"
                              (parameters (class "java.util.SubList"))
                              (returntype . int)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "size" "java.util.SubList" int)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "access$210"
                              (parameters (class "java.util.SubList"))
                              (returntype . int)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 1) (code_length . 12)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (dup))
                                      (2 (getfield (fieldCP "size" "java.util.SubList" int)))
                                      (5 (dup_x1))
                                      (6 (iconst_1))
                                      (7 (isub))
                                      (8 (putfield (fieldCP "size" "java.util.SubList" int)))
                                      (11 (ireturn))
                                      (endofcode 12))
                                   (Exceptions )
                                   (StackMap )))
                        (method "access$208"
                              (parameters (class "java.util.SubList"))
                              (returntype . int)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 1) (code_length . 12)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (dup))
                                      (2 (getfield (fieldCP "size" "java.util.SubList" int)))
                                      (5 (dup_x1))
                                      (6 (iconst_1))
                                      (7 (iadd))
                                      (8 (putfield (fieldCP "size" "java.util.SubList" int)))
                                      (11 (ireturn))
                                      (endofcode 12))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *SubList-class-table*
  (make-static-class-decls 
   *java.util.SubList*))

(defconst *package-name-map* 
  ("java.util.SubList" . "java.util"))
