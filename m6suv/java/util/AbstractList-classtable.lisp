; AbstractList-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:42 CDT 2014.
;

(defconst *java.util.AbstractList*
 (make-class-def
      '(class "java.util.AbstractList"
            "java.util.AbstractCollection"
            (constant_pool
                        (STRING  "Index: ")
                        (STRING  ", Size: "))
            (fields
                        (field "modCount" int (accessflags  *class*  *protected*  *transient* ) -1))
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.util.AbstractCollection" () void)))
                                      (4 (aload_0))
                                      (5 (iconst_0))
                                      (6 (putfield (fieldCP "modCount" "java.util.AbstractList" int)))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "add"
                              (parameters (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_0))
                                      (2 (invokevirtual
					(methodCP "size" "java.util.AbstractList" () int)))
                                      (5 (aload_1))
                                      (6 (invokevirtual
					(methodCP "add" "java.util.AbstractList" (int (class "java.lang.Object")) void)))
                                      (9 (iconst_1))
                                      (10 (ireturn))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "get"
                              (parameters int)
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "set"
                              (parameters int (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 8)
                                   (parsedcode
                                      (0 (new (class "java.lang.UnsupportedOperationException")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.lang.UnsupportedOperationException" () void)))
                                      (7 (athrow))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "add"
                              (parameters int (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 8)
                                   (parsedcode
                                      (0 (new (class "java.lang.UnsupportedOperationException")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.lang.UnsupportedOperationException" () void)))
                                      (7 (athrow))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "remove"
                              (parameters int)
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 8)
                                   (parsedcode
                                      (0 (new (class "java.lang.UnsupportedOperationException")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.lang.UnsupportedOperationException" () void)))
                                      (7 (athrow))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "indexOf"
                              (parameters (class "java.lang.Object"))
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 65)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "listIterator" "java.util.AbstractList" () (class "java.util.ListIterator")))) 
                                      (4 (astore_2)) 
                                      (5 (aload_1)) 
                                      (6 (ifnonnull 34)) ;;to TAG_0
                                      (9 (aload_2)) ;;at TAG_2
                                      (10 (invokeinterface (methodCP "hasNext" "java.util.ListIterator" () boolean) 1)) 
                                      (15 (ifeq 63)) ;;to TAG_1
                                      (18 (aload_2)) 
                                      (19 (invokeinterface (methodCP "next" "java.util.ListIterator" () (class "java.lang.Object")) 1)) 
                                      (24 (ifnonnull 9))  ;;to TAG_2
                                      (27 (aload_2)) 
                                      (28 (invokeinterface (methodCP "previousIndex" "java.util.ListIterator" () int) 1)) 
                                      (33 (ireturn)) 
                                      (34 (aload_2)) ;;at TAG_0
                                      (35 (invokeinterface (methodCP "hasNext" "java.util.ListIterator" () boolean) 1)) 
                                      (40 (ifeq 63)) ;;to TAG_1
                                      (43 (aload_1)) 
                                      (44 (aload_2)) 
                                      (45 (invokeinterface (methodCP "next" "java.util.ListIterator" () (class "java.lang.Object")) 1)) 
                                      (50 (invokevirtual (methodCP "equals" "java.lang.Object" ((class "java.lang.Object")) boolean))) 
                                      (53 (ifeq 34)) ;;to TAG_0
                                      (56 (aload_2)) 
                                      (57 (invokeinterface (methodCP "previousIndex" "java.util.ListIterator" () int) 1)) 
                                      (62 (ireturn)) 
                                      (63 (iconst_m1)) ;;at TAG_1
                                      (64 (ireturn)) 
                                      (endofcode 65))
                                   (Exceptions )
                                   (StackMap )))
                        (method "lastIndexOf"
                              (parameters (class "java.lang.Object"))
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 69)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (aload_0)) 
                                      (2 (invokevirtual (methodCP "size" "java.util.AbstractList" () int))) 
                                      (5 (invokevirtual (methodCP "listIterator" "java.util.AbstractList" (int) (class "java.util.ListIterator")))) 
                                      (8 (astore_2)) 
                                      (9 (aload_1)) 
                                      (10 (ifnonnull 38)) ;;to TAG_0
                                      (13 (aload_2)) ;;at TAG_2
                                      (14 (invokeinterface (methodCP "hasPrevious" "java.util.ListIterator" () boolean) 1)) 
                                      (19 (ifeq 67)) ;;to TAG_1
                                      (22 (aload_2)) 
                                      (23 (invokeinterface (methodCP "previous" "java.util.ListIterator" () (class "java.lang.Object")) 1)) 
                                      (28 (ifnonnull 13))  ;;to TAG_2
                                      (31 (aload_2)) 
                                      (32 (invokeinterface (methodCP "nextIndex" "java.util.ListIterator" () int) 1)) 
                                      (37 (ireturn)) 
                                      (38 (aload_2)) ;;at TAG_0
                                      (39 (invokeinterface (methodCP "hasPrevious" "java.util.ListIterator" () boolean) 1)) 
                                      (44 (ifeq 67)) ;;to TAG_1
                                      (47 (aload_1)) 
                                      (48 (aload_2)) 
                                      (49 (invokeinterface (methodCP "previous" "java.util.ListIterator" () (class "java.lang.Object")) 1)) 
                                      (54 (invokevirtual (methodCP "equals" "java.lang.Object" ((class "java.lang.Object")) boolean))) 
                                      (57 (ifeq 38)) ;;to TAG_0
                                      (60 (aload_2)) 
                                      (61 (invokeinterface (methodCP "nextIndex" "java.util.ListIterator" () int) 1)) 
                                      (66 (ireturn)) 
                                      (67 (iconst_m1)) ;;at TAG_1
                                      (68 (ireturn)) 
                                      (endofcode 69))
                                   (Exceptions )
                                   (StackMap )))
                        (method "clear"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (iconst_0))
                                      (2 (aload_0))
                                      (3 (invokevirtual
					(methodCP "size" "java.util.AbstractList" () int)))
                                      (6 (invokevirtual
					(methodCP "removeRange" "java.util.AbstractList" (int int) void)))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "addAll"
                              (parameters int (class "java.util.Collection"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 6) (code_length . 51)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (iload_1)) 
                                      (2 (invokespecial (methodCP "rangeCheckForAdd" "java.util.AbstractList" (int) void))) 
                                      (5 (iconst_0)) 
                                      (6 (istore_3)) 
                                      (7 (aload_2)) 
                                      (8 (invokeinterface (methodCP "iterator" "java.util.Collection" () (class "java.util.Iterator")) 1)) 
                                      (13 (astore 4)) 
                                      (15 (aload 4)) ;;at TAG_1
                                      (17 (invokeinterface (methodCP "hasNext" "java.util.Iterator" () boolean) 1)) 
                                      (22 (ifeq 49))  ;;to TAG_0
                                      (25 (aload 4)) 
                                      (27 (invokeinterface (methodCP "next" "java.util.Iterator" () (class "java.lang.Object")) 1)) 
                                      (32 (astore 5)) 
                                      (34 (aload_0)) 
                                      (35 (iload_1)) 
                                      (36 (iinc 1 1)) 
                                      (39 (aload 5)) 
                                      (41 (invokevirtual (methodCP "add" "java.util.AbstractList" (int (class "java.lang.Object")) void))) 
                                      (44 (iconst_1)) 
                                      (45 (istore_3)) 
                                      (46 (goto 15)) ;;to TAG_1
                                      (49 (iload_3)) ;;at TAG_0
                                      (50 (ireturn)) 
                                      (endofcode 51))
                                   (Exceptions )
                                   (StackMap )))
                        (method "iterator"
                              (parameters )
                              (returntype . (class "java.util.Iterator"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (new (class "java.util.AbstractList$Itr")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (aconst_null))
                                      (6 (invokespecial
					(methodCP "<init>" "java.util.AbstractList$Itr" ((class "java.util.AbstractList") (class "java.util.AbstractList$1")) void)))
                                      (9 (areturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "listIterator"
                              (parameters )
                              (returntype . (class "java.util.ListIterator"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (iconst_0))
                                      (2 (invokevirtual
					(methodCP "listIterator" "java.util.AbstractList" (int) (class "java.util.ListIterator"))))
                                      (5 (areturn))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "listIterator"
                              (parameters int)
                              (returntype . (class "java.util.ListIterator"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (iload_1))
                                      (2 (invokespecial
					(methodCP "rangeCheckForAdd" "java.util.AbstractList" (int) void)))
                                      (5 (new (class "java.util.AbstractList$ListItr")))
                                      (8 (dup))
                                      (9 (aload_0))
                                      (10 (iload_1))
                                      (11 (invokespecial
					(methodCP "<init>" "java.util.AbstractList$ListItr" ((class "java.util.AbstractList") int) void)))
                                      (14 (areturn))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "subList"
                              (parameters int int)
                              (returntype . (class "java.util.List"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 3) (code_length . 31)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (instanceof (class "java.util.RandomAccess"))) 
                                      (4 (ifeq 20))  ;;to TAG_0
                                      (7 (new (class "java.util.RandomAccessSubList"))) 
                                      (10 (dup)) 
                                      (11 (aload_0)) 
                                      (12 (iload_1)) 
                                      (13 (iload_2)) 
                                      (14 (invokespecial (methodCP "<init>" "java.util.RandomAccessSubList" ((class "java.util.AbstractList") int int) void))) 
                                      (17 (goto 30)) ;;to TAG_1
                                      (20 (new (class "java.util.SubList"))) ;;at TAG_0
                                      (23 (dup)) 
                                      (24 (aload_0)) 
                                      (25 (iload_1)) 
                                      (26 (iload_2)) 
                                      (27 (invokespecial (methodCP "<init>" "java.util.SubList" ((class "java.util.AbstractList") int int) void))) 
                                      (30 (areturn)) ;;at TAG_1
                                      (endofcode 31))
                                   (Exceptions )
                                   (StackMap )))
                        (method "equals"
                              (parameters (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 6) (code_length . 117)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (aload_0)) 
                                      (2 (if_acmpne 7)) ;;to TAG_0
                                      (5 (iconst_1)) 
                                      (6 (ireturn)) 
                                      (7 (aload_1)) ;;at TAG_0
                                      (8 (instanceof (class "java.util.List"))) 
                                      (11 (ifne 16))  ;;to TAG_1
                                      (14 (iconst_0)) 
                                      (15 (ireturn)) 
                                      (16 (aload_0)) ;;at TAG_1
                                      (17 (invokevirtual (methodCP "listIterator" "java.util.AbstractList" () (class "java.util.ListIterator")))) 
                                      (20 (astore_2)) 
                                      (21 (aload_1)) 
                                      (22 (checkcast (class "java.util.List"))) 
                                      (25 (invokeinterface (methodCP "listIterator" "java.util.List" () (class "java.util.ListIterator")) 1)) 
                                      (30 (astore_3)) 
                                      (31 (aload_2)) ;;at TAG_6
                                      (32 (invokeinterface (methodCP "hasNext" "java.util.ListIterator" () boolean) 1)) 
                                      (37 (ifeq 93)) ;;to TAG_2
                                      (40 (aload_3)) 
                                      (41 (invokeinterface (methodCP "hasNext" "java.util.ListIterator" () boolean) 1)) 
                                      (46 (ifeq 93)) ;;to TAG_2
                                      (49 (aload_2)) 
                                      (50 (invokeinterface (methodCP "next" "java.util.ListIterator" () (class "java.lang.Object")) 1)) 
                                      (55 (astore 4)) 
                                      (57 (aload_3)) 
                                      (58 (invokeinterface (methodCP "next" "java.util.ListIterator" () (class "java.lang.Object")) 1)) 
                                      (63 (astore 5)) 
                                      (65 (aload 4)) 
                                      (67 (ifnonnull 78)) ;;to TAG_3
                                      (70 (aload 5)) 
                                      (72 (ifnonnull 88)) ;;to TAG_4
                                      (75 (goto 90)) ;;to TAG_5
                                      (78 (aload 4)) ;;at TAG_3
                                      (80 (aload 5)) 
                                      (82 (invokevirtual (methodCP "equals" "java.lang.Object" ((class "java.lang.Object")) boolean))) 
                                      (85 (ifne 90)) ;;to TAG_5
                                      (88 (iconst_0)) ;;at TAG_4
                                      (89 (ireturn)) 
                                      (90 (goto 31)) ;;to TAG_6;;at TAG_5
                                      (93 (aload_2)) ;;at TAG_2
                                      (94 (invokeinterface (methodCP "hasNext" "java.util.ListIterator" () boolean) 1)) 
                                      (99 (ifne 115)) ;;to TAG_7
                                      (102 (aload_3)) 
                                      (103 (invokeinterface (methodCP "hasNext" "java.util.ListIterator" () boolean) 1)) 
                                      (108 (ifne 115)) ;;to TAG_7
                                      (111 (iconst_1)) 
                                      (112 (goto 116)) ;;to TAG_8
                                      (115 (iconst_0)) ;;at TAG_7
                                      (116 (ireturn)) ;;at TAG_8
                                      (endofcode 117))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hashCode"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 46)
                                   (parsedcode
                                      (0 (iconst_1)) 
                                      (1 (istore_1)) 
                                      (2 (aload_0)) 
                                      (3 (invokevirtual (methodCP "iterator" "java.util.AbstractList" () (class "java.util.Iterator")))) 
                                      (6 (astore_2)) 
                                      (7 (aload_2)) ;;at TAG_3
                                      (8 (invokeinterface (methodCP "hasNext" "java.util.Iterator" () boolean) 1)) 
                                      (13 (ifeq 44)) ;;to TAG_0
                                      (16 (aload_2)) 
                                      (17 (invokeinterface (methodCP "next" "java.util.Iterator" () (class "java.lang.Object")) 1)) 
                                      (22 (astore_3)) 
                                      (23 (bipush 31)) 
                                      (25 (iload_1)) 
                                      (26 (imul)) 
                                      (27 (aload_3)) 
                                      (28 (ifnonnull 35)) ;;to TAG_1
                                      (31 (iconst_0)) 
                                      (32 (goto 39))  ;;to TAG_2
                                      (35 (aload_3)) ;;at TAG_1
                                      (36 (invokevirtual (methodCP "hashCode" "java.lang.Object" () int))) 
                                      (39 (iadd)) ;;at TAG_2
                                      (40 (istore_1)) 
                                      (41 (goto 7)) ;;to TAG_3
                                      (44 (iload_1)) ;;at TAG_0
                                      (45 (ireturn)) 
                                      (endofcode 46))
                                   (Exceptions )
                                   (StackMap )))
                        (method "removeRange"
                              (parameters int int)
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 2) (max_locals . 6) (code_length . 41)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (iload_1)) 
                                      (2 (invokevirtual (methodCP "listIterator" "java.util.AbstractList" (int) (class "java.util.ListIterator")))) 
                                      (5 (astore_3)) 
                                      (6 (iconst_0)) 
                                      (7 (istore 4)) 
                                      (9 (iload_2)) 
                                      (10 (iload_1)) 
                                      (11 (isub)) 
                                      (12 (istore 5)) 
                                      (14 (iload 4)) ;;at TAG_1
                                      (16 (iload 5)) 
                                      (18 (if_icmpge 40))  ;;to TAG_0
                                      (21 (aload_3)) 
                                      (22 (invokeinterface (methodCP "next" "java.util.ListIterator" () (class "java.lang.Object")) 1)) 
                                      (27 (pop)) 
                                      (28 (aload_3)) 
                                      (29 (invokeinterface (methodCP "remove" "java.util.ListIterator" () void) 1)) 
                                      (34 (iinc 4 1)) 
                                      (37 (goto 14)) ;;to TAG_1
                                      (40 (return)) ;;at TAG_0
                                      (endofcode 41))
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
                                      (6 (invokevirtual (methodCP "size" "java.util.AbstractList" () int))) 
                                      (9 (if_icmple 25)) ;;to TAG_1
                                      (12 (new (class "java.lang.IndexOutOfBoundsException"))) ;;at TAG_0
                                      (15 (dup)) 
                                      (16 (aload_0)) 
                                      (17 (iload_1)) 
                                      (18 (invokespecial (methodCP "outOfBoundsMsg" "java.util.AbstractList" (int) (class "java.lang.String")))) 
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
                                      (7 (ldc 0))         ;;STRING:: "Index: "
                                      (9 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (12 (iload_1))
                                      (13 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" (int) (class "java.lang.StringBuilder"))))
                                      (16 (ldc 1))        ;;STRING:: ", Size: "
                                      (18 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (21 (aload_0))
                                      (22 (invokevirtual
					(methodCP "size" "java.util.AbstractList" () int)))
                                      (25 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" (int) (class "java.lang.StringBuilder"))))
                                      (28 (invokevirtual
					(methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String"))))
                                      (31 (areturn))
                                      (endofcode 32))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.util.List")
            (accessflags  *abstract*  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *AbstractList-class-table*
  (make-static-class-decls 
   *java.util.AbstractList*))

(defconst *package-name-map* 
  ("java.util.AbstractList" . "java.util"))

