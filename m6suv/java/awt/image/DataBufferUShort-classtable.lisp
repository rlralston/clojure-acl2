; DataBufferUShort-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:28 CDT 2014.
;

(defconst *java.awt.image.DataBufferUShort*
 (make-class-def
      '(class "java.awt.image.DataBufferUShort"
            "java.awt.image.DataBuffer"
            (constant_pool
                        (STRING  "dataArray is null")
                        (STRING  "Length of dataArray is less  than size+offset.")
                        (STRING  "dataArray[")
                        (STRING  "] is null")
                        (STRING  "Length of dataArray[")
                        (STRING  "] is less than size+")
                        (STRING  "offsets[")
                        (STRING  "].")
                        (INT 65535))
            (fields
                        (field "data" (array short) (accessflags  *class* ) -1)
                        (field "bankdata" (array (array short)) (accessflags  *class* ) -1))
            (methods
                        (method "<init>"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 35)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getstatic (fieldCP "STABLE" "sun.java2d.StateTrackable$State" (class "sun.java2d.StateTrackable$State"))))
                                      (4 (iconst_1))
                                      (5 (iload_1))
                                      (6 (invokespecial
					(methodCP "<init>" "java.awt.image.DataBuffer" ((class "sun.java2d.StateTrackable$State") int int) void)))
                                      (9 (aload_0))
                                      (10 (iload_1))
                                      (11 (newarray SHORT))
                                      (13 (putfield (fieldCP "data" "java.awt.image.DataBufferUShort" (array short))))
                                      (16 (aload_0))
                                      (17 (iconst_1))
                                      (18 (anewarray (array short)))
                                      (21 (putfield (fieldCP "bankdata" "java.awt.image.DataBufferUShort" (array (array short)))))
                                      (24 (aload_0))
                                      (25 (getfield (fieldCP "bankdata" "java.awt.image.DataBufferUShort" (array (array short)))))
                                      (28 (iconst_0))
                                      (29 (aload_0))
                                      (30 (getfield (fieldCP "data" "java.awt.image.DataBufferUShort" (array short))))
                                      (33 (aastore))
                                      (34 (return))
                                      (endofcode 35))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters int int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 4) (code_length . 51)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getstatic (fieldCP "STABLE" "sun.java2d.StateTrackable$State" (class "sun.java2d.StateTrackable$State")))) 
                                      (4 (iconst_1)) 
                                      (5 (iload_1)) 
                                      (6 (iload_2)) 
                                      (7 (invokespecial (methodCP "<init>" "java.awt.image.DataBuffer" ((class "sun.java2d.StateTrackable$State") int int int) void))) 
                                      (10 (aload_0)) 
                                      (11 (iload_2)) 
                                      (12 (anewarray (array short))) 
                                      (15 (putfield (fieldCP "bankdata" "java.awt.image.DataBufferUShort" (array (array short))))) 
                                      (18 (iconst_0)) 
                                      (19 (istore_3)) 
                                      (20 (iload_3)) ;;at TAG_1
                                      (21 (iload_2)) 
                                      (22 (if_icmpge 40))  ;;to TAG_0
                                      (25 (aload_0)) 
                                      (26 (getfield (fieldCP "bankdata" "java.awt.image.DataBufferUShort" (array (array short))))) 
                                      (29 (iload_3)) 
                                      (30 (iload_1)) 
                                      (31 (newarray SHORT)) 
                                      (33 (aastore)) 
                                      (34 (iinc 3 1)) 
                                      (37 (goto 20)) ;;to TAG_1
                                      (40 (aload_0)) ;;at TAG_0
                                      (41 (aload_0)) 
                                      (42 (getfield (fieldCP "bankdata" "java.awt.image.DataBufferUShort" (array (array short))))) 
                                      (45 (iconst_0)) 
                                      (46 (aaload)) 
                                      (47 (putfield (fieldCP "data" "java.awt.image.DataBufferUShort" (array short)))) 
                                      (50 (return)) 
                                      (endofcode 51))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (array short) int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 47)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getstatic (fieldCP "UNTRACKABLE" "sun.java2d.StateTrackable$State" (class "sun.java2d.StateTrackable$State")))) 
                                      (4 (iconst_1)) 
                                      (5 (iload_2)) 
                                      (6 (invokespecial (methodCP "<init>" "java.awt.image.DataBuffer" ((class "sun.java2d.StateTrackable$State") int int) void))) 
                                      (9 (aload_1)) 
                                      (10 (ifnonnull 23))  ;;to TAG_0
                                      (13 (new (class "java.lang.NullPointerException"))) 
                                      (16 (dup)) 
                                      (17 (ldc 0)) ;;STRING:: "dataArray is null"
                                      (19 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" ((class "java.lang.String")) void))) 
                                      (22 (athrow)) 
                                      (23 (aload_0)) ;;at TAG_0
                                      (24 (aload_1)) 
                                      (25 (putfield (fieldCP "data" "java.awt.image.DataBufferUShort" (array short)))) 
                                      (28 (aload_0)) 
                                      (29 (iconst_1)) 
                                      (30 (anewarray (array short))) 
                                      (33 (putfield (fieldCP "bankdata" "java.awt.image.DataBufferUShort" (array (array short))))) 
                                      (36 (aload_0)) 
                                      (37 (getfield (fieldCP "bankdata" "java.awt.image.DataBufferUShort" (array (array short))))) 
                                      (40 (iconst_0)) 
                                      (41 (aload_0)) 
                                      (42 (getfield (fieldCP "data" "java.awt.image.DataBufferUShort" (array short)))) 
                                      (45 (aastore)) 
                                      (46 (return)) 
                                      (endofcode 47))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (array short) int int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 4) (code_length . 67)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getstatic (fieldCP "UNTRACKABLE" "sun.java2d.StateTrackable$State" (class "sun.java2d.StateTrackable$State")))) 
                                      (4 (iconst_1)) 
                                      (5 (iload_2)) 
                                      (6 (iconst_1)) 
                                      (7 (iload_3)) 
                                      (8 (invokespecial (methodCP "<init>" "java.awt.image.DataBuffer" ((class "sun.java2d.StateTrackable$State") int int int int) void))) 
                                      (11 (aload_1)) 
                                      (12 (ifnonnull 25))  ;;to TAG_0
                                      (15 (new (class "java.lang.NullPointerException"))) 
                                      (18 (dup)) 
                                      (19 (ldc 0)) ;;STRING:: "dataArray is null"
                                      (21 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" ((class "java.lang.String")) void))) 
                                      (24 (athrow)) 
                                      (25 (iload_2)) ;;at TAG_0
                                      (26 (iload_3)) 
                                      (27 (iadd)) 
                                      (28 (aload_1)) 
                                      (29 (arraylength)) 
                                      (30 (if_icmple 43)) ;;to TAG_1
                                      (33 (new (class "java.lang.IllegalArgumentException"))) 
                                      (36 (dup)) 
                                      (37 (ldc 1)) ;;STRING:: "Length of dataArray is less  than size+offset."
                                      (39 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (42 (athrow)) 
                                      (43 (aload_0)) ;;at TAG_1
                                      (44 (aload_1)) 
                                      (45 (putfield (fieldCP "data" "java.awt.image.DataBufferUShort" (array short)))) 
                                      (48 (aload_0)) 
                                      (49 (iconst_1)) 
                                      (50 (anewarray (array short))) 
                                      (53 (putfield (fieldCP "bankdata" "java.awt.image.DataBufferUShort" (array (array short))))) 
                                      (56 (aload_0)) 
                                      (57 (getfield (fieldCP "bankdata" "java.awt.image.DataBufferUShort" (array (array short))))) 
                                      (60 (iconst_0)) 
                                      (61 (aload_0)) 
                                      (62 (getfield (fieldCP "data" "java.awt.image.DataBufferUShort" (array short)))) 
                                      (65 (aastore)) 
                                      (66 (return)) 
                                      (endofcode 67))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (array (array short)) int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 4) (code_length . 102)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getstatic (fieldCP "UNTRACKABLE" "sun.java2d.StateTrackable$State" (class "sun.java2d.StateTrackable$State")))) 
                                      (4 (iconst_1)) 
                                      (5 (iload_2)) 
                                      (6 (aload_1)) 
                                      (7 (arraylength)) 
                                      (8 (invokespecial (methodCP "<init>" "java.awt.image.DataBuffer" ((class "sun.java2d.StateTrackable$State") int int int) void))) 
                                      (11 (aload_1)) 
                                      (12 (ifnonnull 25)) ;;to TAG_0
                                      (15 (new (class "java.lang.NullPointerException"))) 
                                      (18 (dup)) 
                                      (19 (ldc 0)) ;;STRING:: "dataArray is null"
                                      (21 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" ((class "java.lang.String")) void))) 
                                      (24 (athrow)) 
                                      (25 (iconst_0)) ;;at TAG_0
                                      (26 (istore_3)) 
                                      (27 (iload_3)) ;;at TAG_3
                                      (28 (aload_1)) 
                                      (29 (arraylength)) 
                                      (30 (if_icmpge 77)) ;;to TAG_1
                                      (33 (aload_1)) 
                                      (34 (iload_3)) 
                                      (35 (aaload)) 
                                      (36 (ifnonnull 71))  ;;to TAG_2
                                      (39 (new (class "java.lang.NullPointerException"))) 
                                      (42 (dup)) 
                                      (43 (new (class "java.lang.StringBuilder"))) 
                                      (46 (dup)) 
                                      (47 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (50 (ldc 2)) ;;STRING:: "dataArray["
                                      (52 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (55 (iload_3)) 
                                      (56 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (int) (class "java.lang.StringBuilder")))) 
                                      (59 (ldc 3)) ;;STRING:: "] is null"
                                      (61 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (64 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (67 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" ((class "java.lang.String")) void))) 
                                      (70 (athrow)) 
                                      (71 (iinc 3 1)) ;;at TAG_2
                                      (74 (goto 27)) ;;to TAG_3
                                      (77 (aload_0)) ;;at TAG_1
                                      (78 (aload_1)) 
                                      (79 (invokevirtual (methodCP "clone" "short[][]" () (class "java.lang.Object")))) 
                                      (82 (checkcast (array (array short)))) 
                                      (85 (checkcast (array (array short)))) 
                                      (88 (putfield (fieldCP "bankdata" "java.awt.image.DataBufferUShort" (array (array short))))) 
                                      (91 (aload_0)) 
                                      (92 (aload_0)) 
                                      (93 (getfield (fieldCP "bankdata" "java.awt.image.DataBufferUShort" (array (array short))))) 
                                      (96 (iconst_0)) 
                                      (97 (aaload)) 
                                      (98 (putfield (fieldCP "data" "java.awt.image.DataBufferUShort" (array short)))) 
                                      (101 (return)) 
                                      (endofcode 102))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (array (array short)) int (array int))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 5) (code_length . 169)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getstatic (fieldCP "UNTRACKABLE" "sun.java2d.StateTrackable$State" (class "sun.java2d.StateTrackable$State")))) 
                                      (4 (iconst_1)) 
                                      (5 (iload_2)) 
                                      (6 (aload_1)) 
                                      (7 (arraylength)) 
                                      (8 (aload_3)) 
                                      (9 (invokespecial (methodCP "<init>" "java.awt.image.DataBuffer" ((class "sun.java2d.StateTrackable$State") int int int (array int)) void))) 
                                      (12 (aload_1)) 
                                      (13 (ifnonnull 26)) ;;to TAG_0
                                      (16 (new (class "java.lang.NullPointerException"))) 
                                      (19 (dup)) 
                                      (20 (ldc 0)) ;;STRING:: "dataArray is null"
                                      (22 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" ((class "java.lang.String")) void))) 
                                      (25 (athrow)) 
                                      (26 (iconst_0)) ;;at TAG_0
                                      (27 (istore 4)) 
                                      (29 (iload 4)) ;;at TAG_4
                                      (31 (aload_1)) 
                                      (32 (arraylength)) 
                                      (33 (if_icmpge 144)) ;;to TAG_1
                                      (36 (aload_1)) 
                                      (37 (iload 4)) 
                                      (39 (aaload)) 
                                      (40 (ifnonnull 76))  ;;to TAG_2
                                      (43 (new (class "java.lang.NullPointerException"))) 
                                      (46 (dup)) 
                                      (47 (new (class "java.lang.StringBuilder"))) 
                                      (50 (dup)) 
                                      (51 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (54 (ldc 2)) ;;STRING:: "dataArray["
                                      (56 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (59 (iload 4)) 
                                      (61 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (int) (class "java.lang.StringBuilder")))) 
                                      (64 (ldc 3)) ;;STRING:: "] is null"
                                      (66 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (69 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (72 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" ((class "java.lang.String")) void))) 
                                      (75 (athrow)) 
                                      (76 (iload_2)) ;;at TAG_2
                                      (77 (aload_3)) 
                                      (78 (iload 4)) 
                                      (80 (iaload)) 
                                      (81 (iadd)) 
                                      (82 (aload_1)) 
                                      (83 (iload 4)) 
                                      (85 (aaload)) 
                                      (86 (arraylength)) 
                                      (87 (if_icmple 138)) ;;to TAG_3
                                      (90 (new (class "java.lang.IllegalArgumentException"))) 
                                      (93 (dup)) 
                                      (94 (new (class "java.lang.StringBuilder"))) 
                                      (97 (dup)) 
                                      (98 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (101 (ldc 4)) ;;STRING:: "Length of dataArray["
                                      (103 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (106 (iload 4)) 
                                      (108 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (int) (class "java.lang.StringBuilder")))) 
                                      (111 (ldc 5)) ;;STRING:: "] is less than size+"
                                      (113 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (116 (ldc 6)) ;;STRING:: "offsets["
                                      (118 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (121 (iload 4)) 
                                      (123 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (int) (class "java.lang.StringBuilder")))) 
                                      (126 (ldc 7)) ;;STRING:: "]."
                                      (128 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (131 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (134 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (137 (athrow)) 
                                      (138 (iinc 4 1)) ;;at TAG_3
                                      (141 (goto 29)) ;;to TAG_4
                                      (144 (aload_0)) ;;at TAG_1
                                      (145 (aload_1)) 
                                      (146 (invokevirtual (methodCP "clone" "short[][]" () (class "java.lang.Object")))) 
                                      (149 (checkcast (array (array short)))) 
                                      (152 (checkcast (array (array short)))) 
                                      (155 (putfield (fieldCP "bankdata" "java.awt.image.DataBufferUShort" (array (array short))))) 
                                      (158 (aload_0)) 
                                      (159 (aload_0)) 
                                      (160 (getfield (fieldCP "bankdata" "java.awt.image.DataBufferUShort" (array (array short))))) 
                                      (163 (iconst_0)) 
                                      (164 (aaload)) 
                                      (165 (putfield (fieldCP "data" "java.awt.image.DataBufferUShort" (array short)))) 
                                      (168 (return)) 
                                      (endofcode 169))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getData"
                              (parameters )
                              (returntype . (array short))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 12)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "theTrackable" "java.awt.image.DataBufferUShort" (class "sun.java2d.StateTrackableDelegate"))))
                                      (4 (invokevirtual
					(methodCP "setUntrackable" "sun.java2d.StateTrackableDelegate" () void)))
                                      (7 (aload_0))
                                      (8 (getfield (fieldCP "data" "java.awt.image.DataBufferUShort" (array short))))
                                      (11 (areturn))
                                      (endofcode 12))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getData"
                              (parameters int)
                              (returntype . (array short))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 14)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "theTrackable" "java.awt.image.DataBufferUShort" (class "sun.java2d.StateTrackableDelegate"))))
                                      (4 (invokevirtual
					(methodCP "setUntrackable" "sun.java2d.StateTrackableDelegate" () void)))
                                      (7 (aload_0))
                                      (8 (getfield (fieldCP "bankdata" "java.awt.image.DataBufferUShort" (array (array short)))))
                                      (11 (iload_1))
                                      (12 (aaload))
                                      (13 (areturn))
                                      (endofcode 14))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getBankData"
                              (parameters )
                              (returntype . (array (array short)))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 21)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "theTrackable" "java.awt.image.DataBufferUShort" (class "sun.java2d.StateTrackableDelegate"))))
                                      (4 (invokevirtual
					(methodCP "setUntrackable" "sun.java2d.StateTrackableDelegate" () void)))
                                      (7 (aload_0))
                                      (8 (getfield (fieldCP "bankdata" "java.awt.image.DataBufferUShort" (array (array short)))))
                                      (11 (invokevirtual
					(methodCP "clone" "short[][]" () (class "java.lang.Object"))))
                                      (14 (checkcast (array (array short))))
                                      (17 (checkcast (array (array short))))
                                      (20 (areturn))
                                      (endofcode 21))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getElem"
                              (parameters int)
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "data" "java.awt.image.DataBufferUShort" (array short))))
                                      (4 (iload_1))
                                      (5 (aload_0))
                                      (6 (getfield (fieldCP "offset" "java.awt.image.DataBufferUShort" int)))
                                      (9 (iadd))
                                      (10 (saload))
                                      (11 (ldc 8))        ;;INT:: "65535"
                                      (13 (iand))
                                      (14 (ireturn))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getElem"
                              (parameters int int)
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 19)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "bankdata" "java.awt.image.DataBufferUShort" (array (array short)))))
                                      (4 (iload_1))
                                      (5 (aaload))
                                      (6 (iload_2))
                                      (7 (aload_0))
                                      (8 (getfield (fieldCP "offsets" "java.awt.image.DataBufferUShort" (array int))))
                                      (11 (iload_1))
                                      (12 (iaload))
                                      (13 (iadd))
                                      (14 (saload))
                                      (15 (ldc 8))        ;;INT:: "65535"
                                      (17 (iand))
                                      (18 (ireturn))
                                      (endofcode 19))
                                   (Exceptions )
                                   (StackMap )))
                        (method "setElem"
                              (parameters int int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 24)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "data" "java.awt.image.DataBufferUShort" (array short))))
                                      (4 (iload_1))
                                      (5 (aload_0))
                                      (6 (getfield (fieldCP "offset" "java.awt.image.DataBufferUShort" int)))
                                      (9 (iadd))
                                      (10 (iload_2))
                                      (11 (ldc 8))        ;;INT:: "65535"
                                      (13 (iand))
                                      (14 (i2s))
                                      (15 (sastore))
                                      (16 (aload_0))
                                      (17 (getfield (fieldCP "theTrackable" "java.awt.image.DataBufferUShort" (class "sun.java2d.StateTrackableDelegate"))))
                                      (20 (invokevirtual
					(methodCP "markDirty" "sun.java2d.StateTrackableDelegate" () void)))
                                      (23 (return))
                                      (endofcode 24))
                                   (Exceptions )
                                   (StackMap )))
                        (method "setElem"
                              (parameters int int int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 4) (code_length . 28)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "bankdata" "java.awt.image.DataBufferUShort" (array (array short)))))
                                      (4 (iload_1))
                                      (5 (aaload))
                                      (6 (iload_2))
                                      (7 (aload_0))
                                      (8 (getfield (fieldCP "offsets" "java.awt.image.DataBufferUShort" (array int))))
                                      (11 (iload_1))
                                      (12 (iaload))
                                      (13 (iadd))
                                      (14 (iload_3))
                                      (15 (ldc 8))        ;;INT:: "65535"
                                      (17 (iand))
                                      (18 (i2s))
                                      (19 (sastore))
                                      (20 (aload_0))
                                      (21 (getfield (fieldCP "theTrackable" "java.awt.image.DataBufferUShort" (class "sun.java2d.StateTrackableDelegate"))))
                                      (24 (invokevirtual
					(methodCP "markDirty" "sun.java2d.StateTrackableDelegate" () void)))
                                      (27 (return))
                                      (endofcode 28))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *DataBufferUShort-class-table*
  (make-static-class-decls 
   *java.awt.image.DataBufferUShort*))

(defconst *package-name-map* 
  ("java.awt.image.DataBufferUShort" . "java.awt.image"))
