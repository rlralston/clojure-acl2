; RectIterator-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:27 CDT 2014.
;

(defconst *java.awt.geom.RectIterator*
 (make-class-def
      '(class "java.awt.geom.RectIterator"
            "java.lang.Object"
            (constant_pool
                        (STRING  "rect iterator out of bounds"))
            (fields
                        (field "x" double (accessflags  *class* ) -1)
                        (field "y" double (accessflags  *class* ) -1)
                        (field "w" double (accessflags  *class* ) -1)
                        (field "h" double (accessflags  *class* ) -1)
                        (field "affine" (class "java.awt.geom.AffineTransform") (accessflags  *class* ) -1)
                        (field "index" int (accessflags  *class* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.awt.geom.Rectangle2D") (class "java.awt.geom.AffineTransform"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 66)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokespecial (methodCP "<init>" "java.lang.Object" () void))) 
                                      (4 (aload_0)) 
                                      (5 (aload_1)) 
                                      (6 (invokevirtual (methodCP "getX" "java.awt.geom.Rectangle2D" () double))) 
                                      (9 (putfield (fieldCP "x" "java.awt.geom.RectIterator" double))) 
                                      (12 (aload_0)) 
                                      (13 (aload_1)) 
                                      (14 (invokevirtual (methodCP "getY" "java.awt.geom.Rectangle2D" () double))) 
                                      (17 (putfield (fieldCP "y" "java.awt.geom.RectIterator" double))) 
                                      (20 (aload_0)) 
                                      (21 (aload_1)) 
                                      (22 (invokevirtual (methodCP "getWidth" "java.awt.geom.Rectangle2D" () double))) 
                                      (25 (putfield (fieldCP "w" "java.awt.geom.RectIterator" double))) 
                                      (28 (aload_0)) 
                                      (29 (aload_1)) 
                                      (30 (invokevirtual (methodCP "getHeight" "java.awt.geom.Rectangle2D" () double))) 
                                      (33 (putfield (fieldCP "h" "java.awt.geom.RectIterator" double))) 
                                      (36 (aload_0)) 
                                      (37 (aload_2)) 
                                      (38 (putfield (fieldCP "affine" "java.awt.geom.RectIterator" (class "java.awt.geom.AffineTransform")))) 
                                      (41 (aload_0)) 
                                      (42 (getfield (fieldCP "w" "java.awt.geom.RectIterator" double))) 
                                      (45 (dconst_0)) 
                                      (46 (dcmpg)) 
                                      (47 (iflt 59))  ;;to TAG_0
                                      (50 (aload_0)) 
                                      (51 (getfield (fieldCP "h" "java.awt.geom.RectIterator" double))) 
                                      (54 (dconst_0)) 
                                      (55 (dcmpg)) 
                                      (56 (ifge 65)) ;;to TAG_1
                                      (59 (aload_0)) ;;at TAG_0
                                      (60 (bipush 6)) 
                                      (62 (putfield (fieldCP "index" "java.awt.geom.RectIterator" int))) 
                                      (65 (return)) ;;at TAG_1
                                      (endofcode 66))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getWindingRule"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 2)
                                   (parsedcode
                                      (0 (iconst_1))
                                      (1 (ireturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isDone"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 14)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "index" "java.awt.geom.RectIterator" int))) 
                                      (4 (iconst_5)) 
                                      (5 (if_icmple 12))  ;;to TAG_0
                                      (8 (iconst_1)) 
                                      (9 (goto 13)) ;;to TAG_1
                                      (12 (iconst_0)) ;;at TAG_0
                                      (13 (ireturn)) ;;at TAG_1
                                      (endofcode 14))
                                   (Exceptions )
                                   (StackMap )))
                        (method "next"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (dup))
                                      (2 (getfield (fieldCP "index" "java.awt.geom.RectIterator" int)))
                                      (5 (iconst_1))
                                      (6 (iadd))
                                      (7 (putfield (fieldCP "index" "java.awt.geom.RectIterator" int)))
                                      (10 (return))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "currentSegment"
                              (parameters (array float))
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 2) (code_length . 129)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "isDone" "java.awt.geom.RectIterator" () boolean))) 
                                      (4 (ifeq 17)) ;;to TAG_0
                                      (7 (new (class "java.util.NoSuchElementException"))) 
                                      (10 (dup)) 
                                      (11 (ldc 0)) ;;STRING:: "rect iterator out of bounds"
                                      (13 (invokespecial (methodCP "<init>" "java.util.NoSuchElementException" ((class "java.lang.String")) void))) 
                                      (16 (athrow)) 
                                      (17 (aload_0)) ;;at TAG_0
                                      (18 (getfield (fieldCP "index" "java.awt.geom.RectIterator" int))) 
                                      (21 (iconst_5)) 
                                      (22 (if_icmpne 27))  ;;to TAG_1
                                      (25 (iconst_4)) 
                                      (26 (ireturn)) 
                                      (27 (aload_1)) ;;at TAG_1
                                      (28 (iconst_0)) 
                                      (29 (aload_0)) 
                                      (30 (getfield (fieldCP "x" "java.awt.geom.RectIterator" double))) 
                                      (33 (d2f)) 
                                      (34 (fastore)) 
                                      (35 (aload_1)) 
                                      (36 (iconst_1)) 
                                      (37 (aload_0)) 
                                      (38 (getfield (fieldCP "y" "java.awt.geom.RectIterator" double))) 
                                      (41 (d2f)) 
                                      (42 (fastore)) 
                                      (43 (aload_0)) 
                                      (44 (getfield (fieldCP "index" "java.awt.geom.RectIterator" int))) 
                                      (47 (iconst_1)) 
                                      (48 (if_icmpeq 59)) ;;to TAG_2
                                      (51 (aload_0)) 
                                      (52 (getfield (fieldCP "index" "java.awt.geom.RectIterator" int))) 
                                      (55 (iconst_2)) 
                                      (56 (if_icmpne 70)) ;;to TAG_3
                                      (59 (aload_1)) ;;at TAG_2
                                      (60 (iconst_0)) 
                                      (61 (dup2)) 
                                      (62 (faload)) 
                                      (63 (aload_0)) 
                                      (64 (getfield (fieldCP "w" "java.awt.geom.RectIterator" double))) 
                                      (67 (d2f)) 
                                      (68 (fadd)) 
                                      (69 (fastore)) 
                                      (70 (aload_0)) ;;at TAG_3
                                      (71 (getfield (fieldCP "index" "java.awt.geom.RectIterator" int))) 
                                      (74 (iconst_2)) 
                                      (75 (if_icmpeq 86)) ;;to TAG_4
                                      (78 (aload_0)) 
                                      (79 (getfield (fieldCP "index" "java.awt.geom.RectIterator" int))) 
                                      (82 (iconst_3)) 
                                      (83 (if_icmpne 97)) ;;to TAG_5
                                      (86 (aload_1)) ;;at TAG_4
                                      (87 (iconst_1)) 
                                      (88 (dup2)) 
                                      (89 (faload)) 
                                      (90 (aload_0)) 
                                      (91 (getfield (fieldCP "h" "java.awt.geom.RectIterator" double))) 
                                      (94 (d2f)) 
                                      (95 (fadd)) 
                                      (96 (fastore)) 
                                      (97 (aload_0)) ;;at TAG_5
                                      (98 (getfield (fieldCP "affine" "java.awt.geom.RectIterator" (class "java.awt.geom.AffineTransform")))) 
                                      (101 (ifnull 116)) ;;to TAG_6
                                      (104 (aload_0)) 
                                      (105 (getfield (fieldCP "affine" "java.awt.geom.RectIterator" (class "java.awt.geom.AffineTransform")))) 
                                      (108 (aload_1)) 
                                      (109 (iconst_0)) 
                                      (110 (aload_1)) 
                                      (111 (iconst_0)) 
                                      (112 (iconst_1)) 
                                      (113 (invokevirtual (methodCP "transform" "java.awt.geom.AffineTransform" ((array float) int (array float) int int) void))) 
                                      (116 (aload_0)) ;;at TAG_6
                                      (117 (getfield (fieldCP "index" "java.awt.geom.RectIterator" int))) 
                                      (120 (ifne 127)) ;;to TAG_7
                                      (123 (iconst_0)) 
                                      (124 (goto 128)) ;;to TAG_8
                                      (127 (iconst_1)) ;;at TAG_7
                                      (128 (ireturn)) ;;at TAG_8
                                      (endofcode 129))
                                   (Exceptions )
                                   (StackMap )))
                        (method "currentSegment"
                              (parameters (array double))
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 2) (code_length . 125)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "isDone" "java.awt.geom.RectIterator" () boolean))) 
                                      (4 (ifeq 17)) ;;to TAG_0
                                      (7 (new (class "java.util.NoSuchElementException"))) 
                                      (10 (dup)) 
                                      (11 (ldc 0)) ;;STRING:: "rect iterator out of bounds"
                                      (13 (invokespecial (methodCP "<init>" "java.util.NoSuchElementException" ((class "java.lang.String")) void))) 
                                      (16 (athrow)) 
                                      (17 (aload_0)) ;;at TAG_0
                                      (18 (getfield (fieldCP "index" "java.awt.geom.RectIterator" int))) 
                                      (21 (iconst_5)) 
                                      (22 (if_icmpne 27))  ;;to TAG_1
                                      (25 (iconst_4)) 
                                      (26 (ireturn)) 
                                      (27 (aload_1)) ;;at TAG_1
                                      (28 (iconst_0)) 
                                      (29 (aload_0)) 
                                      (30 (getfield (fieldCP "x" "java.awt.geom.RectIterator" double))) 
                                      (33 (dastore)) 
                                      (34 (aload_1)) 
                                      (35 (iconst_1)) 
                                      (36 (aload_0)) 
                                      (37 (getfield (fieldCP "y" "java.awt.geom.RectIterator" double))) 
                                      (40 (dastore)) 
                                      (41 (aload_0)) 
                                      (42 (getfield (fieldCP "index" "java.awt.geom.RectIterator" int))) 
                                      (45 (iconst_1)) 
                                      (46 (if_icmpeq 57)) ;;to TAG_2
                                      (49 (aload_0)) 
                                      (50 (getfield (fieldCP "index" "java.awt.geom.RectIterator" int))) 
                                      (53 (iconst_2)) 
                                      (54 (if_icmpne 67)) ;;to TAG_3
                                      (57 (aload_1)) ;;at TAG_2
                                      (58 (iconst_0)) 
                                      (59 (dup2)) 
                                      (60 (daload)) 
                                      (61 (aload_0)) 
                                      (62 (getfield (fieldCP "w" "java.awt.geom.RectIterator" double))) 
                                      (65 (dadd)) 
                                      (66 (dastore)) 
                                      (67 (aload_0)) ;;at TAG_3
                                      (68 (getfield (fieldCP "index" "java.awt.geom.RectIterator" int))) 
                                      (71 (iconst_2)) 
                                      (72 (if_icmpeq 83)) ;;to TAG_4
                                      (75 (aload_0)) 
                                      (76 (getfield (fieldCP "index" "java.awt.geom.RectIterator" int))) 
                                      (79 (iconst_3)) 
                                      (80 (if_icmpne 93)) ;;to TAG_5
                                      (83 (aload_1)) ;;at TAG_4
                                      (84 (iconst_1)) 
                                      (85 (dup2)) 
                                      (86 (daload)) 
                                      (87 (aload_0)) 
                                      (88 (getfield (fieldCP "h" "java.awt.geom.RectIterator" double))) 
                                      (91 (dadd)) 
                                      (92 (dastore)) 
                                      (93 (aload_0)) ;;at TAG_5
                                      (94 (getfield (fieldCP "affine" "java.awt.geom.RectIterator" (class "java.awt.geom.AffineTransform")))) 
                                      (97 (ifnull 112)) ;;to TAG_6
                                      (100 (aload_0)) 
                                      (101 (getfield (fieldCP "affine" "java.awt.geom.RectIterator" (class "java.awt.geom.AffineTransform")))) 
                                      (104 (aload_1)) 
                                      (105 (iconst_0)) 
                                      (106 (aload_1)) 
                                      (107 (iconst_0)) 
                                      (108 (iconst_1)) 
                                      (109 (invokevirtual (methodCP "transform" "java.awt.geom.AffineTransform" ((array double) int (array double) int int) void))) 
                                      (112 (aload_0)) ;;at TAG_6
                                      (113 (getfield (fieldCP "index" "java.awt.geom.RectIterator" int))) 
                                      (116 (ifne 123)) ;;to TAG_7
                                      (119 (iconst_0)) 
                                      (120 (goto 124)) ;;to TAG_8
                                      (123 (iconst_1)) ;;at TAG_7
                                      (124 (ireturn)) ;;at TAG_8
                                      (endofcode 125))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.awt.geom.PathIterator")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *RectIterator-class-table*
  (make-static-class-decls 
   *java.awt.geom.RectIterator*))

(defconst *package-name-map* 
  ("java.awt.geom.RectIterator" . "java.awt.geom"))

