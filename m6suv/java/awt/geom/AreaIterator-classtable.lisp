; AreaIterator-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:27 CDT 2014.
;

(defconst *java.awt.geom.AreaIterator*
 (make-class-def
      '(class "java.awt.geom.AreaIterator"
            "java.lang.Object"
            (constant_pool
                        (STRING  "area iterator out of bounds"))
            (fields
                        (field "transform" (class "java.awt.geom.AffineTransform") (accessflags  *class*  *private* ) -1)
                        (field "curves" (class "java.util.Vector") (accessflags  *class*  *private* ) -1)
                        (field "index" int (accessflags  *class*  *private* ) -1)
                        (field "prevcurve" (class "sun.awt.geom.Curve") (accessflags  *class*  *private* ) -1)
                        (field "thiscurve" (class "sun.awt.geom.Curve") (accessflags  *class*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.Vector") (class "java.awt.geom.AffineTransform"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 35)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokespecial (methodCP "<init>" "java.lang.Object" () void))) 
                                      (4 (aload_0)) 
                                      (5 (aload_1)) 
                                      (6 (putfield (fieldCP "curves" "java.awt.geom.AreaIterator" (class "java.util.Vector")))) 
                                      (9 (aload_0)) 
                                      (10 (aload_2)) 
                                      (11 (putfield (fieldCP "transform" "java.awt.geom.AreaIterator" (class "java.awt.geom.AffineTransform")))) 
                                      (14 (aload_1)) 
                                      (15 (invokevirtual (methodCP "size" "java.util.Vector" () int))) 
                                      (18 (iconst_1)) 
                                      (19 (if_icmplt 34))  ;;to TAG_0
                                      (22 (aload_0)) 
                                      (23 (aload_1)) 
                                      (24 (iconst_0)) 
                                      (25 (invokevirtual (methodCP "get" "java.util.Vector" (int) (class "java.lang.Object")))) 
                                      (28 (checkcast (class "sun.awt.geom.Curve"))) 
                                      (31 (putfield (fieldCP "thiscurve" "java.awt.geom.AreaIterator" (class "sun.awt.geom.Curve")))) 
                                      (34 (return)) ;;at TAG_0
                                      (endofcode 35))
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
                                   (max_stack . 1) (max_locals . 1) (code_length . 20)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "prevcurve" "java.awt.geom.AreaIterator" (class "sun.awt.geom.Curve")))) 
                                      (4 (ifnonnull 18))  ;;to TAG_0
                                      (7 (aload_0)) 
                                      (8 (getfield (fieldCP "thiscurve" "java.awt.geom.AreaIterator" (class "sun.awt.geom.Curve")))) 
                                      (11 (ifnonnull 18))  ;;to TAG_0
                                      (14 (iconst_1)) 
                                      (15 (goto 19)) ;;to TAG_1
                                      (18 (iconst_0)) ;;at TAG_0
                                      (19 (ireturn)) ;;at TAG_1
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap )))
                        (method "next"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 1) (code_length . 125)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "prevcurve" "java.awt.geom.AreaIterator" (class "sun.awt.geom.Curve")))) 
                                      (4 (ifnull 15)) ;;to TAG_0
                                      (7 (aload_0)) 
                                      (8 (aconst_null)) 
                                      (9 (putfield (fieldCP "prevcurve" "java.awt.geom.AreaIterator" (class "sun.awt.geom.Curve")))) 
                                      (12 (goto 124)) ;;to TAG_1
                                      (15 (aload_0)) ;;at TAG_0
                                      (16 (aload_0)) 
                                      (17 (getfield (fieldCP "thiscurve" "java.awt.geom.AreaIterator" (class "sun.awt.geom.Curve")))) 
                                      (20 (putfield (fieldCP "prevcurve" "java.awt.geom.AreaIterator" (class "sun.awt.geom.Curve")))) 
                                      (23 (aload_0)) 
                                      (24 (dup)) 
                                      (25 (getfield (fieldCP "index" "java.awt.geom.AreaIterator" int))) 
                                      (28 (iconst_1)) 
                                      (29 (iadd)) 
                                      (30 (putfield (fieldCP "index" "java.awt.geom.AreaIterator" int))) 
                                      (33 (aload_0)) 
                                      (34 (getfield (fieldCP "index" "java.awt.geom.AreaIterator" int))) 
                                      (37 (aload_0)) 
                                      (38 (getfield (fieldCP "curves" "java.awt.geom.AreaIterator" (class "java.util.Vector")))) 
                                      (41 (invokevirtual (methodCP "size" "java.util.Vector" () int))) 
                                      (44 (if_icmpge 119))  ;;to TAG_2
                                      (47 (aload_0)) 
                                      (48 (aload_0)) 
                                      (49 (getfield (fieldCP "curves" "java.awt.geom.AreaIterator" (class "java.util.Vector")))) 
                                      (52 (aload_0)) 
                                      (53 (getfield (fieldCP "index" "java.awt.geom.AreaIterator" int))) 
                                      (56 (invokevirtual (methodCP "get" "java.util.Vector" (int) (class "java.lang.Object")))) 
                                      (59 (checkcast (class "sun.awt.geom.Curve"))) 
                                      (62 (putfield (fieldCP "thiscurve" "java.awt.geom.AreaIterator" (class "sun.awt.geom.Curve")))) 
                                      (65 (aload_0)) 
                                      (66 (getfield (fieldCP "thiscurve" "java.awt.geom.AreaIterator" (class "sun.awt.geom.Curve")))) 
                                      (69 (invokevirtual (methodCP "getOrder" "sun.awt.geom.Curve" () int))) 
                                      (72 (ifeq 124)) ;;to TAG_1
                                      (75 (aload_0)) 
                                      (76 (getfield (fieldCP "prevcurve" "java.awt.geom.AreaIterator" (class "sun.awt.geom.Curve")))) 
                                      (79 (invokevirtual (methodCP "getX1" "sun.awt.geom.Curve" () double))) 
                                      (82 (aload_0)) 
                                      (83 (getfield (fieldCP "thiscurve" "java.awt.geom.AreaIterator" (class "sun.awt.geom.Curve")))) 
                                      (86 (invokevirtual (methodCP "getX0" "sun.awt.geom.Curve" () double))) 
                                      (89 (dcmpl)) 
                                      (90 (ifne 124)) ;;to TAG_1
                                      (93 (aload_0)) 
                                      (94 (getfield (fieldCP "prevcurve" "java.awt.geom.AreaIterator" (class "sun.awt.geom.Curve")))) 
                                      (97 (invokevirtual (methodCP "getY1" "sun.awt.geom.Curve" () double))) 
                                      (100 (aload_0)) 
                                      (101 (getfield (fieldCP "thiscurve" "java.awt.geom.AreaIterator" (class "sun.awt.geom.Curve")))) 
                                      (104 (invokevirtual (methodCP "getY0" "sun.awt.geom.Curve" () double))) 
                                      (107 (dcmpl)) 
                                      (108 (ifne 124)) ;;to TAG_1
                                      (111 (aload_0)) 
                                      (112 (aconst_null)) 
                                      (113 (putfield (fieldCP "prevcurve" "java.awt.geom.AreaIterator" (class "sun.awt.geom.Curve")))) 
                                      (116 (goto 124)) ;;to TAG_1
                                      (119 (aload_0)) ;;at TAG_2
                                      (120 (aconst_null)) 
                                      (121 (putfield (fieldCP "thiscurve" "java.awt.geom.AreaIterator" (class "sun.awt.geom.Curve")))) 
                                      (124 (return)) ;;at TAG_1
                                      (endofcode 125))
                                   (Exceptions )
                                   (StackMap )))
                        (method "currentSegment"
                              (parameters (array float))
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 6) (code_length . 70)
                                   (parsedcode
                                      (0 (bipush 6)) 
                                      (2 (newarray DOUBLE)) 
                                      (4 (astore_2)) 
                                      (5 (aload_0)) 
                                      (6 (aload_2)) 
                                      (7 (invokevirtual (methodCP "currentSegment" "java.awt.geom.AreaIterator" ((array double)) int))) 
                                      (10 (istore_3)) 
                                      (11 (iload_3)) 
                                      (12 (iconst_4)) 
                                      (13 (if_icmpne 20)) ;;to TAG_0
                                      (16 (iconst_0)) 
                                      (17 (goto 39)) ;;to TAG_1
                                      (20 (iload_3)) ;;at TAG_0
                                      (21 (iconst_2)) 
                                      (22 (if_icmpne 29))  ;;to TAG_2
                                      (25 (iconst_2)) 
                                      (26 (goto 39)) ;;to TAG_1
                                      (29 (iload_3)) ;;at TAG_2
                                      (30 (iconst_3)) 
                                      (31 (if_icmpne 38)) ;;to TAG_3
                                      (34 (iconst_3)) 
                                      (35 (goto 39)) ;;to TAG_1
                                      (38 (iconst_1)) ;;at TAG_3
                                      (39 (istore 4)) ;;at TAG_1
                                      (41 (iconst_0)) 
                                      (42 (istore 5)) 
                                      (44 (iload 5)) ;;at TAG_5
                                      (46 (iload 4)) 
                                      (48 (iconst_2)) 
                                      (49 (imul)) 
                                      (50 (if_icmpge 68)) ;;to TAG_4
                                      (53 (aload_1)) 
                                      (54 (iload 5)) 
                                      (56 (aload_2)) 
                                      (57 (iload 5)) 
                                      (59 (daload)) 
                                      (60 (d2f)) 
                                      (61 (fastore)) 
                                      (62 (iinc 5 1)) 
                                      (65 (goto 44)) ;;to TAG_5
                                      (68 (iload_3)) ;;at TAG_4
                                      (69 (ireturn)) 
                                      (endofcode 70))
                                   (Exceptions )
                                   (StackMap )))
                        (method "currentSegment"
                              (parameters (array double))
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 4) (code_length . 114)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "prevcurve" "java.awt.geom.AreaIterator" (class "sun.awt.geom.Curve")))) 
                                      (4 (ifnull 53)) ;;to TAG_0
                                      (7 (aload_0)) 
                                      (8 (getfield (fieldCP "thiscurve" "java.awt.geom.AreaIterator" (class "sun.awt.geom.Curve")))) 
                                      (11 (ifnull 24)) ;;to TAG_1
                                      (14 (aload_0)) 
                                      (15 (getfield (fieldCP "thiscurve" "java.awt.geom.AreaIterator" (class "sun.awt.geom.Curve")))) 
                                      (18 (invokevirtual (methodCP "getOrder" "sun.awt.geom.Curve" () int))) 
                                      (21 (ifne 26))  ;;to TAG_2
                                      (24 (iconst_4)) ;;at TAG_1
                                      (25 (ireturn)) 
                                      (26 (aload_1)) ;;at TAG_2
                                      (27 (iconst_0)) 
                                      (28 (aload_0)) 
                                      (29 (getfield (fieldCP "thiscurve" "java.awt.geom.AreaIterator" (class "sun.awt.geom.Curve")))) 
                                      (32 (invokevirtual (methodCP "getX0" "sun.awt.geom.Curve" () double))) 
                                      (35 (dastore)) 
                                      (36 (aload_1)) 
                                      (37 (iconst_1)) 
                                      (38 (aload_0)) 
                                      (39 (getfield (fieldCP "thiscurve" "java.awt.geom.AreaIterator" (class "sun.awt.geom.Curve")))) 
                                      (42 (invokevirtual (methodCP "getY0" "sun.awt.geom.Curve" () double))) 
                                      (45 (dastore)) 
                                      (46 (iconst_1)) 
                                      (47 (istore_2)) 
                                      (48 (iconst_1)) 
                                      (49 (istore_3)) 
                                      (50 (goto 93)) ;;to TAG_3
                                      (53 (aload_0)) ;;at TAG_0
                                      (54 (getfield (fieldCP "thiscurve" "java.awt.geom.AreaIterator" (class "sun.awt.geom.Curve")))) 
                                      (57 (ifnonnull 70)) ;;to TAG_4
                                      (60 (new (class "java.util.NoSuchElementException"))) 
                                      (63 (dup)) 
                                      (64 (ldc 0)) ;;STRING:: "area iterator out of bounds"
                                      (66 (invokespecial (methodCP "<init>" "java.util.NoSuchElementException" ((class "java.lang.String")) void))) 
                                      (69 (athrow)) 
                                      (70 (aload_0)) ;;at TAG_4
                                      (71 (getfield (fieldCP "thiscurve" "java.awt.geom.AreaIterator" (class "sun.awt.geom.Curve")))) 
                                      (74 (aload_1)) 
                                      (75 (invokevirtual (methodCP "getSegment" "sun.awt.geom.Curve" ((array double)) int))) 
                                      (78 (istore_2)) 
                                      (79 (aload_0)) 
                                      (80 (getfield (fieldCP "thiscurve" "java.awt.geom.AreaIterator" (class "sun.awt.geom.Curve")))) 
                                      (83 (invokevirtual (methodCP "getOrder" "sun.awt.geom.Curve" () int))) 
                                      (86 (istore_3)) 
                                      (87 (iload_3)) 
                                      (88 (ifne 93)) ;;to TAG_3
                                      (91 (iconst_1)) 
                                      (92 (istore_3)) 
                                      (93 (aload_0)) ;;at TAG_3
                                      (94 (getfield (fieldCP "transform" "java.awt.geom.AreaIterator" (class "java.awt.geom.AffineTransform")))) 
                                      (97 (ifnull 112)) ;;to TAG_5
                                      (100 (aload_0)) 
                                      (101 (getfield (fieldCP "transform" "java.awt.geom.AreaIterator" (class "java.awt.geom.AffineTransform")))) 
                                      (104 (aload_1)) 
                                      (105 (iconst_0)) 
                                      (106 (aload_1)) 
                                      (107 (iconst_0)) 
                                      (108 (iload_3)) 
                                      (109 (invokevirtual (methodCP "transform" "java.awt.geom.AffineTransform" ((array double) int (array double) int int) void))) 
                                      (112 (iload_2)) ;;at TAG_5
                                      (113 (ireturn)) 
                                      (endofcode 114))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.awt.geom.PathIterator")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *AreaIterator-class-table*
  (make-static-class-decls 
   *java.awt.geom.AreaIterator*))

(defconst *package-name-map* 
  ("java.awt.geom.AreaIterator" . "java.awt.geom"))
