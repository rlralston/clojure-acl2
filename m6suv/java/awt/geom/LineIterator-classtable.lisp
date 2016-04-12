; LineIterator-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:27 CDT 2014.
;

(defconst *java.awt.geom.LineIterator*
 (make-class-def
      '(class "java.awt.geom.LineIterator"
            "java.lang.Object"
            (constant_pool
                        (STRING  "line iterator out of bounds"))
            (fields
                        (field "line" (class "java.awt.geom.Line2D") (accessflags  *class* ) -1)
                        (field "affine" (class "java.awt.geom.AffineTransform") (accessflags  *class* ) -1)
                        (field "index" int (accessflags  *class* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.awt.geom.Line2D") (class "java.awt.geom.AffineTransform"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "line" "java.awt.geom.LineIterator" (class "java.awt.geom.Line2D"))))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "affine" "java.awt.geom.LineIterator" (class "java.awt.geom.AffineTransform"))))
                                      (14 (return))
                                      (endofcode 15))
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
                                      (1 (getfield (fieldCP "index" "java.awt.geom.LineIterator" int))) 
                                      (4 (iconst_1)) 
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
                                      (2 (getfield (fieldCP "index" "java.awt.geom.LineIterator" int)))
                                      (5 (iconst_1))
                                      (6 (iadd))
                                      (7 (putfield (fieldCP "index" "java.awt.geom.LineIterator" int)))
                                      (10 (return))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "currentSegment"
                              (parameters (array float))
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 3) (code_length . 96)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "isDone" "java.awt.geom.LineIterator" () boolean))) 
                                      (4 (ifeq 17)) ;;to TAG_0
                                      (7 (new (class "java.util.NoSuchElementException"))) 
                                      (10 (dup)) 
                                      (11 (ldc 0)) ;;STRING:: "line iterator out of bounds"
                                      (13 (invokespecial (methodCP "<init>" "java.util.NoSuchElementException" ((class "java.lang.String")) void))) 
                                      (16 (athrow)) 
                                      (17 (aload_0)) ;;at TAG_0
                                      (18 (getfield (fieldCP "index" "java.awt.geom.LineIterator" int))) 
                                      (21 (ifne 51)) ;;to TAG_1
                                      (24 (aload_1)) 
                                      (25 (iconst_0)) 
                                      (26 (aload_0)) 
                                      (27 (getfield (fieldCP "line" "java.awt.geom.LineIterator" (class "java.awt.geom.Line2D")))) 
                                      (30 (invokevirtual (methodCP "getX1" "java.awt.geom.Line2D" () double))) 
                                      (33 (d2f)) 
                                      (34 (fastore)) 
                                      (35 (aload_1)) 
                                      (36 (iconst_1)) 
                                      (37 (aload_0)) 
                                      (38 (getfield (fieldCP "line" "java.awt.geom.LineIterator" (class "java.awt.geom.Line2D")))) 
                                      (41 (invokevirtual (methodCP "getY1" "java.awt.geom.Line2D" () double))) 
                                      (44 (d2f)) 
                                      (45 (fastore)) 
                                      (46 (iconst_0)) 
                                      (47 (istore_2)) 
                                      (48 (goto 75))  ;;to TAG_2
                                      (51 (aload_1)) ;;at TAG_1
                                      (52 (iconst_0)) 
                                      (53 (aload_0)) 
                                      (54 (getfield (fieldCP "line" "java.awt.geom.LineIterator" (class "java.awt.geom.Line2D")))) 
                                      (57 (invokevirtual (methodCP "getX2" "java.awt.geom.Line2D" () double))) 
                                      (60 (d2f)) 
                                      (61 (fastore)) 
                                      (62 (aload_1)) 
                                      (63 (iconst_1)) 
                                      (64 (aload_0)) 
                                      (65 (getfield (fieldCP "line" "java.awt.geom.LineIterator" (class "java.awt.geom.Line2D")))) 
                                      (68 (invokevirtual (methodCP "getY2" "java.awt.geom.Line2D" () double))) 
                                      (71 (d2f)) 
                                      (72 (fastore)) 
                                      (73 (iconst_1)) 
                                      (74 (istore_2)) 
                                      (75 (aload_0)) ;;at TAG_2
                                      (76 (getfield (fieldCP "affine" "java.awt.geom.LineIterator" (class "java.awt.geom.AffineTransform")))) 
                                      (79 (ifnull 94)) ;;to TAG_3
                                      (82 (aload_0)) 
                                      (83 (getfield (fieldCP "affine" "java.awt.geom.LineIterator" (class "java.awt.geom.AffineTransform")))) 
                                      (86 (aload_1)) 
                                      (87 (iconst_0)) 
                                      (88 (aload_1)) 
                                      (89 (iconst_0)) 
                                      (90 (iconst_1)) 
                                      (91 (invokevirtual (methodCP "transform" "java.awt.geom.AffineTransform" ((array float) int (array float) int int) void))) 
                                      (94 (iload_2)) ;;at TAG_3
                                      (95 (ireturn)) 
                                      (endofcode 96))
                                   (Exceptions )
                                   (StackMap )))
                        (method "currentSegment"
                              (parameters (array double))
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 3) (code_length . 92)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "isDone" "java.awt.geom.LineIterator" () boolean))) 
                                      (4 (ifeq 17)) ;;to TAG_0
                                      (7 (new (class "java.util.NoSuchElementException"))) 
                                      (10 (dup)) 
                                      (11 (ldc 0)) ;;STRING:: "line iterator out of bounds"
                                      (13 (invokespecial (methodCP "<init>" "java.util.NoSuchElementException" ((class "java.lang.String")) void))) 
                                      (16 (athrow)) 
                                      (17 (aload_0)) ;;at TAG_0
                                      (18 (getfield (fieldCP "index" "java.awt.geom.LineIterator" int))) 
                                      (21 (ifne 49)) ;;to TAG_1
                                      (24 (aload_1)) 
                                      (25 (iconst_0)) 
                                      (26 (aload_0)) 
                                      (27 (getfield (fieldCP "line" "java.awt.geom.LineIterator" (class "java.awt.geom.Line2D")))) 
                                      (30 (invokevirtual (methodCP "getX1" "java.awt.geom.Line2D" () double))) 
                                      (33 (dastore)) 
                                      (34 (aload_1)) 
                                      (35 (iconst_1)) 
                                      (36 (aload_0)) 
                                      (37 (getfield (fieldCP "line" "java.awt.geom.LineIterator" (class "java.awt.geom.Line2D")))) 
                                      (40 (invokevirtual (methodCP "getY1" "java.awt.geom.Line2D" () double))) 
                                      (43 (dastore)) 
                                      (44 (iconst_0)) 
                                      (45 (istore_2)) 
                                      (46 (goto 71))  ;;to TAG_2
                                      (49 (aload_1)) ;;at TAG_1
                                      (50 (iconst_0)) 
                                      (51 (aload_0)) 
                                      (52 (getfield (fieldCP "line" "java.awt.geom.LineIterator" (class "java.awt.geom.Line2D")))) 
                                      (55 (invokevirtual (methodCP "getX2" "java.awt.geom.Line2D" () double))) 
                                      (58 (dastore)) 
                                      (59 (aload_1)) 
                                      (60 (iconst_1)) 
                                      (61 (aload_0)) 
                                      (62 (getfield (fieldCP "line" "java.awt.geom.LineIterator" (class "java.awt.geom.Line2D")))) 
                                      (65 (invokevirtual (methodCP "getY2" "java.awt.geom.Line2D" () double))) 
                                      (68 (dastore)) 
                                      (69 (iconst_1)) 
                                      (70 (istore_2)) 
                                      (71 (aload_0)) ;;at TAG_2
                                      (72 (getfield (fieldCP "affine" "java.awt.geom.LineIterator" (class "java.awt.geom.AffineTransform")))) 
                                      (75 (ifnull 90)) ;;to TAG_3
                                      (78 (aload_0)) 
                                      (79 (getfield (fieldCP "affine" "java.awt.geom.LineIterator" (class "java.awt.geom.AffineTransform")))) 
                                      (82 (aload_1)) 
                                      (83 (iconst_0)) 
                                      (84 (aload_1)) 
                                      (85 (iconst_0)) 
                                      (86 (iconst_1)) 
                                      (87 (invokevirtual (methodCP "transform" "java.awt.geom.AffineTransform" ((array double) int (array double) int int) void))) 
                                      (90 (iload_2)) ;;at TAG_3
                                      (91 (ireturn)) 
                                      (endofcode 92))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.awt.geom.PathIterator")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *LineIterator-class-table*
  (make-static-class-decls 
   *java.awt.geom.LineIterator*))

(defconst *package-name-map* 
  ("java.awt.geom.LineIterator" . "java.awt.geom"))

