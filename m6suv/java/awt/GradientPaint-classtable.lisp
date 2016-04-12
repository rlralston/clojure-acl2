; GradientPaint-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:27 CDT 2014.
;

(defconst *java.awt.GradientPaint*
 (make-class-def
      '(class "java.awt.GradientPaint"
            "java.lang.Object"
            (constant_pool
                        (STRING  "Colors cannot be null")
                        (STRING  "Colors and points should be non-null"))
            (fields
                        (field "p1" (class "java.awt.geom.Point2D$Float") (accessflags  *class* ) -1)
                        (field "p2" (class "java.awt.geom.Point2D$Float") (accessflags  *class* ) -1)
                        (field "color1" (class "java.awt.Color") (accessflags  *class* ) -1)
                        (field "color2" (class "java.awt.Color") (accessflags  *class* ) -1)
                        (field "cyclic" boolean (accessflags  *class* ) -1))
            (methods
                        (method "<init>"
                              (parameters float float (class "java.awt.Color") float float (class "java.awt.Color"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 7) (code_length . 63)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokespecial (methodCP "<init>" "java.lang.Object" () void))) 
                                      (4 (aload_3)) 
                                      (5 (ifnull 13))  ;;to TAG_0
                                      (8 (aload 6)) 
                                      (10 (ifnonnull 23)) ;;to TAG_1
                                      (13 (new (class "java.lang.NullPointerException"))) ;;at TAG_0
                                      (16 (dup)) 
                                      (17 (ldc 0)) ;;STRING:: "Colors cannot be null"
                                      (19 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" ((class "java.lang.String")) void))) 
                                      (22 (athrow)) 
                                      (23 (aload_0)) ;;at TAG_1
                                      (24 (new (class "java.awt.geom.Point2D$Float"))) 
                                      (27 (dup)) 
                                      (28 (fload_1)) 
                                      (29 (fload_2)) 
                                      (30 (invokespecial (methodCP "<init>" "java.awt.geom.Point2D$Float" (float float) void))) 
                                      (33 (putfield (fieldCP "p1" "java.awt.GradientPaint" (class "java.awt.geom.Point2D$Float")))) 
                                      (36 (aload_0)) 
                                      (37 (new (class "java.awt.geom.Point2D$Float"))) 
                                      (40 (dup)) 
                                      (41 (fload 4)) 
                                      (43 (fload 5)) 
                                      (45 (invokespecial (methodCP "<init>" "java.awt.geom.Point2D$Float" (float float) void))) 
                                      (48 (putfield (fieldCP "p2" "java.awt.GradientPaint" (class "java.awt.geom.Point2D$Float")))) 
                                      (51 (aload_0)) 
                                      (52 (aload_3)) 
                                      (53 (putfield (fieldCP "color1" "java.awt.GradientPaint" (class "java.awt.Color")))) 
                                      (56 (aload_0)) 
                                      (57 (aload 6)) 
                                      (59 (putfield (fieldCP "color2" "java.awt.GradientPaint" (class "java.awt.Color")))) 
                                      (62 (return)) 
                                      (endofcode 63))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.awt.geom.Point2D") (class "java.awt.Color") (class "java.awt.geom.Point2D") (class "java.awt.Color"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 5) (code_length . 85)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokespecial (methodCP "<init>" "java.lang.Object" () void))) 
                                      (4 (aload_2)) 
                                      (5 (ifnull 21))  ;;to TAG_0
                                      (8 (aload 4)) 
                                      (10 (ifnull 21))  ;;to TAG_0
                                      (13 (aload_1)) 
                                      (14 (ifnull 21))  ;;to TAG_0
                                      (17 (aload_3)) 
                                      (18 (ifnonnull 31)) ;;to TAG_1
                                      (21 (new (class "java.lang.NullPointerException"))) ;;at TAG_0
                                      (24 (dup)) 
                                      (25 (ldc 1)) ;;STRING:: "Colors and points should be non-null"
                                      (27 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" ((class "java.lang.String")) void))) 
                                      (30 (athrow)) 
                                      (31 (aload_0)) ;;at TAG_1
                                      (32 (new (class "java.awt.geom.Point2D$Float"))) 
                                      (35 (dup)) 
                                      (36 (aload_1)) 
                                      (37 (invokevirtual (methodCP "getX" "java.awt.geom.Point2D" () double))) 
                                      (40 (d2f)) 
                                      (41 (aload_1)) 
                                      (42 (invokevirtual (methodCP "getY" "java.awt.geom.Point2D" () double))) 
                                      (45 (d2f)) 
                                      (46 (invokespecial (methodCP "<init>" "java.awt.geom.Point2D$Float" (float float) void))) 
                                      (49 (putfield (fieldCP "p1" "java.awt.GradientPaint" (class "java.awt.geom.Point2D$Float")))) 
                                      (52 (aload_0)) 
                                      (53 (new (class "java.awt.geom.Point2D$Float"))) 
                                      (56 (dup)) 
                                      (57 (aload_3)) 
                                      (58 (invokevirtual (methodCP "getX" "java.awt.geom.Point2D" () double))) 
                                      (61 (d2f)) 
                                      (62 (aload_3)) 
                                      (63 (invokevirtual (methodCP "getY" "java.awt.geom.Point2D" () double))) 
                                      (66 (d2f)) 
                                      (67 (invokespecial (methodCP "<init>" "java.awt.geom.Point2D$Float" (float float) void))) 
                                      (70 (putfield (fieldCP "p2" "java.awt.GradientPaint" (class "java.awt.geom.Point2D$Float")))) 
                                      (73 (aload_0)) 
                                      (74 (aload_2)) 
                                      (75 (putfield (fieldCP "color1" "java.awt.GradientPaint" (class "java.awt.Color")))) 
                                      (78 (aload_0)) 
                                      (79 (aload 4)) 
                                      (81 (putfield (fieldCP "color2" "java.awt.GradientPaint" (class "java.awt.Color")))) 
                                      (84 (return)) 
                                      (endofcode 85))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters float float (class "java.awt.Color") float float (class "java.awt.Color") boolean)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 7) (max_locals . 8) (code_length . 20)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (fload_1))
                                      (2 (fload_2))
                                      (3 (aload_3))
                                      (4 (fload 4))
                                      (6 (fload 5))
                                      (8 (aload 6))
                                      (10 (invokespecial
					(methodCP "<init>" "java.awt.GradientPaint" (float float (class "java.awt.Color") float float (class "java.awt.Color")) void)))
                                      (13 (aload_0))
                                      (14 (iload 7))
                                      (16 (putfield (fieldCP "cyclic" "java.awt.GradientPaint" boolean)))
                                      (19 (return))
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.awt.geom.Point2D") (class "java.awt.Color") (class "java.awt.geom.Point2D") (class "java.awt.Color") boolean)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 6) (code_length . 16)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (aload_2))
                                      (3 (aload_3))
                                      (4 (aload 4))
                                      (6 (invokespecial
					(methodCP "<init>" "java.awt.GradientPaint" ((class "java.awt.geom.Point2D") (class "java.awt.Color") (class "java.awt.geom.Point2D") (class "java.awt.Color")) void)))
                                      (9 (aload_0))
                                      (10 (iload 5))
                                      (12 (putfield (fieldCP "cyclic" "java.awt.GradientPaint" boolean)))
                                      (15 (return))
                                      (endofcode 16))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getPoint1"
                              (parameters )
                              (returntype . (class "java.awt.geom.Point2D"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 1) (code_length . 22)
                                   (parsedcode
                                      (0 (new (class "java.awt.geom.Point2D$Float")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "p1" "java.awt.GradientPaint" (class "java.awt.geom.Point2D$Float"))))
                                      (8 (getfield (fieldCP "x" "java.awt.geom.Point2D$Float" float)))
                                      (11 (aload_0))
                                      (12 (getfield (fieldCP "p1" "java.awt.GradientPaint" (class "java.awt.geom.Point2D$Float"))))
                                      (15 (getfield (fieldCP "y" "java.awt.geom.Point2D$Float" float)))
                                      (18 (invokespecial
					(methodCP "<init>" "java.awt.geom.Point2D$Float" (float float) void)))
                                      (21 (areturn))
                                      (endofcode 22))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getColor1"
                              (parameters )
                              (returntype . (class "java.awt.Color"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "color1" "java.awt.GradientPaint" (class "java.awt.Color"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getPoint2"
                              (parameters )
                              (returntype . (class "java.awt.geom.Point2D"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 1) (code_length . 22)
                                   (parsedcode
                                      (0 (new (class "java.awt.geom.Point2D$Float")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "p2" "java.awt.GradientPaint" (class "java.awt.geom.Point2D$Float"))))
                                      (8 (getfield (fieldCP "x" "java.awt.geom.Point2D$Float" float)))
                                      (11 (aload_0))
                                      (12 (getfield (fieldCP "p2" "java.awt.GradientPaint" (class "java.awt.geom.Point2D$Float"))))
                                      (15 (getfield (fieldCP "y" "java.awt.geom.Point2D$Float" float)))
                                      (18 (invokespecial
					(methodCP "<init>" "java.awt.geom.Point2D$Float" (float float) void)))
                                      (21 (areturn))
                                      (endofcode 22))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getColor2"
                              (parameters )
                              (returntype . (class "java.awt.Color"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "color2" "java.awt.GradientPaint" (class "java.awt.Color"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isCyclic"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "cyclic" "java.awt.GradientPaint" boolean)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "createContext"
                              (parameters (class "java.awt.image.ColorModel") (class "java.awt.Rectangle") (class "java.awt.geom.Rectangle2D") (class "java.awt.geom.AffineTransform") (class "java.awt.RenderingHints"))
                              (returntype . (class "java.awt.PaintContext"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 9) (max_locals . 6) (code_length . 31)
                                   (parsedcode
                                      (0 (new (class "java.awt.GradientPaintContext")))
                                      (3 (dup))
                                      (4 (aload_1))
                                      (5 (aload_0))
                                      (6 (getfield (fieldCP "p1" "java.awt.GradientPaint" (class "java.awt.geom.Point2D$Float"))))
                                      (9 (aload_0))
                                      (10 (getfield (fieldCP "p2" "java.awt.GradientPaint" (class "java.awt.geom.Point2D$Float"))))
                                      (13 (aload 4))
                                      (15 (aload_0))
                                      (16 (getfield (fieldCP "color1" "java.awt.GradientPaint" (class "java.awt.Color"))))
                                      (19 (aload_0))
                                      (20 (getfield (fieldCP "color2" "java.awt.GradientPaint" (class "java.awt.Color"))))
                                      (23 (aload_0))
                                      (24 (getfield (fieldCP "cyclic" "java.awt.GradientPaint" boolean)))
                                      (27 (invokespecial
					(methodCP "<init>" "java.awt.GradientPaintContext" ((class "java.awt.image.ColorModel") (class "java.awt.geom.Point2D") (class "java.awt.geom.Point2D") (class "java.awt.geom.AffineTransform") (class "java.awt.Color") (class "java.awt.Color") boolean) void)))
                                      (30 (areturn))
                                      (endofcode 31))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getTransparency"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 31)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "color1" "java.awt.GradientPaint" (class "java.awt.Color")))) 
                                      (4 (invokevirtual (methodCP "getAlpha" "java.awt.Color" () int))) 
                                      (7 (istore_1)) 
                                      (8 (aload_0)) 
                                      (9 (getfield (fieldCP "color2" "java.awt.GradientPaint" (class "java.awt.Color")))) 
                                      (12 (invokevirtual (methodCP "getAlpha" "java.awt.Color" () int))) 
                                      (15 (istore_2)) 
                                      (16 (iload_1)) 
                                      (17 (iload_2)) 
                                      (18 (iand)) 
                                      (19 (sipush 255)) 
                                      (22 (if_icmpne 29))  ;;to TAG_0
                                      (25 (iconst_1)) 
                                      (26 (goto 30)) ;;to TAG_1
                                      (29 (iconst_3)) ;;at TAG_0
                                      (30 (ireturn)) ;;at TAG_1
                                      (endofcode 31))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.awt.Paint")
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *GradientPaint-class-table*
  (make-static-class-decls 
   *java.awt.GradientPaint*))

(defconst *package-name-map* 
  ("java.awt.GradientPaint" . "java.awt"))
