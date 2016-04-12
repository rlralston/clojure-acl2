; Graphics2D-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:27 CDT 2014.
;

(defconst *java.awt.Graphics2D*
 (make-class-def
      '(class "java.awt.Graphics2D"
            "java.awt.Graphics"
            (constant_pool)
            (fields)
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.awt.Graphics" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "draw3DRect"
                              (parameters int int int int boolean)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 10) (code_length . 112)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "getPaint" "java.awt.Graphics2D" () (class "java.awt.Paint")))) 
                                      (4 (astore 6)) 
                                      (6 (aload_0)) 
                                      (7 (invokevirtual (methodCP "getColor" "java.awt.Graphics2D" () (class "java.awt.Color")))) 
                                      (10 (astore 7)) 
                                      (12 (aload 7)) 
                                      (14 (invokevirtual (methodCP "brighter" "java.awt.Color" () (class "java.awt.Color")))) 
                                      (17 (astore 8)) 
                                      (19 (aload 7)) 
                                      (21 (invokevirtual (methodCP "darker" "java.awt.Color" () (class "java.awt.Color")))) 
                                      (24 (astore 9)) 
                                      (26 (aload_0)) 
                                      (27 (iload 5)) 
                                      (29 (ifeq 37)) ;;to TAG_0
                                      (32 (aload 8)) 
                                      (34 (goto 39)) ;;to TAG_1
                                      (37 (aload 9)) ;;at TAG_0
                                      (39 (invokevirtual (methodCP "setColor" "java.awt.Graphics2D" ((class "java.awt.Color")) void))) ;;at TAG_1
                                      (42 (aload_0)) 
                                      (43 (iload_1)) 
                                      (44 (iload_2)) 
                                      (45 (iconst_1)) 
                                      (46 (iload 4)) 
                                      (48 (iconst_1)) 
                                      (49 (iadd)) 
                                      (50 (invokevirtual (methodCP "fillRect" "java.awt.Graphics2D" (int int int int) void))) 
                                      (53 (aload_0)) 
                                      (54 (iload_1)) 
                                      (55 (iconst_1)) 
                                      (56 (iadd)) 
                                      (57 (iload_2)) 
                                      (58 (iload_3)) 
                                      (59 (iconst_1)) 
                                      (60 (isub)) 
                                      (61 (iconst_1)) 
                                      (62 (invokevirtual (methodCP "fillRect" "java.awt.Graphics2D" (int int int int) void))) 
                                      (65 (aload_0)) 
                                      (66 (iload 5)) 
                                      (68 (ifeq 76))  ;;to TAG_2
                                      (71 (aload 9)) 
                                      (73 (goto 78)) ;;to TAG_3
                                      (76 (aload 8)) ;;at TAG_2
                                      (78 (invokevirtual (methodCP "setColor" "java.awt.Graphics2D" ((class "java.awt.Color")) void))) ;;at TAG_3
                                      (81 (aload_0)) 
                                      (82 (iload_1)) 
                                      (83 (iconst_1)) 
                                      (84 (iadd)) 
                                      (85 (iload_2)) 
                                      (86 (iload 4)) 
                                      (88 (iadd)) 
                                      (89 (iload_3)) 
                                      (90 (iconst_1)) 
                                      (91 (invokevirtual (methodCP "fillRect" "java.awt.Graphics2D" (int int int int) void))) 
                                      (94 (aload_0)) 
                                      (95 (iload_1)) 
                                      (96 (iload_3)) 
                                      (97 (iadd)) 
                                      (98 (iload_2)) 
                                      (99 (iconst_1)) 
                                      (100 (iload 4)) 
                                      (102 (invokevirtual (methodCP "fillRect" "java.awt.Graphics2D" (int int int int) void))) 
                                      (105 (aload_0)) 
                                      (106 (aload 6)) 
                                      (108 (invokevirtual (methodCP "setPaint" "java.awt.Graphics2D" ((class "java.awt.Paint")) void))) 
                                      (111 (return)) 
                                      (endofcode 112))
                                   (Exceptions )
                                   (StackMap )))
                        (method "fill3DRect"
                              (parameters int int int int boolean)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 10) (code_length . 162)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "getPaint" "java.awt.Graphics2D" () (class "java.awt.Paint")))) 
                                      (4 (astore 6)) 
                                      (6 (aload_0)) 
                                      (7 (invokevirtual (methodCP "getColor" "java.awt.Graphics2D" () (class "java.awt.Color")))) 
                                      (10 (astore 7)) 
                                      (12 (aload 7)) 
                                      (14 (invokevirtual (methodCP "brighter" "java.awt.Color" () (class "java.awt.Color")))) 
                                      (17 (astore 8)) 
                                      (19 (aload 7)) 
                                      (21 (invokevirtual (methodCP "darker" "java.awt.Color" () (class "java.awt.Color")))) 
                                      (24 (astore 9)) 
                                      (26 (iload 5)) 
                                      (28 (ifne 40)) ;;to TAG_0
                                      (31 (aload_0)) 
                                      (32 (aload 9)) 
                                      (34 (invokevirtual (methodCP "setColor" "java.awt.Graphics2D" ((class "java.awt.Color")) void))) 
                                      (37 (goto 53)) ;;to TAG_1
                                      (40 (aload 6)) ;;at TAG_0
                                      (42 (aload 7)) 
                                      (44 (if_acmpeq 53)) ;;to TAG_1
                                      (47 (aload_0)) 
                                      (48 (aload 7)) 
                                      (50 (invokevirtual (methodCP "setColor" "java.awt.Graphics2D" ((class "java.awt.Color")) void))) 
                                      (53 (aload_0)) ;;at TAG_1
                                      (54 (iload_1)) 
                                      (55 (iconst_1)) 
                                      (56 (iadd)) 
                                      (57 (iload_2)) 
                                      (58 (iconst_1)) 
                                      (59 (iadd)) 
                                      (60 (iload_3)) 
                                      (61 (iconst_2)) 
                                      (62 (isub)) 
                                      (63 (iload 4)) 
                                      (65 (iconst_2)) 
                                      (66 (isub)) 
                                      (67 (invokevirtual (methodCP "fillRect" "java.awt.Graphics2D" (int int int int) void))) 
                                      (70 (aload_0)) 
                                      (71 (iload 5)) 
                                      (73 (ifeq 81))  ;;to TAG_2
                                      (76 (aload 8)) 
                                      (78 (goto 83)) ;;to TAG_3
                                      (81 (aload 9)) ;;at TAG_2
                                      (83 (invokevirtual (methodCP "setColor" "java.awt.Graphics2D" ((class "java.awt.Color")) void))) ;;at TAG_3
                                      (86 (aload_0)) 
                                      (87 (iload_1)) 
                                      (88 (iload_2)) 
                                      (89 (iconst_1)) 
                                      (90 (iload 4)) 
                                      (92 (invokevirtual (methodCP "fillRect" "java.awt.Graphics2D" (int int int int) void))) 
                                      (95 (aload_0)) 
                                      (96 (iload_1)) 
                                      (97 (iconst_1)) 
                                      (98 (iadd)) 
                                      (99 (iload_2)) 
                                      (100 (iload_3)) 
                                      (101 (iconst_2)) 
                                      (102 (isub)) 
                                      (103 (iconst_1)) 
                                      (104 (invokevirtual (methodCP "fillRect" "java.awt.Graphics2D" (int int int int) void))) 
                                      (107 (aload_0)) 
                                      (108 (iload 5)) 
                                      (110 (ifeq 118)) ;;to TAG_4
                                      (113 (aload 9)) 
                                      (115 (goto 120)) ;;to TAG_5
                                      (118 (aload 8)) ;;at TAG_4
                                      (120 (invokevirtual (methodCP "setColor" "java.awt.Graphics2D" ((class "java.awt.Color")) void))) ;;at TAG_5
                                      (123 (aload_0)) 
                                      (124 (iload_1)) 
                                      (125 (iconst_1)) 
                                      (126 (iadd)) 
                                      (127 (iload_2)) 
                                      (128 (iload 4)) 
                                      (130 (iadd)) 
                                      (131 (iconst_1)) 
                                      (132 (isub)) 
                                      (133 (iload_3)) 
                                      (134 (iconst_1)) 
                                      (135 (isub)) 
                                      (136 (iconst_1)) 
                                      (137 (invokevirtual (methodCP "fillRect" "java.awt.Graphics2D" (int int int int) void))) 
                                      (140 (aload_0)) 
                                      (141 (iload_1)) 
                                      (142 (iload_3)) 
                                      (143 (iadd)) 
                                      (144 (iconst_1)) 
                                      (145 (isub)) 
                                      (146 (iload_2)) 
                                      (147 (iconst_1)) 
                                      (148 (iload 4)) 
                                      (150 (iconst_1)) 
                                      (151 (isub)) 
                                      (152 (invokevirtual (methodCP "fillRect" "java.awt.Graphics2D" (int int int int) void))) 
                                      (155 (aload_0)) 
                                      (156 (aload 6)) 
                                      (158 (invokevirtual (methodCP "setPaint" "java.awt.Graphics2D" ((class "java.awt.Paint")) void))) 
                                      (161 (return)) 
                                      (endofcode 162))
                                   (Exceptions )
                                   (StackMap )))
                        (method "draw"
                              (parameters (class "java.awt.Shape"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "drawImage"
                              (parameters (class "java.awt.Image") (class "java.awt.geom.AffineTransform") (class "java.awt.image.ImageObserver"))
                              (returntype . boolean)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "drawImage"
                              (parameters (class "java.awt.image.BufferedImage") (class "java.awt.image.BufferedImageOp") int int)
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "drawRenderedImage"
                              (parameters (class "java.awt.image.RenderedImage") (class "java.awt.geom.AffineTransform"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "drawRenderableImage"
                              (parameters (class "java.awt.image.renderable.RenderableImage") (class "java.awt.geom.AffineTransform"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "drawString"
                              (parameters (class "java.lang.String") int int)
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "drawString"
                              (parameters (class "java.lang.String") float float)
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "drawString"
                              (parameters (class "java.text.AttributedCharacterIterator") int int)
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "drawString"
                              (parameters (class "java.text.AttributedCharacterIterator") float float)
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "drawGlyphVector"
                              (parameters (class "java.awt.font.GlyphVector") float float)
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "fill"
                              (parameters (class "java.awt.Shape"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "hit"
                              (parameters (class "java.awt.Rectangle") (class "java.awt.Shape") boolean)
                              (returntype . boolean)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getDeviceConfiguration"
                              (parameters )
                              (returntype . (class "java.awt.GraphicsConfiguration"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "setComposite"
                              (parameters (class "java.awt.Composite"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "setPaint"
                              (parameters (class "java.awt.Paint"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "setStroke"
                              (parameters (class "java.awt.Stroke"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "setRenderingHint"
                              (parameters (class "java.awt.RenderingHints$Key") (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getRenderingHint"
                              (parameters (class "java.awt.RenderingHints$Key"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "setRenderingHints"
                              (parameters (class "java.util.Map"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "addRenderingHints"
                              (parameters (class "java.util.Map"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getRenderingHints"
                              (parameters )
                              (returntype . (class "java.awt.RenderingHints"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "translate"
                              (parameters int int)
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "translate"
                              (parameters double double)
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "rotate"
                              (parameters double)
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "rotate"
                              (parameters double double double)
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "scale"
                              (parameters double double)
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "shear"
                              (parameters double double)
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "transform"
                              (parameters (class "java.awt.geom.AffineTransform"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "setTransform"
                              (parameters (class "java.awt.geom.AffineTransform"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getTransform"
                              (parameters )
                              (returntype . (class "java.awt.geom.AffineTransform"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getPaint"
                              (parameters )
                              (returntype . (class "java.awt.Paint"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getComposite"
                              (parameters )
                              (returntype . (class "java.awt.Composite"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "setBackground"
                              (parameters (class "java.awt.Color"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getBackground"
                              (parameters )
                              (returntype . (class "java.awt.Color"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getStroke"
                              (parameters )
                              (returntype . (class "java.awt.Stroke"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "clip"
                              (parameters (class "java.awt.Shape"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getFontRenderContext"
                              (parameters )
                              (returntype . (class "java.awt.font.FontRenderContext"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces)
            (accessflags  *abstract*  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *Graphics2D-class-table*
  (make-static-class-decls 
   *java.awt.Graphics2D*))

(defconst *package-name-map* 
  ("java.awt.Graphics2D" . "java.awt"))

