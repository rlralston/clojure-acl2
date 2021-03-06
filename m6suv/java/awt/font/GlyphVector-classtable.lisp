; GlyphVector-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:26 CDT 2014.
;

(defconst *java.awt.font.GlyphVector*
 (make-class-def
      '(class "java.awt.font.GlyphVector"
            "java.lang.Object"
            (constant_pool
                        (INT 1)
                        (INT 2)
                        (INT 4)
                        (INT 8)
                        (INT 15))
            (fields
                        (field "FLAG_HAS_TRANSFORMS" int (accessflags  *class*  *final*  *public*  *static* ) 0)
                        (field "FLAG_HAS_POSITION_ADJUSTMENTS" int (accessflags  *class*  *final*  *public*  *static* ) 1)
                        (field "FLAG_RUN_RTL" int (accessflags  *class*  *final*  *public*  *static* ) 2)
                        (field "FLAG_COMPLEX_GLYPHS" int (accessflags  *class*  *final*  *public*  *static* ) 3)
                        (field "FLAG_MASK" int (accessflags  *class*  *final*  *public*  *static* ) 4))
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getFont"
                              (parameters )
                              (returntype . (class "java.awt.Font"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getFontRenderContext"
                              (parameters )
                              (returntype . (class "java.awt.font.FontRenderContext"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "performDefaultLayout"
                              (parameters )
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getNumGlyphs"
                              (parameters )
                              (returntype . int)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getGlyphCode"
                              (parameters int)
                              (returntype . int)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getGlyphCodes"
                              (parameters int int (array int))
                              (returntype . (array int))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getGlyphCharIndex"
                              (parameters int)
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 2)
                                   (parsedcode
                                      (0 (iload_1))
                                      (1 (ireturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getGlyphCharIndices"
                              (parameters int int (array int))
                              (returntype . (array int))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 6) (code_length . 41)
                                   (parsedcode
                                      (0 (aload_3)) 
                                      (1 (ifnonnull 8)) ;;to TAG_0
                                      (4 (iload_2)) 
                                      (5 (newarray INT)) 
                                      (7 (astore_3)) 
                                      (8 (iconst_0)) ;;at TAG_0
                                      (9 (istore 4)) 
                                      (11 (iload_1)) 
                                      (12 (istore 5)) 
                                      (14 (iload 4)) ;;at TAG_2
                                      (16 (iload_2)) 
                                      (17 (if_icmpge 39)) ;;to TAG_1
                                      (20 (aload_3)) 
                                      (21 (iload 4)) 
                                      (23 (aload_0)) 
                                      (24 (iload 5)) 
                                      (26 (invokevirtual (methodCP "getGlyphCharIndex" "java.awt.font.GlyphVector" (int) int))) 
                                      (29 (iastore)) 
                                      (30 (iinc 4 1)) 
                                      (33 (iinc 5 1)) 
                                      (36 (goto 14))  ;;to TAG_2
                                      (39 (aload_3)) ;;at TAG_1
                                      (40 (areturn)) 
                                      (endofcode 41))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getLogicalBounds"
                              (parameters )
                              (returntype . (class "java.awt.geom.Rectangle2D"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getVisualBounds"
                              (parameters )
                              (returntype . (class "java.awt.geom.Rectangle2D"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getPixelBounds"
                              (parameters (class "java.awt.font.FontRenderContext") float float)
                              (returntype . (class "java.awt.Rectangle"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 7) (max_locals . 9) (code_length . 84)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokevirtual
					(methodCP "getVisualBounds" "java.awt.font.GlyphVector" () (class "java.awt.geom.Rectangle2D"))))
                                      (4 (astore 4))
                                      (6 (aload 4))
                                      (8 (invokevirtual
					(methodCP "getX" "java.awt.geom.Rectangle2D" () double)))
                                      (11 (fload_2))
                                      (12 (f2d))
                                      (13 (dadd))
                                      (14 (invokestatic
					(methodCP "floor" "java.lang.Math" (double) double)))
                                      (17 (d2i))
                                      (18 (istore 5))
                                      (20 (aload 4))
                                      (22 (invokevirtual
					(methodCP "getY" "java.awt.geom.Rectangle2D" () double)))
                                      (25 (fload_3))
                                      (26 (f2d))
                                      (27 (dadd))
                                      (28 (invokestatic
					(methodCP "floor" "java.lang.Math" (double) double)))
                                      (31 (d2i))
                                      (32 (istore 6))
                                      (34 (aload 4))
                                      (36 (invokevirtual
					(methodCP "getMaxX" "java.awt.geom.Rectangle2D" () double)))
                                      (39 (fload_2))
                                      (40 (f2d))
                                      (41 (dadd))
                                      (42 (invokestatic
					(methodCP "ceil" "java.lang.Math" (double) double)))
                                      (45 (d2i))
                                      (46 (istore 7))
                                      (48 (aload 4))
                                      (50 (invokevirtual
					(methodCP "getMaxY" "java.awt.geom.Rectangle2D" () double)))
                                      (53 (fload_3))
                                      (54 (f2d))
                                      (55 (dadd))
                                      (56 (invokestatic
					(methodCP "ceil" "java.lang.Math" (double) double)))
                                      (59 (d2i))
                                      (60 (istore 8))
                                      (62 (new (class "java.awt.Rectangle")))
                                      (65 (dup))
                                      (66 (iload 5))
                                      (68 (iload 6))
                                      (70 (iload 7))
                                      (72 (iload 5))
                                      (74 (isub))
                                      (75 (iload 8))
                                      (77 (iload 6))
                                      (79 (isub))
                                      (80 (invokespecial
					(methodCP "<init>" "java.awt.Rectangle" (int int int int) void)))
                                      (83 (areturn))
                                      (endofcode 84))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getOutline"
                              (parameters )
                              (returntype . (class "java.awt.Shape"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getOutline"
                              (parameters float float)
                              (returntype . (class "java.awt.Shape"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getGlyphOutline"
                              (parameters int)
                              (returntype . (class "java.awt.Shape"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getGlyphOutline"
                              (parameters int float float)
                              (returntype . (class "java.awt.Shape"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 6) (code_length . 24)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (iload_1))
                                      (2 (invokevirtual
					(methodCP "getGlyphOutline" "java.awt.font.GlyphVector" (int) (class "java.awt.Shape"))))
                                      (5 (astore 4))
                                      (7 (fload_2))
                                      (8 (f2d))
                                      (9 (fload_3))
                                      (10 (f2d))
                                      (11 (invokestatic
					(methodCP "getTranslateInstance" "java.awt.geom.AffineTransform" (double double) (class "java.awt.geom.AffineTransform"))))
                                      (14 (astore 5))
                                      (16 (aload 5))
                                      (18 (aload 4))
                                      (20 (invokevirtual
					(methodCP "createTransformedShape" "java.awt.geom.AffineTransform" ((class "java.awt.Shape")) (class "java.awt.Shape"))))
                                      (23 (areturn))
                                      (endofcode 24))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getGlyphPosition"
                              (parameters int)
                              (returntype . (class "java.awt.geom.Point2D"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "setGlyphPosition"
                              (parameters int (class "java.awt.geom.Point2D"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getGlyphTransform"
                              (parameters int)
                              (returntype . (class "java.awt.geom.AffineTransform"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "setGlyphTransform"
                              (parameters int (class "java.awt.geom.AffineTransform"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getLayoutFlags"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 2)
                                   (parsedcode
                                      (0 (iconst_0))
                                      (1 (ireturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getGlyphPositions"
                              (parameters int int (array float))
                              (returntype . (array float))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getGlyphLogicalBounds"
                              (parameters int)
                              (returntype . (class "java.awt.Shape"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getGlyphVisualBounds"
                              (parameters int)
                              (returntype . (class "java.awt.Shape"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getGlyphPixelBounds"
                              (parameters int (class "java.awt.font.FontRenderContext") float float)
                              (returntype . (class "java.awt.Rectangle"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 7) (max_locals . 10) (code_length . 92)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (iload_1))
                                      (2 (invokevirtual
					(methodCP "getGlyphVisualBounds" "java.awt.font.GlyphVector" (int) (class "java.awt.Shape"))))
                                      (5 (invokeinterface
					(methodCP "getBounds2D" "java.awt.Shape" () (class "java.awt.geom.Rectangle2D")) 1))
                                      (10 (astore 5))
                                      (12 (aload 5))
                                      (14 (invokevirtual
					(methodCP "getX" "java.awt.geom.Rectangle2D" () double)))
                                      (17 (fload_3))
                                      (18 (f2d))
                                      (19 (dadd))
                                      (20 (invokestatic
					(methodCP "floor" "java.lang.Math" (double) double)))
                                      (23 (d2i))
                                      (24 (istore 6))
                                      (26 (aload 5))
                                      (28 (invokevirtual
					(methodCP "getY" "java.awt.geom.Rectangle2D" () double)))
                                      (31 (fload 4))
                                      (33 (f2d))
                                      (34 (dadd))
                                      (35 (invokestatic
					(methodCP "floor" "java.lang.Math" (double) double)))
                                      (38 (d2i))
                                      (39 (istore 7))
                                      (41 (aload 5))
                                      (43 (invokevirtual
					(methodCP "getMaxX" "java.awt.geom.Rectangle2D" () double)))
                                      (46 (fload_3))
                                      (47 (f2d))
                                      (48 (dadd))
                                      (49 (invokestatic
					(methodCP "ceil" "java.lang.Math" (double) double)))
                                      (52 (d2i))
                                      (53 (istore 8))
                                      (55 (aload 5))
                                      (57 (invokevirtual
					(methodCP "getMaxY" "java.awt.geom.Rectangle2D" () double)))
                                      (60 (fload 4))
                                      (62 (f2d))
                                      (63 (dadd))
                                      (64 (invokestatic
					(methodCP "ceil" "java.lang.Math" (double) double)))
                                      (67 (d2i))
                                      (68 (istore 9))
                                      (70 (new (class "java.awt.Rectangle")))
                                      (73 (dup))
                                      (74 (iload 6))
                                      (76 (iload 7))
                                      (78 (iload 8))
                                      (80 (iload 6))
                                      (82 (isub))
                                      (83 (iload 9))
                                      (85 (iload 7))
                                      (87 (isub))
                                      (88 (invokespecial
					(methodCP "<init>" "java.awt.Rectangle" (int int int int) void)))
                                      (91 (areturn))
                                      (endofcode 92))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getGlyphMetrics"
                              (parameters int)
                              (returntype . (class "java.awt.font.GlyphMetrics"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getGlyphJustificationInfo"
                              (parameters int)
                              (returntype . (class "java.awt.font.GlyphJustificationInfo"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "equals"
                              (parameters (class "java.awt.font.GlyphVector"))
                              (returntype . boolean)
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces "java.lang.Cloneable")
            (accessflags  *abstract*  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *GlyphVector-class-table*
  (make-static-class-decls 
   *java.awt.font.GlyphVector*))

(defconst *package-name-map* 
  ("java.awt.font.GlyphVector" . "java.awt.font"))

