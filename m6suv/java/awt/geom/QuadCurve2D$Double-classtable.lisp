; QuadCurve2D$Double-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:27 CDT 2014.
;

(defconst *java.awt.geom.QuadCurve2D$Double*
 (make-class-def
      '(class "java.awt.geom.QuadCurve2D$Double"
            "java.awt.geom.QuadCurve2D"
            (constant_pool
                        (LONG 4217149928428559721))
            (fields
                        (field "x1" double (accessflags  *class*  *public* ) -1)
                        (field "y1" double (accessflags  *class*  *public* ) -1)
                        (field "ctrlx" double (accessflags  *class*  *public* ) -1)
                        (field "ctrly" double (accessflags  *class*  *public* ) -1)
                        (field "x2" double (accessflags  *class*  *public* ) -1)
                        (field "y2" double (accessflags  *class*  *public* ) -1)
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0))
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
					(methodCP "<init>" "java.awt.geom.QuadCurve2D" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters double double double double double double)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 13) (max_locals . 13) (code_length . 19)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.awt.geom.QuadCurve2D" () void)))
                                      (4 (aload_0))
                                      (5 (dload_1))
                                      (6 (dload_3))
                                      (7 (dload 5))
                                      (9 (dload 7))
                                      (11 (dload 9))
                                      (13 (dload 11))
                                      (15 (invokevirtual
					(methodCP "setCurve" "java.awt.geom.QuadCurve2D$Double" (double double double double double double) void)))
                                      (18 (return))
                                      (endofcode 19))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getX1"
                              (parameters )
                              (returntype . double)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "x1" "java.awt.geom.QuadCurve2D$Double" double)))
                                      (4 (dreturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getY1"
                              (parameters )
                              (returntype . double)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "y1" "java.awt.geom.QuadCurve2D$Double" double)))
                                      (4 (dreturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getP1"
                              (parameters )
                              (returntype . (class "java.awt.geom.Point2D"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 1) (code_length . 16)
                                   (parsedcode
                                      (0 (new (class "java.awt.geom.Point2D$Double")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "x1" "java.awt.geom.QuadCurve2D$Double" double)))
                                      (8 (aload_0))
                                      (9 (getfield (fieldCP "y1" "java.awt.geom.QuadCurve2D$Double" double)))
                                      (12 (invokespecial
					(methodCP "<init>" "java.awt.geom.Point2D$Double" (double double) void)))
                                      (15 (areturn))
                                      (endofcode 16))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getCtrlX"
                              (parameters )
                              (returntype . double)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "ctrlx" "java.awt.geom.QuadCurve2D$Double" double)))
                                      (4 (dreturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getCtrlY"
                              (parameters )
                              (returntype . double)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "ctrly" "java.awt.geom.QuadCurve2D$Double" double)))
                                      (4 (dreturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getCtrlPt"
                              (parameters )
                              (returntype . (class "java.awt.geom.Point2D"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 1) (code_length . 16)
                                   (parsedcode
                                      (0 (new (class "java.awt.geom.Point2D$Double")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "ctrlx" "java.awt.geom.QuadCurve2D$Double" double)))
                                      (8 (aload_0))
                                      (9 (getfield (fieldCP "ctrly" "java.awt.geom.QuadCurve2D$Double" double)))
                                      (12 (invokespecial
					(methodCP "<init>" "java.awt.geom.Point2D$Double" (double double) void)))
                                      (15 (areturn))
                                      (endofcode 16))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getX2"
                              (parameters )
                              (returntype . double)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "x2" "java.awt.geom.QuadCurve2D$Double" double)))
                                      (4 (dreturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getY2"
                              (parameters )
                              (returntype . double)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "y2" "java.awt.geom.QuadCurve2D$Double" double)))
                                      (4 (dreturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getP2"
                              (parameters )
                              (returntype . (class "java.awt.geom.Point2D"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 1) (code_length . 16)
                                   (parsedcode
                                      (0 (new (class "java.awt.geom.Point2D$Double")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "x2" "java.awt.geom.QuadCurve2D$Double" double)))
                                      (8 (aload_0))
                                      (9 (getfield (fieldCP "y2" "java.awt.geom.QuadCurve2D$Double" double)))
                                      (12 (invokespecial
					(methodCP "<init>" "java.awt.geom.Point2D$Double" (double double) void)))
                                      (15 (areturn))
                                      (endofcode 16))
                                   (Exceptions )
                                   (StackMap )))
                        (method "setCurve"
                              (parameters double double double double double double)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 13) (code_length . 35)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (dload_1))
                                      (2 (putfield (fieldCP "x1" "java.awt.geom.QuadCurve2D$Double" double)))
                                      (5 (aload_0))
                                      (6 (dload_3))
                                      (7 (putfield (fieldCP "y1" "java.awt.geom.QuadCurve2D$Double" double)))
                                      (10 (aload_0))
                                      (11 (dload 5))
                                      (13 (putfield (fieldCP "ctrlx" "java.awt.geom.QuadCurve2D$Double" double)))
                                      (16 (aload_0))
                                      (17 (dload 7))
                                      (19 (putfield (fieldCP "ctrly" "java.awt.geom.QuadCurve2D$Double" double)))
                                      (22 (aload_0))
                                      (23 (dload 9))
                                      (25 (putfield (fieldCP "x2" "java.awt.geom.QuadCurve2D$Double" double)))
                                      (28 (aload_0))
                                      (29 (dload 11))
                                      (31 (putfield (fieldCP "y2" "java.awt.geom.QuadCurve2D$Double" double)))
                                      (34 (return))
                                      (endofcode 35))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getBounds2D"
                              (parameters )
                              (returntype . (class "java.awt.geom.Rectangle2D"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 12) (max_locals . 9) (code_length . 96)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "x1" "java.awt.geom.QuadCurve2D$Double" double)))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "x2" "java.awt.geom.QuadCurve2D$Double" double)))
                                      (8 (invokestatic
					(methodCP "min" "java.lang.Math" (double double) double)))
                                      (11 (aload_0))
                                      (12 (getfield (fieldCP "ctrlx" "java.awt.geom.QuadCurve2D$Double" double)))
                                      (15 (invokestatic
					(methodCP "min" "java.lang.Math" (double double) double)))
                                      (18 (dstore_1))
                                      (19 (aload_0))
                                      (20 (getfield (fieldCP "y1" "java.awt.geom.QuadCurve2D$Double" double)))
                                      (23 (aload_0))
                                      (24 (getfield (fieldCP "y2" "java.awt.geom.QuadCurve2D$Double" double)))
                                      (27 (invokestatic
					(methodCP "min" "java.lang.Math" (double double) double)))
                                      (30 (aload_0))
                                      (31 (getfield (fieldCP "ctrly" "java.awt.geom.QuadCurve2D$Double" double)))
                                      (34 (invokestatic
					(methodCP "min" "java.lang.Math" (double double) double)))
                                      (37 (dstore_3))
                                      (38 (aload_0))
                                      (39 (getfield (fieldCP "x1" "java.awt.geom.QuadCurve2D$Double" double)))
                                      (42 (aload_0))
                                      (43 (getfield (fieldCP "x2" "java.awt.geom.QuadCurve2D$Double" double)))
                                      (46 (invokestatic
					(methodCP "max" "java.lang.Math" (double double) double)))
                                      (49 (aload_0))
                                      (50 (getfield (fieldCP "ctrlx" "java.awt.geom.QuadCurve2D$Double" double)))
                                      (53 (invokestatic
					(methodCP "max" "java.lang.Math" (double double) double)))
                                      (56 (dstore 5))
                                      (58 (aload_0))
                                      (59 (getfield (fieldCP "y1" "java.awt.geom.QuadCurve2D$Double" double)))
                                      (62 (aload_0))
                                      (63 (getfield (fieldCP "y2" "java.awt.geom.QuadCurve2D$Double" double)))
                                      (66 (invokestatic
					(methodCP "max" "java.lang.Math" (double double) double)))
                                      (69 (aload_0))
                                      (70 (getfield (fieldCP "ctrly" "java.awt.geom.QuadCurve2D$Double" double)))
                                      (73 (invokestatic
					(methodCP "max" "java.lang.Math" (double double) double)))
                                      (76 (dstore 7))
                                      (78 (new (class "java.awt.geom.Rectangle2D$Double")))
                                      (81 (dup))
                                      (82 (dload_1))
                                      (83 (dload_3))
                                      (84 (dload 5))
                                      (86 (dload_1))
                                      (87 (dsub))
                                      (88 (dload 7))
                                      (90 (dload_3))
                                      (91 (dsub))
                                      (92 (invokespecial
					(methodCP "<init>" "java.awt.geom.Rectangle2D$Double" (double double double double) void)))
                                      (95 (areturn))
                                      (endofcode 96))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.io.Serializable")
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *QuadCurve2D$Double-class-table*
  (make-static-class-decls 
   *java.awt.geom.QuadCurve2D$Double*))

(defconst *package-name-map* 
  ("java.awt.geom.QuadCurve2D$Double" . "java.awt.geom"))

