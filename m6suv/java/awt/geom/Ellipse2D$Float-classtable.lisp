; Ellipse2D$Float-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:27 CDT 2014.
;

(defconst *java.awt.geom.Ellipse2D$Float*
 (make-class-def
      '(class "java.awt.geom.Ellipse2D$Float"
            "java.awt.geom.Ellipse2D"
            (constant_pool
                        (LONG -6633761252372475977))
            (fields
                        (field "x" float (accessflags  *class*  *public* ) -1)
                        (field "y" float (accessflags  *class*  *public* ) -1)
                        (field "width" float (accessflags  *class*  *public* ) -1)
                        (field "height" float (accessflags  *class*  *public* ) -1)
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
					(methodCP "<init>" "java.awt.geom.Ellipse2D" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters float float float float)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 5) (code_length . 14)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.awt.geom.Ellipse2D" () void)))
                                      (4 (aload_0))
                                      (5 (fload_1))
                                      (6 (fload_2))
                                      (7 (fload_3))
                                      (8 (fload 4))
                                      (10 (invokevirtual
					(methodCP "setFrame" "java.awt.geom.Ellipse2D$Float" (float float float float) void)))
                                      (13 (return))
                                      (endofcode 14))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getX"
                              (parameters )
                              (returntype . double)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "x" "java.awt.geom.Ellipse2D$Float" float)))
                                      (4 (f2d))
                                      (5 (dreturn))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getY"
                              (parameters )
                              (returntype . double)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "y" "java.awt.geom.Ellipse2D$Float" float)))
                                      (4 (f2d))
                                      (5 (dreturn))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getWidth"
                              (parameters )
                              (returntype . double)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "width" "java.awt.geom.Ellipse2D$Float" float)))
                                      (4 (f2d))
                                      (5 (dreturn))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getHeight"
                              (parameters )
                              (returntype . double)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "height" "java.awt.geom.Ellipse2D$Float" float)))
                                      (4 (f2d))
                                      (5 (dreturn))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isEmpty"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 1) (code_length . 26)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "width" "java.awt.geom.Ellipse2D$Float" float))) 
                                      (4 (f2d)) 
                                      (5 (dconst_0)) 
                                      (6 (dcmpg)) 
                                      (7 (ifle 20)) ;;to TAG_0
                                      (10 (aload_0)) 
                                      (11 (getfield (fieldCP "height" "java.awt.geom.Ellipse2D$Float" float))) 
                                      (14 (f2d)) 
                                      (15 (dconst_0)) 
                                      (16 (dcmpg)) 
                                      (17 (ifgt 24)) ;;to TAG_1
                                      (20 (iconst_1)) ;;at TAG_0
                                      (21 (goto 25))  ;;to TAG_2
                                      (24 (iconst_0)) ;;at TAG_1
                                      (25 (ireturn)) ;;at TAG_2
                                      (endofcode 26))
                                   (Exceptions )
                                   (StackMap )))
                        (method "setFrame"
                              (parameters float float float float)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 5) (code_length . 22)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (fload_1))
                                      (2 (putfield (fieldCP "x" "java.awt.geom.Ellipse2D$Float" float)))
                                      (5 (aload_0))
                                      (6 (fload_2))
                                      (7 (putfield (fieldCP "y" "java.awt.geom.Ellipse2D$Float" float)))
                                      (10 (aload_0))
                                      (11 (fload_3))
                                      (12 (putfield (fieldCP "width" "java.awt.geom.Ellipse2D$Float" float)))
                                      (15 (aload_0))
                                      (16 (fload 4))
                                      (18 (putfield (fieldCP "height" "java.awt.geom.Ellipse2D$Float" float)))
                                      (21 (return))
                                      (endofcode 22))
                                   (Exceptions )
                                   (StackMap )))
                        (method "setFrame"
                              (parameters double double double double)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 9) (code_length . 27)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (dload_1))
                                      (2 (d2f))
                                      (3 (putfield (fieldCP "x" "java.awt.geom.Ellipse2D$Float" float)))
                                      (6 (aload_0))
                                      (7 (dload_3))
                                      (8 (d2f))
                                      (9 (putfield (fieldCP "y" "java.awt.geom.Ellipse2D$Float" float)))
                                      (12 (aload_0))
                                      (13 (dload 5))
                                      (15 (d2f))
                                      (16 (putfield (fieldCP "width" "java.awt.geom.Ellipse2D$Float" float)))
                                      (19 (aload_0))
                                      (20 (dload 7))
                                      (22 (d2f))
                                      (23 (putfield (fieldCP "height" "java.awt.geom.Ellipse2D$Float" float)))
                                      (26 (return))
                                      (endofcode 27))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getBounds2D"
                              (parameters )
                              (returntype . (class "java.awt.geom.Rectangle2D"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 1) (code_length . 24)
                                   (parsedcode
                                      (0 (new (class "java.awt.geom.Rectangle2D$Float")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "x" "java.awt.geom.Ellipse2D$Float" float)))
                                      (8 (aload_0))
                                      (9 (getfield (fieldCP "y" "java.awt.geom.Ellipse2D$Float" float)))
                                      (12 (aload_0))
                                      (13 (getfield (fieldCP "width" "java.awt.geom.Ellipse2D$Float" float)))
                                      (16 (aload_0))
                                      (17 (getfield (fieldCP "height" "java.awt.geom.Ellipse2D$Float" float)))
                                      (20 (invokespecial
					(methodCP "<init>" "java.awt.geom.Rectangle2D$Float" (float float float float) void)))
                                      (23 (areturn))
                                      (endofcode 24))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.io.Serializable")
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *Ellipse2D$Float-class-table*
  (make-static-class-decls 
   *java.awt.geom.Ellipse2D$Float*))

(defconst *package-name-map* 
  ("java.awt.geom.Ellipse2D$Float" . "java.awt.geom"))

