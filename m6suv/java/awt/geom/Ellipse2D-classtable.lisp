; Ellipse2D-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:27 CDT 2014.
;

(defconst *java.awt.geom.Ellipse2D*
 (make-class-def
      '(class "java.awt.geom.Ellipse2D"
            "java.awt.geom.RectangularShape"
            (constant_pool
                        (DOUBLE "0.5")
                        (DOUBLE "0.25")
                        (LONG 37)
                        (LONG 43)
                        (LONG 47))
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
					(methodCP "<init>" "java.awt.geom.RectangularShape" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "contains"
                              (parameters double double)
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 13) (code_length . 84)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "getWidth" "java.awt.geom.Ellipse2D" () double))) 
                                      (4 (dstore 5)) 
                                      (6 (dload 5)) 
                                      (8 (dconst_0)) 
                                      (9 (dcmpg)) 
                                      (10 (ifgt 15)) ;;to TAG_0
                                      (13 (iconst_0)) 
                                      (14 (ireturn)) 
                                      (15 (dload_1)) ;;at TAG_0
                                      (16 (aload_0)) 
                                      (17 (invokevirtual (methodCP "getX" "java.awt.geom.Ellipse2D" () double))) 
                                      (20 (dsub)) 
                                      (21 (dload 5)) 
                                      (23 (ddiv)) 
                                      (24 (ldc2_w 0)) ;; DOUBLE:: "0.5"
                                      (27 (dsub)) 
                                      (28 (dstore 7)) 
                                      (30 (aload_0)) 
                                      (31 (invokevirtual (methodCP "getHeight" "java.awt.geom.Ellipse2D" () double))) 
                                      (34 (dstore 9)) 
                                      (36 (dload 9)) 
                                      (38 (dconst_0)) 
                                      (39 (dcmpg)) 
                                      (40 (ifgt 45)) ;;to TAG_1
                                      (43 (iconst_0)) 
                                      (44 (ireturn)) 
                                      (45 (dload_3)) ;;at TAG_1
                                      (46 (aload_0)) 
                                      (47 (invokevirtual (methodCP "getY" "java.awt.geom.Ellipse2D" () double))) 
                                      (50 (dsub)) 
                                      (51 (dload 9)) 
                                      (53 (ddiv)) 
                                      (54 (ldc2_w 0)) ;; DOUBLE:: "0.5"
                                      (57 (dsub)) 
                                      (58 (dstore 11)) 
                                      (60 (dload 7)) 
                                      (62 (dload 7)) 
                                      (64 (dmul)) 
                                      (65 (dload 11)) 
                                      (67 (dload 11)) 
                                      (69 (dmul)) 
                                      (70 (dadd)) 
                                      (71 (ldc2_w 1)) ;; DOUBLE:: "0.25"
                                      (74 (dcmpg)) 
                                      (75 (ifge 82))  ;;to TAG_2
                                      (78 (iconst_1)) 
                                      (79 (goto 83)) ;;to TAG_3
                                      (82 (iconst_0)) ;;at TAG_2
                                      (83 (ireturn)) ;;at TAG_3
                                      (endofcode 84))
                                   (Exceptions )
                                   (StackMap )))
                        (method "intersects"
                              (parameters double double double double)
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 25) (code_length . 182)
                                   (parsedcode
                                      (0 (dload 5)) 
                                      (2 (dconst_0)) 
                                      (3 (dcmpg)) 
                                      (4 (ifle 14)) ;;to TAG_0
                                      (7 (dload 7)) 
                                      (9 (dconst_0)) 
                                      (10 (dcmpg)) 
                                      (11 (ifgt 16))  ;;to TAG_1
                                      (14 (iconst_0)) ;;at TAG_0
                                      (15 (ireturn)) 
                                      (16 (aload_0)) ;;at TAG_1
                                      (17 (invokevirtual (methodCP "getWidth" "java.awt.geom.Ellipse2D" () double))) 
                                      (20 (dstore 9)) 
                                      (22 (dload 9)) 
                                      (24 (dconst_0)) 
                                      (25 (dcmpg)) 
                                      (26 (ifgt 31)) ;;to TAG_2
                                      (29 (iconst_0)) 
                                      (30 (ireturn)) 
                                      (31 (dload_1)) ;;at TAG_2
                                      (32 (aload_0)) 
                                      (33 (invokevirtual (methodCP "getX" "java.awt.geom.Ellipse2D" () double))) 
                                      (36 (dsub)) 
                                      (37 (dload 9)) 
                                      (39 (ddiv)) 
                                      (40 (ldc2_w 0)) ;; DOUBLE:: "0.5"
                                      (43 (dsub)) 
                                      (44 (dstore 11)) 
                                      (46 (dload 11)) 
                                      (48 (dload 5)) 
                                      (50 (dload 9)) 
                                      (52 (ddiv)) 
                                      (53 (dadd)) 
                                      (54 (dstore 13)) 
                                      (56 (aload_0)) 
                                      (57 (invokevirtual (methodCP "getHeight" "java.awt.geom.Ellipse2D" () double))) 
                                      (60 (dstore 15)) 
                                      (62 (dload 15)) 
                                      (64 (dconst_0)) 
                                      (65 (dcmpg)) 
                                      (66 (ifgt 71)) ;;to TAG_3
                                      (69 (iconst_0)) 
                                      (70 (ireturn)) 
                                      (71 (dload_3)) ;;at TAG_3
                                      (72 (aload_0)) 
                                      (73 (invokevirtual (methodCP "getY" "java.awt.geom.Ellipse2D" () double))) 
                                      (76 (dsub)) 
                                      (77 (dload 15)) 
                                      (79 (ddiv)) 
                                      (80 (ldc2_w 0)) ;; DOUBLE:: "0.5"
                                      (83 (dsub)) 
                                      (84 (dstore 17)) 
                                      (86 (dload 17)) 
                                      (88 (dload 7)) 
                                      (90 (dload 15)) 
                                      (92 (ddiv)) 
                                      (93 (dadd)) 
                                      (94 (dstore 19)) 
                                      (96 (dload 11)) 
                                      (98 (dconst_0)) 
                                      (99 (dcmpl)) 
                                      (100 (ifle 110)) ;;to TAG_4
                                      (103 (dload 11)) 
                                      (105 (dstore 21)) 
                                      (107 (goto 127)) ;;to TAG_5
                                      (110 (dload 13)) ;;at TAG_4
                                      (112 (dconst_0)) 
                                      (113 (dcmpg)) 
                                      (114 (ifge 124)) ;;to TAG_6
                                      (117 (dload 13)) 
                                      (119 (dstore 21)) 
                                      (121 (goto 127)) ;;to TAG_5
                                      (124 (dconst_0)) ;;at TAG_6
                                      (125 (dstore 21)) 
                                      (127 (dload 17)) ;;at TAG_5
                                      (129 (dconst_0)) 
                                      (130 (dcmpl)) 
                                      (131 (ifle 141)) ;;to TAG_7
                                      (134 (dload 17)) 
                                      (136 (dstore 23)) 
                                      (138 (goto 158)) ;;to TAG_8
                                      (141 (dload 19)) ;;at TAG_7
                                      (143 (dconst_0)) 
                                      (144 (dcmpg)) 
                                      (145 (ifge 155)) ;;to TAG_9
                                      (148 (dload 19)) 
                                      (150 (dstore 23)) 
                                      (152 (goto 158)) ;;to TAG_8
                                      (155 (dconst_0)) ;;at TAG_9
                                      (156 (dstore 23)) 
                                      (158 (dload 21)) ;;at TAG_8
                                      (160 (dload 21)) 
                                      (162 (dmul)) 
                                      (163 (dload 23)) 
                                      (165 (dload 23)) 
                                      (167 (dmul)) 
                                      (168 (dadd)) 
                                      (169 (ldc2_w 1)) ;; DOUBLE:: "0.25"
                                      (172 (dcmpg)) 
                                      (173 (ifge 180)) ;;to TAG_10
                                      (176 (iconst_1)) 
                                      (177 (goto 181)) ;;to TAG_11
                                      (180 (iconst_0)) ;;at TAG_10
                                      (181 (ireturn)) ;;at TAG_11
                                      (endofcode 182))
                                   (Exceptions )
                                   (StackMap )))
                        (method "contains"
                              (parameters double double double double)
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 7) (max_locals . 9) (code_length . 54)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (dload_1)) 
                                      (2 (dload_3)) 
                                      (3 (invokevirtual (methodCP "contains" "java.awt.geom.Ellipse2D" (double double) boolean))) 
                                      (6 (ifeq 52))  ;;to TAG_0
                                      (9 (aload_0)) 
                                      (10 (dload_1)) 
                                      (11 (dload 5)) 
                                      (13 (dadd)) 
                                      (14 (dload_3)) 
                                      (15 (invokevirtual (methodCP "contains" "java.awt.geom.Ellipse2D" (double double) boolean))) 
                                      (18 (ifeq 52))  ;;to TAG_0
                                      (21 (aload_0)) 
                                      (22 (dload_1)) 
                                      (23 (dload_3)) 
                                      (24 (dload 7)) 
                                      (26 (dadd)) 
                                      (27 (invokevirtual (methodCP "contains" "java.awt.geom.Ellipse2D" (double double) boolean))) 
                                      (30 (ifeq 52))  ;;to TAG_0
                                      (33 (aload_0)) 
                                      (34 (dload_1)) 
                                      (35 (dload 5)) 
                                      (37 (dadd)) 
                                      (38 (dload_3)) 
                                      (39 (dload 7)) 
                                      (41 (dadd)) 
                                      (42 (invokevirtual (methodCP "contains" "java.awt.geom.Ellipse2D" (double double) boolean))) 
                                      (45 (ifeq 52))  ;;to TAG_0
                                      (48 (iconst_1)) 
                                      (49 (goto 53)) ;;to TAG_1
                                      (52 (iconst_0)) ;;at TAG_0
                                      (53 (ireturn)) ;;at TAG_1
                                      (endofcode 54))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getPathIterator"
                              (parameters (class "java.awt.geom.AffineTransform"))
                              (returntype . (class "java.awt.geom.PathIterator"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (new (class "java.awt.geom.EllipseIterator")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (invokespecial
					(methodCP "<init>" "java.awt.geom.EllipseIterator" ((class "java.awt.geom.Ellipse2D") (class "java.awt.geom.AffineTransform")) void)))
                                      (9 (areturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hashCode"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 3) (code_length . 59)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokevirtual
					(methodCP "getX" "java.awt.geom.Ellipse2D" () double)))
                                      (4 (invokestatic
					(methodCP "doubleToLongBits" "java.lang.Double" (double) long)))
                                      (7 (lstore_1))
                                      (8 (lload_1))
                                      (9 (aload_0))
                                      (10 (invokevirtual
					(methodCP "getY" "java.awt.geom.Ellipse2D" () double)))
                                      (13 (invokestatic
					(methodCP "doubleToLongBits" "java.lang.Double" (double) long)))
                                      (16 (ldc2_w 2))     ;; LONG:: "37"
                                      (19 (lmul))
                                      (20 (ladd))
                                      (21 (lstore_1))
                                      (22 (lload_1))
                                      (23 (aload_0))
                                      (24 (invokevirtual
					(methodCP "getWidth" "java.awt.geom.Ellipse2D" () double)))
                                      (27 (invokestatic
					(methodCP "doubleToLongBits" "java.lang.Double" (double) long)))
                                      (30 (ldc2_w 3))     ;; LONG:: "43"
                                      (33 (lmul))
                                      (34 (ladd))
                                      (35 (lstore_1))
                                      (36 (lload_1))
                                      (37 (aload_0))
                                      (38 (invokevirtual
					(methodCP "getHeight" "java.awt.geom.Ellipse2D" () double)))
                                      (41 (invokestatic
					(methodCP "doubleToLongBits" "java.lang.Double" (double) long)))
                                      (44 (ldc2_w 4))     ;; LONG:: "47"
                                      (47 (lmul))
                                      (48 (ladd))
                                      (49 (lstore_1))
                                      (50 (lload_1))
                                      (51 (l2i))
                                      (52 (lload_1))
                                      (53 (bipush 32))
                                      (55 (lshr))
                                      (56 (l2i))
                                      (57 (ixor))
                                      (58 (ireturn))
                                      (endofcode 59))
                                   (Exceptions )
                                   (StackMap )))
                        (method "equals"
                              (parameters (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 75)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (aload_0)) 
                                      (2 (if_acmpne 7)) ;;to TAG_0
                                      (5 (iconst_1)) 
                                      (6 (ireturn)) 
                                      (7 (aload_1)) ;;at TAG_0
                                      (8 (instanceof (class "java.awt.geom.Ellipse2D"))) 
                                      (11 (ifeq 73)) ;;to TAG_1
                                      (14 (aload_1)) 
                                      (15 (checkcast (class "java.awt.geom.Ellipse2D"))) 
                                      (18 (astore_2)) 
                                      (19 (aload_0)) 
                                      (20 (invokevirtual (methodCP "getX" "java.awt.geom.Ellipse2D" () double))) 
                                      (23 (aload_2)) 
                                      (24 (invokevirtual (methodCP "getX" "java.awt.geom.Ellipse2D" () double))) 
                                      (27 (dcmpl)) 
                                      (28 (ifne 71))  ;;to TAG_2
                                      (31 (aload_0)) 
                                      (32 (invokevirtual (methodCP "getY" "java.awt.geom.Ellipse2D" () double))) 
                                      (35 (aload_2)) 
                                      (36 (invokevirtual (methodCP "getY" "java.awt.geom.Ellipse2D" () double))) 
                                      (39 (dcmpl)) 
                                      (40 (ifne 71))  ;;to TAG_2
                                      (43 (aload_0)) 
                                      (44 (invokevirtual (methodCP "getWidth" "java.awt.geom.Ellipse2D" () double))) 
                                      (47 (aload_2)) 
                                      (48 (invokevirtual (methodCP "getWidth" "java.awt.geom.Ellipse2D" () double))) 
                                      (51 (dcmpl)) 
                                      (52 (ifne 71))  ;;to TAG_2
                                      (55 (aload_0)) 
                                      (56 (invokevirtual (methodCP "getHeight" "java.awt.geom.Ellipse2D" () double))) 
                                      (59 (aload_2)) 
                                      (60 (invokevirtual (methodCP "getHeight" "java.awt.geom.Ellipse2D" () double))) 
                                      (63 (dcmpl)) 
                                      (64 (ifne 71))  ;;to TAG_2
                                      (67 (iconst_1)) 
                                      (68 (goto 72)) ;;to TAG_3
                                      (71 (iconst_0)) ;;at TAG_2
                                      (72 (ireturn)) ;;at TAG_3
                                      (73 (iconst_0)) ;;at TAG_1
                                      (74 (ireturn)) 
                                      (endofcode 75))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *abstract*  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *Ellipse2D-class-table*
  (make-static-class-decls 
   *java.awt.geom.Ellipse2D*))

(defconst *package-name-map* 
  ("java.awt.geom.Ellipse2D" . "java.awt.geom"))

