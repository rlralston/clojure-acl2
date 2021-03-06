; EllipseIterator-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:27 CDT 2014.
;

(defconst *java.awt.geom.EllipseIterator*
 (make-class-def
      '(class "java.awt.geom.EllipseIterator"
            "java.lang.Object"
            (constant_pool
                        (DOUBLE "0.5522847498307933")
                        (DOUBLE "0.7761423749153966")
                        (DOUBLE "0.22385762508460333")
                        (STRING  "ellipse iterator out of bounds")
                        (DOUBLE "0.5"))
            (fields
                        (field "x" double (accessflags  *class* ) -1)
                        (field "y" double (accessflags  *class* ) -1)
                        (field "w" double (accessflags  *class* ) -1)
                        (field "h" double (accessflags  *class* ) -1)
                        (field "affine" (class "java.awt.geom.AffineTransform") (accessflags  *class* ) -1)
                        (field "index" int (accessflags  *class* ) -1)
                        (field "CtrlVal" double (accessflags  *class*  *final*  *public*  *static* ) 0)
                        (field "pcv" double (accessflags  *class*  *final*  *private*  *static* ) 1)
                        (field "ncv" double (accessflags  *class*  *final*  *private*  *static* ) 2)
                        (field "ctrlpts" (array (array double)) (accessflags  *class*  *private*  *static* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.awt.geom.Ellipse2D") (class "java.awt.geom.AffineTransform"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 66)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokespecial (methodCP "<init>" "java.lang.Object" () void))) 
                                      (4 (aload_0)) 
                                      (5 (aload_1)) 
                                      (6 (invokevirtual (methodCP "getX" "java.awt.geom.Ellipse2D" () double))) 
                                      (9 (putfield (fieldCP "x" "java.awt.geom.EllipseIterator" double))) 
                                      (12 (aload_0)) 
                                      (13 (aload_1)) 
                                      (14 (invokevirtual (methodCP "getY" "java.awt.geom.Ellipse2D" () double))) 
                                      (17 (putfield (fieldCP "y" "java.awt.geom.EllipseIterator" double))) 
                                      (20 (aload_0)) 
                                      (21 (aload_1)) 
                                      (22 (invokevirtual (methodCP "getWidth" "java.awt.geom.Ellipse2D" () double))) 
                                      (25 (putfield (fieldCP "w" "java.awt.geom.EllipseIterator" double))) 
                                      (28 (aload_0)) 
                                      (29 (aload_1)) 
                                      (30 (invokevirtual (methodCP "getHeight" "java.awt.geom.Ellipse2D" () double))) 
                                      (33 (putfield (fieldCP "h" "java.awt.geom.EllipseIterator" double))) 
                                      (36 (aload_0)) 
                                      (37 (aload_2)) 
                                      (38 (putfield (fieldCP "affine" "java.awt.geom.EllipseIterator" (class "java.awt.geom.AffineTransform")))) 
                                      (41 (aload_0)) 
                                      (42 (getfield (fieldCP "w" "java.awt.geom.EllipseIterator" double))) 
                                      (45 (dconst_0)) 
                                      (46 (dcmpg)) 
                                      (47 (iflt 59))  ;;to TAG_0
                                      (50 (aload_0)) 
                                      (51 (getfield (fieldCP "h" "java.awt.geom.EllipseIterator" double))) 
                                      (54 (dconst_0)) 
                                      (55 (dcmpg)) 
                                      (56 (ifge 65)) ;;to TAG_1
                                      (59 (aload_0)) ;;at TAG_0
                                      (60 (bipush 6)) 
                                      (62 (putfield (fieldCP "index" "java.awt.geom.EllipseIterator" int))) 
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
                                      (1 (getfield (fieldCP "index" "java.awt.geom.EllipseIterator" int))) 
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
                                      (2 (getfield (fieldCP "index" "java.awt.geom.EllipseIterator" int)))
                                      (5 (iconst_1))
                                      (6 (iadd))
                                      (7 (putfield (fieldCP "index" "java.awt.geom.EllipseIterator" int)))
                                      (10 (return))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "currentSegment"
                              (parameters (array float))
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 8) (max_locals . 3) (code_length . 229)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "isDone" "java.awt.geom.EllipseIterator" () boolean))) 
                                      (4 (ifeq 17)) ;;to TAG_0
                                      (7 (new (class "java.util.NoSuchElementException"))) 
                                      (10 (dup)) 
                                      (11 (ldc 3)) ;;STRING:: "ellipse iterator out of bounds"
                                      (13 (invokespecial (methodCP "<init>" "java.util.NoSuchElementException" ((class "java.lang.String")) void))) 
                                      (16 (athrow)) 
                                      (17 (aload_0)) ;;at TAG_0
                                      (18 (getfield (fieldCP "index" "java.awt.geom.EllipseIterator" int))) 
                                      (21 (iconst_5)) 
                                      (22 (if_icmpne 27)) ;;to TAG_1
                                      (25 (iconst_4)) 
                                      (26 (ireturn)) 
                                      (27 (aload_0)) ;;at TAG_1
                                      (28 (getfield (fieldCP "index" "java.awt.geom.EllipseIterator" int))) 
                                      (31 (ifne 95))  ;;to TAG_2
                                      (34 (getstatic (fieldCP "ctrlpts" "java.awt.geom.EllipseIterator" (array (array double))))) 
                                      (37 (iconst_3)) 
                                      (38 (aaload)) 
                                      (39 (astore_2)) 
                                      (40 (aload_1)) 
                                      (41 (iconst_0)) 
                                      (42 (aload_0)) 
                                      (43 (getfield (fieldCP "x" "java.awt.geom.EllipseIterator" double))) 
                                      (46 (aload_2)) 
                                      (47 (iconst_4)) 
                                      (48 (daload)) 
                                      (49 (aload_0)) 
                                      (50 (getfield (fieldCP "w" "java.awt.geom.EllipseIterator" double))) 
                                      (53 (dmul)) 
                                      (54 (dadd)) 
                                      (55 (d2f)) 
                                      (56 (fastore)) 
                                      (57 (aload_1)) 
                                      (58 (iconst_1)) 
                                      (59 (aload_0)) 
                                      (60 (getfield (fieldCP "y" "java.awt.geom.EllipseIterator" double))) 
                                      (63 (aload_2)) 
                                      (64 (iconst_5)) 
                                      (65 (daload)) 
                                      (66 (aload_0)) 
                                      (67 (getfield (fieldCP "h" "java.awt.geom.EllipseIterator" double))) 
                                      (70 (dmul)) 
                                      (71 (dadd)) 
                                      (72 (d2f)) 
                                      (73 (fastore)) 
                                      (74 (aload_0)) 
                                      (75 (getfield (fieldCP "affine" "java.awt.geom.EllipseIterator" (class "java.awt.geom.AffineTransform")))) 
                                      (78 (ifnull 93)) ;;to TAG_3
                                      (81 (aload_0)) 
                                      (82 (getfield (fieldCP "affine" "java.awt.geom.EllipseIterator" (class "java.awt.geom.AffineTransform")))) 
                                      (85 (aload_1)) 
                                      (86 (iconst_0)) 
                                      (87 (aload_1)) 
                                      (88 (iconst_0)) 
                                      (89 (iconst_1)) 
                                      (90 (invokevirtual (methodCP "transform" "java.awt.geom.AffineTransform" ((array float) int (array float) int int) void))) 
                                      (93 (iconst_0)) ;;at TAG_3
                                      (94 (ireturn)) 
                                      (95 (getstatic (fieldCP "ctrlpts" "java.awt.geom.EllipseIterator" (array (array double))))) ;;at TAG_2
                                      (98 (aload_0)) 
                                      (99 (getfield (fieldCP "index" "java.awt.geom.EllipseIterator" int))) 
                                      (102 (iconst_1)) 
                                      (103 (isub)) 
                                      (104 (aaload)) 
                                      (105 (astore_2)) 
                                      (106 (aload_1)) 
                                      (107 (iconst_0)) 
                                      (108 (aload_0)) 
                                      (109 (getfield (fieldCP "x" "java.awt.geom.EllipseIterator" double))) 
                                      (112 (aload_2)) 
                                      (113 (iconst_0)) 
                                      (114 (daload)) 
                                      (115 (aload_0)) 
                                      (116 (getfield (fieldCP "w" "java.awt.geom.EllipseIterator" double))) 
                                      (119 (dmul)) 
                                      (120 (dadd)) 
                                      (121 (d2f)) 
                                      (122 (fastore)) 
                                      (123 (aload_1)) 
                                      (124 (iconst_1)) 
                                      (125 (aload_0)) 
                                      (126 (getfield (fieldCP "y" "java.awt.geom.EllipseIterator" double))) 
                                      (129 (aload_2)) 
                                      (130 (iconst_1)) 
                                      (131 (daload)) 
                                      (132 (aload_0)) 
                                      (133 (getfield (fieldCP "h" "java.awt.geom.EllipseIterator" double))) 
                                      (136 (dmul)) 
                                      (137 (dadd)) 
                                      (138 (d2f)) 
                                      (139 (fastore)) 
                                      (140 (aload_1)) 
                                      (141 (iconst_2)) 
                                      (142 (aload_0)) 
                                      (143 (getfield (fieldCP "x" "java.awt.geom.EllipseIterator" double))) 
                                      (146 (aload_2)) 
                                      (147 (iconst_2)) 
                                      (148 (daload)) 
                                      (149 (aload_0)) 
                                      (150 (getfield (fieldCP "w" "java.awt.geom.EllipseIterator" double))) 
                                      (153 (dmul)) 
                                      (154 (dadd)) 
                                      (155 (d2f)) 
                                      (156 (fastore)) 
                                      (157 (aload_1)) 
                                      (158 (iconst_3)) 
                                      (159 (aload_0)) 
                                      (160 (getfield (fieldCP "y" "java.awt.geom.EllipseIterator" double))) 
                                      (163 (aload_2)) 
                                      (164 (iconst_3)) 
                                      (165 (daload)) 
                                      (166 (aload_0)) 
                                      (167 (getfield (fieldCP "h" "java.awt.geom.EllipseIterator" double))) 
                                      (170 (dmul)) 
                                      (171 (dadd)) 
                                      (172 (d2f)) 
                                      (173 (fastore)) 
                                      (174 (aload_1)) 
                                      (175 (iconst_4)) 
                                      (176 (aload_0)) 
                                      (177 (getfield (fieldCP "x" "java.awt.geom.EllipseIterator" double))) 
                                      (180 (aload_2)) 
                                      (181 (iconst_4)) 
                                      (182 (daload)) 
                                      (183 (aload_0)) 
                                      (184 (getfield (fieldCP "w" "java.awt.geom.EllipseIterator" double))) 
                                      (187 (dmul)) 
                                      (188 (dadd)) 
                                      (189 (d2f)) 
                                      (190 (fastore)) 
                                      (191 (aload_1)) 
                                      (192 (iconst_5)) 
                                      (193 (aload_0)) 
                                      (194 (getfield (fieldCP "y" "java.awt.geom.EllipseIterator" double))) 
                                      (197 (aload_2)) 
                                      (198 (iconst_5)) 
                                      (199 (daload)) 
                                      (200 (aload_0)) 
                                      (201 (getfield (fieldCP "h" "java.awt.geom.EllipseIterator" double))) 
                                      (204 (dmul)) 
                                      (205 (dadd)) 
                                      (206 (d2f)) 
                                      (207 (fastore)) 
                                      (208 (aload_0)) 
                                      (209 (getfield (fieldCP "affine" "java.awt.geom.EllipseIterator" (class "java.awt.geom.AffineTransform")))) 
                                      (212 (ifnull 227)) ;;to TAG_4
                                      (215 (aload_0)) 
                                      (216 (getfield (fieldCP "affine" "java.awt.geom.EllipseIterator" (class "java.awt.geom.AffineTransform")))) 
                                      (219 (aload_1)) 
                                      (220 (iconst_0)) 
                                      (221 (aload_1)) 
                                      (222 (iconst_0)) 
                                      (223 (iconst_3)) 
                                      (224 (invokevirtual (methodCP "transform" "java.awt.geom.AffineTransform" ((array float) int (array float) int int) void))) 
                                      (227 (iconst_3)) ;;at TAG_4
                                      (228 (ireturn)) 
                                      (endofcode 229))
                                   (Exceptions )
                                   (StackMap )))
                        (method "currentSegment"
                              (parameters (array double))
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 8) (max_locals . 3) (code_length . 221)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "isDone" "java.awt.geom.EllipseIterator" () boolean))) 
                                      (4 (ifeq 17)) ;;to TAG_0
                                      (7 (new (class "java.util.NoSuchElementException"))) 
                                      (10 (dup)) 
                                      (11 (ldc 3)) ;;STRING:: "ellipse iterator out of bounds"
                                      (13 (invokespecial (methodCP "<init>" "java.util.NoSuchElementException" ((class "java.lang.String")) void))) 
                                      (16 (athrow)) 
                                      (17 (aload_0)) ;;at TAG_0
                                      (18 (getfield (fieldCP "index" "java.awt.geom.EllipseIterator" int))) 
                                      (21 (iconst_5)) 
                                      (22 (if_icmpne 27)) ;;to TAG_1
                                      (25 (iconst_4)) 
                                      (26 (ireturn)) 
                                      (27 (aload_0)) ;;at TAG_1
                                      (28 (getfield (fieldCP "index" "java.awt.geom.EllipseIterator" int))) 
                                      (31 (ifne 93))  ;;to TAG_2
                                      (34 (getstatic (fieldCP "ctrlpts" "java.awt.geom.EllipseIterator" (array (array double))))) 
                                      (37 (iconst_3)) 
                                      (38 (aaload)) 
                                      (39 (astore_2)) 
                                      (40 (aload_1)) 
                                      (41 (iconst_0)) 
                                      (42 (aload_0)) 
                                      (43 (getfield (fieldCP "x" "java.awt.geom.EllipseIterator" double))) 
                                      (46 (aload_2)) 
                                      (47 (iconst_4)) 
                                      (48 (daload)) 
                                      (49 (aload_0)) 
                                      (50 (getfield (fieldCP "w" "java.awt.geom.EllipseIterator" double))) 
                                      (53 (dmul)) 
                                      (54 (dadd)) 
                                      (55 (dastore)) 
                                      (56 (aload_1)) 
                                      (57 (iconst_1)) 
                                      (58 (aload_0)) 
                                      (59 (getfield (fieldCP "y" "java.awt.geom.EllipseIterator" double))) 
                                      (62 (aload_2)) 
                                      (63 (iconst_5)) 
                                      (64 (daload)) 
                                      (65 (aload_0)) 
                                      (66 (getfield (fieldCP "h" "java.awt.geom.EllipseIterator" double))) 
                                      (69 (dmul)) 
                                      (70 (dadd)) 
                                      (71 (dastore)) 
                                      (72 (aload_0)) 
                                      (73 (getfield (fieldCP "affine" "java.awt.geom.EllipseIterator" (class "java.awt.geom.AffineTransform")))) 
                                      (76 (ifnull 91)) ;;to TAG_3
                                      (79 (aload_0)) 
                                      (80 (getfield (fieldCP "affine" "java.awt.geom.EllipseIterator" (class "java.awt.geom.AffineTransform")))) 
                                      (83 (aload_1)) 
                                      (84 (iconst_0)) 
                                      (85 (aload_1)) 
                                      (86 (iconst_0)) 
                                      (87 (iconst_1)) 
                                      (88 (invokevirtual (methodCP "transform" "java.awt.geom.AffineTransform" ((array double) int (array double) int int) void))) 
                                      (91 (iconst_0)) ;;at TAG_3
                                      (92 (ireturn)) 
                                      (93 (getstatic (fieldCP "ctrlpts" "java.awt.geom.EllipseIterator" (array (array double))))) ;;at TAG_2
                                      (96 (aload_0)) 
                                      (97 (getfield (fieldCP "index" "java.awt.geom.EllipseIterator" int))) 
                                      (100 (iconst_1)) 
                                      (101 (isub)) 
                                      (102 (aaload)) 
                                      (103 (astore_2)) 
                                      (104 (aload_1)) 
                                      (105 (iconst_0)) 
                                      (106 (aload_0)) 
                                      (107 (getfield (fieldCP "x" "java.awt.geom.EllipseIterator" double))) 
                                      (110 (aload_2)) 
                                      (111 (iconst_0)) 
                                      (112 (daload)) 
                                      (113 (aload_0)) 
                                      (114 (getfield (fieldCP "w" "java.awt.geom.EllipseIterator" double))) 
                                      (117 (dmul)) 
                                      (118 (dadd)) 
                                      (119 (dastore)) 
                                      (120 (aload_1)) 
                                      (121 (iconst_1)) 
                                      (122 (aload_0)) 
                                      (123 (getfield (fieldCP "y" "java.awt.geom.EllipseIterator" double))) 
                                      (126 (aload_2)) 
                                      (127 (iconst_1)) 
                                      (128 (daload)) 
                                      (129 (aload_0)) 
                                      (130 (getfield (fieldCP "h" "java.awt.geom.EllipseIterator" double))) 
                                      (133 (dmul)) 
                                      (134 (dadd)) 
                                      (135 (dastore)) 
                                      (136 (aload_1)) 
                                      (137 (iconst_2)) 
                                      (138 (aload_0)) 
                                      (139 (getfield (fieldCP "x" "java.awt.geom.EllipseIterator" double))) 
                                      (142 (aload_2)) 
                                      (143 (iconst_2)) 
                                      (144 (daload)) 
                                      (145 (aload_0)) 
                                      (146 (getfield (fieldCP "w" "java.awt.geom.EllipseIterator" double))) 
                                      (149 (dmul)) 
                                      (150 (dadd)) 
                                      (151 (dastore)) 
                                      (152 (aload_1)) 
                                      (153 (iconst_3)) 
                                      (154 (aload_0)) 
                                      (155 (getfield (fieldCP "y" "java.awt.geom.EllipseIterator" double))) 
                                      (158 (aload_2)) 
                                      (159 (iconst_3)) 
                                      (160 (daload)) 
                                      (161 (aload_0)) 
                                      (162 (getfield (fieldCP "h" "java.awt.geom.EllipseIterator" double))) 
                                      (165 (dmul)) 
                                      (166 (dadd)) 
                                      (167 (dastore)) 
                                      (168 (aload_1)) 
                                      (169 (iconst_4)) 
                                      (170 (aload_0)) 
                                      (171 (getfield (fieldCP "x" "java.awt.geom.EllipseIterator" double))) 
                                      (174 (aload_2)) 
                                      (175 (iconst_4)) 
                                      (176 (daload)) 
                                      (177 (aload_0)) 
                                      (178 (getfield (fieldCP "w" "java.awt.geom.EllipseIterator" double))) 
                                      (181 (dmul)) 
                                      (182 (dadd)) 
                                      (183 (dastore)) 
                                      (184 (aload_1)) 
                                      (185 (iconst_5)) 
                                      (186 (aload_0)) 
                                      (187 (getfield (fieldCP "y" "java.awt.geom.EllipseIterator" double))) 
                                      (190 (aload_2)) 
                                      (191 (iconst_5)) 
                                      (192 (daload)) 
                                      (193 (aload_0)) 
                                      (194 (getfield (fieldCP "h" "java.awt.geom.EllipseIterator" double))) 
                                      (197 (dmul)) 
                                      (198 (dadd)) 
                                      (199 (dastore)) 
                                      (200 (aload_0)) 
                                      (201 (getfield (fieldCP "affine" "java.awt.geom.EllipseIterator" (class "java.awt.geom.AffineTransform")))) 
                                      (204 (ifnull 219)) ;;to TAG_4
                                      (207 (aload_0)) 
                                      (208 (getfield (fieldCP "affine" "java.awt.geom.EllipseIterator" (class "java.awt.geom.AffineTransform")))) 
                                      (211 (aload_1)) 
                                      (212 (iconst_0)) 
                                      (213 (aload_1)) 
                                      (214 (iconst_0)) 
                                      (215 (iconst_3)) 
                                      (216 (invokevirtual (methodCP "transform" "java.awt.geom.AffineTransform" ((array double) int (array double) int int) void))) 
                                      (219 (iconst_3)) ;;at TAG_4
                                      (220 (ireturn)) 
                                      (endofcode 221))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 8) (max_locals . 0) (code_length . 156)
                                   (parsedcode
                                      (0 (iconst_4))
                                      (1 (anewarray (array double)))
                                      (4 (dup))
                                      (5 (iconst_0))
                                      (6 (bipush 6))
                                      (8 (newarray DOUBLE))
                                      (10 (dup))
                                      (11 (iconst_0))
                                      (12 (dconst_1))
                                      (13 (dastore))
                                      (14 (dup))
                                      (15 (iconst_1))
                                      (16 (ldc2_w 1))     ;; DOUBLE:: "0.7761423749153966"
                                      (19 (dastore))
                                      (20 (dup))
                                      (21 (iconst_2))
                                      (22 (ldc2_w 1))     ;; DOUBLE:: "0.7761423749153966"
                                      (25 (dastore))
                                      (26 (dup))
                                      (27 (iconst_3))
                                      (28 (dconst_1))
                                      (29 (dastore))
                                      (30 (dup))
                                      (31 (iconst_4))
                                      (32 (ldc2_w 4))     ;; DOUBLE:: "0.5"
                                      (35 (dastore))
                                      (36 (dup))
                                      (37 (iconst_5))
                                      (38 (dconst_1))
                                      (39 (dastore))
                                      (40 (aastore))
                                      (41 (dup))
                                      (42 (iconst_1))
                                      (43 (bipush 6))
                                      (45 (newarray DOUBLE))
                                      (47 (dup))
                                      (48 (iconst_0))
                                      (49 (ldc2_w 2))     ;; DOUBLE:: "0.22385762508460333"
                                      (52 (dastore))
                                      (53 (dup))
                                      (54 (iconst_1))
                                      (55 (dconst_1))
                                      (56 (dastore))
                                      (57 (dup))
                                      (58 (iconst_2))
                                      (59 (dconst_0))
                                      (60 (dastore))
                                      (61 (dup))
                                      (62 (iconst_3))
                                      (63 (ldc2_w 1))     ;; DOUBLE:: "0.7761423749153966"
                                      (66 (dastore))
                                      (67 (dup))
                                      (68 (iconst_4))
                                      (69 (dconst_0))
                                      (70 (dastore))
                                      (71 (dup))
                                      (72 (iconst_5))
                                      (73 (ldc2_w 4))     ;; DOUBLE:: "0.5"
                                      (76 (dastore))
                                      (77 (aastore))
                                      (78 (dup))
                                      (79 (iconst_2))
                                      (80 (bipush 6))
                                      (82 (newarray DOUBLE))
                                      (84 (dup))
                                      (85 (iconst_0))
                                      (86 (dconst_0))
                                      (87 (dastore))
                                      (88 (dup))
                                      (89 (iconst_1))
                                      (90 (ldc2_w 2))     ;; DOUBLE:: "0.22385762508460333"
                                      (93 (dastore))
                                      (94 (dup))
                                      (95 (iconst_2))
                                      (96 (ldc2_w 2))     ;; DOUBLE:: "0.22385762508460333"
                                      (99 (dastore))
                                      (100 (dup))
                                      (101 (iconst_3))
                                      (102 (dconst_0))
                                      (103 (dastore))
                                      (104 (dup))
                                      (105 (iconst_4))
                                      (106 (ldc2_w 4))    ;; DOUBLE:: "0.5"
                                      (109 (dastore))
                                      (110 (dup))
                                      (111 (iconst_5))
                                      (112 (dconst_0))
                                      (113 (dastore))
                                      (114 (aastore))
                                      (115 (dup))
                                      (116 (iconst_3))
                                      (117 (bipush 6))
                                      (119 (newarray DOUBLE))
                                      (121 (dup))
                                      (122 (iconst_0))
                                      (123 (ldc2_w 1))    ;; DOUBLE:: "0.7761423749153966"
                                      (126 (dastore))
                                      (127 (dup))
                                      (128 (iconst_1))
                                      (129 (dconst_0))
                                      (130 (dastore))
                                      (131 (dup))
                                      (132 (iconst_2))
                                      (133 (dconst_1))
                                      (134 (dastore))
                                      (135 (dup))
                                      (136 (iconst_3))
                                      (137 (ldc2_w 2))    ;; DOUBLE:: "0.22385762508460333"
                                      (140 (dastore))
                                      (141 (dup))
                                      (142 (iconst_4))
                                      (143 (dconst_1))
                                      (144 (dastore))
                                      (145 (dup))
                                      (146 (iconst_5))
                                      (147 (ldc2_w 4))    ;; DOUBLE:: "0.5"
                                      (150 (dastore))
                                      (151 (aastore))
                                      (152 (putstatic (fieldCP "ctrlpts" "java.awt.geom.EllipseIterator" (array (array double)))))
                                      (155 (return))
                                      (endofcode 156))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.awt.geom.PathIterator")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *EllipseIterator-class-table*
  (make-static-class-decls 
   *java.awt.geom.EllipseIterator*))

(defconst *package-name-map* 
  ("java.awt.geom.EllipseIterator" . "java.awt.geom"))

