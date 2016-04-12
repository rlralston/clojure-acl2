; BandCombineOp-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:28 CDT 2014.
;

(defconst *java.awt.image.BandCombineOp*
 (make-class-def
      '(class "java.awt.image.BandCombineOp"
            "java.lang.Object"
            (constant_pool
                        (STRING  "row ")
                        (STRING  " too short")
                        (STRING  "Number of columns in the matrix (")
                        (STRING  ") must be equal to the number")
                        (STRING  " of bands ([+1]) in src (")
                        (STRING  ").")
                        (STRING  "Number of rows in the matrix (")
                        (STRING  " of bands ([+1]) in dst (")
                        (STRING  "Don\nt know how to create a  compatible Raster with ")
                        (STRING  " bands."))
            (fields
                        (field "matrix" (array (array float)) (accessflags  *class* ) -1)
                        (field "nrows" int (accessflags  *class* ) -1)
                        (field "ncols" int (accessflags  *class* ) -1)
                        (field "hints" (class "java.awt.RenderingHints") (accessflags  *class* ) -1))
            (methods
                        (method "<init>"
                              (parameters (array (array float)) (class "java.awt.RenderingHints"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 4) (code_length . 120)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokespecial (methodCP "<init>" "java.lang.Object" () void))) 
                                      (4 (aload_0)) 
                                      (5 (iconst_0)) 
                                      (6 (putfield (fieldCP "nrows" "java.awt.image.BandCombineOp" int))) 
                                      (9 (aload_0)) 
                                      (10 (iconst_0)) 
                                      (11 (putfield (fieldCP "ncols" "java.awt.image.BandCombineOp" int))) 
                                      (14 (aload_0)) 
                                      (15 (aload_1)) 
                                      (16 (arraylength)) 
                                      (17 (putfield (fieldCP "nrows" "java.awt.image.BandCombineOp" int))) 
                                      (20 (aload_0)) 
                                      (21 (aload_1)) 
                                      (22 (iconst_0)) 
                                      (23 (aaload)) 
                                      (24 (arraylength)) 
                                      (25 (putfield (fieldCP "ncols" "java.awt.image.BandCombineOp" int))) 
                                      (28 (aload_0)) 
                                      (29 (aload_0)) 
                                      (30 (getfield (fieldCP "nrows" "java.awt.image.BandCombineOp" int))) 
                                      (33 (anewarray (array float))) 
                                      (36 (putfield (fieldCP "matrix" "java.awt.image.BandCombineOp" (array (array float))))) 
                                      (39 (iconst_0)) 
                                      (40 (istore_3)) 
                                      (41 (iload_3)) ;;at TAG_2
                                      (42 (aload_0)) 
                                      (43 (getfield (fieldCP "nrows" "java.awt.image.BandCombineOp" int))) 
                                      (46 (if_icmpge 114)) ;;to TAG_0
                                      (49 (aload_0)) 
                                      (50 (getfield (fieldCP "ncols" "java.awt.image.BandCombineOp" int))) 
                                      (53 (aload_1)) 
                                      (54 (iload_3)) 
                                      (55 (aaload)) 
                                      (56 (arraylength)) 
                                      (57 (if_icmple 92)) ;;to TAG_1
                                      (60 (new (class "java.lang.IndexOutOfBoundsException"))) 
                                      (63 (dup)) 
                                      (64 (new (class "java.lang.StringBuilder"))) 
                                      (67 (dup)) 
                                      (68 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (71 (ldc 0)) ;;STRING:: "row "
                                      (73 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (76 (iload_3)) 
                                      (77 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (int) (class "java.lang.StringBuilder")))) 
                                      (80 (ldc 1)) ;;STRING:: " too short"
                                      (82 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (85 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (88 (invokespecial (methodCP "<init>" "java.lang.IndexOutOfBoundsException" ((class "java.lang.String")) void))) 
                                      (91 (athrow)) 
                                      (92 (aload_0)) ;;at TAG_1
                                      (93 (getfield (fieldCP "matrix" "java.awt.image.BandCombineOp" (array (array float))))) 
                                      (96 (iload_3)) 
                                      (97 (aload_1)) 
                                      (98 (iload_3)) 
                                      (99 (aaload)) 
                                      (100 (aload_0)) 
                                      (101 (getfield (fieldCP "ncols" "java.awt.image.BandCombineOp" int))) 
                                      (104 (invokestatic (methodCP "copyOf" "java.util.Arrays" ((array float) int) (array float)))) 
                                      (107 (aastore)) 
                                      (108 (iinc 3 1)) 
                                      (111 (goto 41))  ;;to TAG_2
                                      (114 (aload_0)) ;;at TAG_0
                                      (115 (aload_2)) 
                                      (116 (putfield (fieldCP "hints" "java.awt.image.BandCombineOp" (class "java.awt.RenderingHints")))) 
                                      (119 (return)) 
                                      (endofcode 120))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getMatrix"
                              (parameters )
                              (returntype . (array (array float)))
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 42)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "nrows" "java.awt.image.BandCombineOp" int))) 
                                      (4 (anewarray (array float))) 
                                      (7 (astore_1)) 
                                      (8 (iconst_0)) 
                                      (9 (istore_2)) 
                                      (10 (iload_2)) ;;at TAG_1
                                      (11 (aload_0)) 
                                      (12 (getfield (fieldCP "nrows" "java.awt.image.BandCombineOp" int))) 
                                      (15 (if_icmpge 40))  ;;to TAG_0
                                      (18 (aload_1)) 
                                      (19 (iload_2)) 
                                      (20 (aload_0)) 
                                      (21 (getfield (fieldCP "matrix" "java.awt.image.BandCombineOp" (array (array float))))) 
                                      (24 (iload_2)) 
                                      (25 (aaload)) 
                                      (26 (aload_0)) 
                                      (27 (getfield (fieldCP "ncols" "java.awt.image.BandCombineOp" int))) 
                                      (30 (invokestatic (methodCP "copyOf" "java.util.Arrays" ((array float) int) (array float)))) 
                                      (33 (aastore)) 
                                      (34 (iinc 2 1)) 
                                      (37 (goto 10)) ;;to TAG_1
                                      (40 (aload_1)) ;;at TAG_0
                                      (41 (areturn)) 
                                      (endofcode 42))
                                   (Exceptions )
                                   (StackMap )))
                        (method "filter"
                              (parameters (class "java.awt.image.Raster") (class "java.awt.image.WritableRaster"))
                              (returntype . (class "java.awt.image.WritableRaster"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 17) (code_length . 505)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (invokevirtual (methodCP "getNumBands" "java.awt.image.Raster" () int))) 
                                      (4 (istore_3)) 
                                      (5 (aload_0)) 
                                      (6 (getfield (fieldCP "ncols" "java.awt.image.BandCombineOp" int))) 
                                      (9 (iload_3)) 
                                      (10 (if_icmpeq 72)) ;;to TAG_0
                                      (13 (aload_0)) 
                                      (14 (getfield (fieldCP "ncols" "java.awt.image.BandCombineOp" int))) 
                                      (17 (iload_3)) 
                                      (18 (iconst_1)) 
                                      (19 (iadd)) 
                                      (20 (if_icmpeq 72)) ;;to TAG_0
                                      (23 (new (class "java.lang.IllegalArgumentException"))) 
                                      (26 (dup)) 
                                      (27 (new (class "java.lang.StringBuilder"))) 
                                      (30 (dup)) 
                                      (31 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (34 (ldc 2)) ;;STRING:: "Number of columns in the matrix ("
                                      (36 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (39 (aload_0)) 
                                      (40 (getfield (fieldCP "ncols" "java.awt.image.BandCombineOp" int))) 
                                      (43 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (int) (class "java.lang.StringBuilder")))) 
                                      (46 (ldc 3)) ;;STRING:: ") must be equal to the number"
                                      (48 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (51 (ldc 4)) ;;STRING:: " of bands ([+1]) in src ("
                                      (53 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (56 (iload_3)) 
                                      (57 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (int) (class "java.lang.StringBuilder")))) 
                                      (60 (ldc 5)) ;;STRING:: ")."
                                      (62 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (65 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (68 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (71 (athrow)) 
                                      (72 (aload_2)) ;;at TAG_0
                                      (73 (ifnonnull 85)) ;;to TAG_1
                                      (76 (aload_0)) 
                                      (77 (aload_1)) 
                                      (78 (invokevirtual (methodCP "createCompatibleDestRaster" "java.awt.image.BandCombineOp" ((class "java.awt.image.Raster")) (class "java.awt.image.WritableRaster")))) 
                                      (81 (astore_2)) 
                                      (82 (goto 145)) ;;to TAG_2
                                      (85 (aload_0)) ;;at TAG_1
                                      (86 (getfield (fieldCP "nrows" "java.awt.image.BandCombineOp" int))) 
                                      (89 (aload_2)) 
                                      (90 (invokevirtual (methodCP "getNumBands" "java.awt.image.WritableRaster" () int))) 
                                      (93 (if_icmpeq 145)) ;;to TAG_2
                                      (96 (new (class "java.lang.IllegalArgumentException"))) 
                                      (99 (dup)) 
                                      (100 (new (class "java.lang.StringBuilder"))) 
                                      (103 (dup)) 
                                      (104 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (107 (ldc 6)) ;;STRING:: "Number of rows in the matrix ("
                                      (109 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (112 (aload_0)) 
                                      (113 (getfield (fieldCP "nrows" "java.awt.image.BandCombineOp" int))) 
                                      (116 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (int) (class "java.lang.StringBuilder")))) 
                                      (119 (ldc 3)) ;;STRING:: ") must be equal to the number"
                                      (121 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (124 (ldc 7)) ;;STRING:: " of bands ([+1]) in dst ("
                                      (126 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (129 (iload_3)) 
                                      (130 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (int) (class "java.lang.StringBuilder")))) 
                                      (133 (ldc 5)) ;;STRING:: ")."
                                      (135 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (138 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (141 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (144 (athrow)) 
                                      (145 (aload_0)) ;;at TAG_2
                                      (146 (aload_1)) 
                                      (147 (aload_2)) 
                                      (148 (invokestatic (methodCP "filter" "sun.awt.image.ImagingLib" ((class "java.awt.image.RasterOp") (class "java.awt.image.Raster") (class "java.awt.image.WritableRaster")) (class "java.awt.image.WritableRaster")))) 
                                      (151 (ifnull 156)) ;;to TAG_3
                                      (154 (aload_2)) 
                                      (155 (areturn)) 
                                      (156 (aconst_null)) ;;at TAG_3
                                      (157 (astore 4)) 
                                      (159 (aload_2)) 
                                      (160 (invokevirtual (methodCP "getNumBands" "java.awt.image.WritableRaster" () int))) 
                                      (163 (newarray INT)) 
                                      (165 (astore 5)) 
                                      (167 (aload_1)) 
                                      (168 (invokevirtual (methodCP "getMinX" "java.awt.image.Raster" () int))) 
                                      (171 (istore 7)) 
                                      (173 (aload_1)) 
                                      (174 (invokevirtual (methodCP "getMinY" "java.awt.image.Raster" () int))) 
                                      (177 (istore 8)) 
                                      (179 (aload_2)) 
                                      (180 (invokevirtual (methodCP "getMinX" "java.awt.image.WritableRaster" () int))) 
                                      (183 (istore 9)) 
                                      (185 (aload_2)) 
                                      (186 (invokevirtual (methodCP "getMinY" "java.awt.image.WritableRaster" () int))) 
                                      (189 (istore 10)) 
                                      (191 (aload_0)) 
                                      (192 (getfield (fieldCP "ncols" "java.awt.image.BandCombineOp" int))) 
                                      (195 (iload_3)) 
                                      (196 (if_icmpne 349)) ;;to TAG_4
                                      (199 (iconst_0)) 
                                      (200 (istore 13)) 
                                      (202 (iload 13)) ;;at TAG_12
                                      (204 (aload_1)) 
                                      (205 (invokevirtual (methodCP "getHeight" "java.awt.image.Raster" () int))) 
                                      (208 (if_icmpge 346))  ;;to TAG_5
                                      (211 (iload 9)) 
                                      (213 (istore 12)) 
                                      (215 (iload 7)) 
                                      (217 (istore 11)) 
                                      (219 (iconst_0)) 
                                      (220 (istore 14)) 
                                      (222 (iload 14)) ;;at TAG_11
                                      (224 (aload_1)) 
                                      (225 (invokevirtual (methodCP "getWidth" "java.awt.image.Raster" () int))) 
                                      (228 (if_icmpge 334)) ;;to TAG_6
                                      (231 (aload_1)) 
                                      (232 (iload 11)) 
                                      (234 (iload 8)) 
                                      (236 (aload 4)) 
                                      (238 (invokevirtual (methodCP "getPixel" "java.awt.image.Raster" (int int (array int)) (array int)))) 
                                      (241 (astore 4)) 
                                      (243 (iconst_0)) 
                                      (244 (istore 15)) 
                                      (246 (iload 15)) ;;at TAG_10
                                      (248 (aload_0)) 
                                      (249 (getfield (fieldCP "nrows" "java.awt.image.BandCombineOp" int))) 
                                      (252 (if_icmpge 312)) ;;to TAG_7
                                      (255 (fconst_0)) 
                                      (256 (fstore 6)) 
                                      (258 (iconst_0)) 
                                      (259 (istore 16)) 
                                      (261 (iload 16)) ;;at TAG_9
                                      (263 (aload_0)) 
                                      (264 (getfield (fieldCP "ncols" "java.awt.image.BandCombineOp" int))) 
                                      (267 (if_icmpge 298)) ;;to TAG_8
                                      (270 (fload 6)) 
                                      (272 (aload_0)) 
                                      (273 (getfield (fieldCP "matrix" "java.awt.image.BandCombineOp" (array (array float))))) 
                                      (276 (iload 15)) 
                                      (278 (aaload)) 
                                      (279 (iload 16)) 
                                      (281 (faload)) 
                                      (282 (aload 4)) 
                                      (284 (iload 16)) 
                                      (286 (iaload)) 
                                      (287 (i2f)) 
                                      (288 (fmul)) 
                                      (289 (fadd)) 
                                      (290 (fstore 6)) 
                                      (292 (iinc 16 1)) 
                                      (295 (goto 261)) ;;to TAG_9
                                      (298 (aload 5)) ;;at TAG_8
                                      (300 (iload 15)) 
                                      (302 (fload 6)) 
                                      (304 (f2i)) 
                                      (305 (iastore)) 
                                      (306 (iinc 15 1)) 
                                      (309 (goto 246)) ;;to TAG_10
                                      (312 (aload_2)) ;;at TAG_7
                                      (313 (iload 12)) 
                                      (315 (iload 10)) 
                                      (317 (aload 5)) 
                                      (319 (invokevirtual (methodCP "setPixel" "java.awt.image.WritableRaster" (int int (array int)) void))) 
                                      (322 (iinc 14 1)) 
                                      (325 (iinc 11 1)) 
                                      (328 (iinc 12 1)) 
                                      (331 (goto 222)) ;;to TAG_11
                                      (334 (iinc 13 1)) ;;at TAG_6
                                      (337 (iinc 8 1)) 
                                      (340 (iinc 10 1)) 
                                      (343 (goto 202)) ;;to TAG_12
                                      (346 (goto 503)) ;;to TAG_13;;at TAG_5
                                      (349 (iconst_0)) ;;at TAG_4
                                      (350 (istore 13)) 
                                      (352 (iload 13)) ;;at TAG_20
                                      (354 (aload_1)) 
                                      (355 (invokevirtual (methodCP "getHeight" "java.awt.image.Raster" () int))) 
                                      (358 (if_icmpge 503)) ;;to TAG_13
                                      (361 (iload 9)) 
                                      (363 (istore 12)) 
                                      (365 (iload 7)) 
                                      (367 (istore 11)) 
                                      (369 (iconst_0)) 
                                      (370 (istore 14)) 
                                      (372 (iload 14)) ;;at TAG_19
                                      (374 (aload_1)) 
                                      (375 (invokevirtual (methodCP "getWidth" "java.awt.image.Raster" () int))) 
                                      (378 (if_icmpge 491)) ;;to TAG_14
                                      (381 (aload_1)) 
                                      (382 (iload 11)) 
                                      (384 (iload 8)) 
                                      (386 (aload 4)) 
                                      (388 (invokevirtual (methodCP "getPixel" "java.awt.image.Raster" (int int (array int)) (array int)))) 
                                      (391 (astore 4)) 
                                      (393 (iconst_0)) 
                                      (394 (istore 15)) 
                                      (396 (iload 15)) ;;at TAG_18
                                      (398 (aload_0)) 
                                      (399 (getfield (fieldCP "nrows" "java.awt.image.BandCombineOp" int))) 
                                      (402 (if_icmpge 469)) ;;to TAG_15
                                      (405 (fconst_0)) 
                                      (406 (fstore 6)) 
                                      (408 (iconst_0)) 
                                      (409 (istore 16)) 
                                      (411 (iload 16)) ;;at TAG_17
                                      (413 (iload_3)) 
                                      (414 (if_icmpge 445)) ;;to TAG_16
                                      (417 (fload 6)) 
                                      (419 (aload_0)) 
                                      (420 (getfield (fieldCP "matrix" "java.awt.image.BandCombineOp" (array (array float))))) 
                                      (423 (iload 15)) 
                                      (425 (aaload)) 
                                      (426 (iload 16)) 
                                      (428 (faload)) 
                                      (429 (aload 4)) 
                                      (431 (iload 16)) 
                                      (433 (iaload)) 
                                      (434 (i2f)) 
                                      (435 (fmul)) 
                                      (436 (fadd)) 
                                      (437 (fstore 6)) 
                                      (439 (iinc 16 1)) 
                                      (442 (goto 411)) ;;to TAG_17
                                      (445 (aload 5)) ;;at TAG_16
                                      (447 (iload 15)) 
                                      (449 (fload 6)) 
                                      (451 (aload_0)) 
                                      (452 (getfield (fieldCP "matrix" "java.awt.image.BandCombineOp" (array (array float))))) 
                                      (455 (iload 15)) 
                                      (457 (aaload)) 
                                      (458 (iload_3)) 
                                      (459 (faload)) 
                                      (460 (fadd)) 
                                      (461 (f2i)) 
                                      (462 (iastore)) 
                                      (463 (iinc 15 1)) 
                                      (466 (goto 396)) ;;to TAG_18
                                      (469 (aload_2)) ;;at TAG_15
                                      (470 (iload 12)) 
                                      (472 (iload 10)) 
                                      (474 (aload 5)) 
                                      (476 (invokevirtual (methodCP "setPixel" "java.awt.image.WritableRaster" (int int (array int)) void))) 
                                      (479 (iinc 14 1)) 
                                      (482 (iinc 11 1)) 
                                      (485 (iinc 12 1)) 
                                      (488 (goto 372)) ;;to TAG_19
                                      (491 (iinc 13 1)) ;;at TAG_14
                                      (494 (iinc 8 1)) 
                                      (497 (iinc 10 1)) 
                                      (500 (goto 352)) ;;to TAG_20
                                      (503 (aload_2)) ;;at TAG_13
                                      (504 (areturn)) 
                                      (endofcode 505))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getBounds2D"
                              (parameters (class "java.awt.image.Raster"))
                              (returntype . (class "java.awt.geom.Rectangle2D"))
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (invokevirtual
					(methodCP "getBounds" "java.awt.image.Raster" () (class "java.awt.Rectangle"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "createCompatibleDestRaster"
                              (parameters (class "java.awt.image.Raster"))
                              (returntype . (class "java.awt.image.WritableRaster"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 123)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (invokevirtual (methodCP "getNumBands" "java.awt.image.Raster" () int))) 
                                      (4 (istore_2)) 
                                      (5 (aload_0)) 
                                      (6 (getfield (fieldCP "ncols" "java.awt.image.BandCombineOp" int))) 
                                      (9 (iload_2)) 
                                      (10 (if_icmpeq 72))  ;;to TAG_0
                                      (13 (aload_0)) 
                                      (14 (getfield (fieldCP "ncols" "java.awt.image.BandCombineOp" int))) 
                                      (17 (iload_2)) 
                                      (18 (iconst_1)) 
                                      (19 (iadd)) 
                                      (20 (if_icmpeq 72))  ;;to TAG_0
                                      (23 (new (class "java.lang.IllegalArgumentException"))) 
                                      (26 (dup)) 
                                      (27 (new (class "java.lang.StringBuilder"))) 
                                      (30 (dup)) 
                                      (31 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (34 (ldc 2)) ;;STRING:: "Number of columns in the matrix ("
                                      (36 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (39 (aload_0)) 
                                      (40 (getfield (fieldCP "ncols" "java.awt.image.BandCombineOp" int))) 
                                      (43 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (int) (class "java.lang.StringBuilder")))) 
                                      (46 (ldc 3)) ;;STRING:: ") must be equal to the number"
                                      (48 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (51 (ldc 4)) ;;STRING:: " of bands ([+1]) in src ("
                                      (53 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (56 (iload_2)) 
                                      (57 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (int) (class "java.lang.StringBuilder")))) 
                                      (60 (ldc 5)) ;;STRING:: ")."
                                      (62 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (65 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (68 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (71 (athrow)) 
                                      (72 (aload_1)) ;;at TAG_0
                                      (73 (invokevirtual (methodCP "getNumBands" "java.awt.image.Raster" () int))) 
                                      (76 (aload_0)) 
                                      (77 (getfield (fieldCP "nrows" "java.awt.image.BandCombineOp" int))) 
                                      (80 (if_icmpne 88)) ;;to TAG_1
                                      (83 (aload_1)) 
                                      (84 (invokevirtual (methodCP "createCompatibleWritableRaster" "java.awt.image.Raster" () (class "java.awt.image.WritableRaster")))) 
                                      (87 (areturn)) 
                                      (88 (new (class "java.lang.IllegalArgumentException"))) ;;at TAG_1
                                      (91 (dup)) 
                                      (92 (new (class "java.lang.StringBuilder"))) 
                                      (95 (dup)) 
                                      (96 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (99 (ldc 8)) ;;STRING:: "Don\nt know how to create a  compatible Raster with "
                                      (101 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (104 (aload_0)) 
                                      (105 (getfield (fieldCP "nrows" "java.awt.image.BandCombineOp" int))) 
                                      (108 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (int) (class "java.lang.StringBuilder")))) 
                                      (111 (ldc 9)) ;;STRING:: " bands."
                                      (113 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (116 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (119 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (122 (athrow)) 
                                      (endofcode 123))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getPoint2D"
                              (parameters (class "java.awt.geom.Point2D") (class "java.awt.geom.Point2D"))
                              (returntype . (class "java.awt.geom.Point2D"))
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 3) (code_length . 26)
                                   (parsedcode
                                      (0 (aload_2)) 
                                      (1 (ifnonnull 12))  ;;to TAG_0
                                      (4 (new (class "java.awt.geom.Point2D$Float"))) 
                                      (7 (dup)) 
                                      (8 (invokespecial (methodCP "<init>" "java.awt.geom.Point2D$Float" () void))) 
                                      (11 (astore_2)) 
                                      (12 (aload_2)) ;;at TAG_0
                                      (13 (aload_1)) 
                                      (14 (invokevirtual (methodCP "getX" "java.awt.geom.Point2D" () double))) 
                                      (17 (aload_1)) 
                                      (18 (invokevirtual (methodCP "getY" "java.awt.geom.Point2D" () double))) 
                                      (21 (invokevirtual (methodCP "setLocation" "java.awt.geom.Point2D" (double double) void))) 
                                      (24 (aload_2)) 
                                      (25 (areturn)) 
                                      (endofcode 26))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getRenderingHints"
                              (parameters )
                              (returntype . (class "java.awt.RenderingHints"))
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "hints" "java.awt.image.BandCombineOp" (class "java.awt.RenderingHints"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.awt.image.RasterOp")
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *BandCombineOp-class-table*
  (make-static-class-decls 
   *java.awt.image.BandCombineOp*))

(defconst *package-name-map* 
  ("java.awt.image.BandCombineOp" . "java.awt.image"))

