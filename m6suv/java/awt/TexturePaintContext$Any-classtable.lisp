; TexturePaintContext$Any-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:30 CDT 2014.
;

(defconst *java.awt.TexturePaintContext$Any*
 (make-class-def
      '(class "java.awt.TexturePaintContext$Any"
            "java.awt.TexturePaintContext"
            (constant_pool
                        (INT 2147483647))
            (fields
                        (field "srcRas" (class "java.awt.image.WritableRaster") (accessflags  *class* ) -1)
                        (field "filter" boolean (accessflags  *class* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.awt.image.WritableRaster") (class "java.awt.image.ColorModel") (class "java.awt.geom.AffineTransform") int boolean)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 6) (code_length . 28)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_2))
                                      (2 (aload_3))
                                      (3 (aload_1))
                                      (4 (invokevirtual
					(methodCP "getWidth" "java.awt.image.WritableRaster" () int)))
                                      (7 (aload_1))
                                      (8 (invokevirtual
					(methodCP "getHeight" "java.awt.image.WritableRaster" () int)))
                                      (11 (iload 4))
                                      (13 (invokespecial
					(methodCP "<init>" "java.awt.TexturePaintContext" ((class "java.awt.image.ColorModel") (class "java.awt.geom.AffineTransform") int int int) void)))
                                      (16 (aload_0))
                                      (17 (aload_1))
                                      (18 (putfield (fieldCP "srcRas" "java.awt.TexturePaintContext$Any" (class "java.awt.image.WritableRaster"))))
                                      (21 (aload_0))
                                      (22 (iload 5))
                                      (24 (putfield (fieldCP "filter" "java.awt.TexturePaintContext$Any" boolean)))
                                      (27 (return))
                                      (endofcode 28))
                                   (Exceptions )
                                   (StackMap )))
                        (method "makeRaster"
                              (parameters int int)
                              (returntype . (class "java.awt.image.WritableRaster"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 14)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "colorModel" "java.awt.TexturePaintContext$Any" (class "java.awt.image.ColorModel"))))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "srcRas" "java.awt.TexturePaintContext$Any" (class "java.awt.image.WritableRaster"))))
                                      (8 (iload_1))
                                      (9 (iload_2))
                                      (10 (invokestatic
					(methodCP "makeRaster" "java.awt.TexturePaintContext$Any" ((class "java.awt.image.ColorModel") (class "java.awt.image.Raster") int int) (class "java.awt.image.WritableRaster"))))
                                      (13 (areturn))
                                      (endofcode 14))
                                   (Exceptions )
                                   (StackMap )))
                        (method "setRaster"
                              (parameters int int int int int int int int int int int int int int int int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 30) (code_length . 411)
                                   (parsedcode
                                      (0 (aconst_null)) 
                                      (1 (astore 17)) 
                                      (3 (iload_1)) 
                                      (4 (istore 18)) 
                                      (6 (iload_2)) 
                                      (7 (istore 19)) 
                                      (9 (iload_3)) 
                                      (10 (istore 20)) 
                                      (12 (iload 4)) 
                                      (14 (istore 21)) 
                                      (16 (aload_0)) 
                                      (17 (getfield (fieldCP "srcRas" "java.awt.TexturePaintContext$Any" (class "java.awt.image.WritableRaster")))) 
                                      (20 (astore 22)) 
                                      (22 (aload_0)) 
                                      (23 (getfield (fieldCP "outRas" "java.awt.TexturePaintContext$Any" (class "java.awt.image.WritableRaster")))) 
                                      (26 (astore 23)) 
                                      (28 (aload_0)) 
                                      (29 (getfield (fieldCP "filter" "java.awt.TexturePaintContext$Any" boolean))) 
                                      (32 (ifeq 41)) ;;to TAG_0
                                      (35 (iconst_4)) 
                                      (36 (newarray INT)) 
                                      (38 (goto 42))  ;;to TAG_1
                                      (41 (aconst_null)) ;;at TAG_0
                                      (42 (astore 24)) ;;at TAG_1
                                      (44 (iconst_0)) 
                                      (45 (istore 25)) 
                                      (47 (iload 25)) ;;at TAG_16
                                      (49 (iload 6)) 
                                      (51 (if_icmpge 410)) ;;to TAG_2
                                      (54 (iload 18)) 
                                      (56 (istore_1)) 
                                      (57 (iload 19)) 
                                      (59 (istore_2)) 
                                      (60 (iload 20)) 
                                      (62 (istore_3)) 
                                      (63 (iload 21)) 
                                      (65 (istore 4)) 
                                      (67 (iconst_0)) 
                                      (68 (istore 26)) 
                                      (70 (iload 26)) ;;at TAG_11
                                      (72 (iload 5)) 
                                      (74 (if_icmpge 322)) ;;to TAG_3
                                      (77 (aload 22)) 
                                      (79 (iload_1)) 
                                      (80 (iload_2)) 
                                      (81 (aload 17)) 
                                      (83 (invokevirtual (methodCP "getDataElements" "java.awt.image.WritableRaster" (int int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (86 (astore 17)) 
                                      (88 (aload_0)) 
                                      (89 (getfield (fieldCP "filter" "java.awt.TexturePaintContext$Any" boolean))) 
                                      (92 (ifeq 235)) ;;to TAG_4
                                      (95 (iload_1)) 
                                      (96 (iconst_1)) 
                                      (97 (iadd)) 
                                      (98 (dup)) 
                                      (99 (istore 27)) 
                                      (101 (iload 7)) 
                                      (103 (if_icmplt 109)) ;;to TAG_5
                                      (106 (iconst_0)) 
                                      (107 (istore 27)) 
                                      (109 (iload_2)) ;;at TAG_5
                                      (110 (iconst_1)) 
                                      (111 (iadd)) 
                                      (112 (dup)) 
                                      (113 (istore 28)) 
                                      (115 (iload 8)) 
                                      (117 (if_icmplt 123)) ;;to TAG_6
                                      (120 (iconst_0)) 
                                      (121 (istore 28)) 
                                      (123 (aload 24)) ;;at TAG_6
                                      (125 (iconst_0)) 
                                      (126 (aload_0)) 
                                      (127 (getfield (fieldCP "colorModel" "java.awt.TexturePaintContext$Any" (class "java.awt.image.ColorModel")))) 
                                      (130 (aload 17)) 
                                      (132 (invokevirtual (methodCP "getRGB" "java.awt.image.ColorModel" ((class "java.lang.Object")) int))) 
                                      (135 (iastore)) 
                                      (136 (aload 22)) 
                                      (138 (iload 27)) 
                                      (140 (iload_2)) 
                                      (141 (aload 17)) 
                                      (143 (invokevirtual (methodCP "getDataElements" "java.awt.image.WritableRaster" (int int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (146 (astore 17)) 
                                      (148 (aload 24)) 
                                      (150 (iconst_1)) 
                                      (151 (aload_0)) 
                                      (152 (getfield (fieldCP "colorModel" "java.awt.TexturePaintContext$Any" (class "java.awt.image.ColorModel")))) 
                                      (155 (aload 17)) 
                                      (157 (invokevirtual (methodCP "getRGB" "java.awt.image.ColorModel" ((class "java.lang.Object")) int))) 
                                      (160 (iastore)) 
                                      (161 (aload 22)) 
                                      (163 (iload_1)) 
                                      (164 (iload 28)) 
                                      (166 (aload 17)) 
                                      (168 (invokevirtual (methodCP "getDataElements" "java.awt.image.WritableRaster" (int int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (171 (astore 17)) 
                                      (173 (aload 24)) 
                                      (175 (iconst_2)) 
                                      (176 (aload_0)) 
                                      (177 (getfield (fieldCP "colorModel" "java.awt.TexturePaintContext$Any" (class "java.awt.image.ColorModel")))) 
                                      (180 (aload 17)) 
                                      (182 (invokevirtual (methodCP "getRGB" "java.awt.image.ColorModel" ((class "java.lang.Object")) int))) 
                                      (185 (iastore)) 
                                      (186 (aload 22)) 
                                      (188 (iload 27)) 
                                      (190 (iload 28)) 
                                      (192 (aload 17)) 
                                      (194 (invokevirtual (methodCP "getDataElements" "java.awt.image.WritableRaster" (int int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (197 (astore 17)) 
                                      (199 (aload 24)) 
                                      (201 (iconst_3)) 
                                      (202 (aload_0)) 
                                      (203 (getfield (fieldCP "colorModel" "java.awt.TexturePaintContext$Any" (class "java.awt.image.ColorModel")))) 
                                      (206 (aload 17)) 
                                      (208 (invokevirtual (methodCP "getRGB" "java.awt.image.ColorModel" ((class "java.lang.Object")) int))) 
                                      (211 (iastore)) 
                                      (212 (aload 24)) 
                                      (214 (iload_3)) 
                                      (215 (iload 4)) 
                                      (217 (invokestatic (methodCP "blend" "java.awt.TexturePaintContext" ((array int) int int) int))) 
                                      (220 (istore 29)) 
                                      (222 (aload_0)) 
                                      (223 (getfield (fieldCP "colorModel" "java.awt.TexturePaintContext$Any" (class "java.awt.image.ColorModel")))) 
                                      (226 (iload 29)) 
                                      (228 (aload 17)) 
                                      (230 (invokevirtual (methodCP "getDataElements" "java.awt.image.ColorModel" (int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (233 (astore 17)) 
                                      (235 (aload 23)) ;;at TAG_4
                                      (237 (iload 26)) 
                                      (239 (iload 25)) 
                                      (241 (aload 17)) 
                                      (243 (invokevirtual (methodCP "setDataElements" "java.awt.image.WritableRaster" (int int (class "java.lang.Object")) void))) 
                                      (246 (iload_3)) 
                                      (247 (iload 10)) 
                                      (249 (iadd)) 
                                      (250 (dup)) 
                                      (251 (istore_3)) 
                                      (252 (ifge 263)) ;;to TAG_7
                                      (255 (iload_3)) 
                                      (256 (ldc 0)) ;;INT:: "2147483647"
                                      (258 (iand)) 
                                      (259 (istore_3)) 
                                      (260 (iinc 1 1)) 
                                      (263 (iload_1)) ;;at TAG_7
                                      (264 (iload 9)) 
                                      (266 (iadd)) 
                                      (267 (dup)) 
                                      (268 (istore_1)) 
                                      (269 (iload 7)) 
                                      (271 (if_icmplt 279)) ;;to TAG_8
                                      (274 (iload_1)) 
                                      (275 (iload 7)) 
                                      (277 (isub)) 
                                      (278 (istore_1)) 
                                      (279 (iload 4)) ;;at TAG_8
                                      (281 (iload 12)) 
                                      (283 (iadd)) 
                                      (284 (dup)) 
                                      (285 (istore 4)) 
                                      (287 (ifge 300)) ;;to TAG_9
                                      (290 (iload 4)) 
                                      (292 (ldc 0)) ;;INT:: "2147483647"
                                      (294 (iand)) 
                                      (295 (istore 4)) 
                                      (297 (iinc 2 1)) 
                                      (300 (iload_2)) ;;at TAG_9
                                      (301 (iload 11)) 
                                      (303 (iadd)) 
                                      (304 (dup)) 
                                      (305 (istore_2)) 
                                      (306 (iload 8)) 
                                      (308 (if_icmplt 316)) ;;to TAG_10
                                      (311 (iload_2)) 
                                      (312 (iload 8)) 
                                      (314 (isub)) 
                                      (315 (istore_2)) 
                                      (316 (iinc 26 1)) ;;at TAG_10
                                      (319 (goto 70)) ;;to TAG_11
                                      (322 (iload 20)) ;;at TAG_3
                                      (324 (iload 14)) 
                                      (326 (iadd)) 
                                      (327 (dup)) 
                                      (328 (istore 20)) 
                                      (330 (ifge 343)) ;;to TAG_12
                                      (333 (iload 20)) 
                                      (335 (ldc 0)) ;;INT:: "2147483647"
                                      (337 (iand)) 
                                      (338 (istore 20)) 
                                      (340 (iinc 18 1)) 
                                      (343 (iload 18)) ;;at TAG_12
                                      (345 (iload 13)) 
                                      (347 (iadd)) 
                                      (348 (dup)) 
                                      (349 (istore 18)) 
                                      (351 (iload 7)) 
                                      (353 (if_icmplt 363)) ;;to TAG_13
                                      (356 (iload 18)) 
                                      (358 (iload 7)) 
                                      (360 (isub)) 
                                      (361 (istore 18)) 
                                      (363 (iload 21)) ;;at TAG_13
                                      (365 (iload 16)) 
                                      (367 (iadd)) 
                                      (368 (dup)) 
                                      (369 (istore 21)) 
                                      (371 (ifge 384)) ;;to TAG_14
                                      (374 (iload 21)) 
                                      (376 (ldc 0)) ;;INT:: "2147483647"
                                      (378 (iand)) 
                                      (379 (istore 21)) 
                                      (381 (iinc 19 1)) 
                                      (384 (iload 19)) ;;at TAG_14
                                      (386 (iload 15)) 
                                      (388 (iadd)) 
                                      (389 (dup)) 
                                      (390 (istore 19)) 
                                      (392 (iload 8)) 
                                      (394 (if_icmplt 404)) ;;to TAG_15
                                      (397 (iload 19)) 
                                      (399 (iload 8)) 
                                      (401 (isub)) 
                                      (402 (istore 19)) 
                                      (404 (iinc 25 1)) ;;at TAG_15
                                      (407 (goto 47)) ;;to TAG_16
                                      (410 (return)) ;;at TAG_2
                                      (endofcode 411))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *TexturePaintContext$Any-class-table*
  (make-static-class-decls 
   *java.awt.TexturePaintContext$Any*))

(defconst *package-name-map* 
  ("java.awt.TexturePaintContext$Any" . "java.awt"))
