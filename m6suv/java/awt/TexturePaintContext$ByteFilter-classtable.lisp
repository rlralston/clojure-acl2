; TexturePaintContext$ByteFilter-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:30 CDT 2014.
;

(defconst *java.awt.TexturePaintContext$ByteFilter*
 (make-class-def
      '(class "java.awt.TexturePaintContext$ByteFilter"
            "java.awt.TexturePaintContext"
            (constant_pool
                        (INT 2147483647))
            (fields
                        (field "srcRas" (class "sun.awt.image.ByteInterleavedRaster") (accessflags  *class* ) -1)
                        (field "inPalette" (array int) (accessflags  *class* ) -1)
                        (field "inData" (array byte) (accessflags  *class* ) -1)
                        (field "inOff" int (accessflags  *class* ) -1)
                        (field "inSpan" int (accessflags  *class* ) -1)
                        (field "outData" (array int) (accessflags  *class* ) -1)
                        (field "outOff" int (accessflags  *class* ) -1)
                        (field "outSpan" int (accessflags  *class* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "sun.awt.image.ByteInterleavedRaster") (class "java.awt.image.ColorModel") (class "java.awt.geom.AffineTransform") int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 5) (code_length . 83)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (aload_2)) 
                                      (2 (invokevirtual (methodCP "getTransparency" "java.awt.image.ColorModel" () int))) 
                                      (5 (iconst_1)) 
                                      (6 (if_icmpne 15))  ;;to TAG_0
                                      (9 (getstatic (fieldCP "xrgbmodel" "java.awt.TexturePaintContext$ByteFilter" (class "java.awt.image.ColorModel")))) 
                                      (12 (goto 18)) ;;to TAG_1
                                      (15 (getstatic (fieldCP "argbmodel" "java.awt.TexturePaintContext$ByteFilter" (class "java.awt.image.ColorModel")))) ;;at TAG_0
                                      (18 (aload_3)) ;;at TAG_1
                                      (19 (aload_1)) 
                                      (20 (invokevirtual (methodCP "getWidth" "sun.awt.image.ByteInterleavedRaster" () int))) 
                                      (23 (aload_1)) 
                                      (24 (invokevirtual (methodCP "getHeight" "sun.awt.image.ByteInterleavedRaster" () int))) 
                                      (27 (iload 4)) 
                                      (29 (invokespecial (methodCP "<init>" "java.awt.TexturePaintContext" ((class "java.awt.image.ColorModel") (class "java.awt.geom.AffineTransform") int int int) void))) 
                                      (32 (aload_0)) 
                                      (33 (sipush 256)) 
                                      (36 (newarray INT)) 
                                      (38 (putfield (fieldCP "inPalette" "java.awt.TexturePaintContext$ByteFilter" (array int)))) 
                                      (41 (aload_2)) 
                                      (42 (checkcast (class "java.awt.image.IndexColorModel"))) 
                                      (45 (aload_0)) 
                                      (46 (getfield (fieldCP "inPalette" "java.awt.TexturePaintContext$ByteFilter" (array int)))) 
                                      (49 (invokevirtual (methodCP "getRGBs" "java.awt.image.IndexColorModel" ((array int)) void))) 
                                      (52 (aload_0)) 
                                      (53 (aload_1)) 
                                      (54 (putfield (fieldCP "srcRas" "java.awt.TexturePaintContext$ByteFilter" (class "sun.awt.image.ByteInterleavedRaster")))) 
                                      (57 (aload_0)) 
                                      (58 (aload_1)) 
                                      (59 (invokevirtual (methodCP "getDataStorage" "sun.awt.image.ByteInterleavedRaster" () (array byte)))) 
                                      (62 (putfield (fieldCP "inData" "java.awt.TexturePaintContext$ByteFilter" (array byte)))) 
                                      (65 (aload_0)) 
                                      (66 (aload_1)) 
                                      (67 (invokevirtual (methodCP "getScanlineStride" "sun.awt.image.ByteInterleavedRaster" () int))) 
                                      (70 (putfield (fieldCP "inSpan" "java.awt.TexturePaintContext$ByteFilter" int))) 
                                      (73 (aload_0)) 
                                      (74 (aload_1)) 
                                      (75 (iconst_0)) 
                                      (76 (invokevirtual (methodCP "getDataOffset" "sun.awt.image.ByteInterleavedRaster" (int) int))) 
                                      (79 (putfield (fieldCP "inOff" "java.awt.TexturePaintContext$ByteFilter" int))) 
                                      (82 (return)) 
                                      (endofcode 83))
                                   (Exceptions )
                                   (StackMap )))
                        (method "makeRaster"
                              (parameters int int)
                              (returntype . (class "java.awt.image.WritableRaster"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 5) (code_length . 47)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "colorModel" "java.awt.TexturePaintContext$ByteFilter" (class "java.awt.image.ColorModel"))))
                                      (4 (aconst_null))
                                      (5 (iload_1))
                                      (6 (iload_2))
                                      (7 (invokestatic
					(methodCP "makeRaster" "java.awt.TexturePaintContext$ByteFilter" ((class "java.awt.image.ColorModel") (class "java.awt.image.Raster") int int) (class "java.awt.image.WritableRaster"))))
                                      (10 (astore_3))
                                      (11 (aload_3))
                                      (12 (checkcast (class "sun.awt.image.IntegerInterleavedRaster")))
                                      (15 (astore 4))
                                      (17 (aload_0))
                                      (18 (aload 4))
                                      (20 (invokevirtual
					(methodCP "getDataStorage" "sun.awt.image.IntegerInterleavedRaster" () (array int))))
                                      (23 (putfield (fieldCP "outData" "java.awt.TexturePaintContext$ByteFilter" (array int))))
                                      (26 (aload_0))
                                      (27 (aload 4))
                                      (29 (invokevirtual
					(methodCP "getScanlineStride" "sun.awt.image.IntegerInterleavedRaster" () int)))
                                      (32 (putfield (fieldCP "outSpan" "java.awt.TexturePaintContext$ByteFilter" int)))
                                      (35 (aload_0))
                                      (36 (aload 4))
                                      (38 (iconst_0))
                                      (39 (invokevirtual
					(methodCP "getDataOffset" "sun.awt.image.IntegerInterleavedRaster" (int) int)))
                                      (42 (putfield (fieldCP "outOff" "java.awt.TexturePaintContext$ByteFilter" int)))
                                      (45 (aload_3))
                                      (46 (areturn))
                                      (endofcode 47))
                                   (Exceptions )
                                   (StackMap )))
                        (method "setRaster"
                              (parameters int int int int int int int int int int int int int int int int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 8) (max_locals . 32) (code_length . 407)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "inData" "java.awt.TexturePaintContext$ByteFilter" (array byte)))) 
                                      (4 (astore 17)) 
                                      (6 (aload_0)) 
                                      (7 (getfield (fieldCP "outData" "java.awt.TexturePaintContext$ByteFilter" (array int)))) 
                                      (10 (astore 18)) 
                                      (12 (aload_0)) 
                                      (13 (getfield (fieldCP "outOff" "java.awt.TexturePaintContext$ByteFilter" int))) 
                                      (16 (istore 19)) 
                                      (18 (aload_0)) 
                                      (19 (getfield (fieldCP "inSpan" "java.awt.TexturePaintContext$ByteFilter" int))) 
                                      (22 (istore 20)) 
                                      (24 (aload_0)) 
                                      (25 (getfield (fieldCP "inOff" "java.awt.TexturePaintContext$ByteFilter" int))) 
                                      (28 (istore 21)) 
                                      (30 (aload_0)) 
                                      (31 (getfield (fieldCP "outSpan" "java.awt.TexturePaintContext$ByteFilter" int))) 
                                      (34 (istore 22)) 
                                      (36 (iload_1)) 
                                      (37 (istore 23)) 
                                      (39 (iload_2)) 
                                      (40 (istore 24)) 
                                      (42 (iload_3)) 
                                      (43 (istore 25)) 
                                      (45 (iload 4)) 
                                      (47 (istore 26)) 
                                      (49 (iconst_4)) 
                                      (50 (newarray INT)) 
                                      (52 (astore 27)) 
                                      (54 (iconst_0)) 
                                      (55 (istore 28)) 
                                      (57 (iload 28)) ;;at TAG_13
                                      (59 (iload 6)) 
                                      (61 (if_icmpge 406)) ;;to TAG_0
                                      (64 (iload 23)) 
                                      (66 (istore_1)) 
                                      (67 (iload 24)) 
                                      (69 (istore_2)) 
                                      (70 (iload 25)) 
                                      (72 (istore_3)) 
                                      (73 (iload 26)) 
                                      (75 (istore 4)) 
                                      (77 (iconst_0)) 
                                      (78 (istore 29)) 
                                      (80 (iload 29)) ;;at TAG_8
                                      (82 (iload 5)) 
                                      (84 (if_icmpge 311))  ;;to TAG_1
                                      (87 (iload_1)) 
                                      (88 (iconst_1)) 
                                      (89 (iadd)) 
                                      (90 (dup)) 
                                      (91 (istore 30)) 
                                      (93 (iload 7)) 
                                      (95 (if_icmplt 101)) ;;to TAG_2
                                      (98 (iconst_0)) 
                                      (99 (istore 30)) 
                                      (101 (iload_2)) ;;at TAG_2
                                      (102 (iconst_1)) 
                                      (103 (iadd)) 
                                      (104 (dup)) 
                                      (105 (istore 31)) 
                                      (107 (iload 8)) 
                                      (109 (if_icmplt 115)) ;;to TAG_3
                                      (112 (iconst_0)) 
                                      (113 (istore 31)) 
                                      (115 (aload 27)) ;;at TAG_3
                                      (117 (iconst_0)) 
                                      (118 (aload_0)) 
                                      (119 (getfield (fieldCP "inPalette" "java.awt.TexturePaintContext$ByteFilter" (array int)))) 
                                      (122 (sipush 255)) 
                                      (125 (aload 17)) 
                                      (127 (iload 21)) 
                                      (129 (iload_1)) 
                                      (130 (iadd)) 
                                      (131 (iload 20)) 
                                      (133 (iload_2)) 
                                      (134 (imul)) 
                                      (135 (iadd)) 
                                      (136 (baload)) 
                                      (137 (iand)) 
                                      (138 (iaload)) 
                                      (139 (iastore)) 
                                      (140 (aload 27)) 
                                      (142 (iconst_1)) 
                                      (143 (aload_0)) 
                                      (144 (getfield (fieldCP "inPalette" "java.awt.TexturePaintContext$ByteFilter" (array int)))) 
                                      (147 (sipush 255)) 
                                      (150 (aload 17)) 
                                      (152 (iload 21)) 
                                      (154 (iload 30)) 
                                      (156 (iadd)) 
                                      (157 (iload 20)) 
                                      (159 (iload_2)) 
                                      (160 (imul)) 
                                      (161 (iadd)) 
                                      (162 (baload)) 
                                      (163 (iand)) 
                                      (164 (iaload)) 
                                      (165 (iastore)) 
                                      (166 (aload 27)) 
                                      (168 (iconst_2)) 
                                      (169 (aload_0)) 
                                      (170 (getfield (fieldCP "inPalette" "java.awt.TexturePaintContext$ByteFilter" (array int)))) 
                                      (173 (sipush 255)) 
                                      (176 (aload 17)) 
                                      (178 (iload 21)) 
                                      (180 (iload_1)) 
                                      (181 (iadd)) 
                                      (182 (iload 20)) 
                                      (184 (iload 31)) 
                                      (186 (imul)) 
                                      (187 (iadd)) 
                                      (188 (baload)) 
                                      (189 (iand)) 
                                      (190 (iaload)) 
                                      (191 (iastore)) 
                                      (192 (aload 27)) 
                                      (194 (iconst_3)) 
                                      (195 (aload_0)) 
                                      (196 (getfield (fieldCP "inPalette" "java.awt.TexturePaintContext$ByteFilter" (array int)))) 
                                      (199 (sipush 255)) 
                                      (202 (aload 17)) 
                                      (204 (iload 21)) 
                                      (206 (iload 30)) 
                                      (208 (iadd)) 
                                      (209 (iload 20)) 
                                      (211 (iload 31)) 
                                      (213 (imul)) 
                                      (214 (iadd)) 
                                      (215 (baload)) 
                                      (216 (iand)) 
                                      (217 (iaload)) 
                                      (218 (iastore)) 
                                      (219 (aload 18)) 
                                      (221 (iload 19)) 
                                      (223 (iload 29)) 
                                      (225 (iadd)) 
                                      (226 (aload 27)) 
                                      (228 (iload_3)) 
                                      (229 (iload 4)) 
                                      (231 (invokestatic (methodCP "blend" "java.awt.TexturePaintContext" ((array int) int int) int))) 
                                      (234 (iastore)) 
                                      (235 (iload_3)) 
                                      (236 (iload 10)) 
                                      (238 (iadd)) 
                                      (239 (dup)) 
                                      (240 (istore_3)) 
                                      (241 (ifge 252)) ;;to TAG_4
                                      (244 (iload_3)) 
                                      (245 (ldc 0)) ;;INT:: "2147483647"
                                      (247 (iand)) 
                                      (248 (istore_3)) 
                                      (249 (iinc 1 1)) 
                                      (252 (iload_1)) ;;at TAG_4
                                      (253 (iload 9)) 
                                      (255 (iadd)) 
                                      (256 (dup)) 
                                      (257 (istore_1)) 
                                      (258 (iload 7)) 
                                      (260 (if_icmplt 268)) ;;to TAG_5
                                      (263 (iload_1)) 
                                      (264 (iload 7)) 
                                      (266 (isub)) 
                                      (267 (istore_1)) 
                                      (268 (iload 4)) ;;at TAG_5
                                      (270 (iload 12)) 
                                      (272 (iadd)) 
                                      (273 (dup)) 
                                      (274 (istore 4)) 
                                      (276 (ifge 289)) ;;to TAG_6
                                      (279 (iload 4)) 
                                      (281 (ldc 0)) ;;INT:: "2147483647"
                                      (283 (iand)) 
                                      (284 (istore 4)) 
                                      (286 (iinc 2 1)) 
                                      (289 (iload_2)) ;;at TAG_6
                                      (290 (iload 11)) 
                                      (292 (iadd)) 
                                      (293 (dup)) 
                                      (294 (istore_2)) 
                                      (295 (iload 8)) 
                                      (297 (if_icmplt 305)) ;;to TAG_7
                                      (300 (iload_2)) 
                                      (301 (iload 8)) 
                                      (303 (isub)) 
                                      (304 (istore_2)) 
                                      (305 (iinc 29 1)) ;;at TAG_7
                                      (308 (goto 80)) ;;to TAG_8
                                      (311 (iload 25)) ;;at TAG_1
                                      (313 (iload 14)) 
                                      (315 (iadd)) 
                                      (316 (dup)) 
                                      (317 (istore 25)) 
                                      (319 (ifge 332)) ;;to TAG_9
                                      (322 (iload 25)) 
                                      (324 (ldc 0)) ;;INT:: "2147483647"
                                      (326 (iand)) 
                                      (327 (istore 25)) 
                                      (329 (iinc 23 1)) 
                                      (332 (iload 23)) ;;at TAG_9
                                      (334 (iload 13)) 
                                      (336 (iadd)) 
                                      (337 (dup)) 
                                      (338 (istore 23)) 
                                      (340 (iload 7)) 
                                      (342 (if_icmplt 352)) ;;to TAG_10
                                      (345 (iload 23)) 
                                      (347 (iload 7)) 
                                      (349 (isub)) 
                                      (350 (istore 23)) 
                                      (352 (iload 26)) ;;at TAG_10
                                      (354 (iload 16)) 
                                      (356 (iadd)) 
                                      (357 (dup)) 
                                      (358 (istore 26)) 
                                      (360 (ifge 373)) ;;to TAG_11
                                      (363 (iload 26)) 
                                      (365 (ldc 0)) ;;INT:: "2147483647"
                                      (367 (iand)) 
                                      (368 (istore 26)) 
                                      (370 (iinc 24 1)) 
                                      (373 (iload 24)) ;;at TAG_11
                                      (375 (iload 15)) 
                                      (377 (iadd)) 
                                      (378 (dup)) 
                                      (379 (istore 24)) 
                                      (381 (iload 8)) 
                                      (383 (if_icmplt 393)) ;;to TAG_12
                                      (386 (iload 24)) 
                                      (388 (iload 8)) 
                                      (390 (isub)) 
                                      (391 (istore 24)) 
                                      (393 (iload 19)) ;;at TAG_12
                                      (395 (iload 22)) 
                                      (397 (iadd)) 
                                      (398 (istore 19)) 
                                      (400 (iinc 28 1)) 
                                      (403 (goto 57)) ;;to TAG_13
                                      (406 (return)) ;;at TAG_0
                                      (endofcode 407))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *TexturePaintContext$ByteFilter-class-table*
  (make-static-class-decls 
   *java.awt.TexturePaintContext$ByteFilter*))

(defconst *package-name-map* 
  ("java.awt.TexturePaintContext$ByteFilter" . "java.awt"))

