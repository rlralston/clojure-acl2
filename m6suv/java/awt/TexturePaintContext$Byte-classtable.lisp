; TexturePaintContext$Byte-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:30 CDT 2014.
;

(defconst *java.awt.TexturePaintContext$Byte*
 (make-class-def
      '(class "java.awt.TexturePaintContext$Byte"
            "java.awt.TexturePaintContext"
            (constant_pool
                        (INT 2147483647))
            (fields
                        (field "srcRas" (class "sun.awt.image.ByteInterleavedRaster") (accessflags  *class* ) -1)
                        (field "inData" (array byte) (accessflags  *class* ) -1)
                        (field "inOff" int (accessflags  *class* ) -1)
                        (field "inSpan" int (accessflags  *class* ) -1)
                        (field "outData" (array byte) (accessflags  *class* ) -1)
                        (field "outOff" int (accessflags  *class* ) -1)
                        (field "outSpan" int (accessflags  *class* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "sun.awt.image.ByteInterleavedRaster") (class "java.awt.image.ColorModel") (class "java.awt.geom.AffineTransform") int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 5) (code_length . 47)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_2))
                                      (2 (aload_3))
                                      (3 (aload_1))
                                      (4 (invokevirtual
					(methodCP "getWidth" "sun.awt.image.ByteInterleavedRaster" () int)))
                                      (7 (aload_1))
                                      (8 (invokevirtual
					(methodCP "getHeight" "sun.awt.image.ByteInterleavedRaster" () int)))
                                      (11 (iload 4))
                                      (13 (invokespecial
					(methodCP "<init>" "java.awt.TexturePaintContext" ((class "java.awt.image.ColorModel") (class "java.awt.geom.AffineTransform") int int int) void)))
                                      (16 (aload_0))
                                      (17 (aload_1))
                                      (18 (putfield (fieldCP "srcRas" "java.awt.TexturePaintContext$Byte" (class "sun.awt.image.ByteInterleavedRaster"))))
                                      (21 (aload_0))
                                      (22 (aload_1))
                                      (23 (invokevirtual
					(methodCP "getDataStorage" "sun.awt.image.ByteInterleavedRaster" () (array byte))))
                                      (26 (putfield (fieldCP "inData" "java.awt.TexturePaintContext$Byte" (array byte))))
                                      (29 (aload_0))
                                      (30 (aload_1))
                                      (31 (invokevirtual
					(methodCP "getScanlineStride" "sun.awt.image.ByteInterleavedRaster" () int)))
                                      (34 (putfield (fieldCP "inSpan" "java.awt.TexturePaintContext$Byte" int)))
                                      (37 (aload_0))
                                      (38 (aload_1))
                                      (39 (iconst_0))
                                      (40 (invokevirtual
					(methodCP "getDataOffset" "sun.awt.image.ByteInterleavedRaster" (int) int)))
                                      (43 (putfield (fieldCP "inOff" "java.awt.TexturePaintContext$Byte" int)))
                                      (46 (return))
                                      (endofcode 47))
                                   (Exceptions )
                                   (StackMap )))
                        (method "makeRaster"
                              (parameters int int)
                              (returntype . (class "java.awt.image.WritableRaster"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 5) (code_length . 46)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "srcRas" "java.awt.TexturePaintContext$Byte" (class "sun.awt.image.ByteInterleavedRaster"))))
                                      (4 (iload_1))
                                      (5 (iload_2))
                                      (6 (invokestatic
					(methodCP "makeByteRaster" "java.awt.TexturePaintContext$Byte" ((class "java.awt.image.Raster") int int) (class "java.awt.image.WritableRaster"))))
                                      (9 (astore_3))
                                      (10 (aload_3))
                                      (11 (checkcast (class "sun.awt.image.ByteInterleavedRaster")))
                                      (14 (astore 4))
                                      (16 (aload_0))
                                      (17 (aload 4))
                                      (19 (invokevirtual
					(methodCP "getDataStorage" "sun.awt.image.ByteInterleavedRaster" () (array byte))))
                                      (22 (putfield (fieldCP "outData" "java.awt.TexturePaintContext$Byte" (array byte))))
                                      (25 (aload_0))
                                      (26 (aload 4))
                                      (28 (invokevirtual
					(methodCP "getScanlineStride" "sun.awt.image.ByteInterleavedRaster" () int)))
                                      (31 (putfield (fieldCP "outSpan" "java.awt.TexturePaintContext$Byte" int)))
                                      (34 (aload_0))
                                      (35 (aload 4))
                                      (37 (iconst_0))
                                      (38 (invokevirtual
					(methodCP "getDataOffset" "sun.awt.image.ByteInterleavedRaster" (int) int)))
                                      (41 (putfield (fieldCP "outOff" "java.awt.TexturePaintContext$Byte" int)))
                                      (44 (aload_3))
                                      (45 (areturn))
                                      (endofcode 46))
                                   (Exceptions )
                                   (StackMap )))
                        (method "dispose"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "outRas" "java.awt.TexturePaintContext$Byte" (class "java.awt.image.WritableRaster"))))
                                      (4 (invokestatic
					(methodCP "dropByteRaster" "java.awt.TexturePaintContext$Byte" ((class "java.awt.image.Raster")) void)))
                                      (7 (return))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "setRaster"
                              (parameters int int int int int int int int int int int int int int int int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 32) (code_length . 461)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "inData" "java.awt.TexturePaintContext$Byte" (array byte)))) 
                                      (4 (astore 17)) 
                                      (6 (aload_0)) 
                                      (7 (getfield (fieldCP "outData" "java.awt.TexturePaintContext$Byte" (array byte)))) 
                                      (10 (astore 18)) 
                                      (12 (aload_0)) 
                                      (13 (getfield (fieldCP "outOff" "java.awt.TexturePaintContext$Byte" int))) 
                                      (16 (istore 19)) 
                                      (18 (aload_0)) 
                                      (19 (getfield (fieldCP "inSpan" "java.awt.TexturePaintContext$Byte" int))) 
                                      (22 (istore 20)) 
                                      (24 (aload_0)) 
                                      (25 (getfield (fieldCP "inOff" "java.awt.TexturePaintContext$Byte" int))) 
                                      (28 (istore 21)) 
                                      (30 (aload_0)) 
                                      (31 (getfield (fieldCP "outSpan" "java.awt.TexturePaintContext$Byte" int))) 
                                      (34 (istore 22)) 
                                      (36 (iload 9)) 
                                      (38 (iconst_1)) 
                                      (39 (if_icmpne 61)) ;;to TAG_0
                                      (42 (iload 10)) 
                                      (44 (ifne 61)) ;;to TAG_0
                                      (47 (iload 11)) 
                                      (49 (ifne 61)) ;;to TAG_0
                                      (52 (iload 12)) 
                                      (54 (ifne 61)) ;;to TAG_0
                                      (57 (iconst_1)) 
                                      (58 (goto 62)) ;;to TAG_1
                                      (61 (iconst_0)) ;;at TAG_0
                                      (62 (istore 23)) ;;at TAG_1
                                      (64 (iload_1)) 
                                      (65 (istore 24)) 
                                      (67 (iload_2)) 
                                      (68 (istore 25)) 
                                      (70 (iload_3)) 
                                      (71 (istore 26)) 
                                      (73 (iload 4)) 
                                      (75 (istore 27)) 
                                      (77 (iload 23)) 
                                      (79 (ifeq 89)) ;;to TAG_2
                                      (82 (iload 22)) 
                                      (84 (iload 5)) 
                                      (86 (isub)) 
                                      (87 (istore 22)) 
                                      (89 (iconst_0)) ;;at TAG_2
                                      (90 (istore 28)) 
                                      (92 (iload 28)) ;;at TAG_24
                                      (94 (iload 6)) 
                                      (96 (if_icmpge 460)) ;;to TAG_3
                                      (99 (iload 23)) 
                                      (101 (ifeq 246)) ;;to TAG_4
                                      (104 (iload 21)) 
                                      (106 (iload 25)) 
                                      (108 (iload 20)) 
                                      (110 (imul)) 
                                      (111 (iadd)) 
                                      (112 (iload 7)) 
                                      (114 (iadd)) 
                                      (115 (istore 29)) 
                                      (117 (iload 7)) 
                                      (119 (iload 24)) 
                                      (121 (isub)) 
                                      (122 (istore_1)) 
                                      (123 (iload 19)) 
                                      (125 (iload 5)) 
                                      (127 (iadd)) 
                                      (128 (istore 19)) 
                                      (130 (iload 7)) 
                                      (132 (bipush 32)) 
                                      (134 (if_icmplt 203)) ;;to TAG_5
                                      (137 (iload 5)) 
                                      (139 (istore 30)) 
                                      (141 (iload 30)) ;;at TAG_10
                                      (143 (ifle 200)) ;;to TAG_6
                                      (146 (iload 30)) 
                                      (148 (iload_1)) 
                                      (149 (if_icmpge 157)) ;;to TAG_7
                                      (152 (iload 30)) 
                                      (154 (goto 158)) ;;to TAG_8
                                      (157 (iload_1)) ;;at TAG_7
                                      (158 (istore 31)) ;;at TAG_8
                                      (160 (aload 17)) 
                                      (162 (iload 29)) 
                                      (164 (iload_1)) 
                                      (165 (isub)) 
                                      (166 (aload 18)) 
                                      (168 (iload 19)) 
                                      (170 (iload 30)) 
                                      (172 (isub)) 
                                      (173 (iload 31)) 
                                      (175 (invokestatic (methodCP "arraycopy" "java.lang.System" ((class "java.lang.Object") int (class "java.lang.Object") int int) void))) 
                                      (178 (iload 30)) 
                                      (180 (iload 31)) 
                                      (182 (isub)) 
                                      (183 (istore 30)) 
                                      (185 (iload_1)) 
                                      (186 (iload 31)) 
                                      (188 (isub)) 
                                      (189 (dup)) 
                                      (190 (istore_1)) 
                                      (191 (ifne 197)) ;;to TAG_9
                                      (194 (iload 7)) 
                                      (196 (istore_1)) 
                                      (197 (goto 141)) ;;to TAG_10;;at TAG_9
                                      (200 (goto 243)) ;;to TAG_11;;at TAG_6
                                      (203 (iload 5)) ;;at TAG_5
                                      (205 (istore 30)) 
                                      (207 (iload 30)) ;;at TAG_13
                                      (209 (ifle 243)) ;;to TAG_11
                                      (212 (aload 18)) 
                                      (214 (iload 19)) 
                                      (216 (iload 30)) 
                                      (218 (isub)) 
                                      (219 (aload 17)) 
                                      (221 (iload 29)) 
                                      (223 (iload_1)) 
                                      (224 (isub)) 
                                      (225 (baload)) 
                                      (226 (bastore)) 
                                      (227 (iinc 1 -1)) 
                                      (230 (iload_1)) 
                                      (231 (ifne 237)) ;;to TAG_12
                                      (234 (iload 7)) 
                                      (236 (istore_1)) 
                                      (237 (iinc 30 -1)) ;;at TAG_12
                                      (240 (goto 207))  ;;to TAG_13
                                      (243 (goto 365)) ;;to TAG_14;;at TAG_11
                                      (246 (iload 24)) ;;at TAG_4
                                      (248 (istore_1)) 
                                      (249 (iload 25)) 
                                      (251 (istore_2)) 
                                      (252 (iload 26)) 
                                      (254 (istore_3)) 
                                      (255 (iload 27)) 
                                      (257 (istore 4)) 
                                      (259 (iconst_0)) 
                                      (260 (istore 29)) 
                                      (262 (iload 29)) ;;at TAG_19
                                      (264 (iload 5)) 
                                      (266 (if_icmpge 365)) ;;to TAG_14
                                      (269 (aload 18)) 
                                      (271 (iload 19)) 
                                      (273 (iload 29)) 
                                      (275 (iadd)) 
                                      (276 (aload 17)) 
                                      (278 (iload 21)) 
                                      (280 (iload_2)) 
                                      (281 (iload 20)) 
                                      (283 (imul)) 
                                      (284 (iadd)) 
                                      (285 (iload_1)) 
                                      (286 (iadd)) 
                                      (287 (baload)) 
                                      (288 (bastore)) 
                                      (289 (iload_3)) 
                                      (290 (iload 10)) 
                                      (292 (iadd)) 
                                      (293 (dup)) 
                                      (294 (istore_3)) 
                                      (295 (ifge 306)) ;;to TAG_15
                                      (298 (iload_3)) 
                                      (299 (ldc 0)) ;;INT:: "2147483647"
                                      (301 (iand)) 
                                      (302 (istore_3)) 
                                      (303 (iinc 1 1)) 
                                      (306 (iload_1)) ;;at TAG_15
                                      (307 (iload 9)) 
                                      (309 (iadd)) 
                                      (310 (dup)) 
                                      (311 (istore_1)) 
                                      (312 (iload 7)) 
                                      (314 (if_icmplt 322)) ;;to TAG_16
                                      (317 (iload_1)) 
                                      (318 (iload 7)) 
                                      (320 (isub)) 
                                      (321 (istore_1)) 
                                      (322 (iload 4)) ;;at TAG_16
                                      (324 (iload 12)) 
                                      (326 (iadd)) 
                                      (327 (dup)) 
                                      (328 (istore 4)) 
                                      (330 (ifge 343)) ;;to TAG_17
                                      (333 (iload 4)) 
                                      (335 (ldc 0)) ;;INT:: "2147483647"
                                      (337 (iand)) 
                                      (338 (istore 4)) 
                                      (340 (iinc 2 1)) 
                                      (343 (iload_2)) ;;at TAG_17
                                      (344 (iload 11)) 
                                      (346 (iadd)) 
                                      (347 (dup)) 
                                      (348 (istore_2)) 
                                      (349 (iload 8)) 
                                      (351 (if_icmplt 359)) ;;to TAG_18
                                      (354 (iload_2)) 
                                      (355 (iload 8)) 
                                      (357 (isub)) 
                                      (358 (istore_2)) 
                                      (359 (iinc 29 1)) ;;at TAG_18
                                      (362 (goto 262)) ;;to TAG_19
                                      (365 (iload 26)) ;;at TAG_14
                                      (367 (iload 14)) 
                                      (369 (iadd)) 
                                      (370 (dup)) 
                                      (371 (istore 26)) 
                                      (373 (ifge 386)) ;;to TAG_20
                                      (376 (iload 26)) 
                                      (378 (ldc 0)) ;;INT:: "2147483647"
                                      (380 (iand)) 
                                      (381 (istore 26)) 
                                      (383 (iinc 24 1)) 
                                      (386 (iload 24)) ;;at TAG_20
                                      (388 (iload 13)) 
                                      (390 (iadd)) 
                                      (391 (dup)) 
                                      (392 (istore 24)) 
                                      (394 (iload 7)) 
                                      (396 (if_icmplt 406)) ;;to TAG_21
                                      (399 (iload 24)) 
                                      (401 (iload 7)) 
                                      (403 (isub)) 
                                      (404 (istore 24)) 
                                      (406 (iload 27)) ;;at TAG_21
                                      (408 (iload 16)) 
                                      (410 (iadd)) 
                                      (411 (dup)) 
                                      (412 (istore 27)) 
                                      (414 (ifge 427)) ;;to TAG_22
                                      (417 (iload 27)) 
                                      (419 (ldc 0)) ;;INT:: "2147483647"
                                      (421 (iand)) 
                                      (422 (istore 27)) 
                                      (424 (iinc 25 1)) 
                                      (427 (iload 25)) ;;at TAG_22
                                      (429 (iload 15)) 
                                      (431 (iadd)) 
                                      (432 (dup)) 
                                      (433 (istore 25)) 
                                      (435 (iload 8)) 
                                      (437 (if_icmplt 447)) ;;to TAG_23
                                      (440 (iload 25)) 
                                      (442 (iload 8)) 
                                      (444 (isub)) 
                                      (445 (istore 25)) 
                                      (447 (iload 19)) ;;at TAG_23
                                      (449 (iload 22)) 
                                      (451 (iadd)) 
                                      (452 (istore 19)) 
                                      (454 (iinc 28 1)) 
                                      (457 (goto 92)) ;;to TAG_24
                                      (460 (return)) ;;at TAG_3
                                      (endofcode 461))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *TexturePaintContext$Byte-class-table*
  (make-static-class-decls 
   *java.awt.TexturePaintContext$Byte*))

(defconst *package-name-map* 
  ("java.awt.TexturePaintContext$Byte" . "java.awt"))

