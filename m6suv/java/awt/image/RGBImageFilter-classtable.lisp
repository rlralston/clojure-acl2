; RGBImageFilter-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:29 CDT 2014.
;

(defconst *java.awt.image.RGBImageFilter*
 (make-class-def
      '(class "java.awt.image.RGBImageFilter"
            "java.awt.image.ImageFilter"
            (constant_pool)
            (fields
                        (field "origmodel" (class "java.awt.image.ColorModel") (accessflags  *class*  *protected* ) -1)
                        (field "newmodel" (class "java.awt.image.ColorModel") (accessflags  *class*  *protected* ) -1)
                        (field "canFilterIndexColorModel" boolean (accessflags  *class*  *protected* ) -1))
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
					(methodCP "<init>" "java.awt.image.ImageFilter" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "setColorModel"
                              (parameters (class "java.awt.image.ColorModel"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 55)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "canFilterIndexColorModel" "java.awt.image.RGBImageFilter" boolean))) 
                                      (4 (ifeq 42))  ;;to TAG_0
                                      (7 (aload_1)) 
                                      (8 (instanceof (class "java.awt.image.IndexColorModel"))) 
                                      (11 (ifeq 42))  ;;to TAG_0
                                      (14 (aload_0)) 
                                      (15 (aload_1)) 
                                      (16 (checkcast (class "java.awt.image.IndexColorModel"))) 
                                      (19 (invokevirtual (methodCP "filterIndexColorModel" "java.awt.image.RGBImageFilter" ((class "java.awt.image.IndexColorModel")) (class "java.awt.image.IndexColorModel")))) 
                                      (22 (astore_2)) 
                                      (23 (aload_0)) 
                                      (24 (aload_1)) 
                                      (25 (aload_2)) 
                                      (26 (invokevirtual (methodCP "substituteColorModel" "java.awt.image.RGBImageFilter" ((class "java.awt.image.ColorModel") (class "java.awt.image.ColorModel")) void))) 
                                      (29 (aload_0)) 
                                      (30 (getfield (fieldCP "consumer" "java.awt.image.RGBImageFilter" (class "java.awt.image.ImageConsumer")))) 
                                      (33 (aload_2)) 
                                      (34 (invokeinterface (methodCP "setColorModel" "java.awt.image.ImageConsumer" ((class "java.awt.image.ColorModel")) void) 2)) 
                                      (39 (goto 54)) ;;to TAG_1
                                      (42 (aload_0)) ;;at TAG_0
                                      (43 (getfield (fieldCP "consumer" "java.awt.image.RGBImageFilter" (class "java.awt.image.ImageConsumer")))) 
                                      (46 (invokestatic (methodCP "getRGBdefault" "java.awt.image.ColorModel" () (class "java.awt.image.ColorModel")))) 
                                      (49 (invokeinterface (methodCP "setColorModel" "java.awt.image.ImageConsumer" ((class "java.awt.image.ColorModel")) void) 2)) 
                                      (54 (return)) ;;at TAG_1
                                      (endofcode 55))
                                   (Exceptions )
                                   (StackMap )))
                        (method "substituteColorModel"
                              (parameters (class "java.awt.image.ColorModel") (class "java.awt.image.ColorModel"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "origmodel" "java.awt.image.RGBImageFilter" (class "java.awt.image.ColorModel"))))
                                      (5 (aload_0))
                                      (6 (aload_2))
                                      (7 (putfield (fieldCP "newmodel" "java.awt.image.RGBImageFilter" (class "java.awt.image.ColorModel"))))
                                      (10 (return))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "filterIndexColorModel"
                              (parameters (class "java.awt.image.IndexColorModel"))
                              (returntype . (class "java.awt.image.IndexColorModel"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 8) (max_locals . 11) (code_length . 191)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (invokevirtual (methodCP "getMapSize" "java.awt.image.IndexColorModel" () int))) 
                                      (4 (istore_2)) 
                                      (5 (iload_2)) 
                                      (6 (newarray BYTE)) 
                                      (8 (astore_3)) 
                                      (9 (iload_2)) 
                                      (10 (newarray BYTE)) 
                                      (12 (astore 4)) 
                                      (14 (iload_2)) 
                                      (15 (newarray BYTE)) 
                                      (17 (astore 5)) 
                                      (19 (iload_2)) 
                                      (20 (newarray BYTE)) 
                                      (22 (astore 6)) 
                                      (24 (aload_1)) 
                                      (25 (aload_3)) 
                                      (26 (invokevirtual (methodCP "getReds" "java.awt.image.IndexColorModel" ((array byte)) void))) 
                                      (29 (aload_1)) 
                                      (30 (aload 4)) 
                                      (32 (invokevirtual (methodCP "getGreens" "java.awt.image.IndexColorModel" ((array byte)) void))) 
                                      (35 (aload_1)) 
                                      (36 (aload 5)) 
                                      (38 (invokevirtual (methodCP "getBlues" "java.awt.image.IndexColorModel" ((array byte)) void))) 
                                      (41 (aload_1)) 
                                      (42 (aload 6)) 
                                      (44 (invokevirtual (methodCP "getAlphas" "java.awt.image.IndexColorModel" ((array byte)) void))) 
                                      (47 (aload_1)) 
                                      (48 (invokevirtual (methodCP "getTransparentPixel" "java.awt.image.IndexColorModel" () int))) 
                                      (51 (istore 7)) 
                                      (53 (iconst_0)) 
                                      (54 (istore 8)) 
                                      (56 (iconst_0)) 
                                      (57 (istore 9)) 
                                      (59 (iload 9)) ;;at TAG_2
                                      (61 (iload_2)) 
                                      (62 (if_icmpge 146)) ;;to TAG_0
                                      (65 (aload_0)) 
                                      (66 (iconst_m1)) 
                                      (67 (iconst_m1)) 
                                      (68 (aload_1)) 
                                      (69 (iload 9)) 
                                      (71 (invokevirtual (methodCP "getRGB" "java.awt.image.IndexColorModel" (int) int))) 
                                      (74 (invokevirtual (methodCP "filterRGB" "java.awt.image.RGBImageFilter" (int int int) int))) 
                                      (77 (istore 10)) 
                                      (79 (aload 6)) 
                                      (81 (iload 9)) 
                                      (83 (iload 10)) 
                                      (85 (bipush 24)) 
                                      (87 (ishr)) 
                                      (88 (i2b)) 
                                      (89 (bastore)) 
                                      (90 (aload 6)) 
                                      (92 (iload 9)) 
                                      (94 (baload)) 
                                      (95 (iconst_m1)) 
                                      (96 (if_icmpeq 109)) ;;to TAG_1
                                      (99 (iload 9)) 
                                      (101 (iload 7)) 
                                      (103 (if_icmpeq 109)) ;;to TAG_1
                                      (106 (iconst_1)) 
                                      (107 (istore 8)) 
                                      (109 (aload_3)) ;;at TAG_1
                                      (110 (iload 9)) 
                                      (112 (iload 10)) 
                                      (114 (bipush 16)) 
                                      (116 (ishr)) 
                                      (117 (i2b)) 
                                      (118 (bastore)) 
                                      (119 (aload 4)) 
                                      (121 (iload 9)) 
                                      (123 (iload 10)) 
                                      (125 (bipush 8)) 
                                      (127 (ishr)) 
                                      (128 (i2b)) 
                                      (129 (bastore)) 
                                      (130 (aload 5)) 
                                      (132 (iload 9)) 
                                      (134 (iload 10)) 
                                      (136 (iconst_0)) 
                                      (137 (ishr)) 
                                      (138 (i2b)) 
                                      (139 (bastore)) 
                                      (140 (iinc 9 1)) 
                                      (143 (goto 59))  ;;to TAG_2
                                      (146 (iload 8)) ;;at TAG_0
                                      (148 (ifeq 171)) ;;to TAG_3
                                      (151 (new (class "java.awt.image.IndexColorModel"))) 
                                      (154 (dup)) 
                                      (155 (aload_1)) 
                                      (156 (invokevirtual (methodCP "getPixelSize" "java.awt.image.IndexColorModel" () int))) 
                                      (159 (iload_2)) 
                                      (160 (aload_3)) 
                                      (161 (aload 4)) 
                                      (163 (aload 5)) 
                                      (165 (aload 6)) 
                                      (167 (invokespecial (methodCP "<init>" "java.awt.image.IndexColorModel" (int int (array byte) (array byte) (array byte) (array byte)) void))) 
                                      (170 (areturn)) 
                                      (171 (new (class "java.awt.image.IndexColorModel"))) ;;at TAG_3
                                      (174 (dup)) 
                                      (175 (aload_1)) 
                                      (176 (invokevirtual (methodCP "getPixelSize" "java.awt.image.IndexColorModel" () int))) 
                                      (179 (iload_2)) 
                                      (180 (aload_3)) 
                                      (181 (aload 4)) 
                                      (183 (aload 5)) 
                                      (185 (iload 7)) 
                                      (187 (invokespecial (methodCP "<init>" "java.awt.image.IndexColorModel" (int int (array byte) (array byte) (array byte) int) void))) 
                                      (190 (areturn)) 
                                      (endofcode 191))
                                   (Exceptions )
                                   (StackMap )))
                        (method "filterRGBPixels"
                              (parameters int int int int (array int) int int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 9) (max_locals . 11) (code_length . 93)
                                   (parsedcode
                                      (0 (iload 6)) 
                                      (2 (istore 8)) 
                                      (4 (iconst_0)) 
                                      (5 (istore 9)) 
                                      (7 (iload 9)) ;;at TAG_3
                                      (9 (iload 4)) 
                                      (11 (if_icmpge 69)) ;;to TAG_0
                                      (14 (iconst_0)) 
                                      (15 (istore 10)) 
                                      (17 (iload 10)) ;;at TAG_2
                                      (19 (iload_3)) 
                                      (20 (if_icmpge 54)) ;;to TAG_1
                                      (23 (aload 5)) 
                                      (25 (iload 8)) 
                                      (27 (aload_0)) 
                                      (28 (iload_1)) 
                                      (29 (iload 10)) 
                                      (31 (iadd)) 
                                      (32 (iload_2)) 
                                      (33 (iload 9)) 
                                      (35 (iadd)) 
                                      (36 (aload 5)) 
                                      (38 (iload 8)) 
                                      (40 (iaload)) 
                                      (41 (invokevirtual (methodCP "filterRGB" "java.awt.image.RGBImageFilter" (int int int) int))) 
                                      (44 (iastore)) 
                                      (45 (iinc 8 1)) 
                                      (48 (iinc 10 1)) 
                                      (51 (goto 17))  ;;to TAG_2
                                      (54 (iload 8)) ;;at TAG_1
                                      (56 (iload 7)) 
                                      (58 (iload_3)) 
                                      (59 (isub)) 
                                      (60 (iadd)) 
                                      (61 (istore 8)) 
                                      (63 (iinc 9 1)) 
                                      (66 (goto 7)) ;;to TAG_3
                                      (69 (aload_0)) ;;at TAG_0
                                      (70 (getfield (fieldCP "consumer" "java.awt.image.RGBImageFilter" (class "java.awt.image.ImageConsumer")))) 
                                      (73 (iload_1)) 
                                      (74 (iload_2)) 
                                      (75 (iload_3)) 
                                      (76 (iload 4)) 
                                      (78 (invokestatic (methodCP "getRGBdefault" "java.awt.image.ColorModel" () (class "java.awt.image.ColorModel")))) 
                                      (81 (aload 5)) 
                                      (83 (iload 6)) 
                                      (85 (iload 7)) 
                                      (87 (invokeinterface (methodCP "setPixels" "java.awt.image.ImageConsumer" (int int int int (class "java.awt.image.ColorModel") (array int) int int) void) 9)) 
                                      (92 (return)) 
                                      (endofcode 93))
                                   (Exceptions )
                                   (StackMap )))
                        (method "setPixels"
                              (parameters int int int int (class "java.awt.image.ColorModel") (array byte) int int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 9) (max_locals . 13) (code_length . 123)
                                   (parsedcode
                                      (0 (aload 5)) 
                                      (2 (aload_0)) 
                                      (3 (getfield (fieldCP "origmodel" "java.awt.image.RGBImageFilter" (class "java.awt.image.ColorModel")))) 
                                      (6 (if_acmpne 36)) ;;to TAG_0
                                      (9 (aload_0)) 
                                      (10 (getfield (fieldCP "consumer" "java.awt.image.RGBImageFilter" (class "java.awt.image.ImageConsumer")))) 
                                      (13 (iload_1)) 
                                      (14 (iload_2)) 
                                      (15 (iload_3)) 
                                      (16 (iload 4)) 
                                      (18 (aload_0)) 
                                      (19 (getfield (fieldCP "newmodel" "java.awt.image.RGBImageFilter" (class "java.awt.image.ColorModel")))) 
                                      (22 (aload 6)) 
                                      (24 (iload 7)) 
                                      (26 (iload 8)) 
                                      (28 (invokeinterface (methodCP "setPixels" "java.awt.image.ImageConsumer" (int int int int (class "java.awt.image.ColorModel") (array byte) int int) void) 9)) 
                                      (33 (goto 122)) ;;to TAG_1
                                      (36 (iload_3)) ;;at TAG_0
                                      (37 (newarray INT)) 
                                      (39 (astore 9)) 
                                      (41 (iload 7)) 
                                      (43 (istore 10)) 
                                      (45 (iconst_0)) 
                                      (46 (istore 11)) 
                                      (48 (iload 11)) ;;at TAG_4
                                      (50 (iload 4)) 
                                      (52 (if_icmpge 122)) ;;to TAG_1
                                      (55 (iconst_0)) 
                                      (56 (istore 12)) 
                                      (58 (iload 12)) ;;at TAG_3
                                      (60 (iload_3)) 
                                      (61 (if_icmpge 92))  ;;to TAG_2
                                      (64 (aload 9)) 
                                      (66 (iload 12)) 
                                      (68 (aload 5)) 
                                      (70 (aload 6)) 
                                      (72 (iload 10)) 
                                      (74 (baload)) 
                                      (75 (sipush 255)) 
                                      (78 (iand)) 
                                      (79 (invokevirtual (methodCP "getRGB" "java.awt.image.ColorModel" (int) int))) 
                                      (82 (iastore)) 
                                      (83 (iinc 10 1)) 
                                      (86 (iinc 12 1)) 
                                      (89 (goto 58)) ;;to TAG_3
                                      (92 (iload 10)) ;;at TAG_2
                                      (94 (iload 8)) 
                                      (96 (iload_3)) 
                                      (97 (isub)) 
                                      (98 (iadd)) 
                                      (99 (istore 10)) 
                                      (101 (aload_0)) 
                                      (102 (iload_1)) 
                                      (103 (iload_2)) 
                                      (104 (iload 11)) 
                                      (106 (iadd)) 
                                      (107 (iload_3)) 
                                      (108 (iconst_1)) 
                                      (109 (aload 9)) 
                                      (111 (iconst_0)) 
                                      (112 (iload_3)) 
                                      (113 (invokevirtual (methodCP "filterRGBPixels" "java.awt.image.RGBImageFilter" (int int int int (array int) int int) void))) 
                                      (116 (iinc 11 1)) 
                                      (119 (goto 48)) ;;to TAG_4
                                      (122 (return)) ;;at TAG_1
                                      (endofcode 123))
                                   (Exceptions )
                                   (StackMap )))
                        (method "setPixels"
                              (parameters int int int int (class "java.awt.image.ColorModel") (array int) int int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 9) (max_locals . 13) (code_length . 119)
                                   (parsedcode
                                      (0 (aload 5)) 
                                      (2 (aload_0)) 
                                      (3 (getfield (fieldCP "origmodel" "java.awt.image.RGBImageFilter" (class "java.awt.image.ColorModel")))) 
                                      (6 (if_acmpne 36)) ;;to TAG_0
                                      (9 (aload_0)) 
                                      (10 (getfield (fieldCP "consumer" "java.awt.image.RGBImageFilter" (class "java.awt.image.ImageConsumer")))) 
                                      (13 (iload_1)) 
                                      (14 (iload_2)) 
                                      (15 (iload_3)) 
                                      (16 (iload 4)) 
                                      (18 (aload_0)) 
                                      (19 (getfield (fieldCP "newmodel" "java.awt.image.RGBImageFilter" (class "java.awt.image.ColorModel")))) 
                                      (22 (aload 6)) 
                                      (24 (iload 7)) 
                                      (26 (iload 8)) 
                                      (28 (invokeinterface (methodCP "setPixels" "java.awt.image.ImageConsumer" (int int int int (class "java.awt.image.ColorModel") (array int) int int) void) 9)) 
                                      (33 (goto 118)) ;;to TAG_1
                                      (36 (iload_3)) ;;at TAG_0
                                      (37 (newarray INT)) 
                                      (39 (astore 9)) 
                                      (41 (iload 7)) 
                                      (43 (istore 10)) 
                                      (45 (iconst_0)) 
                                      (46 (istore 11)) 
                                      (48 (iload 11)) ;;at TAG_4
                                      (50 (iload 4)) 
                                      (52 (if_icmpge 118)) ;;to TAG_1
                                      (55 (iconst_0)) 
                                      (56 (istore 12)) 
                                      (58 (iload 12)) ;;at TAG_3
                                      (60 (iload_3)) 
                                      (61 (if_icmpge 88))  ;;to TAG_2
                                      (64 (aload 9)) 
                                      (66 (iload 12)) 
                                      (68 (aload 5)) 
                                      (70 (aload 6)) 
                                      (72 (iload 10)) 
                                      (74 (iaload)) 
                                      (75 (invokevirtual (methodCP "getRGB" "java.awt.image.ColorModel" (int) int))) 
                                      (78 (iastore)) 
                                      (79 (iinc 10 1)) 
                                      (82 (iinc 12 1)) 
                                      (85 (goto 58)) ;;to TAG_3
                                      (88 (iload 10)) ;;at TAG_2
                                      (90 (iload 8)) 
                                      (92 (iload_3)) 
                                      (93 (isub)) 
                                      (94 (iadd)) 
                                      (95 (istore 10)) 
                                      (97 (aload_0)) 
                                      (98 (iload_1)) 
                                      (99 (iload_2)) 
                                      (100 (iload 11)) 
                                      (102 (iadd)) 
                                      (103 (iload_3)) 
                                      (104 (iconst_1)) 
                                      (105 (aload 9)) 
                                      (107 (iconst_0)) 
                                      (108 (iload_3)) 
                                      (109 (invokevirtual (methodCP "filterRGBPixels" "java.awt.image.RGBImageFilter" (int int int int (array int) int int) void))) 
                                      (112 (iinc 11 1)) 
                                      (115 (goto 48)) ;;to TAG_4
                                      (118 (return)) ;;at TAG_1
                                      (endofcode 119))
                                   (Exceptions )
                                   (StackMap )))
                        (method "filterRGB"
                              (parameters int int int)
                              (returntype . int)
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces)
            (accessflags  *abstract*  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *RGBImageFilter-class-table*
  (make-static-class-decls 
   *java.awt.image.RGBImageFilter*))

(defconst *package-name-map* 
  ("java.awt.image.RGBImageFilter" . "java.awt.image"))

