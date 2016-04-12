; Formatter$FormatSpecifier$BigDecimalLayout-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:45 CDT 2014.
;

(defconst *java.util.Formatter$FormatSpecifier$BigDecimalLayout*
 (make-class-def
      '(class "java.util.Formatter$FormatSpecifier$BigDecimalLayout"
            "java.lang.Object"
            (constant_pool
                        (STRING  "+")
                        (STRING  "0")
                        (STRING  "+00")
                        (STRING  "0.")
                        (LONG 10))
            (fields
                        (field "mant" (class "java.lang.StringBuilder") (accessflags  *class*  *private* ) -1)
                        (field "exp" (class "java.lang.StringBuilder") (accessflags  *class*  *private* ) -1)
                        (field "dot" boolean (accessflags  *class*  *private* ) -1)
                        (field "scale" int (accessflags  *class*  *private* ) -1)
                        (field "this$1" (class "java.util.Formatter$FormatSpecifier") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.Formatter$FormatSpecifier") (class "java.math.BigInteger") int (class "java.util.Formatter$BigDecimalLayoutForm"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 5) (code_length . 23)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$1" "java.util.Formatter$FormatSpecifier$BigDecimalLayout" (class "java.util.Formatter$FormatSpecifier"))))
                                      (5 (aload_0))
                                      (6 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (9 (aload_0))
                                      (10 (iconst_0))
                                      (11 (putfield (fieldCP "dot" "java.util.Formatter$FormatSpecifier$BigDecimalLayout" boolean)))
                                      (14 (aload_0))
                                      (15 (aload_2))
                                      (16 (iload_3))
                                      (17 (aload 4))
                                      (19 (invokespecial
					(methodCP "layout" "java.util.Formatter$FormatSpecifier$BigDecimalLayout" ((class "java.math.BigInteger") int (class "java.util.Formatter$BigDecimalLayoutForm")) void)))
                                      (22 (return))
                                      (endofcode 23))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hasDot"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "dot" "java.util.Formatter$FormatSpecifier$BigDecimalLayout" boolean)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "scale"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "scale" "java.util.Formatter$FormatSpecifier$BigDecimalLayout" int)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "layoutChars"
                              (parameters )
                              (returntype . (array char))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 41)
                                   (parsedcode
                                      (0 (new (class "java.lang.StringBuilder"))) 
                                      (3 (dup)) 
                                      (4 (aload_0)) 
                                      (5 (getfield (fieldCP "mant" "java.util.Formatter$FormatSpecifier$BigDecimalLayout" (class "java.lang.StringBuilder")))) 
                                      (8 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" ((class "java.lang.CharSequence")) void))) 
                                      (11 (astore_1)) 
                                      (12 (aload_0)) 
                                      (13 (getfield (fieldCP "exp" "java.util.Formatter$FormatSpecifier$BigDecimalLayout" (class "java.lang.StringBuilder")))) 
                                      (16 (ifnull 35))  ;;to TAG_0
                                      (19 (aload_1)) 
                                      (20 (bipush 69)) 
                                      (22 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (char) (class "java.lang.StringBuilder")))) 
                                      (25 (pop)) 
                                      (26 (aload_1)) 
                                      (27 (aload_0)) 
                                      (28 (getfield (fieldCP "exp" "java.util.Formatter$FormatSpecifier$BigDecimalLayout" (class "java.lang.StringBuilder")))) 
                                      (31 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.CharSequence")) (class "java.lang.StringBuilder")))) 
                                      (34 (pop)) 
                                      (35 (aload_0)) ;;at TAG_0
                                      (36 (aload_1)) 
                                      (37 (invokespecial (methodCP "toCharArray" "java.util.Formatter$FormatSpecifier$BigDecimalLayout" ((class "java.lang.StringBuilder")) (array char)))) 
                                      (40 (areturn)) 
                                      (endofcode 41))
                                   (Exceptions )
                                   (StackMap )))
                        (method "mantissa"
                              (parameters )
                              (returntype . (array char))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_0))
                                      (2 (getfield (fieldCP "mant" "java.util.Formatter$FormatSpecifier$BigDecimalLayout" (class "java.lang.StringBuilder"))))
                                      (5 (invokespecial
					(methodCP "toCharArray" "java.util.Formatter$FormatSpecifier$BigDecimalLayout" ((class "java.lang.StringBuilder")) (array char))))
                                      (8 (areturn))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "exponent"
                              (parameters )
                              (returntype . (array char))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_0))
                                      (2 (getfield (fieldCP "exp" "java.util.Formatter$FormatSpecifier$BigDecimalLayout" (class "java.lang.StringBuilder"))))
                                      (5 (invokespecial
					(methodCP "toCharArray" "java.util.Formatter$FormatSpecifier$BigDecimalLayout" ((class "java.lang.StringBuilder")) (array char))))
                                      (8 (areturn))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "toCharArray"
                              (parameters (class "java.lang.StringBuilder"))
                              (returntype . (array char))
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 5) (max_locals . 3) (code_length . 24)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (ifnonnull 6))  ;;to TAG_0
                                      (4 (aconst_null)) 
                                      (5 (areturn)) 
                                      (6 (aload_1)) ;;at TAG_0
                                      (7 (invokevirtual (methodCP "length" "java.lang.StringBuilder" () int))) 
                                      (10 (newarray CHAR)) 
                                      (12 (astore_2)) 
                                      (13 (aload_1)) 
                                      (14 (iconst_0)) 
                                      (15 (aload_2)) 
                                      (16 (arraylength)) 
                                      (17 (aload_2)) 
                                      (18 (iconst_0)) 
                                      (19 (invokevirtual (methodCP "getChars" "java.lang.StringBuilder" (int int (array char) int) void))) 
                                      (22 (aload_2)) 
                                      (23 (areturn)) 
                                      (endofcode 24))
                                   (Exceptions )
                                   (StackMap )))
                        (method "layout"
                              (parameters (class "java.math.BigInteger") int (class "java.util.Formatter$BigDecimalLayoutForm"))
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 5) (max_locals . 9) (code_length . 522)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (invokevirtual (methodCP "toString" "java.math.BigInteger" () (class "java.lang.String")))) 
                                      (4 (invokevirtual (methodCP "toCharArray" "java.lang.String" () (array char)))) 
                                      (7 (astore 4)) 
                                      (9 (aload_0)) 
                                      (10 (iload_2)) 
                                      (11 (putfield (fieldCP "scale" "java.util.Formatter$FormatSpecifier$BigDecimalLayout" int))) 
                                      (14 (aload_0)) 
                                      (15 (new (class "java.lang.StringBuilder"))) 
                                      (18 (dup)) 
                                      (19 (aload 4)) 
                                      (21 (arraylength)) 
                                      (22 (bipush 14)) 
                                      (24 (iadd)) 
                                      (25 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" (int) void))) 
                                      (28 (putfield (fieldCP "mant" "java.util.Formatter$FormatSpecifier$BigDecimalLayout" (class "java.lang.StringBuilder")))) 
                                      (31 (iload_2)) 
                                      (32 (ifne 199)) ;;to TAG_0
                                      (35 (aload 4)) 
                                      (37 (arraylength)) 
                                      (38 (istore 5)) 
                                      (40 (iload 5)) 
                                      (42 (iconst_1)) 
                                      (43 (if_icmple 168)) ;;to TAG_1
                                      (46 (aload_0)) 
                                      (47 (getfield (fieldCP "mant" "java.util.Formatter$FormatSpecifier$BigDecimalLayout" (class "java.lang.StringBuilder")))) 
                                      (50 (aload 4)) 
                                      (52 (iconst_0)) 
                                      (53 (caload)) 
                                      (54 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (char) (class "java.lang.StringBuilder")))) 
                                      (57 (pop)) 
                                      (58 (aload_3)) 
                                      (59 (getstatic (fieldCP "SCIENTIFIC" "java.util.Formatter$BigDecimalLayoutForm" (class "java.util.Formatter$BigDecimalLayoutForm")))) 
                                      (62 (if_acmpne 150)) ;;to TAG_2
                                      (65 (aload_0)) 
                                      (66 (getfield (fieldCP "mant" "java.util.Formatter$FormatSpecifier$BigDecimalLayout" (class "java.lang.StringBuilder")))) 
                                      (69 (bipush 46)) 
                                      (71 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (char) (class "java.lang.StringBuilder")))) 
                                      (74 (pop)) 
                                      (75 (aload_0)) 
                                      (76 (iconst_1)) 
                                      (77 (putfield (fieldCP "dot" "java.util.Formatter$FormatSpecifier$BigDecimalLayout" boolean))) 
                                      (80 (aload_0)) 
                                      (81 (getfield (fieldCP "mant" "java.util.Formatter$FormatSpecifier$BigDecimalLayout" (class "java.lang.StringBuilder")))) 
                                      (84 (aload 4)) 
                                      (86 (iconst_1)) 
                                      (87 (iload 5)) 
                                      (89 (iconst_1)) 
                                      (90 (isub)) 
                                      (91 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((array char) int int) (class "java.lang.StringBuilder")))) 
                                      (94 (pop)) 
                                      (95 (aload_0)) 
                                      (96 (new (class "java.lang.StringBuilder"))) 
                                      (99 (dup)) 
                                      (100 (ldc 0)) ;;STRING:: "+"
                                      (102 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" ((class "java.lang.String")) void))) 
                                      (105 (putfield (fieldCP "exp" "java.util.Formatter$FormatSpecifier$BigDecimalLayout" (class "java.lang.StringBuilder")))) 
                                      (108 (iload 5)) 
                                      (110 (bipush 10)) 
                                      (112 (if_icmpge 135)) ;;to TAG_3
                                      (115 (aload_0)) 
                                      (116 (getfield (fieldCP "exp" "java.util.Formatter$FormatSpecifier$BigDecimalLayout" (class "java.lang.StringBuilder")))) 
                                      (119 (ldc 1)) ;;STRING:: "0"
                                      (121 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (124 (iload 5)) 
                                      (126 (iconst_1)) 
                                      (127 (isub)) 
                                      (128 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (int) (class "java.lang.StringBuilder")))) 
                                      (131 (pop)) 
                                      (132 (goto 198)) ;;to TAG_4
                                      (135 (aload_0)) ;;at TAG_3
                                      (136 (getfield (fieldCP "exp" "java.util.Formatter$FormatSpecifier$BigDecimalLayout" (class "java.lang.StringBuilder")))) 
                                      (139 (iload 5)) 
                                      (141 (iconst_1)) 
                                      (142 (isub)) 
                                      (143 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (int) (class "java.lang.StringBuilder")))) 
                                      (146 (pop)) 
                                      (147 (goto 198)) ;;to TAG_4
                                      (150 (aload_0)) ;;at TAG_2
                                      (151 (getfield (fieldCP "mant" "java.util.Formatter$FormatSpecifier$BigDecimalLayout" (class "java.lang.StringBuilder")))) 
                                      (154 (aload 4)) 
                                      (156 (iconst_1)) 
                                      (157 (iload 5)) 
                                      (159 (iconst_1)) 
                                      (160 (isub)) 
                                      (161 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((array char) int int) (class "java.lang.StringBuilder")))) 
                                      (164 (pop)) 
                                      (165 (goto 198)) ;;to TAG_4
                                      (168 (aload_0)) ;;at TAG_1
                                      (169 (getfield (fieldCP "mant" "java.util.Formatter$FormatSpecifier$BigDecimalLayout" (class "java.lang.StringBuilder")))) 
                                      (172 (aload 4)) 
                                      (174 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((array char)) (class "java.lang.StringBuilder")))) 
                                      (177 (pop)) 
                                      (178 (aload_3)) 
                                      (179 (getstatic (fieldCP "SCIENTIFIC" "java.util.Formatter$BigDecimalLayoutForm" (class "java.util.Formatter$BigDecimalLayoutForm")))) 
                                      (182 (if_acmpne 198)) ;;to TAG_4
                                      (185 (aload_0)) 
                                      (186 (new (class "java.lang.StringBuilder"))) 
                                      (189 (dup)) 
                                      (190 (ldc 2)) ;;STRING:: "+00"
                                      (192 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" ((class "java.lang.String")) void))) 
                                      (195 (putfield (fieldCP "exp" "java.util.Formatter$FormatSpecifier$BigDecimalLayout" (class "java.lang.StringBuilder")))) 
                                      (198 (return)) ;;at TAG_4
                                      (199 (iload_2)) ;;at TAG_0
                                      (200 (i2l)) 
                                      (201 (lneg)) 
                                      (202 (aload 4)) 
                                      (204 (arraylength)) 
                                      (205 (iconst_1)) 
                                      (206 (isub)) 
                                      (207 (i2l)) 
                                      (208 (ladd)) 
                                      (209 (lstore 5)) 
                                      (211 (aload_3)) 
                                      (212 (getstatic (fieldCP "DECIMAL_FLOAT" "java.util.Formatter$BigDecimalLayoutForm" (class "java.util.Formatter$BigDecimalLayoutForm")))) 
                                      (215 (if_acmpne 382))  ;;to TAG_5
                                      (218 (iload_2)) 
                                      (219 (aload 4)) 
                                      (221 (arraylength)) 
                                      (222 (isub)) 
                                      (223 (istore 7)) 
                                      (225 (iload 7)) 
                                      (227 (iflt 279)) ;;to TAG_6
                                      (230 (aload_0)) 
                                      (231 (getfield (fieldCP "mant" "java.util.Formatter$FormatSpecifier$BigDecimalLayout" (class "java.lang.StringBuilder")))) 
                                      (234 (ldc 3)) ;;STRING:: "0."
                                      (236 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (239 (pop)) 
                                      (240 (aload_0)) 
                                      (241 (iconst_1)) 
                                      (242 (putfield (fieldCP "dot" "java.util.Formatter$FormatSpecifier$BigDecimalLayout" boolean))) 
                                      (245 (iload 7)) ;;at TAG_8
                                      (247 (ifle 266)) ;;to TAG_7
                                      (250 (aload_0)) 
                                      (251 (getfield (fieldCP "mant" "java.util.Formatter$FormatSpecifier$BigDecimalLayout" (class "java.lang.StringBuilder")))) 
                                      (254 (bipush 48)) 
                                      (256 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (char) (class "java.lang.StringBuilder")))) 
                                      (259 (pop)) 
                                      (260 (iinc 7 -1)) 
                                      (263 (goto 245)) ;;to TAG_8
                                      (266 (aload_0)) ;;at TAG_7
                                      (267 (getfield (fieldCP "mant" "java.util.Formatter$FormatSpecifier$BigDecimalLayout" (class "java.lang.StringBuilder")))) 
                                      (270 (aload 4)) 
                                      (272 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((array char)) (class "java.lang.StringBuilder")))) 
                                      (275 (pop)) 
                                      (276 (goto 379)) ;;to TAG_9
                                      (279 (iload 7)) ;;at TAG_6
                                      (281 (ineg)) 
                                      (282 (aload 4)) 
                                      (284 (arraylength)) 
                                      (285 (if_icmpge 334)) ;;to TAG_10
                                      (288 (aload_0)) 
                                      (289 (getfield (fieldCP "mant" "java.util.Formatter$FormatSpecifier$BigDecimalLayout" (class "java.lang.StringBuilder")))) 
                                      (292 (aload 4)) 
                                      (294 (iconst_0)) 
                                      (295 (iload 7)) 
                                      (297 (ineg)) 
                                      (298 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((array char) int int) (class "java.lang.StringBuilder")))) 
                                      (301 (pop)) 
                                      (302 (aload_0)) 
                                      (303 (getfield (fieldCP "mant" "java.util.Formatter$FormatSpecifier$BigDecimalLayout" (class "java.lang.StringBuilder")))) 
                                      (306 (bipush 46)) 
                                      (308 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (char) (class "java.lang.StringBuilder")))) 
                                      (311 (pop)) 
                                      (312 (aload_0)) 
                                      (313 (iconst_1)) 
                                      (314 (putfield (fieldCP "dot" "java.util.Formatter$FormatSpecifier$BigDecimalLayout" boolean))) 
                                      (317 (aload_0)) 
                                      (318 (getfield (fieldCP "mant" "java.util.Formatter$FormatSpecifier$BigDecimalLayout" (class "java.lang.StringBuilder")))) 
                                      (321 (aload 4)) 
                                      (323 (iload 7)) 
                                      (325 (ineg)) 
                                      (326 (iload_2)) 
                                      (327 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((array char) int int) (class "java.lang.StringBuilder")))) 
                                      (330 (pop)) 
                                      (331 (goto 379)) ;;to TAG_9
                                      (334 (aload_0)) ;;at TAG_10
                                      (335 (getfield (fieldCP "mant" "java.util.Formatter$FormatSpecifier$BigDecimalLayout" (class "java.lang.StringBuilder")))) 
                                      (338 (aload 4)) 
                                      (340 (iconst_0)) 
                                      (341 (aload 4)) 
                                      (343 (arraylength)) 
                                      (344 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((array char) int int) (class "java.lang.StringBuilder")))) 
                                      (347 (pop)) 
                                      (348 (iconst_0)) 
                                      (349 (istore 8)) 
                                      (351 (iload 8)) ;;at TAG_12
                                      (353 (iload_2)) 
                                      (354 (ineg)) 
                                      (355 (if_icmpge 374)) ;;to TAG_11
                                      (358 (aload_0)) 
                                      (359 (getfield (fieldCP "mant" "java.util.Formatter$FormatSpecifier$BigDecimalLayout" (class "java.lang.StringBuilder")))) 
                                      (362 (bipush 48)) 
                                      (364 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (char) (class "java.lang.StringBuilder")))) 
                                      (367 (pop)) 
                                      (368 (iinc 8 1)) 
                                      (371 (goto 351)) ;;to TAG_12
                                      (374 (aload_0)) ;;at TAG_11
                                      (375 (iconst_0)) 
                                      (376 (putfield (fieldCP "scale" "java.util.Formatter$FormatSpecifier$BigDecimalLayout" int))) 
                                      (379 (goto 521)) ;;to TAG_13;;at TAG_9
                                      (382 (aload_0)) ;;at TAG_5
                                      (383 (getfield (fieldCP "mant" "java.util.Formatter$FormatSpecifier$BigDecimalLayout" (class "java.lang.StringBuilder")))) 
                                      (386 (aload 4)) 
                                      (388 (iconst_0)) 
                                      (389 (caload)) 
                                      (390 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (char) (class "java.lang.StringBuilder")))) 
                                      (393 (pop)) 
                                      (394 (aload 4)) 
                                      (396 (arraylength)) 
                                      (397 (iconst_1)) 
                                      (398 (if_icmple 432)) ;;to TAG_14
                                      (401 (aload_0)) 
                                      (402 (getfield (fieldCP "mant" "java.util.Formatter$FormatSpecifier$BigDecimalLayout" (class "java.lang.StringBuilder")))) 
                                      (405 (bipush 46)) 
                                      (407 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (char) (class "java.lang.StringBuilder")))) 
                                      (410 (pop)) 
                                      (411 (aload_0)) 
                                      (412 (iconst_1)) 
                                      (413 (putfield (fieldCP "dot" "java.util.Formatter$FormatSpecifier$BigDecimalLayout" boolean))) 
                                      (416 (aload_0)) 
                                      (417 (getfield (fieldCP "mant" "java.util.Formatter$FormatSpecifier$BigDecimalLayout" (class "java.lang.StringBuilder")))) 
                                      (420 (aload 4)) 
                                      (422 (iconst_1)) 
                                      (423 (aload 4)) 
                                      (425 (arraylength)) 
                                      (426 (iconst_1)) 
                                      (427 (isub)) 
                                      (428 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((array char) int int) (class "java.lang.StringBuilder")))) 
                                      (431 (pop)) 
                                      (432 (aload_0)) ;;at TAG_14
                                      (433 (new (class "java.lang.StringBuilder"))) 
                                      (436 (dup)) 
                                      (437 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (440 (putfield (fieldCP "exp" "java.util.Formatter$FormatSpecifier$BigDecimalLayout" (class "java.lang.StringBuilder")))) 
                                      (443 (lload 5)) 
                                      (445 (lconst_0)) 
                                      (446 (lcmp)) 
                                      (447 (ifeq 511)) ;;to TAG_15
                                      (450 (lload 5)) 
                                      (452 (invokestatic (methodCP "abs" "java.lang.Math" (long) long))) 
                                      (455 (lstore 7)) 
                                      (457 (aload_0)) 
                                      (458 (getfield (fieldCP "exp" "java.util.Formatter$FormatSpecifier$BigDecimalLayout" (class "java.lang.StringBuilder")))) 
                                      (461 (lload 5)) 
                                      (463 (lconst_0)) 
                                      (464 (lcmp)) 
                                      (465 (ifge 473)) ;;to TAG_16
                                      (468 (bipush 45)) 
                                      (470 (goto 475)) ;;to TAG_17
                                      (473 (bipush 43)) ;;at TAG_16
                                      (475 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (char) (class "java.lang.StringBuilder")))) ;;at TAG_17
                                      (478 (pop)) 
                                      (479 (lload 7)) 
                                      (481 (ldc2_w 4)) ;; LONG:: "10"
                                      (484 (lcmp)) 
                                      (485 (ifge 498)) ;;to TAG_18
                                      (488 (aload_0)) 
                                      (489 (getfield (fieldCP "exp" "java.util.Formatter$FormatSpecifier$BigDecimalLayout" (class "java.lang.StringBuilder")))) 
                                      (492 (bipush 48)) 
                                      (494 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (char) (class "java.lang.StringBuilder")))) 
                                      (497 (pop)) 
                                      (498 (aload_0)) ;;at TAG_18
                                      (499 (getfield (fieldCP "exp" "java.util.Formatter$FormatSpecifier$BigDecimalLayout" (class "java.lang.StringBuilder")))) 
                                      (502 (lload 7)) 
                                      (504 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (long) (class "java.lang.StringBuilder")))) 
                                      (507 (pop)) 
                                      (508 (goto 521)) ;;to TAG_13
                                      (511 (aload_0)) ;;at TAG_15
                                      (512 (getfield (fieldCP "exp" "java.util.Formatter$FormatSpecifier$BigDecimalLayout" (class "java.lang.StringBuilder")))) 
                                      (515 (ldc 2)) ;;STRING:: "+00"
                                      (517 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (520 (pop)) 
                                      (521 (return)) ;;at TAG_13
                                      (endofcode 522))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *Formatter$FormatSpecifier$BigDecimalLayout-class-table*
  (make-static-class-decls 
   *java.util.Formatter$FormatSpecifier$BigDecimalLayout*))

(defconst *package-name-map* 
  ("java.util.Formatter$FormatSpecifier$BigDecimalLayout" . "java.util"))

