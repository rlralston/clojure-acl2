; PatternEntry$Parser-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:42 CDT 2014.
;

(defconst *java.text.PatternEntry$Parser*
 (make-class-def
      '(class "java.text.PatternEntry$Parser"
            "java.lang.Object"
            (constant_pool
                        (STRING  "missing char (=,;<&) : ")
                        (STRING  "Unquoted punctuation character : ")
                        (STRING  "missing chars (=,;<&): "))
            (fields
                        (field "pattern" (class "java.lang.String") (accessflags  *class*  *private* ) -1)
                        (field "i" int (accessflags  *class*  *private* ) -1)
                        (field "newChars" (class "java.lang.StringBuffer") (accessflags  *class*  *private* ) -1)
                        (field "newExtension" (class "java.lang.StringBuffer") (accessflags  *class*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 37)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (new (class "java.lang.StringBuffer")))
                                      (8 (dup))
                                      (9 (invokespecial
					(methodCP "<init>" "java.lang.StringBuffer" () void)))
                                      (12 (putfield (fieldCP "newChars" "java.text.PatternEntry$Parser" (class "java.lang.StringBuffer"))))
                                      (15 (aload_0))
                                      (16 (new (class "java.lang.StringBuffer")))
                                      (19 (dup))
                                      (20 (invokespecial
					(methodCP "<init>" "java.lang.StringBuffer" () void)))
                                      (23 (putfield (fieldCP "newExtension" "java.text.PatternEntry$Parser" (class "java.lang.StringBuffer"))))
                                      (26 (aload_0))
                                      (27 (aload_1))
                                      (28 (putfield (fieldCP "pattern" "java.text.PatternEntry$Parser" (class "java.lang.String"))))
                                      (31 (aload_0))
                                      (32 (iconst_0))
                                      (33 (putfield (fieldCP "i" "java.text.PatternEntry$Parser" int)))
                                      (36 (return))
                                      (endofcode 37))
                                   (Exceptions )
                                   (StackMap )))
                        (method "next"
                              (parameters )
                              (returntype . (class "java.text.PatternEntry"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 7) (max_locals . 5) (code_length . 655)
                                   (parsedcode
                                      (0 (iconst_m1)) 
                                      (1 (istore_1)) 
                                      (2 (aload_0)) 
                                      (3 (getfield (fieldCP "newChars" "java.text.PatternEntry$Parser" (class "java.lang.StringBuffer")))) 
                                      (6 (iconst_0)) 
                                      (7 (invokevirtual (methodCP "setLength" "java.lang.StringBuffer" (int) void))) 
                                      (10 (aload_0)) 
                                      (11 (getfield (fieldCP "newExtension" "java.text.PatternEntry$Parser" (class "java.lang.StringBuffer")))) 
                                      (14 (iconst_0)) 
                                      (15 (invokevirtual (methodCP "setLength" "java.lang.StringBuffer" (int) void))) 
                                      (18 (iconst_1)) 
                                      (19 (istore_2)) 
                                      (20 (iconst_0)) 
                                      (21 (istore_3)) 
                                      (22 (aload_0)) ;;at TAG_27
                                      (23 (getfield (fieldCP "i" "java.text.PatternEntry$Parser" int))) 
                                      (26 (aload_0)) 
                                      (27 (getfield (fieldCP "pattern" "java.text.PatternEntry$Parser" (class "java.lang.String")))) 
                                      (30 (invokevirtual (methodCP "length" "java.lang.String" () int))) 
                                      (33 (if_icmpge 546)) ;;to TAG_0
                                      (36 (aload_0)) 
                                      (37 (getfield (fieldCP "pattern" "java.text.PatternEntry$Parser" (class "java.lang.String")))) 
                                      (40 (aload_0)) 
                                      (41 (getfield (fieldCP "i" "java.text.PatternEntry$Parser" int))) 
                                      (44 (invokevirtual (methodCP "charAt" "java.lang.String" (int) char))) 
                                      (47 (istore 4)) 
                                      (49 (iload_3)) 
                                      (50 (ifeq 118)) ;;to TAG_1
                                      (53 (iload 4)) 
                                      (55 (bipush 39)) 
                                      (57 (if_icmpne 65)) ;;to TAG_2
                                      (60 (iconst_0)) 
                                      (61 (istore_3)) 
                                      (62 (goto 533)) ;;to TAG_3
                                      (65 (aload_0)) ;;at TAG_2
                                      (66 (getfield (fieldCP "newChars" "java.text.PatternEntry$Parser" (class "java.lang.StringBuffer")))) 
                                      (69 (invokevirtual (methodCP "length" "java.lang.StringBuffer" () int))) 
                                      (72 (ifne 88)) ;;to TAG_4
                                      (75 (aload_0)) 
                                      (76 (getfield (fieldCP "newChars" "java.text.PatternEntry$Parser" (class "java.lang.StringBuffer")))) 
                                      (79 (iload 4)) 
                                      (81 (invokevirtual (methodCP "append" "java.lang.StringBuffer" (char) (class "java.lang.StringBuffer")))) 
                                      (84 (pop)) 
                                      (85 (goto 533)) ;;to TAG_3
                                      (88 (iload_2)) ;;at TAG_4
                                      (89 (ifeq 105)) ;;to TAG_5
                                      (92 (aload_0)) 
                                      (93 (getfield (fieldCP "newChars" "java.text.PatternEntry$Parser" (class "java.lang.StringBuffer")))) 
                                      (96 (iload 4)) 
                                      (98 (invokevirtual (methodCP "append" "java.lang.StringBuffer" (char) (class "java.lang.StringBuffer")))) 
                                      (101 (pop)) 
                                      (102 (goto 533)) ;;to TAG_3
                                      (105 (aload_0)) ;;at TAG_5
                                      (106 (getfield (fieldCP "newExtension" "java.text.PatternEntry$Parser" (class "java.lang.StringBuffer")))) 
                                      (109 (iload 4)) 
                                      (111 (invokevirtual (methodCP "append" "java.lang.StringBuffer" (char) (class "java.lang.StringBuffer")))) 
                                      (114 (pop)) 
                                      (115 (goto 533)) ;;to TAG_3
                                      (118 (iload 4)) ;;at TAG_1
                                      (120 (lookupswitch (lookupswitchinfo 377 12 ((9 . 294) (10 . 294) (12 . 294) (13 . 294) (32 . 294) (38 . 280) (39 . 302) (44 . 241) (47 . 297) (59 . 254) (60 . 267) (61 . 228)))))  ;;to TAG_13;;to TAG_14;;to TAG_6;;to TAG_7;;to TAG_8;;to TAG_9;;to TAG_10;;to TAG_11;;to TAG_12
                                      (228 (iload_1)) ;;at TAG_14
                                      (229 (iconst_m1)) 
                                      (230 (if_icmpeq 236)) ;;to TAG_15
                                      (233 (goto 546)) ;;to TAG_0
                                      (236 (iconst_3)) ;;at TAG_15
                                      (237 (istore_1)) 
                                      (238 (goto 533)) ;;to TAG_3
                                      (241 (iload_1)) ;;at TAG_10
                                      (242 (iconst_m1)) 
                                      (243 (if_icmpeq 249)) ;;to TAG_16
                                      (246 (goto 546)) ;;to TAG_0
                                      (249 (iconst_2)) ;;at TAG_16
                                      (250 (istore_1)) 
                                      (251 (goto 533)) ;;to TAG_3
                                      (254 (iload_1)) ;;at TAG_12
                                      (255 (iconst_m1)) 
                                      (256 (if_icmpeq 262)) ;;to TAG_17
                                      (259 (goto 546)) ;;to TAG_0
                                      (262 (iconst_1)) ;;at TAG_17
                                      (263 (istore_1)) 
                                      (264 (goto 533)) ;;to TAG_3
                                      (267 (iload_1)) ;;at TAG_13
                                      (268 (iconst_m1)) 
                                      (269 (if_icmpeq 275)) ;;to TAG_18
                                      (272 (goto 546)) ;;to TAG_0
                                      (275 (iconst_0)) ;;at TAG_18
                                      (276 (istore_1)) 
                                      (277 (goto 533)) ;;to TAG_3
                                      (280 (iload_1)) ;;at TAG_8
                                      (281 (iconst_m1)) 
                                      (282 (if_icmpeq 288)) ;;to TAG_19
                                      (285 (goto 546)) ;;to TAG_0
                                      (288 (bipush -2)) ;;at TAG_19
                                      (290 (istore_1)) 
                                      (291 (goto 533)) ;;to TAG_3
                                      (294 (goto 533)) ;;to TAG_3;;at TAG_7
                                      (297 (iconst_0)) ;;at TAG_11
                                      (298 (istore_2)) 
                                      (299 (goto 533)) ;;to TAG_3
                                      (302 (iconst_1)) ;;at TAG_9
                                      (303 (istore_3)) 
                                      (304 (aload_0)) 
                                      (305 (getfield (fieldCP "pattern" "java.text.PatternEntry$Parser" (class "java.lang.String")))) 
                                      (308 (aload_0)) 
                                      (309 (dup)) 
                                      (310 (getfield (fieldCP "i" "java.text.PatternEntry$Parser" int))) 
                                      (313 (iconst_1)) 
                                      (314 (iadd)) 
                                      (315 (dup_x1)) 
                                      (316 (putfield (fieldCP "i" "java.text.PatternEntry$Parser" int))) 
                                      (319 (invokevirtual (methodCP "charAt" "java.lang.String" (int) char))) 
                                      (322 (istore 4)) 
                                      (324 (aload_0)) 
                                      (325 (getfield (fieldCP "newChars" "java.text.PatternEntry$Parser" (class "java.lang.StringBuffer")))) 
                                      (328 (invokevirtual (methodCP "length" "java.lang.StringBuffer" () int))) 
                                      (331 (ifne 347)) ;;to TAG_20
                                      (334 (aload_0)) 
                                      (335 (getfield (fieldCP "newChars" "java.text.PatternEntry$Parser" (class "java.lang.StringBuffer")))) 
                                      (338 (iload 4)) 
                                      (340 (invokevirtual (methodCP "append" "java.lang.StringBuffer" (char) (class "java.lang.StringBuffer")))) 
                                      (343 (pop)) 
                                      (344 (goto 533)) ;;to TAG_3
                                      (347 (iload_2)) ;;at TAG_20
                                      (348 (ifeq 364)) ;;to TAG_21
                                      (351 (aload_0)) 
                                      (352 (getfield (fieldCP "newChars" "java.text.PatternEntry$Parser" (class "java.lang.StringBuffer")))) 
                                      (355 (iload 4)) 
                                      (357 (invokevirtual (methodCP "append" "java.lang.StringBuffer" (char) (class "java.lang.StringBuffer")))) 
                                      (360 (pop)) 
                                      (361 (goto 533)) ;;to TAG_3
                                      (364 (aload_0)) ;;at TAG_21
                                      (365 (getfield (fieldCP "newExtension" "java.text.PatternEntry$Parser" (class "java.lang.StringBuffer")))) 
                                      (368 (iload 4)) 
                                      (370 (invokevirtual (methodCP "append" "java.lang.StringBuffer" (char) (class "java.lang.StringBuffer")))) 
                                      (373 (pop)) 
                                      (374 (goto 533)) ;;to TAG_3
                                      (377 (iload_1)) ;;at TAG_6
                                      (378 (iconst_m1)) 
                                      (379 (if_icmpne 457)) ;;to TAG_22
                                      (382 (new (class "java.text.ParseException"))) 
                                      (385 (dup)) 
                                      (386 (new (class "java.lang.StringBuilder"))) 
                                      (389 (dup)) 
                                      (390 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (393 (ldc 0)) ;;STRING:: "missing char (=,;<&) : "
                                      (395 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (398 (aload_0)) 
                                      (399 (getfield (fieldCP "pattern" "java.text.PatternEntry$Parser" (class "java.lang.String")))) 
                                      (402 (aload_0)) 
                                      (403 (getfield (fieldCP "i" "java.text.PatternEntry$Parser" int))) 
                                      (406 (aload_0)) 
                                      (407 (getfield (fieldCP "i" "java.text.PatternEntry$Parser" int))) 
                                      (410 (bipush 10)) 
                                      (412 (iadd)) 
                                      (413 (aload_0)) 
                                      (414 (getfield (fieldCP "pattern" "java.text.PatternEntry$Parser" (class "java.lang.String")))) 
                                      (417 (invokevirtual (methodCP "length" "java.lang.String" () int))) 
                                      (420 (if_icmpge 433)) ;;to TAG_23
                                      (423 (aload_0)) 
                                      (424 (getfield (fieldCP "i" "java.text.PatternEntry$Parser" int))) 
                                      (427 (bipush 10)) 
                                      (429 (iadd)) 
                                      (430 (goto 440)) ;;to TAG_24
                                      (433 (aload_0)) ;;at TAG_23
                                      (434 (getfield (fieldCP "pattern" "java.text.PatternEntry$Parser" (class "java.lang.String")))) 
                                      (437 (invokevirtual (methodCP "length" "java.lang.String" () int))) 
                                      (440 (invokevirtual (methodCP "substring" "java.lang.String" (int int) (class "java.lang.String")))) ;;at TAG_24
                                      (443 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (446 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (449 (aload_0)) 
                                      (450 (getfield (fieldCP "i" "java.text.PatternEntry$Parser" int))) 
                                      (453 (invokespecial (methodCP "<init>" "java.text.ParseException" ((class "java.lang.String") int) void))) 
                                      (456 (athrow)) 
                                      (457 (iload 4)) ;;at TAG_22
                                      (459 (invokestatic (methodCP "isSpecialChar" "java.text.PatternEntry" (char) boolean))) 
                                      (462 (ifeq 506)) ;;to TAG_25
                                      (465 (iload_3)) 
                                      (466 (ifne 506)) ;;to TAG_25
                                      (469 (new (class "java.text.ParseException"))) 
                                      (472 (dup)) 
                                      (473 (new (class "java.lang.StringBuilder"))) 
                                      (476 (dup)) 
                                      (477 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (480 (ldc 1)) ;;STRING:: "Unquoted punctuation character : "
                                      (482 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (485 (iload 4)) 
                                      (487 (bipush 16)) 
                                      (489 (invokestatic (methodCP "toString" "java.lang.Integer" (int int) (class "java.lang.String")))) 
                                      (492 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (495 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (498 (aload_0)) 
                                      (499 (getfield (fieldCP "i" "java.text.PatternEntry$Parser" int))) 
                                      (502 (invokespecial (methodCP "<init>" "java.text.ParseException" ((class "java.lang.String") int) void))) 
                                      (505 (athrow)) 
                                      (506 (iload_2)) ;;at TAG_25
                                      (507 (ifeq 523)) ;;to TAG_26
                                      (510 (aload_0)) 
                                      (511 (getfield (fieldCP "newChars" "java.text.PatternEntry$Parser" (class "java.lang.StringBuffer")))) 
                                      (514 (iload 4)) 
                                      (516 (invokevirtual (methodCP "append" "java.lang.StringBuffer" (char) (class "java.lang.StringBuffer")))) 
                                      (519 (pop)) 
                                      (520 (goto 533)) ;;to TAG_3
                                      (523 (aload_0)) ;;at TAG_26
                                      (524 (getfield (fieldCP "newExtension" "java.text.PatternEntry$Parser" (class "java.lang.StringBuffer")))) 
                                      (527 (iload 4)) 
                                      (529 (invokevirtual (methodCP "append" "java.lang.StringBuffer" (char) (class "java.lang.StringBuffer")))) 
                                      (532 (pop)) 
                                      (533 (aload_0)) ;;at TAG_3
                                      (534 (dup)) 
                                      (535 (getfield (fieldCP "i" "java.text.PatternEntry$Parser" int))) 
                                      (538 (iconst_1)) 
                                      (539 (iadd)) 
                                      (540 (putfield (fieldCP "i" "java.text.PatternEntry$Parser" int))) 
                                      (543 (goto 22)) ;;to TAG_27
                                      (546 (iload_1)) ;;at TAG_0
                                      (547 (iconst_m1)) 
                                      (548 (if_icmpne 553)) ;;to TAG_28
                                      (551 (aconst_null)) 
                                      (552 (areturn)) 
                                      (553 (aload_0)) ;;at TAG_28
                                      (554 (getfield (fieldCP "newChars" "java.text.PatternEntry$Parser" (class "java.lang.StringBuffer")))) 
                                      (557 (invokevirtual (methodCP "length" "java.lang.StringBuffer" () int))) 
                                      (560 (ifne 638)) ;;to TAG_29
                                      (563 (new (class "java.text.ParseException"))) 
                                      (566 (dup)) 
                                      (567 (new (class "java.lang.StringBuilder"))) 
                                      (570 (dup)) 
                                      (571 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (574 (ldc 2)) ;;STRING:: "missing chars (=,;<&): "
                                      (576 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (579 (aload_0)) 
                                      (580 (getfield (fieldCP "pattern" "java.text.PatternEntry$Parser" (class "java.lang.String")))) 
                                      (583 (aload_0)) 
                                      (584 (getfield (fieldCP "i" "java.text.PatternEntry$Parser" int))) 
                                      (587 (aload_0)) 
                                      (588 (getfield (fieldCP "i" "java.text.PatternEntry$Parser" int))) 
                                      (591 (bipush 10)) 
                                      (593 (iadd)) 
                                      (594 (aload_0)) 
                                      (595 (getfield (fieldCP "pattern" "java.text.PatternEntry$Parser" (class "java.lang.String")))) 
                                      (598 (invokevirtual (methodCP "length" "java.lang.String" () int))) 
                                      (601 (if_icmpge 614)) ;;to TAG_30
                                      (604 (aload_0)) 
                                      (605 (getfield (fieldCP "i" "java.text.PatternEntry$Parser" int))) 
                                      (608 (bipush 10)) 
                                      (610 (iadd)) 
                                      (611 (goto 621)) ;;to TAG_31
                                      (614 (aload_0)) ;;at TAG_30
                                      (615 (getfield (fieldCP "pattern" "java.text.PatternEntry$Parser" (class "java.lang.String")))) 
                                      (618 (invokevirtual (methodCP "length" "java.lang.String" () int))) 
                                      (621 (invokevirtual (methodCP "substring" "java.lang.String" (int int) (class "java.lang.String")))) ;;at TAG_31
                                      (624 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (627 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (630 (aload_0)) 
                                      (631 (getfield (fieldCP "i" "java.text.PatternEntry$Parser" int))) 
                                      (634 (invokespecial (methodCP "<init>" "java.text.ParseException" ((class "java.lang.String") int) void))) 
                                      (637 (athrow)) 
                                      (638 (new (class "java.text.PatternEntry"))) ;;at TAG_29
                                      (641 (dup)) 
                                      (642 (iload_1)) 
                                      (643 (aload_0)) 
                                      (644 (getfield (fieldCP "newChars" "java.text.PatternEntry$Parser" (class "java.lang.StringBuffer")))) 
                                      (647 (aload_0)) 
                                      (648 (getfield (fieldCP "newExtension" "java.text.PatternEntry$Parser" (class "java.lang.StringBuffer")))) 
                                      (651 (invokespecial (methodCP "<init>" "java.text.PatternEntry" (int (class "java.lang.StringBuffer") (class "java.lang.StringBuffer")) void))) 
                                      (654 (areturn)) 
                                      (endofcode 655))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *PatternEntry$Parser-class-table*
  (make-static-class-decls 
   *java.text.PatternEntry$Parser*))

(defconst *package-name-map* 
  ("java.text.PatternEntry$Parser" . "java.text"))

