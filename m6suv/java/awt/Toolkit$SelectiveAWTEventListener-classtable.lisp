; Toolkit$SelectiveAWTEventListener-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:30 CDT 2014.
;

(defconst *java.awt.Toolkit$SelectiveAWTEventListener*
 (make-class-def
      '(class "java.awt.Toolkit$SelectiveAWTEventListener"
            "java.lang.Object"
            (constant_pool
                        (LONG 2)
                        (LONG 4)
                        (LONG 8)
                        (LONG 131072)
                        (LONG 32)
                        (LONG 16)
                        (LONG 64)
                        (LONG 128)
                        (LONG 256)
                        (LONG 512)
                        (LONG 1024)
                        (LONG 2048)
                        (LONG 8192)
                        (LONG 16384)
                        (LONG 32768)
                        (LONG 65536)
                        (LONG 262144)
                        (LONG 524288)
                        (LONG -2147483648))
            (fields
                        (field "listener" (class "java.awt.event.AWTEventListener") (accessflags  *class* ) -1)
                        (field "eventMask" long (accessflags  *class*  *private* ) -1)
                        (field "calls" (array int) (accessflags  *class* ) -1)
                        (field "this$0" (class "java.awt.Toolkit") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "getListener"
                              (parameters )
                              (returntype . (class "java.awt.event.AWTEventListener"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "listener" "java.awt.Toolkit$SelectiveAWTEventListener" (class "java.awt.event.AWTEventListener"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getEventMask"
                              (parameters )
                              (returntype . long)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "eventMask" "java.awt.Toolkit$SelectiveAWTEventListener" long)))
                                      (4 (lreturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getCalls"
                              (parameters )
                              (returntype . (array int))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "calls" "java.awt.Toolkit$SelectiveAWTEventListener" (array int))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "orEventMasks"
                              (parameters long)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 4) (code_length . 56)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (dup)) 
                                      (2 (getfield (fieldCP "eventMask" "java.awt.Toolkit$SelectiveAWTEventListener" long))) 
                                      (5 (lload_1)) 
                                      (6 (lor)) 
                                      (7 (putfield (fieldCP "eventMask" "java.awt.Toolkit$SelectiveAWTEventListener" long))) 
                                      (10 (iconst_0)) 
                                      (11 (istore_3)) 
                                      (12 (iload_3)) ;;at TAG_3
                                      (13 (bipush 64)) 
                                      (15 (if_icmpge 55)) ;;to TAG_0
                                      (18 (lload_1)) 
                                      (19 (lconst_0)) 
                                      (20 (lcmp)) 
                                      (21 (ifne 27)) ;;to TAG_1
                                      (24 (goto 55)) ;;to TAG_0
                                      (27 (lload_1)) ;;at TAG_1
                                      (28 (lconst_1)) 
                                      (29 (land)) 
                                      (30 (lconst_0)) 
                                      (31 (lcmp)) 
                                      (32 (ifeq 45))  ;;to TAG_2
                                      (35 (aload_0)) 
                                      (36 (getfield (fieldCP "calls" "java.awt.Toolkit$SelectiveAWTEventListener" (array int)))) 
                                      (39 (iload_3)) 
                                      (40 (dup2)) 
                                      (41 (iaload)) 
                                      (42 (iconst_1)) 
                                      (43 (iadd)) 
                                      (44 (iastore)) 
                                      (45 (lload_1)) ;;at TAG_2
                                      (46 (iconst_1)) 
                                      (47 (lushr)) 
                                      (48 (lstore_1)) 
                                      (49 (iinc 3 1)) 
                                      (52 (goto 12)) ;;to TAG_3
                                      (55 (return)) ;;at TAG_0
                                      (endofcode 56))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.awt.Toolkit") (class "java.awt.event.AWTEventListener") long)
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 3) (max_locals . 5) (code_length . 28)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.awt.Toolkit$SelectiveAWTEventListener" (class "java.awt.Toolkit"))))
                                      (5 (aload_0))
                                      (6 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (9 (aload_0))
                                      (10 (bipush 64))
                                      (12 (newarray INT))
                                      (14 (putfield (fieldCP "calls" "java.awt.Toolkit$SelectiveAWTEventListener" (array int))))
                                      (17 (aload_0))
                                      (18 (aload_2))
                                      (19 (putfield (fieldCP "listener" "java.awt.Toolkit$SelectiveAWTEventListener" (class "java.awt.event.AWTEventListener"))))
                                      (22 (aload_0))
                                      (23 (lload_3))
                                      (24 (putfield (fieldCP "eventMask" "java.awt.Toolkit$SelectiveAWTEventListener" long)))
                                      (27 (return))
                                      (endofcode 28))
                                   (Exceptions )
                                   (StackMap )))
                        (method "eventDispatched"
                              (parameters (class "java.awt.AWTEvent"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 7) (code_length . 745)
                                   (parsedcode
                                      (0 (lconst_0)) 
                                      (1 (lstore_2)) 
                                      (2 (aload_0)) 
                                      (3 (getfield (fieldCP "eventMask" "java.awt.Toolkit$SelectiveAWTEventListener" long))) 
                                      (6 (lconst_1)) 
                                      (7 (land)) 
                                      (8 (dup2)) 
                                      (9 (lstore_2)) 
                                      (10 (lconst_0)) 
                                      (11 (lcmp)) 
                                      (12 (ifeq 33)) ;;to TAG_0
                                      (15 (aload_1)) 
                                      (16 (getfield (fieldCP "id" "java.awt.AWTEvent" int))) 
                                      (19 (bipush 100)) 
                                      (21 (if_icmplt 33)) ;;to TAG_0
                                      (24 (aload_1)) 
                                      (25 (getfield (fieldCP "id" "java.awt.AWTEvent" int))) 
                                      (28 (bipush 103)) 
                                      (30 (if_icmple 685)) ;;to TAG_1
                                      (33 (aload_0)) ;;at TAG_0
                                      (34 (getfield (fieldCP "eventMask" "java.awt.Toolkit$SelectiveAWTEventListener" long))) 
                                      (37 (ldc2_w 0)) ;; LONG:: "2"
                                      (40 (land)) 
                                      (41 (dup2)) 
                                      (42 (lstore_2)) 
                                      (43 (lconst_0)) 
                                      (44 (lcmp)) 
                                      (45 (ifeq 68)) ;;to TAG_2
                                      (48 (aload_1)) 
                                      (49 (getfield (fieldCP "id" "java.awt.AWTEvent" int))) 
                                      (52 (sipush 300)) 
                                      (55 (if_icmplt 68)) ;;to TAG_2
                                      (58 (aload_1)) 
                                      (59 (getfield (fieldCP "id" "java.awt.AWTEvent" int))) 
                                      (62 (sipush 301)) 
                                      (65 (if_icmple 685)) ;;to TAG_1
                                      (68 (aload_0)) ;;at TAG_2
                                      (69 (getfield (fieldCP "eventMask" "java.awt.Toolkit$SelectiveAWTEventListener" long))) 
                                      (72 (ldc2_w 1)) ;; LONG:: "4"
                                      (75 (land)) 
                                      (76 (dup2)) 
                                      (77 (lstore_2)) 
                                      (78 (lconst_0)) 
                                      (79 (lcmp)) 
                                      (80 (ifeq 103)) ;;to TAG_3
                                      (83 (aload_1)) 
                                      (84 (getfield (fieldCP "id" "java.awt.AWTEvent" int))) 
                                      (87 (sipush 1004)) 
                                      (90 (if_icmplt 103)) ;;to TAG_3
                                      (93 (aload_1)) 
                                      (94 (getfield (fieldCP "id" "java.awt.AWTEvent" int))) 
                                      (97 (sipush 1005)) 
                                      (100 (if_icmple 685)) ;;to TAG_1
                                      (103 (aload_0)) ;;at TAG_3
                                      (104 (getfield (fieldCP "eventMask" "java.awt.Toolkit$SelectiveAWTEventListener" long))) 
                                      (107 (ldc2_w 2)) ;; LONG:: "8"
                                      (110 (land)) 
                                      (111 (dup2)) 
                                      (112 (lstore_2)) 
                                      (113 (lconst_0)) 
                                      (114 (lcmp)) 
                                      (115 (ifeq 138)) ;;to TAG_4
                                      (118 (aload_1)) 
                                      (119 (getfield (fieldCP "id" "java.awt.AWTEvent" int))) 
                                      (122 (sipush 400)) 
                                      (125 (if_icmplt 138)) ;;to TAG_4
                                      (128 (aload_1)) 
                                      (129 (getfield (fieldCP "id" "java.awt.AWTEvent" int))) 
                                      (132 (sipush 402)) 
                                      (135 (if_icmple 685)) ;;to TAG_1
                                      (138 (aload_0)) ;;at TAG_4
                                      (139 (getfield (fieldCP "eventMask" "java.awt.Toolkit$SelectiveAWTEventListener" long))) 
                                      (142 (ldc2_w 3)) ;; LONG:: "131072"
                                      (145 (land)) 
                                      (146 (dup2)) 
                                      (147 (lstore_2)) 
                                      (148 (lconst_0)) 
                                      (149 (lcmp)) 
                                      (150 (ifeq 163)) ;;to TAG_5
                                      (153 (aload_1)) 
                                      (154 (getfield (fieldCP "id" "java.awt.AWTEvent" int))) 
                                      (157 (sipush 507)) 
                                      (160 (if_icmpeq 685)) ;;to TAG_1
                                      (163 (aload_0)) ;;at TAG_5
                                      (164 (getfield (fieldCP "eventMask" "java.awt.Toolkit$SelectiveAWTEventListener" long))) 
                                      (167 (ldc2_w 4)) ;; LONG:: "32"
                                      (170 (land)) 
                                      (171 (dup2)) 
                                      (172 (lstore_2)) 
                                      (173 (lconst_0)) 
                                      (174 (lcmp)) 
                                      (175 (ifeq 198)) ;;to TAG_6
                                      (178 (aload_1)) 
                                      (179 (getfield (fieldCP "id" "java.awt.AWTEvent" int))) 
                                      (182 (sipush 503)) 
                                      (185 (if_icmpeq 685)) ;;to TAG_1
                                      (188 (aload_1)) 
                                      (189 (getfield (fieldCP "id" "java.awt.AWTEvent" int))) 
                                      (192 (sipush 506)) 
                                      (195 (if_icmpeq 685)) ;;to TAG_1
                                      (198 (aload_0)) ;;at TAG_6
                                      (199 (getfield (fieldCP "eventMask" "java.awt.Toolkit$SelectiveAWTEventListener" long))) 
                                      (202 (ldc2_w 5)) ;; LONG:: "16"
                                      (205 (land)) 
                                      (206 (dup2)) 
                                      (207 (lstore_2)) 
                                      (208 (lconst_0)) 
                                      (209 (lcmp)) 
                                      (210 (ifeq 263)) ;;to TAG_7
                                      (213 (aload_1)) 
                                      (214 (getfield (fieldCP "id" "java.awt.AWTEvent" int))) 
                                      (217 (sipush 503)) 
                                      (220 (if_icmpeq 263)) ;;to TAG_7
                                      (223 (aload_1)) 
                                      (224 (getfield (fieldCP "id" "java.awt.AWTEvent" int))) 
                                      (227 (sipush 506)) 
                                      (230 (if_icmpeq 263)) ;;to TAG_7
                                      (233 (aload_1)) 
                                      (234 (getfield (fieldCP "id" "java.awt.AWTEvent" int))) 
                                      (237 (sipush 507)) 
                                      (240 (if_icmpeq 263)) ;;to TAG_7
                                      (243 (aload_1)) 
                                      (244 (getfield (fieldCP "id" "java.awt.AWTEvent" int))) 
                                      (247 (sipush 500)) 
                                      (250 (if_icmplt 263)) ;;to TAG_7
                                      (253 (aload_1)) 
                                      (254 (getfield (fieldCP "id" "java.awt.AWTEvent" int))) 
                                      (257 (sipush 507)) 
                                      (260 (if_icmple 685)) ;;to TAG_1
                                      (263 (aload_0)) ;;at TAG_7
                                      (264 (getfield (fieldCP "eventMask" "java.awt.Toolkit$SelectiveAWTEventListener" long))) 
                                      (267 (ldc2_w 6)) ;; LONG:: "64"
                                      (270 (land)) 
                                      (271 (dup2)) 
                                      (272 (lstore_2)) 
                                      (273 (lconst_0)) 
                                      (274 (lcmp)) 
                                      (275 (ifeq 298)) ;;to TAG_8
                                      (278 (aload_1)) 
                                      (279 (getfield (fieldCP "id" "java.awt.AWTEvent" int))) 
                                      (282 (sipush 200)) 
                                      (285 (if_icmplt 298)) ;;to TAG_8
                                      (288 (aload_1)) 
                                      (289 (getfield (fieldCP "id" "java.awt.AWTEvent" int))) 
                                      (292 (sipush 209)) 
                                      (295 (if_icmple 685)) ;;to TAG_1
                                      (298 (aload_0)) ;;at TAG_8
                                      (299 (getfield (fieldCP "eventMask" "java.awt.Toolkit$SelectiveAWTEventListener" long))) 
                                      (302 (ldc2_w 7)) ;; LONG:: "128"
                                      (305 (land)) 
                                      (306 (dup2)) 
                                      (307 (lstore_2)) 
                                      (308 (lconst_0)) 
                                      (309 (lcmp)) 
                                      (310 (ifeq 333)) ;;to TAG_9
                                      (313 (aload_1)) 
                                      (314 (getfield (fieldCP "id" "java.awt.AWTEvent" int))) 
                                      (317 (sipush 1001)) 
                                      (320 (if_icmplt 333)) ;;to TAG_9
                                      (323 (aload_1)) 
                                      (324 (getfield (fieldCP "id" "java.awt.AWTEvent" int))) 
                                      (327 (sipush 1001)) 
                                      (330 (if_icmple 685)) ;;to TAG_1
                                      (333 (aload_0)) ;;at TAG_9
                                      (334 (getfield (fieldCP "eventMask" "java.awt.Toolkit$SelectiveAWTEventListener" long))) 
                                      (337 (ldc2_w 8)) ;; LONG:: "256"
                                      (340 (land)) 
                                      (341 (dup2)) 
                                      (342 (lstore_2)) 
                                      (343 (lconst_0)) 
                                      (344 (lcmp)) 
                                      (345 (ifeq 368)) ;;to TAG_10
                                      (348 (aload_1)) 
                                      (349 (getfield (fieldCP "id" "java.awt.AWTEvent" int))) 
                                      (352 (sipush 601)) 
                                      (355 (if_icmplt 368)) ;;to TAG_10
                                      (358 (aload_1)) 
                                      (359 (getfield (fieldCP "id" "java.awt.AWTEvent" int))) 
                                      (362 (sipush 601)) 
                                      (365 (if_icmple 685)) ;;to TAG_1
                                      (368 (aload_0)) ;;at TAG_10
                                      (369 (getfield (fieldCP "eventMask" "java.awt.Toolkit$SelectiveAWTEventListener" long))) 
                                      (372 (ldc2_w 9)) ;; LONG:: "512"
                                      (375 (land)) 
                                      (376 (dup2)) 
                                      (377 (lstore_2)) 
                                      (378 (lconst_0)) 
                                      (379 (lcmp)) 
                                      (380 (ifeq 403)) ;;to TAG_11
                                      (383 (aload_1)) 
                                      (384 (getfield (fieldCP "id" "java.awt.AWTEvent" int))) 
                                      (387 (sipush 701)) 
                                      (390 (if_icmplt 403)) ;;to TAG_11
                                      (393 (aload_1)) 
                                      (394 (getfield (fieldCP "id" "java.awt.AWTEvent" int))) 
                                      (397 (sipush 701)) 
                                      (400 (if_icmple 685)) ;;to TAG_1
                                      (403 (aload_0)) ;;at TAG_11
                                      (404 (getfield (fieldCP "eventMask" "java.awt.Toolkit$SelectiveAWTEventListener" long))) 
                                      (407 (ldc2_w 10)) ;; LONG:: "1024"
                                      (410 (land)) 
                                      (411 (dup2)) 
                                      (412 (lstore_2)) 
                                      (413 (lconst_0)) 
                                      (414 (lcmp)) 
                                      (415 (ifeq 438)) ;;to TAG_12
                                      (418 (aload_1)) 
                                      (419 (getfield (fieldCP "id" "java.awt.AWTEvent" int))) 
                                      (422 (sipush 900)) 
                                      (425 (if_icmplt 438)) ;;to TAG_12
                                      (428 (aload_1)) 
                                      (429 (getfield (fieldCP "id" "java.awt.AWTEvent" int))) 
                                      (432 (sipush 900)) 
                                      (435 (if_icmple 685)) ;;to TAG_1
                                      (438 (aload_0)) ;;at TAG_12
                                      (439 (getfield (fieldCP "eventMask" "java.awt.Toolkit$SelectiveAWTEventListener" long))) 
                                      (442 (ldc2_w 11)) ;; LONG:: "2048"
                                      (445 (land)) 
                                      (446 (dup2)) 
                                      (447 (lstore_2)) 
                                      (448 (lconst_0)) 
                                      (449 (lcmp)) 
                                      (450 (ifeq 473))  ;;to TAG_13
                                      (453 (aload_1)) 
                                      (454 (getfield (fieldCP "id" "java.awt.AWTEvent" int))) 
                                      (457 (sipush 1100)) 
                                      (460 (if_icmplt 473))  ;;to TAG_13
                                      (463 (aload_1)) 
                                      (464 (getfield (fieldCP "id" "java.awt.AWTEvent" int))) 
                                      (467 (sipush 1101)) 
                                      (470 (if_icmple 685)) ;;to TAG_1
                                      (473 (aload_0)) ;;at TAG_13
                                      (474 (getfield (fieldCP "eventMask" "java.awt.Toolkit$SelectiveAWTEventListener" long))) 
                                      (477 (ldc2_w 12)) ;; LONG:: "8192"
                                      (480 (land)) 
                                      (481 (dup2)) 
                                      (482 (lstore_2)) 
                                      (483 (lconst_0)) 
                                      (484 (lcmp)) 
                                      (485 (ifeq 508)) ;;to TAG_14
                                      (488 (aload_1)) 
                                      (489 (getfield (fieldCP "id" "java.awt.AWTEvent" int))) 
                                      (492 (sipush 800)) 
                                      (495 (if_icmplt 508)) ;;to TAG_14
                                      (498 (aload_1)) 
                                      (499 (getfield (fieldCP "id" "java.awt.AWTEvent" int))) 
                                      (502 (sipush 801)) 
                                      (505 (if_icmple 685)) ;;to TAG_1
                                      (508 (aload_0)) ;;at TAG_14
                                      (509 (getfield (fieldCP "eventMask" "java.awt.Toolkit$SelectiveAWTEventListener" long))) 
                                      (512 (ldc2_w 13)) ;; LONG:: "16384"
                                      (515 (land)) 
                                      (516 (dup2)) 
                                      (517 (lstore_2)) 
                                      (518 (lconst_0)) 
                                      (519 (lcmp)) 
                                      (520 (ifeq 543)) ;;to TAG_15
                                      (523 (aload_1)) 
                                      (524 (getfield (fieldCP "id" "java.awt.AWTEvent" int))) 
                                      (527 (sipush 1200)) 
                                      (530 (if_icmplt 543)) ;;to TAG_15
                                      (533 (aload_1)) 
                                      (534 (getfield (fieldCP "id" "java.awt.AWTEvent" int))) 
                                      (537 (sipush 1200)) 
                                      (540 (if_icmple 685)) ;;to TAG_1
                                      (543 (aload_0)) ;;at TAG_15
                                      (544 (getfield (fieldCP "eventMask" "java.awt.Toolkit$SelectiveAWTEventListener" long))) 
                                      (547 (ldc2_w 14)) ;; LONG:: "32768"
                                      (550 (land)) 
                                      (551 (dup2)) 
                                      (552 (lstore_2)) 
                                      (553 (lconst_0)) 
                                      (554 (lcmp)) 
                                      (555 (ifeq 568)) ;;to TAG_16
                                      (558 (aload_1)) 
                                      (559 (getfield (fieldCP "id" "java.awt.AWTEvent" int))) 
                                      (562 (sipush 1400)) 
                                      (565 (if_icmpeq 685)) ;;to TAG_1
                                      (568 (aload_0)) ;;at TAG_16
                                      (569 (getfield (fieldCP "eventMask" "java.awt.Toolkit$SelectiveAWTEventListener" long))) 
                                      (572 (ldc2_w 15)) ;; LONG:: "65536"
                                      (575 (land)) 
                                      (576 (dup2)) 
                                      (577 (lstore_2)) 
                                      (578 (lconst_0)) 
                                      (579 (lcmp)) 
                                      (580 (ifeq 603)) ;;to TAG_17
                                      (583 (aload_1)) 
                                      (584 (getfield (fieldCP "id" "java.awt.AWTEvent" int))) 
                                      (587 (sipush 1401)) 
                                      (590 (if_icmpeq 685)) ;;to TAG_1
                                      (593 (aload_1)) 
                                      (594 (getfield (fieldCP "id" "java.awt.AWTEvent" int))) 
                                      (597 (sipush 1402)) 
                                      (600 (if_icmpeq 685)) ;;to TAG_1
                                      (603 (aload_0)) ;;at TAG_17
                                      (604 (getfield (fieldCP "eventMask" "java.awt.Toolkit$SelectiveAWTEventListener" long))) 
                                      (607 (ldc2_w 16)) ;; LONG:: "262144"
                                      (610 (land)) 
                                      (611 (dup2)) 
                                      (612 (lstore_2)) 
                                      (613 (lconst_0)) 
                                      (614 (lcmp)) 
                                      (615 (ifeq 628)) ;;to TAG_18
                                      (618 (aload_1)) 
                                      (619 (getfield (fieldCP "id" "java.awt.AWTEvent" int))) 
                                      (622 (sipush 209)) 
                                      (625 (if_icmpeq 685)) ;;to TAG_1
                                      (628 (aload_0)) ;;at TAG_18
                                      (629 (getfield (fieldCP "eventMask" "java.awt.Toolkit$SelectiveAWTEventListener" long))) 
                                      (632 (ldc2_w 17)) ;; LONG:: "524288"
                                      (635 (land)) 
                                      (636 (dup2)) 
                                      (637 (lstore_2)) 
                                      (638 (lconst_0)) 
                                      (639 (lcmp)) 
                                      (640 (ifeq 663)) ;;to TAG_19
                                      (643 (aload_1)) 
                                      (644 (getfield (fieldCP "id" "java.awt.AWTEvent" int))) 
                                      (647 (sipush 207)) 
                                      (650 (if_icmpeq 685)) ;;to TAG_1
                                      (653 (aload_1)) 
                                      (654 (getfield (fieldCP "id" "java.awt.AWTEvent" int))) 
                                      (657 (sipush 208)) 
                                      (660 (if_icmpeq 685)) ;;to TAG_1
                                      (663 (aload_0)) ;;at TAG_19
                                      (664 (getfield (fieldCP "eventMask" "java.awt.Toolkit$SelectiveAWTEventListener" long))) 
                                      (667 (ldc2_w 18)) ;; LONG:: "-2147483648"
                                      (670 (land)) 
                                      (671 (dup2)) 
                                      (672 (lstore_2)) 
                                      (673 (lconst_0)) 
                                      (674 (lcmp)) 
                                      (675 (ifeq 744)) ;;to TAG_20
                                      (678 (aload_1)) 
                                      (679 (instanceof (class "sun.awt.UngrabEvent"))) 
                                      (682 (ifeq 744)) ;;to TAG_20
                                      (685 (iconst_0)) ;;at TAG_1
                                      (686 (istore 4)) 
                                      (688 (lload_2)) 
                                      (689 (lstore 5)) 
                                      (691 (lload 5)) ;;at TAG_22
                                      (693 (lconst_0)) 
                                      (694 (lcmp)) 
                                      (695 (ifeq 710)) ;;to TAG_21
                                      (698 (lload 5)) 
                                      (700 (iconst_1)) 
                                      (701 (lushr)) 
                                      (702 (lstore 5)) 
                                      (704 (iinc 4 1)) 
                                      (707 (goto 691)) ;;to TAG_22
                                      (710 (iinc 4 -1)) ;;at TAG_21
                                      (713 (iconst_0)) 
                                      (714 (istore 5)) 
                                      (716 (iload 5)) ;;at TAG_23
                                      (718 (aload_0)) 
                                      (719 (getfield (fieldCP "calls" "java.awt.Toolkit$SelectiveAWTEventListener" (array int)))) 
                                      (722 (iload 4)) 
                                      (724 (iaload)) 
                                      (725 (if_icmpge 744)) ;;to TAG_20
                                      (728 (aload_0)) 
                                      (729 (getfield (fieldCP "listener" "java.awt.Toolkit$SelectiveAWTEventListener" (class "java.awt.event.AWTEventListener")))) 
                                      (732 (aload_1)) 
                                      (733 (invokeinterface (methodCP "eventDispatched" "java.awt.event.AWTEventListener" ((class "java.awt.AWTEvent")) void) 2)) 
                                      (738 (iinc 5 1)) 
                                      (741 (goto 716)) ;;to TAG_23
                                      (744 (return)) ;;at TAG_20
                                      (endofcode 745))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.awt.event.AWTEventListener")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *Toolkit$SelectiveAWTEventListener-class-table*
  (make-static-class-decls 
   *java.awt.Toolkit$SelectiveAWTEventListener*))

(defconst *package-name-map* 
  ("java.awt.Toolkit$SelectiveAWTEventListener" . "java.awt"))

