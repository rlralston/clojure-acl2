; Properties$LineReader-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:47 CDT 2014.
;

(defconst *java.util.Properties$LineReader*
 (make-class-def
      '(class "java.util.Properties$LineReader"
            "java.lang.Object"
            (constant_pool
                        (INT 2147483647))
            (fields
                        (field "inByteBuf" (array byte) (accessflags  *class* ) -1)
                        (field "inCharBuf" (array char) (accessflags  *class* ) -1)
                        (field "lineBuf" (array char) (accessflags  *class* ) -1)
                        (field "inLimit" int (accessflags  *class* ) -1)
                        (field "inOff" int (accessflags  *class* ) -1)
                        (field "inStream" (class "java.io.InputStream") (accessflags  *class* ) -1)
                        (field "reader" (class "java.io.Reader") (accessflags  *class* ) -1)
                        (field "this$0" (class "java.util.Properties") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.Properties") (class "java.io.InputStream"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 43)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.util.Properties$LineReader" (class "java.util.Properties"))))
                                      (5 (aload_0))
                                      (6 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (9 (aload_0))
                                      (10 (sipush 1024))
                                      (13 (newarray CHAR))
                                      (15 (putfield (fieldCP "lineBuf" "java.util.Properties$LineReader" (array char))))
                                      (18 (aload_0))
                                      (19 (iconst_0))
                                      (20 (putfield (fieldCP "inLimit" "java.util.Properties$LineReader" int)))
                                      (23 (aload_0))
                                      (24 (iconst_0))
                                      (25 (putfield (fieldCP "inOff" "java.util.Properties$LineReader" int)))
                                      (28 (aload_0))
                                      (29 (aload_2))
                                      (30 (putfield (fieldCP "inStream" "java.util.Properties$LineReader" (class "java.io.InputStream"))))
                                      (33 (aload_0))
                                      (34 (sipush 8192))
                                      (37 (newarray BYTE))
                                      (39 (putfield (fieldCP "inByteBuf" "java.util.Properties$LineReader" (array byte))))
                                      (42 (return))
                                      (endofcode 43))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.util.Properties") (class "java.io.Reader"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 43)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.util.Properties$LineReader" (class "java.util.Properties"))))
                                      (5 (aload_0))
                                      (6 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (9 (aload_0))
                                      (10 (sipush 1024))
                                      (13 (newarray CHAR))
                                      (15 (putfield (fieldCP "lineBuf" "java.util.Properties$LineReader" (array char))))
                                      (18 (aload_0))
                                      (19 (iconst_0))
                                      (20 (putfield (fieldCP "inLimit" "java.util.Properties$LineReader" int)))
                                      (23 (aload_0))
                                      (24 (iconst_0))
                                      (25 (putfield (fieldCP "inOff" "java.util.Properties$LineReader" int)))
                                      (28 (aload_0))
                                      (29 (aload_2))
                                      (30 (putfield (fieldCP "reader" "java.util.Properties$LineReader" (class "java.io.Reader"))))
                                      (33 (aload_0))
                                      (34 (sipush 8192))
                                      (37 (newarray CHAR))
                                      (39 (putfield (fieldCP "inCharBuf" "java.util.Properties$LineReader" (array char))))
                                      (42 (return))
                                      (endofcode 43))
                                   (Exceptions )
                                   (StackMap )))
                        (method "readLine"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 6) (max_locals . 11) (code_length . 452)
                                   (parsedcode
                                      (0 (iconst_0)) 
                                      (1 (istore_1)) 
                                      (2 (iconst_0)) 
                                      (3 (istore_2)) 
                                      (4 (iconst_1)) 
                                      (5 (istore_3)) 
                                      (6 (iconst_0)) 
                                      (7 (istore 4)) 
                                      (9 (iconst_1)) 
                                      (10 (istore 5)) 
                                      (12 (iconst_0)) 
                                      (13 (istore 6)) 
                                      (15 (iconst_0)) 
                                      (16 (istore 7)) 
                                      (18 (iconst_0)) 
                                      (19 (istore 8)) 
                                      (21 (aload_0)) ;;at TAG_8
                                      (22 (getfield (fieldCP "inOff" "java.util.Properties$LineReader" int))) 
                                      (25 (aload_0)) 
                                      (26 (getfield (fieldCP "inLimit" "java.util.Properties$LineReader" int))) 
                                      (29 (if_icmplt 93)) ;;to TAG_0
                                      (32 (aload_0)) 
                                      (33 (aload_0)) 
                                      (34 (getfield (fieldCP "inStream" "java.util.Properties$LineReader" (class "java.io.InputStream")))) 
                                      (37 (ifnonnull 54)) ;;to TAG_1
                                      (40 (aload_0)) 
                                      (41 (getfield (fieldCP "reader" "java.util.Properties$LineReader" (class "java.io.Reader")))) 
                                      (44 (aload_0)) 
                                      (45 (getfield (fieldCP "inCharBuf" "java.util.Properties$LineReader" (array char)))) 
                                      (48 (invokevirtual (methodCP "read" "java.io.Reader" ((array char)) int))) 
                                      (51 (goto 65)) ;;to TAG_2
                                      (54 (aload_0)) ;;at TAG_1
                                      (55 (getfield (fieldCP "inStream" "java.util.Properties$LineReader" (class "java.io.InputStream")))) 
                                      (58 (aload_0)) 
                                      (59 (getfield (fieldCP "inByteBuf" "java.util.Properties$LineReader" (array byte)))) 
                                      (62 (invokevirtual (methodCP "read" "java.io.InputStream" ((array byte)) int))) 
                                      (65 (putfield (fieldCP "inLimit" "java.util.Properties$LineReader" int))) ;;at TAG_2
                                      (68 (aload_0)) 
                                      (69 (iconst_0)) 
                                      (70 (putfield (fieldCP "inOff" "java.util.Properties$LineReader" int))) 
                                      (73 (aload_0)) 
                                      (74 (getfield (fieldCP "inLimit" "java.util.Properties$LineReader" int))) 
                                      (77 (ifgt 93)) ;;to TAG_0
                                      (80 (iload_1)) 
                                      (81 (ifeq 89)) ;;to TAG_3
                                      (84 (iload 4)) 
                                      (86 (ifeq 91)) ;;to TAG_4
                                      (89 (iconst_m1)) ;;at TAG_3
                                      (90 (ireturn)) 
                                      (91 (iload_1)) ;;at TAG_4
                                      (92 (ireturn)) 
                                      (93 (aload_0)) ;;at TAG_0
                                      (94 (getfield (fieldCP "inStream" "java.util.Properties$LineReader" (class "java.io.InputStream")))) 
                                      (97 (ifnull 125)) ;;to TAG_5
                                      (100 (sipush 255)) 
                                      (103 (aload_0)) 
                                      (104 (getfield (fieldCP "inByteBuf" "java.util.Properties$LineReader" (array byte)))) 
                                      (107 (aload_0)) 
                                      (108 (dup)) 
                                      (109 (getfield (fieldCP "inOff" "java.util.Properties$LineReader" int))) 
                                      (112 (dup_x1)) 
                                      (113 (iconst_1)) 
                                      (114 (iadd)) 
                                      (115 (putfield (fieldCP "inOff" "java.util.Properties$LineReader" int))) 
                                      (118 (baload)) 
                                      (119 (iand)) 
                                      (120 (i2c)) 
                                      (121 (istore_2)) 
                                      (122 (goto 142)) ;;to TAG_6
                                      (125 (aload_0)) ;;at TAG_5
                                      (126 (getfield (fieldCP "inCharBuf" "java.util.Properties$LineReader" (array char)))) 
                                      (129 (aload_0)) 
                                      (130 (dup)) 
                                      (131 (getfield (fieldCP "inOff" "java.util.Properties$LineReader" int))) 
                                      (134 (dup_x1)) 
                                      (135 (iconst_1)) 
                                      (136 (iadd)) 
                                      (137 (putfield (fieldCP "inOff" "java.util.Properties$LineReader" int))) 
                                      (140 (caload)) 
                                      (141 (istore_2)) 
                                      (142 (iload 8)) ;;at TAG_6
                                      (144 (ifeq 159)) ;;to TAG_7
                                      (147 (iconst_0)) 
                                      (148 (istore 8)) 
                                      (150 (iload_2)) 
                                      (151 (bipush 10)) 
                                      (153 (if_icmpne 159)) ;;to TAG_7
                                      (156 (goto 21)) ;;to TAG_8
                                      (159 (iload_3)) ;;at TAG_7
                                      (160 (ifeq 209)) ;;to TAG_9
                                      (163 (iload_2)) 
                                      (164 (bipush 32)) 
                                      (166 (if_icmpeq 21)) ;;to TAG_8
                                      (169 (iload_2)) 
                                      (170 (bipush 9)) 
                                      (172 (if_icmpeq 21)) ;;to TAG_8
                                      (175 (iload_2)) 
                                      (176 (bipush 12)) 
                                      (178 (if_icmpne 184)) ;;to TAG_10
                                      (181 (goto 21)) ;;to TAG_8
                                      (184 (iload 6)) ;;at TAG_10
                                      (186 (ifne 204)) ;;to TAG_11
                                      (189 (iload_2)) 
                                      (190 (bipush 13)) 
                                      (192 (if_icmpeq 21)) ;;to TAG_8
                                      (195 (iload_2)) 
                                      (196 (bipush 10)) 
                                      (198 (if_icmpne 204)) ;;to TAG_11
                                      (201 (goto 21)) ;;to TAG_8
                                      (204 (iconst_0)) ;;at TAG_11
                                      (205 (istore_3)) 
                                      (206 (iconst_0)) 
                                      (207 (istore 6)) 
                                      (209 (iload 5)) ;;at TAG_9
                                      (211 (ifeq 235)) ;;to TAG_12
                                      (214 (iconst_0)) 
                                      (215 (istore 5)) 
                                      (217 (iload_2)) 
                                      (218 (bipush 35)) 
                                      (220 (if_icmpeq 229))  ;;to TAG_13
                                      (223 (iload_2)) 
                                      (224 (bipush 33)) 
                                      (226 (if_icmpne 235)) ;;to TAG_12
                                      (229 (iconst_1)) ;;at TAG_13
                                      (230 (istore 4)) 
                                      (232 (goto 21)) ;;to TAG_8
                                      (235 (iload_2)) ;;at TAG_12
                                      (236 (bipush 10)) 
                                      (238 (if_icmpeq 339)) ;;to TAG_14
                                      (241 (iload_2)) 
                                      (242 (bipush 13)) 
                                      (244 (if_icmpeq 339)) ;;to TAG_14
                                      (247 (aload_0)) 
                                      (248 (getfield (fieldCP "lineBuf" "java.util.Properties$LineReader" (array char)))) 
                                      (251 (iload_1)) 
                                      (252 (iinc 1 1)) 
                                      (255 (iload_2)) 
                                      (256 (castore)) 
                                      (257 (iload_1)) 
                                      (258 (aload_0)) 
                                      (259 (getfield (fieldCP "lineBuf" "java.util.Properties$LineReader" (array char)))) 
                                      (262 (arraylength)) 
                                      (263 (if_icmpne 312)) ;;to TAG_15
                                      (266 (aload_0)) 
                                      (267 (getfield (fieldCP "lineBuf" "java.util.Properties$LineReader" (array char)))) 
                                      (270 (arraylength)) 
                                      (271 (iconst_2)) 
                                      (272 (imul)) 
                                      (273 (istore 9)) 
                                      (275 (iload 9)) 
                                      (277 (ifge 284)) ;;to TAG_16
                                      (280 (ldc 0)) ;;INT:: "2147483647"
                                      (282 (istore 9)) 
                                      (284 (iload 9)) ;;at TAG_16
                                      (286 (newarray CHAR)) 
                                      (288 (astore 10)) 
                                      (290 (aload_0)) 
                                      (291 (getfield (fieldCP "lineBuf" "java.util.Properties$LineReader" (array char)))) 
                                      (294 (iconst_0)) 
                                      (295 (aload 10)) 
                                      (297 (iconst_0)) 
                                      (298 (aload_0)) 
                                      (299 (getfield (fieldCP "lineBuf" "java.util.Properties$LineReader" (array char)))) 
                                      (302 (arraylength)) 
                                      (303 (invokestatic (methodCP "arraycopy" "java.lang.System" ((class "java.lang.Object") int (class "java.lang.Object") int int) void))) 
                                      (306 (aload_0)) 
                                      (307 (aload 10)) 
                                      (309 (putfield (fieldCP "lineBuf" "java.util.Properties$LineReader" (array char)))) 
                                      (312 (iload_2)) ;;at TAG_15
                                      (313 (bipush 92)) 
                                      (315 (if_icmpne 333)) ;;to TAG_17
                                      (318 (iload 7)) 
                                      (320 (ifne 327)) ;;to TAG_18
                                      (323 (iconst_1)) 
                                      (324 (goto 328)) ;;to TAG_19
                                      (327 (iconst_0)) ;;at TAG_18
                                      (328 (istore 7)) ;;at TAG_19
                                      (330 (goto 21)) ;;to TAG_8
                                      (333 (iconst_0)) ;;at TAG_17
                                      (334 (istore 7)) 
                                      (336 (goto 21)) ;;to TAG_8
                                      (339 (iload 4)) ;;at TAG_14
                                      (341 (ifne 348)) ;;to TAG_20
                                      (344 (iload_1)) 
                                      (345 (ifne 361)) ;;to TAG_21
                                      (348 (iconst_0)) ;;at TAG_20
                                      (349 (istore 4)) 
                                      (351 (iconst_1)) 
                                      (352 (istore 5)) 
                                      (354 (iconst_1)) 
                                      (355 (istore_3)) 
                                      (356 (iconst_0)) 
                                      (357 (istore_1)) 
                                      (358 (goto 21)) ;;to TAG_8
                                      (361 (aload_0)) ;;at TAG_21
                                      (362 (getfield (fieldCP "inOff" "java.util.Properties$LineReader" int))) 
                                      (365 (aload_0)) 
                                      (366 (getfield (fieldCP "inLimit" "java.util.Properties$LineReader" int))) 
                                      (369 (if_icmplt 422)) ;;to TAG_22
                                      (372 (aload_0)) 
                                      (373 (aload_0)) 
                                      (374 (getfield (fieldCP "inStream" "java.util.Properties$LineReader" (class "java.io.InputStream")))) 
                                      (377 (ifnonnull 394)) ;;to TAG_23
                                      (380 (aload_0)) 
                                      (381 (getfield (fieldCP "reader" "java.util.Properties$LineReader" (class "java.io.Reader")))) 
                                      (384 (aload_0)) 
                                      (385 (getfield (fieldCP "inCharBuf" "java.util.Properties$LineReader" (array char)))) 
                                      (388 (invokevirtual (methodCP "read" "java.io.Reader" ((array char)) int))) 
                                      (391 (goto 405)) ;;to TAG_24
                                      (394 (aload_0)) ;;at TAG_23
                                      (395 (getfield (fieldCP "inStream" "java.util.Properties$LineReader" (class "java.io.InputStream")))) 
                                      (398 (aload_0)) 
                                      (399 (getfield (fieldCP "inByteBuf" "java.util.Properties$LineReader" (array byte)))) 
                                      (402 (invokevirtual (methodCP "read" "java.io.InputStream" ((array byte)) int))) 
                                      (405 (putfield (fieldCP "inLimit" "java.util.Properties$LineReader" int))) ;;at TAG_24
                                      (408 (aload_0)) 
                                      (409 (iconst_0)) 
                                      (410 (putfield (fieldCP "inOff" "java.util.Properties$LineReader" int))) 
                                      (413 (aload_0)) 
                                      (414 (getfield (fieldCP "inLimit" "java.util.Properties$LineReader" int))) 
                                      (417 (ifgt 422)) ;;to TAG_22
                                      (420 (iload_1)) 
                                      (421 (ireturn)) 
                                      (422 (iload 7)) ;;at TAG_22
                                      (424 (ifeq 450)) ;;to TAG_25
                                      (427 (iinc 1 -1)) 
                                      (430 (iconst_1)) 
                                      (431 (istore_3)) 
                                      (432 (iconst_1)) 
                                      (433 (istore 6)) 
                                      (435 (iconst_0)) 
                                      (436 (istore 7)) 
                                      (438 (iload_2)) 
                                      (439 (bipush 13)) 
                                      (441 (if_icmpne 21)) ;;to TAG_8
                                      (444 (iconst_1)) 
                                      (445 (istore 8)) 
                                      (447 (goto 21)) ;;to TAG_8
                                      (450 (iload_1)) ;;at TAG_25
                                      (451 (ireturn)) 
                                      (endofcode 452))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *Properties$LineReader-class-table*
  (make-static-class-decls 
   *java.util.Properties$LineReader*))

(defconst *package-name-map* 
  ("java.util.Properties$LineReader" . "java.util"))

