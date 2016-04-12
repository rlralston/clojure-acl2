; UnicodeProp-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:47 CDT 2014.
;

(defconst *java.util.regex.UnicodeProp*
 (make-class-def
      '(class "java.util.regex.UnicodeProp"
            "java.lang.Enum"
            (constant_pool
                        (STRING  "ALPHABETIC")
                        (STRING  "LETTER")
                        (STRING  "IDEOGRAPHIC")
                        (STRING  "LOWERCASE")
                        (STRING  "UPPERCASE")
                        (STRING  "TITLECASE")
                        (STRING  "WHITE_SPACE")
                        (STRING  "CONTROL")
                        (STRING  "PUNCTUATION")
                        (STRING  "HEX_DIGIT")
                        (STRING  "ASSIGNED")
                        (STRING  "NONCHARACTER_CODE_POINT")
                        (STRING  "DIGIT")
                        (STRING  "ALNUM")
                        (STRING  "BLANK")
                        (STRING  "GRAPH")
                        (STRING  "PRINT")
                        (STRING  "WORD")
                        (STRING  "ALPHA")
                        (STRING  "LOWER")
                        (STRING  "UPPER")
                        (STRING  "SPACE")
                        (STRING  "PUNCT")
                        (STRING  "XDIGIT")
                        (STRING  "CNTRL")
                        (STRING  "WHITESPACE")
                        (STRING  "HEXDIGIT")
                        (STRING  "NONCHARACTERCODEPOINT"))
            (fields
                        (field "ALPHABETIC" (class "java.util.regex.UnicodeProp") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "LETTER" (class "java.util.regex.UnicodeProp") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "IDEOGRAPHIC" (class "java.util.regex.UnicodeProp") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "LOWERCASE" (class "java.util.regex.UnicodeProp") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "UPPERCASE" (class "java.util.regex.UnicodeProp") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "TITLECASE" (class "java.util.regex.UnicodeProp") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "WHITE_SPACE" (class "java.util.regex.UnicodeProp") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "CONTROL" (class "java.util.regex.UnicodeProp") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "PUNCTUATION" (class "java.util.regex.UnicodeProp") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "HEX_DIGIT" (class "java.util.regex.UnicodeProp") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "ASSIGNED" (class "java.util.regex.UnicodeProp") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "NONCHARACTER_CODE_POINT" (class "java.util.regex.UnicodeProp") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "DIGIT" (class "java.util.regex.UnicodeProp") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "ALNUM" (class "java.util.regex.UnicodeProp") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "BLANK" (class "java.util.regex.UnicodeProp") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "GRAPH" (class "java.util.regex.UnicodeProp") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "PRINT" (class "java.util.regex.UnicodeProp") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "WORD" (class "java.util.regex.UnicodeProp") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "posix" (class "java.util.HashMap") (accessflags  *class*  *final*  *private*  *static* ) -1)
                        (field "aliases" (class "java.util.HashMap") (accessflags  *class*  *final*  *private*  *static* ) -1)
                        (field "$VALUES" (array (class "java.util.regex.UnicodeProp")) (accessflags  *class*  *final*  *private*  *static* ) -1))
            (methods
                        (method "values"
                              (parameters )
                              (returntype . (array (class "java.util.regex.UnicodeProp")))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 0) (code_length . 10)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "$VALUES" "java.util.regex.UnicodeProp" (array (class "java.util.regex.UnicodeProp")))))
                                      (3 (invokevirtual
					(methodCP "clone" "java.util.regex.UnicodeProp[]" () (class "java.lang.Object"))))
                                      (6 (checkcast (array (class "java.util.regex.UnicodeProp"))))
                                      (9 (areturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "valueOf"
                              (parameters (class "java.lang.String"))
                              (returntype . (class "java.util.regex.UnicodeProp"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 11)
                                   (parsedcode
                                      (0 (ldc_w ))
                                      (3 (aload_0))
                                      (4 (invokestatic
					(methodCP "valueOf" "java.lang.Enum" ((class "java.lang.Class") (class "java.lang.String")) (class "java.lang.Enum"))))
                                      (7 (checkcast (class "java.util.regex.UnicodeProp")))
                                      (10 (areturn))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.String") int)
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 7)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (iload_2))
                                      (3 (invokespecial
					(methodCP "<init>" "java.lang.Enum" ((class "java.lang.String") int) void)))
                                      (6 (return))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap )))
                        (method "forName"
                              (parameters (class "java.lang.String"))
                              (returntype . (class "java.util.regex.UnicodeProp"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 33)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getstatic (fieldCP "ENGLISH" "java.util.Locale" (class "java.util.Locale")))) 
                                      (4 (invokevirtual (methodCP "toUpperCase" "java.lang.String" ((class "java.util.Locale")) (class "java.lang.String")))) 
                                      (7 (astore_0)) 
                                      (8 (getstatic (fieldCP "aliases" "java.util.regex.UnicodeProp" (class "java.util.HashMap")))) 
                                      (11 (aload_0)) 
                                      (12 (invokevirtual (methodCP "get" "java.util.HashMap" ((class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (15 (checkcast (class "java.lang.String"))) 
                                      (18 (astore_1)) 
                                      (19 (aload_1)) 
                                      (20 (ifnull 25)) ;;to TAG_0
                                      (23 (aload_1)) 
                                      (24 (astore_0)) 
                                      (25 (aload_0)) ;;at TAG_0
                                      (26 (invokestatic (methodCP "valueOf" "java.util.regex.UnicodeProp" ((class "java.lang.String")) (class "java.util.regex.UnicodeProp")))) 
                                      (29 (areturn)) ;;at TAG_1
                                      (30 (astore_2)) ;;at TAG_2
                                      (31 (aconst_null)) 
                                      (32 (areturn)) 
                                      (endofcode 33))
                                   (Exceptions 
                                     (handler 25 29  30 (class "java.lang.IllegalArgumentException")))
                                   (StackMap )))
                        (method "forPOSIXName"
                              (parameters (class "java.lang.String"))
                              (returntype . (class "java.util.regex.UnicodeProp"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 28)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "posix" "java.util.regex.UnicodeProp" (class "java.util.HashMap")))) 
                                      (3 (aload_0)) 
                                      (4 (getstatic (fieldCP "ENGLISH" "java.util.Locale" (class "java.util.Locale")))) 
                                      (7 (invokevirtual (methodCP "toUpperCase" "java.lang.String" ((class "java.util.Locale")) (class "java.lang.String")))) 
                                      (10 (invokevirtual (methodCP "get" "java.util.HashMap" ((class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (13 (checkcast (class "java.lang.String"))) 
                                      (16 (astore_0)) 
                                      (17 (aload_0)) 
                                      (18 (ifnonnull 23))  ;;to TAG_0
                                      (21 (aconst_null)) 
                                      (22 (areturn)) 
                                      (23 (aload_0)) ;;at TAG_0
                                      (24 (invokestatic (methodCP "valueOf" "java.util.regex.UnicodeProp" ((class "java.lang.String")) (class "java.util.regex.UnicodeProp")))) 
                                      (27 (areturn)) 
                                      (endofcode 28))
                                   (Exceptions )
                                   (StackMap )))
                        (method "is"
                              (parameters int)
                              (returntype . boolean)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "<init>"
                              (parameters (class "java.lang.String") int (class "java.util.regex.UnicodeProp$1"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 3) (max_locals . 4) (code_length . 7)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (iload_2))
                                      (3 (invokespecial
					(methodCP "<init>" "java.util.regex.UnicodeProp" ((class "java.lang.String") int) void)))
                                      (6 (return))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 0) (code_length . 560)
                                   (parsedcode
                                      (0 (new (class "java.util.regex.UnicodeProp$1")))
                                      (3 (dup))
                                      (4 (ldc 0))         ;;STRING:: "ALPHABETIC"
                                      (6 (iconst_0))
                                      (7 (invokespecial
					(methodCP "<init>" "java.util.regex.UnicodeProp$1" ((class "java.lang.String") int) void)))
                                      (10 (putstatic (fieldCP "ALPHABETIC" "java.util.regex.UnicodeProp" (class "java.util.regex.UnicodeProp"))))
                                      (13 (new (class "java.util.regex.UnicodeProp$2")))
                                      (16 (dup))
                                      (17 (ldc 1))        ;;STRING:: "LETTER"
                                      (19 (iconst_1))
                                      (20 (invokespecial
					(methodCP "<init>" "java.util.regex.UnicodeProp$2" ((class "java.lang.String") int) void)))
                                      (23 (putstatic (fieldCP "LETTER" "java.util.regex.UnicodeProp" (class "java.util.regex.UnicodeProp"))))
                                      (26 (new (class "java.util.regex.UnicodeProp$3")))
                                      (29 (dup))
                                      (30 (ldc 2))        ;;STRING:: "IDEOGRAPHIC"
                                      (32 (iconst_2))
                                      (33 (invokespecial
					(methodCP "<init>" "java.util.regex.UnicodeProp$3" ((class "java.lang.String") int) void)))
                                      (36 (putstatic (fieldCP "IDEOGRAPHIC" "java.util.regex.UnicodeProp" (class "java.util.regex.UnicodeProp"))))
                                      (39 (new (class "java.util.regex.UnicodeProp$4")))
                                      (42 (dup))
                                      (43 (ldc 3))        ;;STRING:: "LOWERCASE"
                                      (45 (iconst_3))
                                      (46 (invokespecial
					(methodCP "<init>" "java.util.regex.UnicodeProp$4" ((class "java.lang.String") int) void)))
                                      (49 (putstatic (fieldCP "LOWERCASE" "java.util.regex.UnicodeProp" (class "java.util.regex.UnicodeProp"))))
                                      (52 (new (class "java.util.regex.UnicodeProp$5")))
                                      (55 (dup))
                                      (56 (ldc 4))        ;;STRING:: "UPPERCASE"
                                      (58 (iconst_4))
                                      (59 (invokespecial
					(methodCP "<init>" "java.util.regex.UnicodeProp$5" ((class "java.lang.String") int) void)))
                                      (62 (putstatic (fieldCP "UPPERCASE" "java.util.regex.UnicodeProp" (class "java.util.regex.UnicodeProp"))))
                                      (65 (new (class "java.util.regex.UnicodeProp$6")))
                                      (68 (dup))
                                      (69 (ldc 5))        ;;STRING:: "TITLECASE"
                                      (71 (iconst_5))
                                      (72 (invokespecial
					(methodCP "<init>" "java.util.regex.UnicodeProp$6" ((class "java.lang.String") int) void)))
                                      (75 (putstatic (fieldCP "TITLECASE" "java.util.regex.UnicodeProp" (class "java.util.regex.UnicodeProp"))))
                                      (78 (new (class "java.util.regex.UnicodeProp$7")))
                                      (81 (dup))
                                      (82 (ldc 6))        ;;STRING:: "WHITE_SPACE"
                                      (84 (bipush 6))
                                      (86 (invokespecial
					(methodCP "<init>" "java.util.regex.UnicodeProp$7" ((class "java.lang.String") int) void)))
                                      (89 (putstatic (fieldCP "WHITE_SPACE" "java.util.regex.UnicodeProp" (class "java.util.regex.UnicodeProp"))))
                                      (92 (new (class "java.util.regex.UnicodeProp$8")))
                                      (95 (dup))
                                      (96 (ldc 7))        ;;STRING:: "CONTROL"
                                      (98 (bipush 7))
                                      (100 (invokespecial
					(methodCP "<init>" "java.util.regex.UnicodeProp$8" ((class "java.lang.String") int) void)))
                                      (103 (putstatic (fieldCP "CONTROL" "java.util.regex.UnicodeProp" (class "java.util.regex.UnicodeProp"))))
                                      (106 (new (class "java.util.regex.UnicodeProp$9")))
                                      (109 (dup))
                                      (110 (ldc 8))       ;;STRING:: "PUNCTUATION"
                                      (112 (bipush 8))
                                      (114 (invokespecial
					(methodCP "<init>" "java.util.regex.UnicodeProp$9" ((class "java.lang.String") int) void)))
                                      (117 (putstatic (fieldCP "PUNCTUATION" "java.util.regex.UnicodeProp" (class "java.util.regex.UnicodeProp"))))
                                      (120 (new (class "java.util.regex.UnicodeProp$10")))
                                      (123 (dup))
                                      (124 (ldc 9))       ;;STRING:: "HEX_DIGIT"
                                      (126 (bipush 9))
                                      (128 (invokespecial
					(methodCP "<init>" "java.util.regex.UnicodeProp$10" ((class "java.lang.String") int) void)))
                                      (131 (putstatic (fieldCP "HEX_DIGIT" "java.util.regex.UnicodeProp" (class "java.util.regex.UnicodeProp"))))
                                      (134 (new (class "java.util.regex.UnicodeProp$11")))
                                      (137 (dup))
                                      (138 (ldc 10))      ;;STRING:: "ASSIGNED"
                                      (140 (bipush 10))
                                      (142 (invokespecial
					(methodCP "<init>" "java.util.regex.UnicodeProp$11" ((class "java.lang.String") int) void)))
                                      (145 (putstatic (fieldCP "ASSIGNED" "java.util.regex.UnicodeProp" (class "java.util.regex.UnicodeProp"))))
                                      (148 (new (class "java.util.regex.UnicodeProp$12")))
                                      (151 (dup))
                                      (152 (ldc 11))      ;;STRING:: "NONCHARACTER_CODE_POINT"
                                      (154 (bipush 11))
                                      (156 (invokespecial
					(methodCP "<init>" "java.util.regex.UnicodeProp$12" ((class "java.lang.String") int) void)))
                                      (159 (putstatic (fieldCP "NONCHARACTER_CODE_POINT" "java.util.regex.UnicodeProp" (class "java.util.regex.UnicodeProp"))))
                                      (162 (new (class "java.util.regex.UnicodeProp$13")))
                                      (165 (dup))
                                      (166 (ldc 12))      ;;STRING:: "DIGIT"
                                      (168 (bipush 12))
                                      (170 (invokespecial
					(methodCP "<init>" "java.util.regex.UnicodeProp$13" ((class "java.lang.String") int) void)))
                                      (173 (putstatic (fieldCP "DIGIT" "java.util.regex.UnicodeProp" (class "java.util.regex.UnicodeProp"))))
                                      (176 (new (class "java.util.regex.UnicodeProp$14")))
                                      (179 (dup))
                                      (180 (ldc 13))      ;;STRING:: "ALNUM"
                                      (182 (bipush 13))
                                      (184 (invokespecial
					(methodCP "<init>" "java.util.regex.UnicodeProp$14" ((class "java.lang.String") int) void)))
                                      (187 (putstatic (fieldCP "ALNUM" "java.util.regex.UnicodeProp" (class "java.util.regex.UnicodeProp"))))
                                      (190 (new (class "java.util.regex.UnicodeProp$15")))
                                      (193 (dup))
                                      (194 (ldc 14))      ;;STRING:: "BLANK"
                                      (196 (bipush 14))
                                      (198 (invokespecial
					(methodCP "<init>" "java.util.regex.UnicodeProp$15" ((class "java.lang.String") int) void)))
                                      (201 (putstatic (fieldCP "BLANK" "java.util.regex.UnicodeProp" (class "java.util.regex.UnicodeProp"))))
                                      (204 (new (class "java.util.regex.UnicodeProp$16")))
                                      (207 (dup))
                                      (208 (ldc 15))      ;;STRING:: "GRAPH"
                                      (210 (bipush 15))
                                      (212 (invokespecial
					(methodCP "<init>" "java.util.regex.UnicodeProp$16" ((class "java.lang.String") int) void)))
                                      (215 (putstatic (fieldCP "GRAPH" "java.util.regex.UnicodeProp" (class "java.util.regex.UnicodeProp"))))
                                      (218 (new (class "java.util.regex.UnicodeProp$17")))
                                      (221 (dup))
                                      (222 (ldc 16))      ;;STRING:: "PRINT"
                                      (224 (bipush 16))
                                      (226 (invokespecial
					(methodCP "<init>" "java.util.regex.UnicodeProp$17" ((class "java.lang.String") int) void)))
                                      (229 (putstatic (fieldCP "PRINT" "java.util.regex.UnicodeProp" (class "java.util.regex.UnicodeProp"))))
                                      (232 (new (class "java.util.regex.UnicodeProp$18")))
                                      (235 (dup))
                                      (236 (ldc 17))      ;;STRING:: "WORD"
                                      (238 (bipush 17))
                                      (240 (invokespecial
					(methodCP "<init>" "java.util.regex.UnicodeProp$18" ((class "java.lang.String") int) void)))
                                      (243 (putstatic (fieldCP "WORD" "java.util.regex.UnicodeProp" (class "java.util.regex.UnicodeProp"))))
                                      (246 (bipush 18))
                                      (248 (anewarray (class "java.util.regex.UnicodeProp")))
                                      (251 (dup))
                                      (252 (iconst_0))
                                      (253 (getstatic (fieldCP "ALPHABETIC" "java.util.regex.UnicodeProp" (class "java.util.regex.UnicodeProp"))))
                                      (256 (aastore))
                                      (257 (dup))
                                      (258 (iconst_1))
                                      (259 (getstatic (fieldCP "LETTER" "java.util.regex.UnicodeProp" (class "java.util.regex.UnicodeProp"))))
                                      (262 (aastore))
                                      (263 (dup))
                                      (264 (iconst_2))
                                      (265 (getstatic (fieldCP "IDEOGRAPHIC" "java.util.regex.UnicodeProp" (class "java.util.regex.UnicodeProp"))))
                                      (268 (aastore))
                                      (269 (dup))
                                      (270 (iconst_3))
                                      (271 (getstatic (fieldCP "LOWERCASE" "java.util.regex.UnicodeProp" (class "java.util.regex.UnicodeProp"))))
                                      (274 (aastore))
                                      (275 (dup))
                                      (276 (iconst_4))
                                      (277 (getstatic (fieldCP "UPPERCASE" "java.util.regex.UnicodeProp" (class "java.util.regex.UnicodeProp"))))
                                      (280 (aastore))
                                      (281 (dup))
                                      (282 (iconst_5))
                                      (283 (getstatic (fieldCP "TITLECASE" "java.util.regex.UnicodeProp" (class "java.util.regex.UnicodeProp"))))
                                      (286 (aastore))
                                      (287 (dup))
                                      (288 (bipush 6))
                                      (290 (getstatic (fieldCP "WHITE_SPACE" "java.util.regex.UnicodeProp" (class "java.util.regex.UnicodeProp"))))
                                      (293 (aastore))
                                      (294 (dup))
                                      (295 (bipush 7))
                                      (297 (getstatic (fieldCP "CONTROL" "java.util.regex.UnicodeProp" (class "java.util.regex.UnicodeProp"))))
                                      (300 (aastore))
                                      (301 (dup))
                                      (302 (bipush 8))
                                      (304 (getstatic (fieldCP "PUNCTUATION" "java.util.regex.UnicodeProp" (class "java.util.regex.UnicodeProp"))))
                                      (307 (aastore))
                                      (308 (dup))
                                      (309 (bipush 9))
                                      (311 (getstatic (fieldCP "HEX_DIGIT" "java.util.regex.UnicodeProp" (class "java.util.regex.UnicodeProp"))))
                                      (314 (aastore))
                                      (315 (dup))
                                      (316 (bipush 10))
                                      (318 (getstatic (fieldCP "ASSIGNED" "java.util.regex.UnicodeProp" (class "java.util.regex.UnicodeProp"))))
                                      (321 (aastore))
                                      (322 (dup))
                                      (323 (bipush 11))
                                      (325 (getstatic (fieldCP "NONCHARACTER_CODE_POINT" "java.util.regex.UnicodeProp" (class "java.util.regex.UnicodeProp"))))
                                      (328 (aastore))
                                      (329 (dup))
                                      (330 (bipush 12))
                                      (332 (getstatic (fieldCP "DIGIT" "java.util.regex.UnicodeProp" (class "java.util.regex.UnicodeProp"))))
                                      (335 (aastore))
                                      (336 (dup))
                                      (337 (bipush 13))
                                      (339 (getstatic (fieldCP "ALNUM" "java.util.regex.UnicodeProp" (class "java.util.regex.UnicodeProp"))))
                                      (342 (aastore))
                                      (343 (dup))
                                      (344 (bipush 14))
                                      (346 (getstatic (fieldCP "BLANK" "java.util.regex.UnicodeProp" (class "java.util.regex.UnicodeProp"))))
                                      (349 (aastore))
                                      (350 (dup))
                                      (351 (bipush 15))
                                      (353 (getstatic (fieldCP "GRAPH" "java.util.regex.UnicodeProp" (class "java.util.regex.UnicodeProp"))))
                                      (356 (aastore))
                                      (357 (dup))
                                      (358 (bipush 16))
                                      (360 (getstatic (fieldCP "PRINT" "java.util.regex.UnicodeProp" (class "java.util.regex.UnicodeProp"))))
                                      (363 (aastore))
                                      (364 (dup))
                                      (365 (bipush 17))
                                      (367 (getstatic (fieldCP "WORD" "java.util.regex.UnicodeProp" (class "java.util.regex.UnicodeProp"))))
                                      (370 (aastore))
                                      (371 (putstatic (fieldCP "$VALUES" "java.util.regex.UnicodeProp" (array (class "java.util.regex.UnicodeProp")))))
                                      (374 (new (class "java.util.HashMap")))
                                      (377 (dup))
                                      (378 (invokespecial
					(methodCP "<init>" "java.util.HashMap" () void)))
                                      (381 (putstatic (fieldCP "posix" "java.util.regex.UnicodeProp" (class "java.util.HashMap"))))
                                      (384 (new (class "java.util.HashMap")))
                                      (387 (dup))
                                      (388 (invokespecial
					(methodCP "<init>" "java.util.HashMap" () void)))
                                      (391 (putstatic (fieldCP "aliases" "java.util.regex.UnicodeProp" (class "java.util.HashMap"))))
                                      (394 (getstatic (fieldCP "posix" "java.util.regex.UnicodeProp" (class "java.util.HashMap"))))
                                      (397 (ldc 18))      ;;STRING:: "ALPHA"
                                      (399 (ldc 0))       ;;STRING:: "ALPHABETIC"
                                      (401 (invokevirtual
					(methodCP "put" "java.util.HashMap" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object"))))
                                      (404 (pop))
                                      (405 (getstatic (fieldCP "posix" "java.util.regex.UnicodeProp" (class "java.util.HashMap"))))
                                      (408 (ldc 19))      ;;STRING:: "LOWER"
                                      (410 (ldc 3))       ;;STRING:: "LOWERCASE"
                                      (412 (invokevirtual
					(methodCP "put" "java.util.HashMap" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object"))))
                                      (415 (pop))
                                      (416 (getstatic (fieldCP "posix" "java.util.regex.UnicodeProp" (class "java.util.HashMap"))))
                                      (419 (ldc 20))      ;;STRING:: "UPPER"
                                      (421 (ldc 4))       ;;STRING:: "UPPERCASE"
                                      (423 (invokevirtual
					(methodCP "put" "java.util.HashMap" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object"))))
                                      (426 (pop))
                                      (427 (getstatic (fieldCP "posix" "java.util.regex.UnicodeProp" (class "java.util.HashMap"))))
                                      (430 (ldc 21))      ;;STRING:: "SPACE"
                                      (432 (ldc 6))       ;;STRING:: "WHITE_SPACE"
                                      (434 (invokevirtual
					(methodCP "put" "java.util.HashMap" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object"))))
                                      (437 (pop))
                                      (438 (getstatic (fieldCP "posix" "java.util.regex.UnicodeProp" (class "java.util.HashMap"))))
                                      (441 (ldc 22))      ;;STRING:: "PUNCT"
                                      (443 (ldc 8))       ;;STRING:: "PUNCTUATION"
                                      (445 (invokevirtual
					(methodCP "put" "java.util.HashMap" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object"))))
                                      (448 (pop))
                                      (449 (getstatic (fieldCP "posix" "java.util.regex.UnicodeProp" (class "java.util.HashMap"))))
                                      (452 (ldc 23))      ;;STRING:: "XDIGIT"
                                      (454 (ldc 9))       ;;STRING:: "HEX_DIGIT"
                                      (456 (invokevirtual
					(methodCP "put" "java.util.HashMap" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object"))))
                                      (459 (pop))
                                      (460 (getstatic (fieldCP "posix" "java.util.regex.UnicodeProp" (class "java.util.HashMap"))))
                                      (463 (ldc 13))      ;;STRING:: "ALNUM"
                                      (465 (ldc 13))      ;;STRING:: "ALNUM"
                                      (467 (invokevirtual
					(methodCP "put" "java.util.HashMap" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object"))))
                                      (470 (pop))
                                      (471 (getstatic (fieldCP "posix" "java.util.regex.UnicodeProp" (class "java.util.HashMap"))))
                                      (474 (ldc 24))      ;;STRING:: "CNTRL"
                                      (476 (ldc 7))       ;;STRING:: "CONTROL"
                                      (478 (invokevirtual
					(methodCP "put" "java.util.HashMap" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object"))))
                                      (481 (pop))
                                      (482 (getstatic (fieldCP "posix" "java.util.regex.UnicodeProp" (class "java.util.HashMap"))))
                                      (485 (ldc 12))      ;;STRING:: "DIGIT"
                                      (487 (ldc 12))      ;;STRING:: "DIGIT"
                                      (489 (invokevirtual
					(methodCP "put" "java.util.HashMap" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object"))))
                                      (492 (pop))
                                      (493 (getstatic (fieldCP "posix" "java.util.regex.UnicodeProp" (class "java.util.HashMap"))))
                                      (496 (ldc 14))      ;;STRING:: "BLANK"
                                      (498 (ldc 14))      ;;STRING:: "BLANK"
                                      (500 (invokevirtual
					(methodCP "put" "java.util.HashMap" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object"))))
                                      (503 (pop))
                                      (504 (getstatic (fieldCP "posix" "java.util.regex.UnicodeProp" (class "java.util.HashMap"))))
                                      (507 (ldc 15))      ;;STRING:: "GRAPH"
                                      (509 (ldc 15))      ;;STRING:: "GRAPH"
                                      (511 (invokevirtual
					(methodCP "put" "java.util.HashMap" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object"))))
                                      (514 (pop))
                                      (515 (getstatic (fieldCP "posix" "java.util.regex.UnicodeProp" (class "java.util.HashMap"))))
                                      (518 (ldc 16))      ;;STRING:: "PRINT"
                                      (520 (ldc 16))      ;;STRING:: "PRINT"
                                      (522 (invokevirtual
					(methodCP "put" "java.util.HashMap" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object"))))
                                      (525 (pop))
                                      (526 (getstatic (fieldCP "aliases" "java.util.regex.UnicodeProp" (class "java.util.HashMap"))))
                                      (529 (ldc 25))      ;;STRING:: "WHITESPACE"
                                      (531 (ldc 6))       ;;STRING:: "WHITE_SPACE"
                                      (533 (invokevirtual
					(methodCP "put" "java.util.HashMap" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object"))))
                                      (536 (pop))
                                      (537 (getstatic (fieldCP "aliases" "java.util.regex.UnicodeProp" (class "java.util.HashMap"))))
                                      (540 (ldc 26))      ;;STRING:: "HEXDIGIT"
                                      (542 (ldc 9))       ;;STRING:: "HEX_DIGIT"
                                      (544 (invokevirtual
					(methodCP "put" "java.util.HashMap" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object"))))
                                      (547 (pop))
                                      (548 (getstatic (fieldCP "aliases" "java.util.regex.UnicodeProp" (class "java.util.HashMap"))))
                                      (551 (ldc 27))      ;;STRING:: "NONCHARACTERCODEPOINT"
                                      (553 (ldc 11))      ;;STRING:: "NONCHARACTER_CODE_POINT"
                                      (555 (invokevirtual
					(methodCP "put" "java.util.HashMap" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object"))))
                                      (558 (pop))
                                      (559 (return))
                                      (endofcode 560))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *abstract*  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *UnicodeProp-class-table*
  (make-static-class-decls 
   *java.util.regex.UnicodeProp*))

(defconst *package-name-map* 
  ("java.util.regex.UnicodeProp" . "java.util.regex"))
