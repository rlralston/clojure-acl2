; core$fn__5486-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:42 CDT 2014.
;

(defconst *clojure.core$fn__5486*
 (make-class-def
      '(class "clojure.core$fn__5486"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "seq")
                        (STRING  "nth")
                        (STRING  "nthnext")
                        (STRING  "=")
                        (STRING  "not=")
                        (STRING  "else")
                        (STRING  "#\"")
                        (STRING  "\\E\\\"\\Q")
                        (STRING  "\\\""))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__9" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__10" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__11" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 124)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "seq"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$fn__5486" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "nth"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$fn__5486" (class "clojure.lang.Var"))))
                                      (26 (lconst_0))
                                      (27 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (30 (putstatic (fieldCP "const__2" "clojure.core$fn__5486" (class "java.lang.Object"))))
                                      (33 (ldc 0))        ;;STRING:: "clojure.core"
                                      (35 (ldc 3))        ;;STRING:: "nthnext"
                                      (37 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (40 (checkcast (class "clojure.lang.Var")))
                                      (43 (putstatic (fieldCP "const__3" "clojure.core$fn__5486" (class "clojure.lang.Var"))))
                                      (46 (lconst_1))
                                      (47 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (50 (putstatic (fieldCP "const__4" "clojure.core$fn__5486" (class "java.lang.Object"))))
                                      (53 (ldc 0))        ;;STRING:: "clojure.core"
                                      (55 (ldc 4))        ;;STRING:: "="
                                      (57 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (60 (checkcast (class "clojure.lang.Var")))
                                      (63 (putstatic (fieldCP "const__5" "clojure.core$fn__5486" (class "clojure.lang.Var"))))
                                      (66 (bipush 92))
                                      (68 (invokestatic
					(methodCP "valueOf" "java.lang.Character" (char) (class "java.lang.Character"))))
                                      (71 (putstatic (fieldCP "const__6" "clojure.core$fn__5486" (class "java.lang.Object"))))
                                      (74 (ldc 0))        ;;STRING:: "clojure.core"
                                      (76 (ldc 5))        ;;STRING:: "not="
                                      (78 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (81 (checkcast (class "clojure.lang.Var")))
                                      (84 (putstatic (fieldCP "const__7" "clojure.core$fn__5486" (class "clojure.lang.Var"))))
                                      (87 (bipush 69))
                                      (89 (invokestatic
					(methodCP "valueOf" "java.lang.Character" (char) (class "java.lang.Character"))))
                                      (92 (putstatic (fieldCP "const__8" "clojure.core$fn__5486" (class "java.lang.Object"))))
                                      (95 (bipush 81))
                                      (97 (invokestatic
					(methodCP "valueOf" "java.lang.Character" (char) (class "java.lang.Character"))))
                                      (100 (putstatic (fieldCP "const__9" "clojure.core$fn__5486" (class "java.lang.Object"))))
                                      (103 (bipush 34))
                                      (105 (invokestatic
					(methodCP "valueOf" "java.lang.Character" (char) (class "java.lang.Character"))))
                                      (108 (putstatic (fieldCP "const__10" "clojure.core$fn__5486" (class "java.lang.Object"))))
                                      (111 (aconst_null))
                                      (112 (ldc 6))       ;;STRING:: "else"
                                      (114 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (117 (checkcast (class "clojure.lang.Keyword")))
                                      (120 (putstatic (fieldCP "const__11" "clojure.core$fn__5486" (class "clojure.lang.Keyword"))))
                                      (123 (return))
                                      (endofcode 124))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.AFunction" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 19) (code_length . 496)
                                   (parsedcode
                                      (0 (aload_2)) 
                                      (1 (checkcast (class "java.io.Writer"))) 
                                      (4 (ldc 7)) ;;STRING:: "#\""
                                      (6 (checkcast (class "java.lang.String"))) 
                                      (9 (invokevirtual (methodCP "write" "java.io.Writer" ((class "java.lang.String")) void))) 
                                      (12 (aconst_null)) 
                                      (13 (pop)) 
                                      (14 (getstatic (fieldCP "const__0" "clojure.core$fn__5486" (class "clojure.lang.Var")))) 
                                      (17 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (20 (checkcast (class "clojure.lang.IFn"))) 
                                      (23 (aload_1)) 
                                      (24 (aconst_null)) 
                                      (25 (astore_1)) 
                                      (26 (checkcast (class "java.util.regex.Pattern"))) 
                                      (29 (invokevirtual (methodCP "pattern" "java.util.regex.Pattern" () (class "java.lang.String")))) 
                                      (32 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (37 (astore_3)) 
                                      (38 (aload_3)) 
                                      (39 (astore 4)) 
                                      (41 (aload 4)) 
                                      (43 (lconst_0)) 
                                      (44 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (47 (aconst_null)) 
                                      (48 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (51 (astore 5)) 
                                      (53 (getstatic (fieldCP "const__3" "clojure.core$fn__5486" (class "clojure.lang.Var")))) 
                                      (56 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (59 (checkcast (class "clojure.lang.IFn"))) 
                                      (62 (aload 4)) 
                                      (64 (getstatic (fieldCP "const__4" "clojure.core$fn__5486" (class "java.lang.Object")))) 
                                      (67 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (72 (astore 6)) 
                                      (74 (aload 4)) 
                                      (76 (aconst_null)) 
                                      (77 (astore 4)) 
                                      (79 (astore 7)) 
                                      (81 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (84 (astore 8)) 
                                      (86 (aload_3)) 
                                      (87 (aconst_null)) 
                                      (88 (astore_3)) 
                                      (89 (astore 9)) 
                                      (91 (aload 8)) 
                                      (93 (aconst_null)) 
                                      (94 (astore 8)) 
                                      (96 (astore 10)) 
                                      (98 (aload 9)) ;;at TAG_5
                                      (100 (astore 11)) 
                                      (102 (aload 11)) 
                                      (104 (lconst_0)) 
                                      (105 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (108 (aconst_null)) 
                                      (109 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (112 (astore 12)) 
                                      (114 (getstatic (fieldCP "const__3" "clojure.core$fn__5486" (class "clojure.lang.Var")))) 
                                      (117 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (120 (checkcast (class "clojure.lang.IFn"))) 
                                      (123 (aload 11)) 
                                      (125 (getstatic (fieldCP "const__4" "clojure.core$fn__5486" (class "java.lang.Object")))) 
                                      (128 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (133 (astore 13)) 
                                      (135 (aload 11)) 
                                      (137 (aconst_null)) 
                                      (138 (astore 11)) 
                                      (140 (astore 14)) 
                                      (142 (aload 10)) 
                                      (144 (astore 15)) 
                                      (146 (aload 14)) 
                                      (148 (aconst_null)) 
                                      (149 (astore 14)) 
                                      (151 (dup)) 
                                      (152 (ifnull 474)) ;;to TAG_0
                                      (155 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (158 (if_acmpeq 475)) ;;to TAG_1
                                      (161 (aload 12)) 
                                      (163 (getstatic (fieldCP "const__6" "clojure.core$fn__5486" (class "java.lang.Object")))) 
                                      (166 (invokestatic (methodCP "equiv" "clojure.lang.Util" ((class "java.lang.Object") (class "java.lang.Object")) boolean))) 
                                      (169 (ifeq 340)) ;;to TAG_2
                                      (172 (aload 13)) 
                                      (174 (aconst_null)) 
                                      (175 (astore 13)) 
                                      (177 (astore 16)) 
                                      (179 (aload 16)) 
                                      (181 (lconst_0)) 
                                      (182 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (185 (aconst_null)) 
                                      (186 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (189 (astore 17)) 
                                      (191 (getstatic (fieldCP "const__3" "clojure.core$fn__5486" (class "clojure.lang.Var")))) 
                                      (194 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (197 (checkcast (class "clojure.lang.IFn"))) 
                                      (200 (aload 16)) 
                                      (202 (aconst_null)) 
                                      (203 (astore 16)) 
                                      (205 (getstatic (fieldCP "const__4" "clojure.core$fn__5486" (class "java.lang.Object")))) 
                                      (208 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (213 (astore 18)) 
                                      (215 (aload_2)) 
                                      (216 (checkcast (class "java.io.Writer"))) 
                                      (219 (getstatic (fieldCP "const__6" "clojure.core$fn__5486" (class "java.lang.Object")))) 
                                      (222 (checkcast (class "java.lang.Character"))) 
                                      (225 (invokevirtual (methodCP "charValue" "java.lang.Character" () char))) 
                                      (228 (invokevirtual (methodCP "append" "java.io.Writer" (char) (class "java.io.Writer")))) 
                                      (231 (pop)) 
                                      (232 (aload_2)) 
                                      (233 (checkcast (class "java.io.Writer"))) 
                                      (236 (aload 17)) 
                                      (238 (checkcast (class "java.lang.Character"))) 
                                      (241 (invokevirtual (methodCP "charValue" "java.lang.Character" () char))) 
                                      (244 (invokevirtual (methodCP "append" "java.io.Writer" (char) (class "java.io.Writer")))) 
                                      (247 (pop)) 
                                      (248 (aload 15)) 
                                      (250 (aconst_null)) 
                                      (251 (astore 15)) 
                                      (253 (dup)) 
                                      (254 (ifnull 300)) ;;to TAG_3
                                      (257 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (260 (if_acmpeq 301)) ;;to TAG_4
                                      (263 (aload 18)) 
                                      (265 (aconst_null)) 
                                      (266 (astore 18)) 
                                      (268 (getstatic (fieldCP "const__7" "clojure.core$fn__5486" (class "clojure.lang.Var")))) 
                                      (271 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (274 (checkcast (class "clojure.lang.IFn"))) 
                                      (277 (aload 17)) 
                                      (279 (aconst_null)) 
                                      (280 (astore 17)) 
                                      (282 (getstatic (fieldCP "const__8" "clojure.core$fn__5486" (class "java.lang.Object")))) 
                                      (285 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (290 (astore 10)) 
                                      (292 (astore 9)) 
                                      (294 (goto 98)) ;;to TAG_5
                                      (297 (goto 336)) ;;to TAG_6
                                      (300 (pop)) ;;at TAG_3
                                      (301 (aload 18)) ;;at TAG_4
                                      (303 (aconst_null)) 
                                      (304 (astore 18)) 
                                      (306 (aload 17)) 
                                      (308 (aconst_null)) 
                                      (309 (astore 17)) 
                                      (311 (getstatic (fieldCP "const__9" "clojure.core$fn__5486" (class "java.lang.Object")))) 
                                      (314 (invokestatic (methodCP "equiv" "clojure.lang.Util" ((class "java.lang.Object") (class "java.lang.Object")) boolean))) 
                                      (317 (ifeq 326)) ;;to TAG_7
                                      (320 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (323 (goto 329)) ;;to TAG_8
                                      (326 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) ;;at TAG_7
                                      (329 (astore 10)) ;;at TAG_8
                                      (331 (astore 9)) 
                                      (333 (goto 98)) ;;to TAG_5
                                      (336 (goto 471)) ;;to TAG_9;;at TAG_6
                                      (339 (pop)) 
                                      (340 (aload 12)) ;;at TAG_2
                                      (342 (getstatic (fieldCP "const__10" "clojure.core$fn__5486" (class "java.lang.Object")))) 
                                      (345 (invokestatic (methodCP "equiv" "clojure.lang.Util" ((class "java.lang.Object") (class "java.lang.Object")) boolean))) 
                                      (348 (ifeq 416)) ;;to TAG_10
                                      (351 (aload 15)) 
                                      (353 (dup)) 
                                      (354 (ifnull 380)) ;;to TAG_11
                                      (357 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (360 (if_acmpeq 381)) ;;to TAG_12
                                      (363 (aload_2)) 
                                      (364 (checkcast (class "java.io.Writer"))) 
                                      (367 (ldc 8)) ;;STRING:: "\\E\\\"\\Q"
                                      (369 (checkcast (class "java.lang.String"))) 
                                      (372 (invokevirtual (methodCP "write" "java.io.Writer" ((class "java.lang.String")) void))) 
                                      (375 (aconst_null)) 
                                      (376 (pop)) 
                                      (377 (goto 395)) ;;to TAG_13
                                      (380 (pop)) ;;at TAG_11
                                      (381 (aload_2)) ;;at TAG_12
                                      (382 (checkcast (class "java.io.Writer"))) 
                                      (385 (ldc 9)) ;;STRING:: "\\\""
                                      (387 (checkcast (class "java.lang.String"))) 
                                      (390 (invokevirtual (methodCP "write" "java.io.Writer" ((class "java.lang.String")) void))) 
                                      (393 (aconst_null)) 
                                      (394 (pop)) 
                                      (395 (aload 13)) ;;at TAG_13
                                      (397 (aconst_null)) 
                                      (398 (astore 13)) 
                                      (400 (aload 15)) 
                                      (402 (aconst_null)) 
                                      (403 (astore 15)) 
                                      (405 (astore 10)) 
                                      (407 (astore 9)) 
                                      (409 (goto 98)) ;;to TAG_5
                                      (412 (goto 471)) ;;to TAG_9
                                      (415 (pop)) 
                                      (416 (getstatic (fieldCP "const__11" "clojure.core$fn__5486" (class "clojure.lang.Keyword")))) ;;at TAG_10
                                      (419 (dup)) 
                                      (420 (ifnull 468))  ;;to TAG_14
                                      (423 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (426 (if_acmpeq 469)) ;;to TAG_15
                                      (429 (aload_2)) 
                                      (430 (checkcast (class "java.io.Writer"))) 
                                      (433 (aload 12)) 
                                      (435 (aconst_null)) 
                                      (436 (astore 12)) 
                                      (438 (checkcast (class "java.lang.Character"))) 
                                      (441 (invokevirtual (methodCP "charValue" "java.lang.Character" () char))) 
                                      (444 (invokevirtual (methodCP "append" "java.io.Writer" (char) (class "java.io.Writer")))) 
                                      (447 (pop)) 
                                      (448 (aload 13)) 
                                      (450 (aconst_null)) 
                                      (451 (astore 13)) 
                                      (453 (aload 15)) 
                                      (455 (aconst_null)) 
                                      (456 (astore 15)) 
                                      (458 (astore 10)) 
                                      (460 (astore 9)) 
                                      (462 (goto 98)) ;;to TAG_5
                                      (465 (goto 471)) ;;to TAG_9
                                      (468 (pop)) ;;at TAG_14
                                      (469 (aconst_null)) ;;at TAG_15
                                      (470 (pop)) 
                                      (471 (goto 477)) ;;to TAG_16;;at TAG_9
                                      (474 (pop)) ;;at TAG_0
                                      (475 (aconst_null)) ;;at TAG_1
                                      (476 (pop)) 
                                      (477 (aload_2)) ;;at TAG_16
                                      (478 (aconst_null)) 
                                      (479 (astore_2)) 
                                      (480 (checkcast (class "java.io.Writer"))) 
                                      (483 (getstatic (fieldCP "const__10" "clojure.core$fn__5486" (class "java.lang.Object")))) 
                                      (486 (checkcast (class "java.lang.Character"))) 
                                      (489 (invokevirtual (methodCP "charValue" "java.lang.Character" () char))) 
                                      (492 (invokevirtual (methodCP "append" "java.io.Writer" (char) (class "java.io.Writer")))) 
                                      (495 (areturn)) 
                                      (endofcode 496))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$fn__5486-class-table*
  (make-static-class-decls 
   *clojure.core$fn__5486*))

(defconst *package-name-map* 
  ("clojure.core$fn__5486" . "clojure"))

