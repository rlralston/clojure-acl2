; core$every_pred$ep3__6388-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:42 CDT 2014.
;

(defconst *clojure.core$every_pred$ep3__6388*
 (make-class-def
      '(class "clojure.core$every_pred$ep3__6388"
            "clojure.lang.RestFn"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "boolean")
                        (STRING  "every?"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "p2" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "p3" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "p1" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 27)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "boolean"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$every_pred$ep3__6388" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "every?"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$every_pred$ep3__6388" (class "clojure.lang.Var"))))
                                      (26 (return))
                                      (endofcode 27))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 20)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.RestFn" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "p2" "clojure.core$every_pred$ep3__6388" (class "java.lang.Object"))))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "p3" "clojure.core$every_pred$ep3__6388" (class "java.lang.Object"))))
                                      (14 (aload_0))
                                      (15 (aload_3))
                                      (16 (putfield (fieldCP "p1" "clojure.core$every_pred$ep3__6388" (class "java.lang.Object"))))
                                      (19 (return))
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap )))
                        (method "doInvoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 6) (code_length . 95)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (checkcast (class "clojure.lang.IFn"))) 
                                      (4 (aload_1)) 
                                      (5 (aconst_null)) 
                                      (6 (astore_1)) 
                                      (7 (aload_2)) 
                                      (8 (aconst_null)) 
                                      (9 (astore_2)) 
                                      (10 (aload_3)) 
                                      (11 (aconst_null)) 
                                      (12 (astore_3)) 
                                      (13 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) 
                                      (18 (astore 5)) 
                                      (20 (aload 5)) 
                                      (22 (dup)) 
                                      (23 (ifnull 73)) ;;to TAG_0
                                      (26 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (29 (if_acmpeq 74)) ;;to TAG_1
                                      (32 (getstatic (fieldCP "const__1" "clojure.core$every_pred$ep3__6388" (class "clojure.lang.Var")))) 
                                      (35 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (38 (checkcast (class "clojure.lang.IFn"))) 
                                      (41 (new (class "clojure.core$every_pred$ep3__6388$fn__6389"))) 
                                      (44 (dup)) 
                                      (45 (aload_0)) 
                                      (46 (getfield (fieldCP "p2" "clojure.core$every_pred$ep3__6388" (class "java.lang.Object")))) 
                                      (49 (aload_0)) 
                                      (50 (getfield (fieldCP "p3" "clojure.core$every_pred$ep3__6388" (class "java.lang.Object")))) 
                                      (53 (aload_0)) 
                                      (54 (getfield (fieldCP "p1" "clojure.core$every_pred$ep3__6388" (class "java.lang.Object")))) 
                                      (57 (invokespecial (methodCP "<init>" "clojure.core$every_pred$ep3__6388$fn__6389" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) void))) 
                                      (60 (aload 4)) 
                                      (62 (aconst_null)) 
                                      (63 (astore 4)) 
                                      (65 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (70 (goto 79))  ;;to TAG_2
                                      (73 (pop)) ;;at TAG_0
                                      (74 (aload 5)) ;;at TAG_1
                                      (76 (aconst_null)) 
                                      (77 (astore 5)) 
                                      (79 (invokestatic (methodCP "booleanCast" "clojure.lang.RT" ((class "java.lang.Object")) boolean))) ;;at TAG_2
                                      (82 (ifeq 91)) ;;to TAG_3
                                      (85 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (88 (goto 94)) ;;to TAG_4
                                      (91 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) ;;at TAG_3
                                      (94 (areturn)) ;;at TAG_4
                                      (endofcode 95))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 12) (code_length . 323)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "p1" "clojure.core$every_pred$ep3__6388" (class "java.lang.Object")))) 
                                      (4 (checkcast (class "clojure.lang.IFn"))) 
                                      (7 (aload_1)) 
                                      (8 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (13 (astore 4)) 
                                      (15 (aload 4)) 
                                      (17 (dup)) 
                                      (18 (ifnull 301)) ;;to TAG_0
                                      (21 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (24 (if_acmpeq 302)) ;;to TAG_1
                                      (27 (aload_0)) 
                                      (28 (getfield (fieldCP "p2" "clojure.core$every_pred$ep3__6388" (class "java.lang.Object")))) 
                                      (31 (checkcast (class "clojure.lang.IFn"))) 
                                      (34 (aload_1)) 
                                      (35 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (40 (astore 5)) 
                                      (42 (aload 5)) 
                                      (44 (dup)) 
                                      (45 (ifnull 292)) ;;to TAG_2
                                      (48 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (51 (if_acmpeq 293)) ;;to TAG_3
                                      (54 (aload_0)) 
                                      (55 (getfield (fieldCP "p3" "clojure.core$every_pred$ep3__6388" (class "java.lang.Object")))) 
                                      (58 (checkcast (class "clojure.lang.IFn"))) 
                                      (61 (aload_1)) 
                                      (62 (aconst_null)) 
                                      (63 (astore_1)) 
                                      (64 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (69 (astore 6)) 
                                      (71 (aload 6)) 
                                      (73 (dup)) 
                                      (74 (ifnull 283)) ;;to TAG_4
                                      (77 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (80 (if_acmpeq 284)) ;;to TAG_5
                                      (83 (aload_0)) 
                                      (84 (getfield (fieldCP "p1" "clojure.core$every_pred$ep3__6388" (class "java.lang.Object")))) 
                                      (87 (checkcast (class "clojure.lang.IFn"))) 
                                      (90 (aload_2)) 
                                      (91 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (96 (astore 7)) 
                                      (98 (aload 7)) 
                                      (100 (dup)) 
                                      (101 (ifnull 274)) ;;to TAG_6
                                      (104 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (107 (if_acmpeq 275)) ;;to TAG_7
                                      (110 (aload_0)) 
                                      (111 (getfield (fieldCP "p2" "clojure.core$every_pred$ep3__6388" (class "java.lang.Object")))) 
                                      (114 (checkcast (class "clojure.lang.IFn"))) 
                                      (117 (aload_2)) 
                                      (118 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (123 (astore 8)) 
                                      (125 (aload 8)) 
                                      (127 (dup)) 
                                      (128 (ifnull 265)) ;;to TAG_8
                                      (131 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (134 (if_acmpeq 266)) ;;to TAG_9
                                      (137 (aload_0)) 
                                      (138 (getfield (fieldCP "p3" "clojure.core$every_pred$ep3__6388" (class "java.lang.Object")))) 
                                      (141 (checkcast (class "clojure.lang.IFn"))) 
                                      (144 (aload_2)) 
                                      (145 (aconst_null)) 
                                      (146 (astore_2)) 
                                      (147 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (152 (astore 9)) 
                                      (154 (aload 9)) 
                                      (156 (dup)) 
                                      (157 (ifnull 256)) ;;to TAG_10
                                      (160 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (163 (if_acmpeq 257)) ;;to TAG_11
                                      (166 (aload_0)) 
                                      (167 (getfield (fieldCP "p1" "clojure.core$every_pred$ep3__6388" (class "java.lang.Object")))) 
                                      (170 (checkcast (class "clojure.lang.IFn"))) 
                                      (173 (aload_3)) 
                                      (174 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (179 (astore 10)) 
                                      (181 (aload 10)) 
                                      (183 (dup)) 
                                      (184 (ifnull 247)) ;;to TAG_12
                                      (187 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (190 (if_acmpeq 248))  ;;to TAG_13
                                      (193 (aload_0)) 
                                      (194 (getfield (fieldCP "p2" "clojure.core$every_pred$ep3__6388" (class "java.lang.Object")))) 
                                      (197 (checkcast (class "clojure.lang.IFn"))) 
                                      (200 (aload_3)) 
                                      (201 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (206 (astore 11)) 
                                      (208 (aload 11)) 
                                      (210 (dup)) 
                                      (211 (ifnull 238)) ;;to TAG_14
                                      (214 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (217 (if_acmpeq 239)) ;;to TAG_15
                                      (220 (aload_0)) 
                                      (221 (getfield (fieldCP "p3" "clojure.core$every_pred$ep3__6388" (class "java.lang.Object")))) 
                                      (224 (checkcast (class "clojure.lang.IFn"))) 
                                      (227 (aload_3)) 
                                      (228 (aconst_null)) 
                                      (229 (astore_3)) 
                                      (230 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (235 (goto 244)) ;;to TAG_16
                                      (238 (pop)) ;;at TAG_14
                                      (239 (aload 11)) ;;at TAG_15
                                      (241 (aconst_null)) 
                                      (242 (astore 11)) 
                                      (244 (goto 253)) ;;to TAG_17;;at TAG_16
                                      (247 (pop)) ;;at TAG_12
                                      (248 (aload 10)) ;;at TAG_13
                                      (250 (aconst_null)) 
                                      (251 (astore 10)) 
                                      (253 (goto 262)) ;;to TAG_18;;at TAG_17
                                      (256 (pop)) ;;at TAG_10
                                      (257 (aload 9)) ;;at TAG_11
                                      (259 (aconst_null)) 
                                      (260 (astore 9)) 
                                      (262 (goto 271)) ;;to TAG_19;;at TAG_18
                                      (265 (pop)) ;;at TAG_8
                                      (266 (aload 8)) ;;at TAG_9
                                      (268 (aconst_null)) 
                                      (269 (astore 8)) 
                                      (271 (goto 280)) ;;to TAG_20;;at TAG_19
                                      (274 (pop)) ;;at TAG_6
                                      (275 (aload 7)) ;;at TAG_7
                                      (277 (aconst_null)) 
                                      (278 (astore 7)) 
                                      (280 (goto 289)) ;;to TAG_21;;at TAG_20
                                      (283 (pop)) ;;at TAG_4
                                      (284 (aload 6)) ;;at TAG_5
                                      (286 (aconst_null)) 
                                      (287 (astore 6)) 
                                      (289 (goto 298)) ;;to TAG_22;;at TAG_21
                                      (292 (pop)) ;;at TAG_2
                                      (293 (aload 5)) ;;at TAG_3
                                      (295 (aconst_null)) 
                                      (296 (astore 5)) 
                                      (298 (goto 307)) ;;to TAG_23;;at TAG_22
                                      (301 (pop)) ;;at TAG_0
                                      (302 (aload 4)) ;;at TAG_1
                                      (304 (aconst_null)) 
                                      (305 (astore 4)) 
                                      (307 (invokestatic (methodCP "booleanCast" "clojure.lang.RT" ((class "java.lang.Object")) boolean))) ;;at TAG_23
                                      (310 (ifeq 319)) ;;to TAG_24
                                      (313 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (316 (goto 322)) ;;to TAG_25
                                      (319 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) ;;at TAG_24
                                      (322 (areturn)) ;;at TAG_25
                                      (endofcode 323))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 8) (code_length . 209)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "p1" "clojure.core$every_pred$ep3__6388" (class "java.lang.Object")))) 
                                      (4 (checkcast (class "clojure.lang.IFn"))) 
                                      (7 (aload_1)) 
                                      (8 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (13 (astore_3)) 
                                      (14 (aload_3)) 
                                      (15 (dup)) 
                                      (16 (ifnull 189)) ;;to TAG_0
                                      (19 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (22 (if_acmpeq 190))  ;;to TAG_1
                                      (25 (aload_0)) 
                                      (26 (getfield (fieldCP "p2" "clojure.core$every_pred$ep3__6388" (class "java.lang.Object")))) 
                                      (29 (checkcast (class "clojure.lang.IFn"))) 
                                      (32 (aload_1)) 
                                      (33 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (38 (astore 4)) 
                                      (40 (aload 4)) 
                                      (42 (dup)) 
                                      (43 (ifnull 180)) ;;to TAG_2
                                      (46 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (49 (if_acmpeq 181)) ;;to TAG_3
                                      (52 (aload_0)) 
                                      (53 (getfield (fieldCP "p3" "clojure.core$every_pred$ep3__6388" (class "java.lang.Object")))) 
                                      (56 (checkcast (class "clojure.lang.IFn"))) 
                                      (59 (aload_1)) 
                                      (60 (aconst_null)) 
                                      (61 (astore_1)) 
                                      (62 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (67 (astore 5)) 
                                      (69 (aload 5)) 
                                      (71 (dup)) 
                                      (72 (ifnull 171)) ;;to TAG_4
                                      (75 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (78 (if_acmpeq 172)) ;;to TAG_5
                                      (81 (aload_0)) 
                                      (82 (getfield (fieldCP "p1" "clojure.core$every_pred$ep3__6388" (class "java.lang.Object")))) 
                                      (85 (checkcast (class "clojure.lang.IFn"))) 
                                      (88 (aload_2)) 
                                      (89 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (94 (astore 6)) 
                                      (96 (aload 6)) 
                                      (98 (dup)) 
                                      (99 (ifnull 162)) ;;to TAG_6
                                      (102 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (105 (if_acmpeq 163)) ;;to TAG_7
                                      (108 (aload_0)) 
                                      (109 (getfield (fieldCP "p2" "clojure.core$every_pred$ep3__6388" (class "java.lang.Object")))) 
                                      (112 (checkcast (class "clojure.lang.IFn"))) 
                                      (115 (aload_2)) 
                                      (116 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (121 (astore 7)) 
                                      (123 (aload 7)) 
                                      (125 (dup)) 
                                      (126 (ifnull 153)) ;;to TAG_8
                                      (129 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (132 (if_acmpeq 154)) ;;to TAG_9
                                      (135 (aload_0)) 
                                      (136 (getfield (fieldCP "p3" "clojure.core$every_pred$ep3__6388" (class "java.lang.Object")))) 
                                      (139 (checkcast (class "clojure.lang.IFn"))) 
                                      (142 (aload_2)) 
                                      (143 (aconst_null)) 
                                      (144 (astore_2)) 
                                      (145 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (150 (goto 159)) ;;to TAG_10
                                      (153 (pop)) ;;at TAG_8
                                      (154 (aload 7)) ;;at TAG_9
                                      (156 (aconst_null)) 
                                      (157 (astore 7)) 
                                      (159 (goto 168)) ;;to TAG_11;;at TAG_10
                                      (162 (pop)) ;;at TAG_6
                                      (163 (aload 6)) ;;at TAG_7
                                      (165 (aconst_null)) 
                                      (166 (astore 6)) 
                                      (168 (goto 177)) ;;to TAG_12;;at TAG_11
                                      (171 (pop)) ;;at TAG_4
                                      (172 (aload 5)) ;;at TAG_5
                                      (174 (aconst_null)) 
                                      (175 (astore 5)) 
                                      (177 (goto 186)) ;;to TAG_13;;at TAG_12
                                      (180 (pop)) ;;at TAG_2
                                      (181 (aload 4)) ;;at TAG_3
                                      (183 (aconst_null)) 
                                      (184 (astore 4)) 
                                      (186 (goto 193)) ;;to TAG_14;;at TAG_13
                                      (189 (pop)) ;;at TAG_0
                                      (190 (aload_3)) ;;at TAG_1
                                      (191 (aconst_null)) 
                                      (192 (astore_3)) 
                                      (193 (invokestatic (methodCP "booleanCast" "clojure.lang.RT" ((class "java.lang.Object")) boolean))) ;;at TAG_14
                                      (196 (ifeq 205)) ;;to TAG_15
                                      (199 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (202 (goto 208)) ;;to TAG_16
                                      (205 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) ;;at TAG_15
                                      (208 (areturn)) ;;at TAG_16
                                      (endofcode 209))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 4) (code_length . 95)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "p1" "clojure.core$every_pred$ep3__6388" (class "java.lang.Object")))) 
                                      (4 (checkcast (class "clojure.lang.IFn"))) 
                                      (7 (aload_1)) 
                                      (8 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (13 (astore_2)) 
                                      (14 (aload_2)) 
                                      (15 (dup)) 
                                      (16 (ifnull 75)) ;;to TAG_0
                                      (19 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (22 (if_acmpeq 76)) ;;to TAG_1
                                      (25 (aload_0)) 
                                      (26 (getfield (fieldCP "p2" "clojure.core$every_pred$ep3__6388" (class "java.lang.Object")))) 
                                      (29 (checkcast (class "clojure.lang.IFn"))) 
                                      (32 (aload_1)) 
                                      (33 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (38 (astore_3)) 
                                      (39 (aload_3)) 
                                      (40 (dup)) 
                                      (41 (ifnull 68))  ;;to TAG_2
                                      (44 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (47 (if_acmpeq 69)) ;;to TAG_3
                                      (50 (aload_0)) 
                                      (51 (getfield (fieldCP "p3" "clojure.core$every_pred$ep3__6388" (class "java.lang.Object")))) 
                                      (54 (checkcast (class "clojure.lang.IFn"))) 
                                      (57 (aload_1)) 
                                      (58 (aconst_null)) 
                                      (59 (astore_1)) 
                                      (60 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (65 (goto 72)) ;;to TAG_4
                                      (68 (pop)) ;;at TAG_2
                                      (69 (aload_3)) ;;at TAG_3
                                      (70 (aconst_null)) 
                                      (71 (astore_3)) 
                                      (72 (goto 79)) ;;to TAG_5;;at TAG_4
                                      (75 (pop)) ;;at TAG_0
                                      (76 (aload_2)) ;;at TAG_1
                                      (77 (aconst_null)) 
                                      (78 (astore_2)) 
                                      (79 (invokestatic (methodCP "booleanCast" "clojure.lang.RT" ((class "java.lang.Object")) boolean))) ;;at TAG_5
                                      (82 (ifeq 91)) ;;to TAG_6
                                      (85 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (88 (goto 94)) ;;to TAG_7
                                      (91 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) ;;at TAG_6
                                      (94 (areturn)) ;;at TAG_7
                                      (endofcode 95))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 4)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean"))))
                                      (3 (areturn))
                                      (endofcode 4))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getRequiredArity"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 2)
                                   (parsedcode
                                      (0 (iconst_3))
                                      (1 (ireturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$every_pred$ep3__6388-class-table*
  (make-static-class-decls 
   *clojure.core$every_pred$ep3__6388*))

(defconst *package-name-map* 
  ("clojure.core$every_pred$ep3__6388" . "clojure"))
