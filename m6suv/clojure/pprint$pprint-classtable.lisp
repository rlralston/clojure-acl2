; pprint$pprint-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:56 CDT 2014.
;

(defconst *clojure.pprint$pprint*
 (make-class-def
      '(class "clojure.pprint$pprint"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.pprint")
                        (STRING  "pprint")
                        (STRING  "clojure.core")
                        (STRING  "*out*")
                        (STRING  "not")
                        (STRING  "pretty-writer?")
                        (STRING  "push-thread-bindings")
                        (STRING  "hash-map")
                        (STRING  "make-pretty-writer")
                        (STRING  "*print-right-margin*")
                        (STRING  "*print-miser-width*")
                        (STRING  "*print-pretty*")
                        (STRING  "=")
                        (STRING  "get-column")
                        (STRING  "prn")
                        (STRING  "pop-thread-bindings")
                        (STRING  "ppflush"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__9" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__10" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__11" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__12" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__13" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__14" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 190)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.pprint"
                                      (2 (ldc 1))         ;;STRING:: "pprint"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.pprint$pprint" (class "clojure.lang.Var"))))
                                      (13 (ldc 2))        ;;STRING:: "clojure.core"
                                      (15 (ldc 3))        ;;STRING:: "*out*"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.pprint$pprint" (class "clojure.lang.Var"))))
                                      (26 (ldc 2))        ;;STRING:: "clojure.core"
                                      (28 (ldc 4))        ;;STRING:: "not"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.pprint$pprint" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.pprint"
                                      (41 (ldc 5))        ;;STRING:: "pretty-writer?"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.pprint$pprint" (class "clojure.lang.Var"))))
                                      (52 (ldc 2))        ;;STRING:: "clojure.core"
                                      (54 (ldc 6))        ;;STRING:: "push-thread-bindings"
                                      (56 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (59 (checkcast (class "clojure.lang.Var")))
                                      (62 (putstatic (fieldCP "const__4" "clojure.pprint$pprint" (class "clojure.lang.Var"))))
                                      (65 (ldc 2))        ;;STRING:: "clojure.core"
                                      (67 (ldc 7))        ;;STRING:: "hash-map"
                                      (69 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (72 (checkcast (class "clojure.lang.Var")))
                                      (75 (putstatic (fieldCP "const__5" "clojure.pprint$pprint" (class "clojure.lang.Var"))))
                                      (78 (ldc 0))        ;;STRING:: "clojure.pprint"
                                      (80 (ldc 8))        ;;STRING:: "make-pretty-writer"
                                      (82 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (85 (checkcast (class "clojure.lang.Var")))
                                      (88 (putstatic (fieldCP "const__6" "clojure.pprint$pprint" (class "clojure.lang.Var"))))
                                      (91 (ldc 0))        ;;STRING:: "clojure.pprint"
                                      (93 (ldc 9))        ;;STRING:: "*print-right-margin*"
                                      (95 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (98 (checkcast (class "clojure.lang.Var")))
                                      (101 (putstatic (fieldCP "const__7" "clojure.pprint$pprint" (class "clojure.lang.Var"))))
                                      (104 (ldc 0))       ;;STRING:: "clojure.pprint"
                                      (106 (ldc 10))      ;;STRING:: "*print-miser-width*"
                                      (108 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (111 (checkcast (class "clojure.lang.Var")))
                                      (114 (putstatic (fieldCP "const__8" "clojure.pprint$pprint" (class "clojure.lang.Var"))))
                                      (117 (ldc 0))       ;;STRING:: "clojure.pprint"
                                      (119 (ldc 11))      ;;STRING:: "*print-pretty*"
                                      (121 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (124 (checkcast (class "clojure.lang.Var")))
                                      (127 (putstatic (fieldCP "const__9" "clojure.pprint$pprint" (class "clojure.lang.Var"))))
                                      (130 (ldc 2))       ;;STRING:: "clojure.core"
                                      (132 (ldc 12))      ;;STRING:: "="
                                      (134 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (137 (checkcast (class "clojure.lang.Var")))
                                      (140 (putstatic (fieldCP "const__10" "clojure.pprint$pprint" (class "clojure.lang.Var"))))
                                      (143 (lconst_0))
                                      (144 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (147 (putstatic (fieldCP "const__11" "clojure.pprint$pprint" (class "java.lang.Object"))))
                                      (150 (ldc 0))       ;;STRING:: "clojure.pprint"
                                      (152 (ldc 13))      ;;STRING:: "get-column"
                                      (154 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (157 (checkcast (class "clojure.lang.Var")))
                                      (160 (putstatic (fieldCP "const__12" "clojure.pprint$pprint" (class "clojure.lang.Var"))))
                                      (163 (ldc 2))       ;;STRING:: "clojure.core"
                                      (165 (ldc 14))      ;;STRING:: "prn"
                                      (167 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (170 (checkcast (class "clojure.lang.Var")))
                                      (173 (putstatic (fieldCP "const__13" "clojure.pprint$pprint" (class "clojure.lang.Var"))))
                                      (176 (ldc 2))       ;;STRING:: "clojure.core"
                                      (178 (ldc 15))      ;;STRING:: "pop-thread-bindings"
                                      (180 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (183 (checkcast (class "clojure.lang.Var")))
                                      (186 (putstatic (fieldCP "const__14" "clojure.pprint$pprint" (class "clojure.lang.Var"))))
                                      (189 (return))
                                      (endofcode 190))
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
                                   (max_stack . 7) (max_locals . 7) (code_length . 307)
                                   (parsedcode
                                      (0 (aload_2)) 
                                      (1 (aconst_null)) 
                                      (2 (astore_2)) 
                                      (3 (astore_3)) 
                                      (4 (getstatic (fieldCP "const__2" "clojure.pprint$pprint" (class "clojure.lang.Var")))) 
                                      (7 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (10 (checkcast (class "clojure.lang.IFn"))) 
                                      (13 (getstatic (fieldCP "const__3" "clojure.pprint$pprint" (class "clojure.lang.Var")))) 
                                      (16 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (19 (checkcast (class "clojure.lang.IFn"))) 
                                      (22 (aload_3)) 
                                      (23 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (28 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (33 (astore 4)) 
                                      (35 (getstatic (fieldCP "const__4" "clojure.pprint$pprint" (class "clojure.lang.Var")))) 
                                      (38 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (41 (checkcast (class "clojure.lang.IFn"))) 
                                      (44 (getstatic (fieldCP "const__5" "clojure.pprint$pprint" (class "clojure.lang.Var")))) 
                                      (47 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (50 (checkcast (class "clojure.lang.IFn"))) 
                                      (53 (getstatic (fieldCP "const__1" "clojure.pprint$pprint" (class "clojure.lang.Var")))) 
                                      (56 (aload 4)) 
                                      (58 (aconst_null)) 
                                      (59 (astore 4)) 
                                      (61 (dup)) 
                                      (62 (ifnull 103)) ;;to TAG_0
                                      (65 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (68 (if_acmpeq 104))  ;;to TAG_1
                                      (71 (getstatic (fieldCP "const__6" "clojure.pprint$pprint" (class "clojure.lang.Var")))) 
                                      (74 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (77 (checkcast (class "clojure.lang.IFn"))) 
                                      (80 (aload_3)) 
                                      (81 (aconst_null)) 
                                      (82 (astore_3)) 
                                      (83 (getstatic (fieldCP "const__7" "clojure.pprint$pprint" (class "clojure.lang.Var")))) 
                                      (86 (invokevirtual (methodCP "get" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (89 (getstatic (fieldCP "const__8" "clojure.pprint$pprint" (class "clojure.lang.Var")))) 
                                      (92 (invokevirtual (methodCP "get" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (95 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) 
                                      (100 (goto 107)) ;;to TAG_2
                                      (103 (pop)) ;;at TAG_0
                                      (104 (aload_3)) ;;at TAG_1
                                      (105 (aconst_null)) 
                                      (106 (astore_3)) 
                                      (107 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) ;;at TAG_2
                                      (112 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (117 (pop)) 
                                      (118 (getstatic (fieldCP "const__4" "clojure.pprint$pprint" (class "clojure.lang.Var")))) ;;at TAG_9
                                      (121 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (124 (checkcast (class "clojure.lang.IFn"))) 
                                      (127 (getstatic (fieldCP "const__5" "clojure.pprint$pprint" (class "clojure.lang.Var")))) 
                                      (130 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (133 (checkcast (class "clojure.lang.IFn"))) 
                                      (136 (getstatic (fieldCP "const__9" "clojure.pprint$pprint" (class "clojure.lang.Var")))) 
                                      (139 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (142 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (147 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (152 (pop)) 
                                      (153 (new (class "clojure.pprint$pprint$fn__7591"))) 
                                      (156 (dup)) 
                                      (157 (aload_1)) 
                                      (158 (aconst_null)) 
                                      (159 (astore_1)) 
                                      (160 (invokespecial (methodCP "<init>" "clojure.pprint$pprint$fn__7591" ((class "java.lang.Object")) void))) 
                                      (163 (checkcast (class "clojure.lang.IFn"))) 
                                      (166 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1)) 
                                      (171 (pop)) 
                                      (172 (getstatic (fieldCP "const__2" "clojure.pprint$pprint" (class "clojure.lang.Var")))) 
                                      (175 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (178 (checkcast (class "clojure.lang.IFn"))) 
                                      (181 (lconst_0)) 
                                      (182 (getstatic (fieldCP "const__12" "clojure.pprint$pprint" (class "clojure.lang.Var")))) 
                                      (185 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (188 (checkcast (class "clojure.lang.IFn"))) 
                                      (191 (getstatic (fieldCP "const__1" "clojure.pprint$pprint" (class "clojure.lang.Var")))) 
                                      (194 (invokevirtual (methodCP "get" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (197 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (202 (invokestatic (methodCP "equiv" "clojure.lang.Util" (long (class "java.lang.Object")) boolean))) 
                                      (205 (ifeq 214)) ;;to TAG_3
                                      (208 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (211 (goto 217)) ;;to TAG_4
                                      (214 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) ;;at TAG_3
                                      (217 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) ;;at TAG_4
                                      (222 (dup)) 
                                      (223 (ifnull 250)) ;;to TAG_5
                                      (226 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (229 (if_acmpeq 251)) ;;to TAG_6
                                      (232 (getstatic (fieldCP "const__13" "clojure.pprint$pprint" (class "clojure.lang.Var")))) 
                                      (235 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (238 (checkcast (class "clojure.lang.IFn"))) 
                                      (241 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1)) 
                                      (246 (pop)) 
                                      (247 (goto 253)) ;;to TAG_7
                                      (250 (pop)) ;;at TAG_5
                                      (251 (aconst_null)) ;;at TAG_6
                                      (252 (pop)) 
                                      (253 (getstatic (fieldCP "const__1" "clojure.pprint$pprint" (class "clojure.lang.Var")))) ;;at TAG_7
                                      (256 (invokevirtual (methodCP "get" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (259 (ldc 16)) ;;STRING:: "ppflush"
                                      (261 (invokestatic (methodCP "invokeNoArgInstanceMember" "clojure.lang.Reflector" ((class "java.lang.Object") (class "java.lang.String")) (class "java.lang.Object")))) 
                                      (264 (astore 5)) 
                                      (266 (getstatic (fieldCP "const__14" "clojure.pprint$pprint" (class "clojure.lang.Var")))) ;;at TAG_10
                                      (269 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (272 (checkcast (class "clojure.lang.IFn"))) 
                                      (275 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1)) 
                                      (280 (pop)) 
                                      (281 (goto 304)) ;;to TAG_8
                                      (284 (astore 6)) ;;at TAG_11
                                      (286 (getstatic (fieldCP "const__14" "clojure.pprint$pprint" (class "clojure.lang.Var")))) 
                                      (289 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (292 (checkcast (class "clojure.lang.IFn"))) 
                                      (295 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1)) 
                                      (300 (pop)) 
                                      (301 (aload 6)) 
                                      (303 (athrow)) 
                                      (304 (aload 5)) ;;at TAG_8
                                      (306 (areturn)) 
                                      (endofcode 307))
                                   (Exceptions 
                                     (handler 118 266  284 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 24)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.pprint$pprint" (class "clojure.lang.Var"))))
                                      (3 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (6 (checkcast (class "clojure.lang.IFn")))
                                      (9 (aload_1))
                                      (10 (aconst_null))
                                      (11 (astore_1))
                                      (12 (getstatic (fieldCP "const__1" "clojure.pprint$pprint" (class "clojure.lang.Var"))))
                                      (15 (invokevirtual
					(methodCP "get" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (18 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (23 (areturn))
                                      (endofcode 24))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *pprint$pprint-class-table*
  (make-static-class-decls 
   *clojure.pprint$pprint*))

(defconst *package-name-map* 
  ("clojure.pprint$pprint" . "clojure"))

