; pprint$pretty_writer-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:57 CDT 2014.
;

(defconst *clojure.pprint$pretty_writer*
 (make-class-def
      '(class "clojure.pprint$pretty_writer"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "struct")
                        (STRING  "clojure.pprint")
                        (STRING  "logical-block")
                        (STRING  "ref")
                        (STRING  "buffer")
                        (STRING  "pretty-writer")
                        (STRING  "buffer-block")
                        (STRING  "sections")
                        (STRING  "buffer-level")
                        (STRING  "trailing-white-space")
                        (STRING  "pos")
                        (STRING  "base")
                        (STRING  "column-writer")
                        (STRING  "logical-blocks")
                        (STRING  "mode")
                        (STRING  "writing")
                        (STRING  "miser-width")
                        (STRING  "init-proxy")
                        (STRING  "close")
                        (STRING  "flush")
                        (STRING  "ppflush")
                        (STRING  "write")
                        (STRING  "deref"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__9" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__10" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__11" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__12" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__13" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__14" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__15" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__16" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__17" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__18" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 224)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "struct"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.pprint$pretty_writer" (class "clojure.lang.Var"))))
                                      (13 (ldc 2))        ;;STRING:: "clojure.pprint"
                                      (15 (ldc 3))        ;;STRING:: "logical-block"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.pprint$pretty_writer" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 4))        ;;STRING:: "ref"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.pprint$pretty_writer" (class "clojure.lang.Var"))))
                                      (39 (lconst_0))
                                      (40 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (43 (putstatic (fieldCP "const__3" "clojure.pprint$pretty_writer" (class "java.lang.Object"))))
                                      (46 (aconst_null))
                                      (47 (ldc 5))        ;;STRING:: "buffer"
                                      (49 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (52 (checkcast (class "clojure.lang.Keyword")))
                                      (55 (putstatic (fieldCP "const__4" "clojure.pprint$pretty_writer" (class "clojure.lang.Keyword"))))
                                      (58 (aconst_null))
                                      (59 (ldc 6))        ;;STRING:: "pretty-writer"
                                      (61 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (64 (checkcast (class "clojure.lang.Keyword")))
                                      (67 (putstatic (fieldCP "const__5" "clojure.pprint$pretty_writer" (class "clojure.lang.Keyword"))))
                                      (70 (aconst_null))
                                      (71 (ldc 7))        ;;STRING:: "buffer-block"
                                      (73 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (76 (checkcast (class "clojure.lang.Keyword")))
                                      (79 (putstatic (fieldCP "const__6" "clojure.pprint$pretty_writer" (class "clojure.lang.Keyword"))))
                                      (82 (aconst_null))
                                      (83 (ldc 8))        ;;STRING:: "sections"
                                      (85 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (88 (checkcast (class "clojure.lang.Keyword")))
                                      (91 (putstatic (fieldCP "const__7" "clojure.pprint$pretty_writer" (class "clojure.lang.Keyword"))))
                                      (94 (aconst_null))
                                      (95 (ldc 9))        ;;STRING:: "buffer-level"
                                      (97 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (100 (checkcast (class "clojure.lang.Keyword")))
                                      (103 (putstatic (fieldCP "const__8" "clojure.pprint$pretty_writer" (class "clojure.lang.Keyword"))))
                                      (106 (lconst_1))
                                      (107 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (110 (putstatic (fieldCP "const__9" "clojure.pprint$pretty_writer" (class "java.lang.Object"))))
                                      (113 (aconst_null))
                                      (114 (ldc 10))      ;;STRING:: "trailing-white-space"
                                      (116 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (119 (checkcast (class "clojure.lang.Keyword")))
                                      (122 (putstatic (fieldCP "const__10" "clojure.pprint$pretty_writer" (class "clojure.lang.Keyword"))))
                                      (125 (aconst_null))
                                      (126 (ldc 11))      ;;STRING:: "pos"
                                      (128 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (131 (checkcast (class "clojure.lang.Keyword")))
                                      (134 (putstatic (fieldCP "const__11" "clojure.pprint$pretty_writer" (class "clojure.lang.Keyword"))))
                                      (137 (aconst_null))
                                      (138 (ldc 12))      ;;STRING:: "base"
                                      (140 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (143 (checkcast (class "clojure.lang.Keyword")))
                                      (146 (putstatic (fieldCP "const__12" "clojure.pprint$pretty_writer" (class "clojure.lang.Keyword"))))
                                      (149 (ldc 2))       ;;STRING:: "clojure.pprint"
                                      (151 (ldc 13))      ;;STRING:: "column-writer"
                                      (153 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (156 (checkcast (class "clojure.lang.Var")))
                                      (159 (putstatic (fieldCP "const__13" "clojure.pprint$pretty_writer" (class "clojure.lang.Var"))))
                                      (162 (aconst_null))
                                      (163 (ldc 14))      ;;STRING:: "logical-blocks"
                                      (165 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (168 (checkcast (class "clojure.lang.Keyword")))
                                      (171 (putstatic (fieldCP "const__14" "clojure.pprint$pretty_writer" (class "clojure.lang.Keyword"))))
                                      (174 (aconst_null))
                                      (175 (ldc 15))      ;;STRING:: "mode"
                                      (177 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (180 (checkcast (class "clojure.lang.Keyword")))
                                      (183 (putstatic (fieldCP "const__15" "clojure.pprint$pretty_writer" (class "clojure.lang.Keyword"))))
                                      (186 (aconst_null))
                                      (187 (ldc 16))      ;;STRING:: "writing"
                                      (189 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (192 (checkcast (class "clojure.lang.Keyword")))
                                      (195 (putstatic (fieldCP "const__16" "clojure.pprint$pretty_writer" (class "clojure.lang.Keyword"))))
                                      (198 (aconst_null))
                                      (199 (ldc 17))      ;;STRING:: "miser-width"
                                      (201 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (204 (checkcast (class "clojure.lang.Keyword")))
                                      (207 (putstatic (fieldCP "const__17" "clojure.pprint$pretty_writer" (class "clojure.lang.Keyword"))))
                                      (210 (ldc 0))       ;;STRING:: "clojure.core"
                                      (212 (ldc 18))      ;;STRING:: "init-proxy"
                                      (214 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (217 (checkcast (class "clojure.lang.Var")))
                                      (220 (putstatic (fieldCP "const__18" "clojure.pprint$pretty_writer" (class "clojure.lang.Var"))))
                                      (223 (return))
                                      (endofcode 224))
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
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 9) (max_locals . 7) (code_length . 402)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.pprint$pretty_writer" (class "clojure.lang.Var"))))
                                      (3 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (6 (checkcast (class "clojure.lang.IFn")))
                                      (9 (getstatic (fieldCP "const__1" "clojure.pprint$pretty_writer" (class "clojure.lang.Var"))))
                                      (12 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (15 (aconst_null))
                                      (16 (aconst_null))
                                      (17 (getstatic (fieldCP "const__2" "clojure.pprint$pretty_writer" (class "clojure.lang.Var"))))
                                      (20 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (23 (checkcast (class "clojure.lang.IFn")))
                                      (26 (getstatic (fieldCP "const__3" "clojure.pprint$pretty_writer" (class "java.lang.Object"))))
                                      (29 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (34 (getstatic (fieldCP "const__2" "clojure.pprint$pretty_writer" (class "clojure.lang.Var"))))
                                      (37 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (40 (checkcast (class "clojure.lang.IFn")))
                                      (43 (getstatic (fieldCP "const__3" "clojure.pprint$pretty_writer" (class "java.lang.Object"))))
                                      (46 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (51 (getstatic (fieldCP "const__2" "clojure.pprint$pretty_writer" (class "clojure.lang.Var"))))
                                      (54 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (57 (checkcast (class "clojure.lang.IFn")))
                                      (60 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean"))))
                                      (63 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (68 (getstatic (fieldCP "const__2" "clojure.pprint$pretty_writer" (class "clojure.lang.Var"))))
                                      (71 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (74 (checkcast (class "clojure.lang.IFn")))
                                      (77 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean"))))
                                      (80 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (85 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 8))
                                      (90 (astore 4))
                                      (92 (getstatic (fieldCP "const__2" "clojure.pprint$pretty_writer" (class "clojure.lang.Var"))))
                                      (95 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (98 (checkcast (class "clojure.lang.IFn")))
                                      (101 (bipush 22))
                                      (103 (anewarray (class "java.lang.Object")))
                                      (106 (dup))
                                      (107 (iconst_0))
                                      (108 (getstatic (fieldCP "const__4" "clojure.pprint$pretty_writer" (class "clojure.lang.Keyword"))))
                                      (111 (aastore))
                                      (112 (dup))
                                      (113 (iconst_1))
                                      (114 (getstatic (fieldCP "EMPTY" "clojure.lang.PersistentVector" (class "clojure.lang.PersistentVector"))))
                                      (117 (aastore))
                                      (118 (dup))
                                      (119 (iconst_2))
                                      (120 (getstatic (fieldCP "const__5" "clojure.pprint$pretty_writer" (class "clojure.lang.Keyword"))))
                                      (123 (aastore))
                                      (124 (dup))
                                      (125 (iconst_3))
                                      (126 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean"))))
                                      (129 (aastore))
                                      (130 (dup))
                                      (131 (iconst_4))
                                      (132 (getstatic (fieldCP "const__6" "clojure.pprint$pretty_writer" (class "clojure.lang.Keyword"))))
                                      (135 (aastore))
                                      (136 (dup))
                                      (137 (iconst_5))
                                      (138 (aload 4))
                                      (140 (aastore))
                                      (141 (dup))
                                      (142 (bipush 6))
                                      (144 (getstatic (fieldCP "const__7" "clojure.pprint$pretty_writer" (class "clojure.lang.Keyword"))))
                                      (147 (aastore))
                                      (148 (dup))
                                      (149 (bipush 7))
                                      (151 (aconst_null))
                                      (152 (aastore))
                                      (153 (dup))
                                      (154 (bipush 8))
                                      (156 (getstatic (fieldCP "const__8" "clojure.pprint$pretty_writer" (class "clojure.lang.Keyword"))))
                                      (159 (aastore))
                                      (160 (dup))
                                      (161 (bipush 9))
                                      (163 (getstatic (fieldCP "const__9" "clojure.pprint$pretty_writer" (class "java.lang.Object"))))
                                      (166 (aastore))
                                      (167 (dup))
                                      (168 (bipush 10))
                                      (170 (getstatic (fieldCP "const__10" "clojure.pprint$pretty_writer" (class "clojure.lang.Keyword"))))
                                      (173 (aastore))
                                      (174 (dup))
                                      (175 (bipush 11))
                                      (177 (aconst_null))
                                      (178 (aastore))
                                      (179 (dup))
                                      (180 (bipush 12))
                                      (182 (getstatic (fieldCP "const__11" "clojure.pprint$pretty_writer" (class "clojure.lang.Keyword"))))
                                      (185 (aastore))
                                      (186 (dup))
                                      (187 (bipush 13))
                                      (189 (getstatic (fieldCP "const__3" "clojure.pprint$pretty_writer" (class "java.lang.Object"))))
                                      (192 (aastore))
                                      (193 (dup))
                                      (194 (bipush 14))
                                      (196 (getstatic (fieldCP "const__12" "clojure.pprint$pretty_writer" (class "clojure.lang.Keyword"))))
                                      (199 (aastore))
                                      (200 (dup))
                                      (201 (bipush 15))
                                      (203 (getstatic (fieldCP "const__13" "clojure.pprint$pretty_writer" (class "clojure.lang.Var"))))
                                      (206 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (209 (checkcast (class "clojure.lang.IFn")))
                                      (212 (aload_1))
                                      (213 (aconst_null))
                                      (214 (astore_1))
                                      (215 (aload_2))
                                      (216 (aconst_null))
                                      (217 (astore_2))
                                      (218 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (223 (aastore))
                                      (224 (dup))
                                      (225 (bipush 16))
                                      (227 (getstatic (fieldCP "const__14" "clojure.pprint$pretty_writer" (class "clojure.lang.Keyword"))))
                                      (230 (aastore))
                                      (231 (dup))
                                      (232 (bipush 17))
                                      (234 (aload 4))
                                      (236 (aconst_null))
                                      (237 (astore 4))
                                      (239 (aastore))
                                      (240 (dup))
                                      (241 (bipush 18))
                                      (243 (getstatic (fieldCP "const__15" "clojure.pprint$pretty_writer" (class "clojure.lang.Keyword"))))
                                      (246 (aastore))
                                      (247 (dup))
                                      (248 (bipush 19))
                                      (250 (getstatic (fieldCP "const__16" "clojure.pprint$pretty_writer" (class "clojure.lang.Keyword"))))
                                      (253 (aastore))
                                      (254 (dup))
                                      (255 (bipush 20))
                                      (257 (getstatic (fieldCP "const__17" "clojure.pprint$pretty_writer" (class "clojure.lang.Keyword"))))
                                      (260 (aastore))
                                      (261 (dup))
                                      (262 (bipush 21))
                                      (264 (aload_3))
                                      (265 (aconst_null))
                                      (266 (astore_3))
                                      (267 (aastore))
                                      (268 (invokestatic
					(methodCP "mapUniqueKeys" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentMap"))))
                                      (271 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (276 (astore 5))
                                      (278 (new (class "clojure.pprint.proxy$java.io.Writer$IDeref$PrettyFlush$8965b0c6")))
                                      (281 (dup))
                                      (282 (invokespecial
					(methodCP "<init>" "clojure.pprint.proxy$java.io.Writer$IDeref$PrettyFlush$8965b0c6" () void)))
                                      (285 (astore 6))
                                      (287 (getstatic (fieldCP "const__18" "clojure.pprint$pretty_writer" (class "clojure.lang.Var"))))
                                      (290 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (293 (checkcast (class "clojure.lang.IFn")))
                                      (296 (aload 6))
                                      (298 (bipush 10))
                                      (300 (anewarray (class "java.lang.Object")))
                                      (303 (dup))
                                      (304 (iconst_0))
                                      (305 (ldc 19))      ;;STRING:: "close"
                                      (307 (aastore))
                                      (308 (dup))
                                      (309 (iconst_1))
                                      (310 (new (class "clojure.pprint$pretty_writer$fn__7517")))
                                      (313 (dup))
                                      (314 (invokespecial
					(methodCP "<init>" "clojure.pprint$pretty_writer$fn__7517" () void)))
                                      (317 (aastore))
                                      (318 (dup))
                                      (319 (iconst_2))
                                      (320 (ldc 20))      ;;STRING:: "flush"
                                      (322 (aastore))
                                      (323 (dup))
                                      (324 (iconst_3))
                                      (325 (new (class "clojure.pprint$pretty_writer$fn__7519")))
                                      (328 (dup))
                                      (329 (invokespecial
					(methodCP "<init>" "clojure.pprint$pretty_writer$fn__7519" () void)))
                                      (332 (aastore))
                                      (333 (dup))
                                      (334 (iconst_4))
                                      (335 (ldc 21))      ;;STRING:: "ppflush"
                                      (337 (aastore))
                                      (338 (dup))
                                      (339 (iconst_5))
                                      (340 (new (class "clojure.pprint$pretty_writer$fn__7521")))
                                      (343 (dup))
                                      (344 (invokespecial
					(methodCP "<init>" "clojure.pprint$pretty_writer$fn__7521" () void)))
                                      (347 (aastore))
                                      (348 (dup))
                                      (349 (bipush 6))
                                      (351 (ldc 22))      ;;STRING:: "write"
                                      (353 (aastore))
                                      (354 (dup))
                                      (355 (bipush 7))
                                      (357 (new (class "clojure.pprint$pretty_writer$fn__7525")))
                                      (360 (dup))
                                      (361 (invokespecial
					(methodCP "<init>" "clojure.pprint$pretty_writer$fn__7525" () void)))
                                      (364 (aastore))
                                      (365 (dup))
                                      (366 (bipush 8))
                                      (368 (ldc 23))      ;;STRING:: "deref"
                                      (370 (aastore))
                                      (371 (dup))
                                      (372 (bipush 9))
                                      (374 (new (class "clojure.pprint$pretty_writer$fn__7532")))
                                      (377 (dup))
                                      (378 (aload 5))
                                      (380 (aconst_null))
                                      (381 (astore 5))
                                      (383 (invokespecial
					(methodCP "<init>" "clojure.pprint$pretty_writer$fn__7532" ((class "java.lang.Object")) void)))
                                      (386 (aastore))
                                      (387 (invokestatic
					(methodCP "mapUniqueKeys" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentMap"))))
                                      (390 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (395 (pop))
                                      (396 (aload 6))
                                      (398 (aconst_null))
                                      (399 (astore 6))
                                      (401 (areturn))
                                      (endofcode 402))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *pprint$pretty_writer-class-table*
  (make-static-class-decls 
   *clojure.pprint$pretty_writer*))

(defconst *package-name-map* 
  ("clojure.pprint$pretty_writer" . "clojure"))

