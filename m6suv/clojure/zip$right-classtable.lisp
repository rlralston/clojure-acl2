; zip$right-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:59 CDT 2014.
;

(defconst *clojure.zip$right*
 (make-class-def
      '(class "clojure.zip$right"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "nth")
                        (STRING  "seq?")
                        (STRING  "seq")
                        (STRING  "get")
                        (STRING  "l")
                        (STRING  "r")
                        (STRING  "nthnext")
                        (STRING  "with-meta")
                        (STRING  "assoc")
                        (STRING  "conj")
                        (STRING  "meta"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__9" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__10" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__11" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__12" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 156)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "nth"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.zip$right" (class "clojure.lang.Var"))))
                                      (13 (lconst_0))
                                      (14 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (17 (putstatic (fieldCP "const__1" "clojure.zip$right" (class "java.lang.Object"))))
                                      (20 (lconst_1))
                                      (21 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (24 (putstatic (fieldCP "const__2" "clojure.zip$right" (class "java.lang.Object"))))
                                      (27 (ldc 0))        ;;STRING:: "clojure.core"
                                      (29 (ldc 2))        ;;STRING:: "seq?"
                                      (31 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (34 (checkcast (class "clojure.lang.Var")))
                                      (37 (putstatic (fieldCP "const__3" "clojure.zip$right" (class "clojure.lang.Var"))))
                                      (40 (ldc 0))        ;;STRING:: "clojure.core"
                                      (42 (ldc 3))        ;;STRING:: "seq"
                                      (44 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (47 (checkcast (class "clojure.lang.Var")))
                                      (50 (putstatic (fieldCP "const__4" "clojure.zip$right" (class "clojure.lang.Var"))))
                                      (53 (ldc 0))        ;;STRING:: "clojure.core"
                                      (55 (ldc 4))        ;;STRING:: "get"
                                      (57 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (60 (checkcast (class "clojure.lang.Var")))
                                      (63 (putstatic (fieldCP "const__5" "clojure.zip$right" (class "clojure.lang.Var"))))
                                      (66 (aconst_null))
                                      (67 (ldc 5))        ;;STRING:: "l"
                                      (69 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (72 (checkcast (class "clojure.lang.Keyword")))
                                      (75 (putstatic (fieldCP "const__6" "clojure.zip$right" (class "clojure.lang.Keyword"))))
                                      (78 (aconst_null))
                                      (79 (ldc 6))        ;;STRING:: "r"
                                      (81 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (84 (checkcast (class "clojure.lang.Keyword")))
                                      (87 (putstatic (fieldCP "const__7" "clojure.zip$right" (class "clojure.lang.Keyword"))))
                                      (90 (ldc 0))        ;;STRING:: "clojure.core"
                                      (92 (ldc 7))        ;;STRING:: "nthnext"
                                      (94 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (97 (checkcast (class "clojure.lang.Var")))
                                      (100 (putstatic (fieldCP "const__8" "clojure.zip$right" (class "clojure.lang.Var"))))
                                      (103 (ldc 0))       ;;STRING:: "clojure.core"
                                      (105 (ldc 8))       ;;STRING:: "with-meta"
                                      (107 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (110 (checkcast (class "clojure.lang.Var")))
                                      (113 (putstatic (fieldCP "const__9" "clojure.zip$right" (class "clojure.lang.Var"))))
                                      (116 (ldc 0))       ;;STRING:: "clojure.core"
                                      (118 (ldc 9))       ;;STRING:: "assoc"
                                      (120 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (123 (checkcast (class "clojure.lang.Var")))
                                      (126 (putstatic (fieldCP "const__10" "clojure.zip$right" (class "clojure.lang.Var"))))
                                      (129 (ldc 0))       ;;STRING:: "clojure.core"
                                      (131 (ldc 10))      ;;STRING:: "conj"
                                      (133 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (136 (checkcast (class "clojure.lang.Var")))
                                      (139 (putstatic (fieldCP "const__11" "clojure.zip$right" (class "clojure.lang.Var"))))
                                      (142 (ldc 0))       ;;STRING:: "clojure.core"
                                      (144 (ldc 11))      ;;STRING:: "meta"
                                      (146 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (149 (checkcast (class "clojure.lang.Var")))
                                      (152 (putstatic (fieldCP "const__12" "clojure.zip$right" (class "clojure.lang.Var"))))
                                      (155 (return))
                                      (endofcode 156))
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
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 11) (max_locals . 13) (code_length . 301)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (astore_2)) 
                                      (2 (aload_2)) 
                                      (3 (lconst_0)) 
                                      (4 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (7 (aconst_null)) 
                                      (8 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (11 (astore_3)) 
                                      (12 (aload_2)) 
                                      (13 (aconst_null)) 
                                      (14 (astore_2)) 
                                      (15 (lconst_1)) 
                                      (16 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (19 (aconst_null)) 
                                      (20 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (23 (astore 4)) 
                                      (25 (getstatic (fieldCP "const__3" "clojure.zip$right" (class "clojure.lang.Var")))) 
                                      (28 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (31 (checkcast (class "clojure.lang.IFn"))) 
                                      (34 (aload 4)) 
                                      (36 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (41 (dup)) 
                                      (42 (ifnull 79)) ;;to TAG_0
                                      (45 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (48 (if_acmpeq 80))  ;;to TAG_1
                                      (51 (getstatic (fieldCP "const__4" "clojure.zip$right" (class "clojure.lang.Var")))) 
                                      (54 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (57 (checkcast (class "clojure.lang.IFn"))) 
                                      (60 (aload 4)) 
                                      (62 (aconst_null)) 
                                      (63 (astore 4)) 
                                      (65 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (70 (checkcast (class "clojure.lang.ISeq"))) 
                                      (73 (invokestatic (methodCP "create" "clojure.lang.PersistentHashMap" ((class "clojure.lang.ISeq")) (class "clojure.lang.PersistentHashMap")))) 
                                      (76 (goto 85)) ;;to TAG_2
                                      (79 (pop)) ;;at TAG_0
                                      (80 (aload 4)) ;;at TAG_1
                                      (82 (aconst_null)) 
                                      (83 (astore 4)) 
                                      (85 (astore 5)) ;;at TAG_2
                                      (87 (aload 5)) 
                                      (89 (astore 6)) 
                                      (91 (aload 5)) 
                                      (93 (getstatic (fieldCP "const__6" "clojure.zip$right" (class "clojure.lang.Keyword")))) 
                                      (96 (invokestatic (methodCP "get" "clojure.lang.RT" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (99 (astore 7)) 
                                      (101 (aload 5)) 
                                      (103 (aconst_null)) 
                                      (104 (astore 5)) 
                                      (106 (getstatic (fieldCP "const__7" "clojure.zip$right" (class "clojure.lang.Keyword")))) 
                                      (109 (invokestatic (methodCP "get" "clojure.lang.RT" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (112 (astore 8)) 
                                      (114 (aload 8)) 
                                      (116 (lconst_0)) 
                                      (117 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (120 (aconst_null)) 
                                      (121 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (124 (astore 9)) 
                                      (126 (getstatic (fieldCP "const__8" "clojure.zip$right" (class "clojure.lang.Var")))) 
                                      (129 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (132 (checkcast (class "clojure.lang.IFn"))) 
                                      (135 (aload 8)) 
                                      (137 (getstatic (fieldCP "const__2" "clojure.zip$right" (class "java.lang.Object")))) 
                                      (140 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (145 (astore 10)) 
                                      (147 (aload 8)) 
                                      (149 (aconst_null)) 
                                      (150 (astore 8)) 
                                      (152 (astore 11)) 
                                      (154 (aload 6)) 
                                      (156 (astore 12)) 
                                      (158 (aload 12)) 
                                      (160 (dup)) 
                                      (161 (ifnull 178)) ;;to TAG_3
                                      (164 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (167 (if_acmpeq 179)) ;;to TAG_4
                                      (170 (aload 11)) 
                                      (172 (aconst_null)) 
                                      (173 (astore 11)) 
                                      (175 (goto 184)) ;;to TAG_5
                                      (178 (pop)) ;;at TAG_3
                                      (179 (aload 12)) ;;at TAG_4
                                      (181 (aconst_null)) 
                                      (182 (astore 12)) 
                                      (184 (dup)) ;;at TAG_5
                                      (185 (ifnull 298)) ;;to TAG_6
                                      (188 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (191 (if_acmpeq 299)) ;;to TAG_7
                                      (194 (getstatic (fieldCP "const__9" "clojure.zip$right" (class "clojure.lang.Var")))) 
                                      (197 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (200 (checkcast (class "clojure.lang.IFn"))) 
                                      (203 (iconst_2)) 
                                      (204 (anewarray (class "java.lang.Object"))) 
                                      (207 (dup)) 
                                      (208 (iconst_0)) 
                                      (209 (aload 9)) 
                                      (211 (aconst_null)) 
                                      (212 (astore 9)) 
                                      (214 (aastore)) 
                                      (215 (dup)) 
                                      (216 (iconst_1)) 
                                      (217 (getstatic (fieldCP "const__10" "clojure.zip$right" (class "clojure.lang.Var")))) 
                                      (220 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (223 (checkcast (class "clojure.lang.IFn"))) 
                                      (226 (aload 6)) 
                                      (228 (aconst_null)) 
                                      (229 (astore 6)) 
                                      (231 (getstatic (fieldCP "const__6" "clojure.zip$right" (class "clojure.lang.Keyword")))) 
                                      (234 (getstatic (fieldCP "const__11" "clojure.zip$right" (class "clojure.lang.Var")))) 
                                      (237 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (240 (checkcast (class "clojure.lang.IFn"))) 
                                      (243 (aload 7)) 
                                      (245 (aconst_null)) 
                                      (246 (astore 7)) 
                                      (248 (aload_3)) 
                                      (249 (aconst_null)) 
                                      (250 (astore_3)) 
                                      (251 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (256 (getstatic (fieldCP "const__7" "clojure.zip$right" (class "clojure.lang.Keyword")))) 
                                      (259 (aload 10)) 
                                      (261 (aconst_null)) 
                                      (262 (astore 10)) 
                                      (264 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 6)) 
                                      (269 (aastore)) 
                                      (270 (invokestatic (methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector")))) 
                                      (273 (getstatic (fieldCP "const__12" "clojure.zip$right" (class "clojure.lang.Var")))) 
                                      (276 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (279 (checkcast (class "clojure.lang.IFn"))) 
                                      (282 (aload_1)) 
                                      (283 (aconst_null)) 
                                      (284 (astore_1)) 
                                      (285 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (290 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (295 (goto 300)) ;;to TAG_8
                                      (298 (pop)) ;;at TAG_6
                                      (299 (aconst_null)) ;;at TAG_7
                                      (300 (areturn)) ;;at TAG_8
                                      (endofcode 301))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *zip$right-class-table*
  (make-static-class-decls 
   *clojure.zip$right*))

(defconst *package-name-map* 
  ("clojure.zip$right" . "clojure"))

