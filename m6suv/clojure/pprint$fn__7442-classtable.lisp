; pprint$fn__7442-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:55 CDT 2014.
;

(defconst *clojure.pprint$fn__7442*
 (make-class-def
      '(class "clojure.pprint$fn__7442"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.pprint")
                        (STRING  "emit-nl?")
                        (STRING  "file")
                        (STRING  "column")
                        (STRING  "line")
                        (STRING  "private")
                        (STRING  "clojure/pprint/pretty_writer.clj")
                        (STRING  "clojure.core")
                        (STRING  "instance?")
                        (STRING  "deref")
                        (STRING  "default")
                        (STRING  "global-hierarchy"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__9" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__10" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__11" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__12" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 5) (max_locals . 0) (code_length . 287)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.pprint"
                                      (2 (ldc 1))         ;;STRING:: "emit-nl?"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.pprint$fn__7442" (class "clojure.lang.Var"))))
                                      (13 (aconst_null))
                                      (14 (ldc 2))        ;;STRING:: "file"
                                      (16 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (19 (checkcast (class "clojure.lang.Keyword")))
                                      (22 (putstatic (fieldCP "const__1" "clojure.pprint$fn__7442" (class "clojure.lang.Keyword"))))
                                      (25 (aconst_null))
                                      (26 (ldc 3))        ;;STRING:: "column"
                                      (28 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (31 (checkcast (class "clojure.lang.Keyword")))
                                      (34 (putstatic (fieldCP "const__2" "clojure.pprint$fn__7442" (class "clojure.lang.Keyword"))))
                                      (37 (iconst_1))
                                      (38 (invokestatic
					(methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer"))))
                                      (41 (putstatic (fieldCP "const__3" "clojure.pprint$fn__7442" (class "java.lang.Object"))))
                                      (44 (aconst_null))
                                      (45 (ldc 4))        ;;STRING:: "line"
                                      (47 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (50 (checkcast (class "clojure.lang.Keyword")))
                                      (53 (putstatic (fieldCP "const__4" "clojure.pprint$fn__7442" (class "clojure.lang.Keyword"))))
                                      (56 (sipush 178))
                                      (59 (invokestatic
					(methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer"))))
                                      (62 (putstatic (fieldCP "const__5" "clojure.pprint$fn__7442" (class "java.lang.Object"))))
                                      (65 (aconst_null))
                                      (66 (ldc 5))        ;;STRING:: "private"
                                      (68 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (71 (checkcast (class "clojure.lang.Keyword")))
                                      (74 (putstatic (fieldCP "const__6" "clojure.pprint$fn__7442" (class "clojure.lang.Keyword"))))
                                      (77 (bipush 8))
                                      (79 (anewarray (class "java.lang.Object")))
                                      (82 (dup))
                                      (83 (iconst_0))
                                      (84 (aconst_null))
                                      (85 (ldc 3))        ;;STRING:: "column"
                                      (87 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (90 (aastore))
                                      (91 (dup))
                                      (92 (iconst_1))
                                      (93 (iconst_1))
                                      (94 (invokestatic
					(methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer"))))
                                      (97 (aastore))
                                      (98 (dup))
                                      (99 (iconst_2))
                                      (100 (aconst_null))
                                      (101 (ldc 5))       ;;STRING:: "private"
                                      (103 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (106 (aastore))
                                      (107 (dup))
                                      (108 (iconst_3))
                                      (109 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean"))))
                                      (112 (aastore))
                                      (113 (dup))
                                      (114 (iconst_4))
                                      (115 (aconst_null))
                                      (116 (ldc 4))       ;;STRING:: "line"
                                      (118 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (121 (aastore))
                                      (122 (dup))
                                      (123 (iconst_5))
                                      (124 (sipush 178))
                                      (127 (invokestatic
					(methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer"))))
                                      (130 (aastore))
                                      (131 (dup))
                                      (132 (bipush 6))
                                      (134 (aconst_null))
                                      (135 (ldc 2))       ;;STRING:: "file"
                                      (137 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (140 (aastore))
                                      (141 (dup))
                                      (142 (bipush 7))
                                      (144 (ldc 6))       ;;STRING:: "clojure/pprint/pretty_writer.clj"
                                      (146 (aastore))
                                      (147 (invokestatic
					(methodCP "map" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentMap"))))
                                      (150 (checkcast (class "clojure.lang.AFn")))
                                      (153 (putstatic (fieldCP "const__7" "clojure.pprint$fn__7442" (class "clojure.lang.AFn"))))
                                      (156 (ldc 7))       ;;STRING:: "clojure.core"
                                      (158 (ldc 8))       ;;STRING:: "instance?"
                                      (160 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (163 (checkcast (class "clojure.lang.Var")))
                                      (166 (putstatic (fieldCP "const__8" "clojure.pprint$fn__7442" (class "clojure.lang.Var"))))
                                      (169 (ldc 7))       ;;STRING:: "clojure.core"
                                      (171 (ldc 9))       ;;STRING:: "deref"
                                      (173 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (176 (checkcast (class "clojure.lang.Var")))
                                      (179 (putstatic (fieldCP "const__9" "clojure.pprint$fn__7442" (class "clojure.lang.Var"))))
                                      (182 (bipush 8))
                                      (184 (anewarray (class "java.lang.Object")))
                                      (187 (dup))
                                      (188 (iconst_0))
                                      (189 (aconst_null))
                                      (190 (ldc 3))       ;;STRING:: "column"
                                      (192 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (195 (aastore))
                                      (196 (dup))
                                      (197 (iconst_1))
                                      (198 (iconst_1))
                                      (199 (invokestatic
					(methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer"))))
                                      (202 (aastore))
                                      (203 (dup))
                                      (204 (iconst_2))
                                      (205 (aconst_null))
                                      (206 (ldc 5))       ;;STRING:: "private"
                                      (208 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (211 (aastore))
                                      (212 (dup))
                                      (213 (iconst_3))
                                      (214 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean"))))
                                      (217 (aastore))
                                      (218 (dup))
                                      (219 (iconst_4))
                                      (220 (aconst_null))
                                      (221 (ldc 4))       ;;STRING:: "line"
                                      (223 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (226 (aastore))
                                      (227 (dup))
                                      (228 (iconst_5))
                                      (229 (sipush 178))
                                      (232 (invokestatic
					(methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer"))))
                                      (235 (aastore))
                                      (236 (dup))
                                      (237 (bipush 6))
                                      (239 (aconst_null))
                                      (240 (ldc 2))       ;;STRING:: "file"
                                      (242 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (245 (aastore))
                                      (246 (dup))
                                      (247 (bipush 7))
                                      (249 (ldc 6))       ;;STRING:: "clojure/pprint/pretty_writer.clj"
                                      (251 (aastore))
                                      (252 (invokestatic
					(methodCP "map" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentMap"))))
                                      (255 (checkcast (class "clojure.lang.AFn")))
                                      (258 (putstatic (fieldCP "const__10" "clojure.pprint$fn__7442" (class "clojure.lang.AFn"))))
                                      (261 (aconst_null))
                                      (262 (ldc 10))      ;;STRING:: "default"
                                      (264 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (267 (checkcast (class "clojure.lang.Keyword")))
                                      (270 (putstatic (fieldCP "const__11" "clojure.pprint$fn__7442" (class "clojure.lang.Keyword"))))
                                      (273 (ldc 7))       ;;STRING:: "clojure.core"
                                      (275 (ldc 11))      ;;STRING:: "global-hierarchy"
                                      (277 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (280 (checkcast (class "clojure.lang.Var")))
                                      (283 (putstatic (fieldCP "const__12" "clojure.pprint$fn__7442" (class "clojure.lang.Var"))))
                                      (286 (return))
                                      (endofcode 287))
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
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 8) (max_locals . 3) (code_length . 108)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.pprint$fn__7442" (class "clojure.lang.Var")))) 
                                      (3 (dup)) 
                                      (4 (getstatic (fieldCP "const__7" "clojure.pprint$fn__7442" (class "clojure.lang.AFn")))) 
                                      (7 (checkcast (class "clojure.lang.IPersistentMap"))) 
                                      (10 (invokevirtual (methodCP "setMeta" "clojure.lang.Var" ((class "clojure.lang.IPersistentMap")) void))) 
                                      (13 (astore_1)) 
                                      (14 (aload_1)) 
                                      (15 (checkcast (class "clojure.lang.Var"))) 
                                      (18 (invokevirtual (methodCP "hasRoot" "clojure.lang.Var" () boolean))) 
                                      (21 (istore_2)) 
                                      (22 (iload_2)) 
                                      (23 (ifeq 50)) ;;to TAG_0
                                      (26 (getstatic (fieldCP "const__9" "clojure.pprint$fn__7442" (class "clojure.lang.Var")))) 
                                      (29 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (32 (checkcast (class "clojure.lang.IFn"))) 
                                      (35 (aload_1)) 
                                      (36 (aconst_null)) 
                                      (37 (astore_1)) 
                                      (38 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (43 (instanceof (class "clojure.lang.MultiFn"))) 
                                      (46 (goto 51)) ;;to TAG_1
                                      (49 (pop)) 
                                      (50 (iload_2)) ;;at TAG_0
                                      (51 (ifeq 59))  ;;to TAG_2;;at TAG_1
                                      (54 (aconst_null)) 
                                      (55 (goto 107)) ;;to TAG_3
                                      (58 (pop)) 
                                      (59 (getstatic (fieldCP "const__0" "clojure.pprint$fn__7442" (class "clojure.lang.Var")))) ;;at TAG_2
                                      (62 (dup)) 
                                      (63 (getstatic (fieldCP "const__10" "clojure.pprint$fn__7442" (class "clojure.lang.AFn")))) 
                                      (66 (checkcast (class "clojure.lang.IPersistentMap"))) 
                                      (69 (invokevirtual (methodCP "setMeta" "clojure.lang.Var" ((class "clojure.lang.IPersistentMap")) void))) 
                                      (72 (dup)) 
                                      (73 (new (class "clojure.lang.MultiFn"))) 
                                      (76 (dup)) 
                                      (77 (ldc 1)) ;;STRING:: "emit-nl?"
                                      (79 (checkcast (class "java.lang.String"))) 
                                      (82 (new (class "clojure.pprint$fn__7442$fn__7443"))) 
                                      (85 (dup)) 
                                      (86 (invokespecial (methodCP "<init>" "clojure.pprint$fn__7442$fn__7443" () void))) 
                                      (89 (checkcast (class "clojure.lang.IFn"))) 
                                      (92 (getstatic (fieldCP "const__11" "clojure.pprint$fn__7442" (class "clojure.lang.Keyword")))) 
                                      (95 (getstatic (fieldCP "const__12" "clojure.pprint$fn__7442" (class "clojure.lang.Var")))) 
                                      (98 (checkcast (class "clojure.lang.IRef"))) 
                                      (101 (invokespecial (methodCP "<init>" "clojure.lang.MultiFn" ((class "java.lang.String") (class "clojure.lang.IFn") (class "java.lang.Object") (class "clojure.lang.IRef")) void))) 
                                      (104 (invokevirtual (methodCP "bindRoot" "clojure.lang.Var" ((class "java.lang.Object")) void))) 
                                      (107 (areturn)) ;;at TAG_3
                                      (endofcode 108))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *pprint$fn__7442-class-table*
  (make-static-class-decls 
   *clojure.pprint$fn__7442*))

(defconst *package-name-map* 
  ("clojure.pprint$fn__7442" . "clojure"))
