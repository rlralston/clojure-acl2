; core$fn__6093-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:42 CDT 2014.
;

(defconst *clojure.core$fn__6093*
 (make-class-def
      '(class "clojure.core$fn__6093"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "->ArrayChunk")
                        (STRING  "file")
                        (STRING  "column")
                        (STRING  "line")
                        (STRING  "arglists")
                        (STRING  "am")
                        (STRING  "arr")
                        (STRING  "off")
                        (STRING  "end")
                        (STRING  "doc")
                        (STRING  "Positional factory function for class clojure.core.ArrayChunk.")
                        (STRING  "clojure/gvec.clj")
                        (STRING  "clojure.core.ArrayChunk"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__9" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__10" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 11) (max_locals . 0) (code_length . 303)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "->ArrayChunk"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$fn__6093" (class "clojure.lang.Var"))))
                                      (13 (aconst_null))
                                      (14 (ldc 2))        ;;STRING:: "file"
                                      (16 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (19 (checkcast (class "clojure.lang.Keyword")))
                                      (22 (putstatic (fieldCP "const__1" "clojure.core$fn__6093" (class "clojure.lang.Keyword"))))
                                      (25 (aconst_null))
                                      (26 (ldc 3))        ;;STRING:: "column"
                                      (28 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (31 (checkcast (class "clojure.lang.Keyword")))
                                      (34 (putstatic (fieldCP "const__2" "clojure.core$fn__6093" (class "clojure.lang.Keyword"))))
                                      (37 (iconst_1))
                                      (38 (invokestatic
					(methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer"))))
                                      (41 (putstatic (fieldCP "const__3" "clojure.core$fn__6093" (class "java.lang.Object"))))
                                      (44 (aconst_null))
                                      (45 (ldc 4))        ;;STRING:: "line"
                                      (47 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (50 (checkcast (class "clojure.lang.Keyword")))
                                      (53 (putstatic (fieldCP "const__4" "clojure.core$fn__6093" (class "clojure.lang.Keyword"))))
                                      (56 (bipush 34))
                                      (58 (invokestatic
					(methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer"))))
                                      (61 (putstatic (fieldCP "const__5" "clojure.core$fn__6093" (class "java.lang.Object"))))
                                      (64 (aconst_null))
                                      (65 (ldc 5))        ;;STRING:: "arglists"
                                      (67 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (70 (checkcast (class "clojure.lang.Keyword")))
                                      (73 (putstatic (fieldCP "const__6" "clojure.core$fn__6093" (class "clojure.lang.Keyword"))))
                                      (76 (iconst_1))
                                      (77 (anewarray (class "java.lang.Object")))
                                      (80 (dup))
                                      (81 (iconst_0))
                                      (82 (iconst_4))
                                      (83 (anewarray (class "java.lang.Object")))
                                      (86 (dup))
                                      (87 (iconst_0))
                                      (88 (aconst_null))
                                      (89 (ldc 6))        ;;STRING:: "am"
                                      (91 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (94 (aastore))
                                      (95 (dup))
                                      (96 (iconst_1))
                                      (97 (aconst_null))
                                      (98 (ldc 7))        ;;STRING:: "arr"
                                      (100 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (103 (aastore))
                                      (104 (dup))
                                      (105 (iconst_2))
                                      (106 (aconst_null))
                                      (107 (ldc 8))       ;;STRING:: "off"
                                      (109 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (112 (aastore))
                                      (113 (dup))
                                      (114 (iconst_3))
                                      (115 (aconst_null))
                                      (116 (ldc 9))       ;;STRING:: "end"
                                      (118 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (121 (aastore))
                                      (122 (invokestatic
					(methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector"))))
                                      (125 (aastore))
                                      (126 (invokestatic
					(methodCP "asList" "java.util.Arrays" ((array (class "java.lang.Object"))) (class "java.util.List"))))
                                      (129 (invokestatic
					(methodCP "create" "clojure.lang.PersistentList" ((class "java.util.List")) (class "clojure.lang.IPersistentList"))))
                                      (132 (putstatic (fieldCP "const__7" "clojure.core$fn__6093" (class "java.lang.Object"))))
                                      (135 (aconst_null))
                                      (136 (ldc 10))      ;;STRING:: "doc"
                                      (138 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (141 (checkcast (class "clojure.lang.Keyword")))
                                      (144 (putstatic (fieldCP "const__8" "clojure.core$fn__6093" (class "clojure.lang.Keyword"))))
                                      (147 (bipush 10))
                                      (149 (anewarray (class "java.lang.Object")))
                                      (152 (dup))
                                      (153 (iconst_0))
                                      (154 (aconst_null))
                                      (155 (ldc 5))       ;;STRING:: "arglists"
                                      (157 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (160 (aastore))
                                      (161 (dup))
                                      (162 (iconst_1))
                                      (163 (iconst_1))
                                      (164 (anewarray (class "java.lang.Object")))
                                      (167 (dup))
                                      (168 (iconst_0))
                                      (169 (iconst_4))
                                      (170 (anewarray (class "java.lang.Object")))
                                      (173 (dup))
                                      (174 (iconst_0))
                                      (175 (aconst_null))
                                      (176 (ldc 6))       ;;STRING:: "am"
                                      (178 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (181 (aastore))
                                      (182 (dup))
                                      (183 (iconst_1))
                                      (184 (aconst_null))
                                      (185 (ldc 7))       ;;STRING:: "arr"
                                      (187 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (190 (aastore))
                                      (191 (dup))
                                      (192 (iconst_2))
                                      (193 (aconst_null))
                                      (194 (ldc 8))       ;;STRING:: "off"
                                      (196 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (199 (aastore))
                                      (200 (dup))
                                      (201 (iconst_3))
                                      (202 (aconst_null))
                                      (203 (ldc 9))       ;;STRING:: "end"
                                      (205 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (208 (aastore))
                                      (209 (invokestatic
					(methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector"))))
                                      (212 (aastore))
                                      (213 (invokestatic
					(methodCP "asList" "java.util.Arrays" ((array (class "java.lang.Object"))) (class "java.util.List"))))
                                      (216 (invokestatic
					(methodCP "create" "clojure.lang.PersistentList" ((class "java.util.List")) (class "clojure.lang.IPersistentList"))))
                                      (219 (aastore))
                                      (220 (dup))
                                      (221 (iconst_2))
                                      (222 (aconst_null))
                                      (223 (ldc 3))       ;;STRING:: "column"
                                      (225 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (228 (aastore))
                                      (229 (dup))
                                      (230 (iconst_3))
                                      (231 (iconst_1))
                                      (232 (invokestatic
					(methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer"))))
                                      (235 (aastore))
                                      (236 (dup))
                                      (237 (iconst_4))
                                      (238 (aconst_null))
                                      (239 (ldc 10))      ;;STRING:: "doc"
                                      (241 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (244 (aastore))
                                      (245 (dup))
                                      (246 (iconst_5))
                                      (247 (ldc 11))      ;;STRING:: "Positional factory function for class clojure.core.ArrayChunk."
                                      (249 (aastore))
                                      (250 (dup))
                                      (251 (bipush 6))
                                      (253 (aconst_null))
                                      (254 (ldc 4))       ;;STRING:: "line"
                                      (256 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (259 (aastore))
                                      (260 (dup))
                                      (261 (bipush 7))
                                      (263 (bipush 34))
                                      (265 (invokestatic
					(methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer"))))
                                      (268 (aastore))
                                      (269 (dup))
                                      (270 (bipush 8))
                                      (272 (aconst_null))
                                      (273 (ldc 2))       ;;STRING:: "file"
                                      (275 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (278 (aastore))
                                      (279 (dup))
                                      (280 (bipush 9))
                                      (282 (ldc 12))      ;;STRING:: "clojure/gvec.clj"
                                      (284 (aastore))
                                      (285 (invokestatic
					(methodCP "map" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentMap"))))
                                      (288 (checkcast (class "clojure.lang.AFn")))
                                      (291 (putstatic (fieldCP "const__9" "clojure.core$fn__6093" (class "clojure.lang.AFn"))))
                                      (294 (ldc 13))      ;;STRING:: "clojure.core.ArrayChunk"
                                      (296 (invokestatic
					(methodCP "forName" "java.lang.Class" ((class "java.lang.String")) (class "java.lang.Class"))))
                                      (299 (putstatic (fieldCP "const__10" "clojure.core$fn__6093" (class "java.lang.Object"))))
                                      (302 (return))
                                      (endofcode 303))
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
                                   (max_stack . 4) (max_locals . 1) (code_length . 49)
                                   (parsedcode
                                      (0 (aconst_null))
                                      (1 (pop))
                                      (2 (getstatic (fieldCP "CURRENT_NS" "clojure.lang.RT" (class "clojure.lang.Var"))))
                                      (5 (invokevirtual
					(methodCP "deref" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (8 (checkcast (class "clojure.lang.Namespace")))
                                      (11 (ldc 13))       ;;STRING:: "clojure.core.ArrayChunk"
                                      (13 (invokestatic
					(methodCP "forName" "java.lang.Class" ((class "java.lang.String")) (class "java.lang.Class"))))
                                      (16 (invokevirtual
					(methodCP "importClass" "clojure.lang.Namespace" ((class "java.lang.Class")) (class "java.lang.Class"))))
                                      (19 (pop))
                                      (20 (getstatic (fieldCP "const__0" "clojure.core$fn__6093" (class "clojure.lang.Var"))))
                                      (23 (dup))
                                      (24 (getstatic (fieldCP "const__9" "clojure.core$fn__6093" (class "clojure.lang.AFn"))))
                                      (27 (checkcast (class "clojure.lang.IPersistentMap")))
                                      (30 (invokevirtual
					(methodCP "setMeta" "clojure.lang.Var" ((class "clojure.lang.IPersistentMap")) void)))
                                      (33 (dup))
                                      (34 (new (class "clojure.core$fn__6093$__GT_ArrayChunk__6095")))
                                      (37 (dup))
                                      (38 (invokespecial
					(methodCP "<init>" "clojure.core$fn__6093$__GT_ArrayChunk__6095" () void)))
                                      (41 (invokevirtual
					(methodCP "bindRoot" "clojure.lang.Var" ((class "java.lang.Object")) void)))
                                      (44 (pop))
                                      (45 (getstatic (fieldCP "const__10" "clojure.core$fn__6093" (class "java.lang.Object"))))
                                      (48 (areturn))
                                      (endofcode 49))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$fn__6093-class-table*
  (make-static-class-decls 
   *clojure.core$fn__6093*))

(defconst *package-name-map* 
  ("clojure.core$fn__6093" . "clojure"))

