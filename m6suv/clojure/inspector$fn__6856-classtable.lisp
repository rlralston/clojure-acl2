; inspector$fn__6856-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:49 CDT 2014.
;

(defconst *clojure.inspector$fn__6856*
 (make-class-def
      '(class "clojure.inspector$fn__6856"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.inspector")
                        (STRING  "get-child")
                        (STRING  "file")
                        (STRING  "column")
                        (STRING  "line")
                        (STRING  "clojure/inspector.clj")
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
                        (field "const__6" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__9" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__10" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__11" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 5) (max_locals . 0) (code_length . 238)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.inspector"
                                      (2 (ldc 1))         ;;STRING:: "get-child"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.inspector$fn__6856" (class "clojure.lang.Var"))))
                                      (13 (aconst_null))
                                      (14 (ldc 2))        ;;STRING:: "file"
                                      (16 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (19 (checkcast (class "clojure.lang.Keyword")))
                                      (22 (putstatic (fieldCP "const__1" "clojure.inspector$fn__6856" (class "clojure.lang.Keyword"))))
                                      (25 (aconst_null))
                                      (26 (ldc 3))        ;;STRING:: "column"
                                      (28 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (31 (checkcast (class "clojure.lang.Keyword")))
                                      (34 (putstatic (fieldCP "const__2" "clojure.inspector$fn__6856" (class "clojure.lang.Keyword"))))
                                      (37 (iconst_1))
                                      (38 (invokestatic
					(methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer"))))
                                      (41 (putstatic (fieldCP "const__3" "clojure.inspector$fn__6856" (class "java.lang.Object"))))
                                      (44 (aconst_null))
                                      (45 (ldc 4))        ;;STRING:: "line"
                                      (47 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (50 (checkcast (class "clojure.lang.Keyword")))
                                      (53 (putstatic (fieldCP "const__4" "clojure.inspector$fn__6856" (class "clojure.lang.Keyword"))))
                                      (56 (bipush 30))
                                      (58 (invokestatic
					(methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer"))))
                                      (61 (putstatic (fieldCP "const__5" "clojure.inspector$fn__6856" (class "java.lang.Object"))))
                                      (64 (bipush 6))
                                      (66 (anewarray (class "java.lang.Object")))
                                      (69 (dup))
                                      (70 (iconst_0))
                                      (71 (aconst_null))
                                      (72 (ldc 3))        ;;STRING:: "column"
                                      (74 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (77 (aastore))
                                      (78 (dup))
                                      (79 (iconst_1))
                                      (80 (iconst_1))
                                      (81 (invokestatic
					(methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer"))))
                                      (84 (aastore))
                                      (85 (dup))
                                      (86 (iconst_2))
                                      (87 (aconst_null))
                                      (88 (ldc 4))        ;;STRING:: "line"
                                      (90 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (93 (aastore))
                                      (94 (dup))
                                      (95 (iconst_3))
                                      (96 (bipush 30))
                                      (98 (invokestatic
					(methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer"))))
                                      (101 (aastore))
                                      (102 (dup))
                                      (103 (iconst_4))
                                      (104 (aconst_null))
                                      (105 (ldc 2))       ;;STRING:: "file"
                                      (107 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (110 (aastore))
                                      (111 (dup))
                                      (112 (iconst_5))
                                      (113 (ldc 5))       ;;STRING:: "clojure/inspector.clj"
                                      (115 (aastore))
                                      (116 (invokestatic
					(methodCP "map" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentMap"))))
                                      (119 (checkcast (class "clojure.lang.AFn")))
                                      (122 (putstatic (fieldCP "const__6" "clojure.inspector$fn__6856" (class "clojure.lang.AFn"))))
                                      (125 (ldc 6))       ;;STRING:: "clojure.core"
                                      (127 (ldc 7))       ;;STRING:: "instance?"
                                      (129 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (132 (checkcast (class "clojure.lang.Var")))
                                      (135 (putstatic (fieldCP "const__7" "clojure.inspector$fn__6856" (class "clojure.lang.Var"))))
                                      (138 (ldc 6))       ;;STRING:: "clojure.core"
                                      (140 (ldc 8))       ;;STRING:: "deref"
                                      (142 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (145 (checkcast (class "clojure.lang.Var")))
                                      (148 (putstatic (fieldCP "const__8" "clojure.inspector$fn__6856" (class "clojure.lang.Var"))))
                                      (151 (bipush 6))
                                      (153 (anewarray (class "java.lang.Object")))
                                      (156 (dup))
                                      (157 (iconst_0))
                                      (158 (aconst_null))
                                      (159 (ldc 3))       ;;STRING:: "column"
                                      (161 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (164 (aastore))
                                      (165 (dup))
                                      (166 (iconst_1))
                                      (167 (iconst_1))
                                      (168 (invokestatic
					(methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer"))))
                                      (171 (aastore))
                                      (172 (dup))
                                      (173 (iconst_2))
                                      (174 (aconst_null))
                                      (175 (ldc 4))       ;;STRING:: "line"
                                      (177 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (180 (aastore))
                                      (181 (dup))
                                      (182 (iconst_3))
                                      (183 (bipush 30))
                                      (185 (invokestatic
					(methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer"))))
                                      (188 (aastore))
                                      (189 (dup))
                                      (190 (iconst_4))
                                      (191 (aconst_null))
                                      (192 (ldc 2))       ;;STRING:: "file"
                                      (194 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (197 (aastore))
                                      (198 (dup))
                                      (199 (iconst_5))
                                      (200 (ldc 5))       ;;STRING:: "clojure/inspector.clj"
                                      (202 (aastore))
                                      (203 (invokestatic
					(methodCP "map" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentMap"))))
                                      (206 (checkcast (class "clojure.lang.AFn")))
                                      (209 (putstatic (fieldCP "const__9" "clojure.inspector$fn__6856" (class "clojure.lang.AFn"))))
                                      (212 (aconst_null))
                                      (213 (ldc 9))       ;;STRING:: "default"
                                      (215 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (218 (checkcast (class "clojure.lang.Keyword")))
                                      (221 (putstatic (fieldCP "const__10" "clojure.inspector$fn__6856" (class "clojure.lang.Keyword"))))
                                      (224 (ldc 6))       ;;STRING:: "clojure.core"
                                      (226 (ldc 10))      ;;STRING:: "global-hierarchy"
                                      (228 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (231 (checkcast (class "clojure.lang.Var")))
                                      (234 (putstatic (fieldCP "const__11" "clojure.inspector$fn__6856" (class "clojure.lang.Var"))))
                                      (237 (return))
                                      (endofcode 238))
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
                                      (0 (getstatic (fieldCP "const__0" "clojure.inspector$fn__6856" (class "clojure.lang.Var")))) 
                                      (3 (dup)) 
                                      (4 (getstatic (fieldCP "const__6" "clojure.inspector$fn__6856" (class "clojure.lang.AFn")))) 
                                      (7 (checkcast (class "clojure.lang.IPersistentMap"))) 
                                      (10 (invokevirtual (methodCP "setMeta" "clojure.lang.Var" ((class "clojure.lang.IPersistentMap")) void))) 
                                      (13 (astore_1)) 
                                      (14 (aload_1)) 
                                      (15 (checkcast (class "clojure.lang.Var"))) 
                                      (18 (invokevirtual (methodCP "hasRoot" "clojure.lang.Var" () boolean))) 
                                      (21 (istore_2)) 
                                      (22 (iload_2)) 
                                      (23 (ifeq 50)) ;;to TAG_0
                                      (26 (getstatic (fieldCP "const__8" "clojure.inspector$fn__6856" (class "clojure.lang.Var")))) 
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
                                      (59 (getstatic (fieldCP "const__0" "clojure.inspector$fn__6856" (class "clojure.lang.Var")))) ;;at TAG_2
                                      (62 (dup)) 
                                      (63 (getstatic (fieldCP "const__9" "clojure.inspector$fn__6856" (class "clojure.lang.AFn")))) 
                                      (66 (checkcast (class "clojure.lang.IPersistentMap"))) 
                                      (69 (invokevirtual (methodCP "setMeta" "clojure.lang.Var" ((class "clojure.lang.IPersistentMap")) void))) 
                                      (72 (dup)) 
                                      (73 (new (class "clojure.lang.MultiFn"))) 
                                      (76 (dup)) 
                                      (77 (ldc 1)) ;;STRING:: "get-child"
                                      (79 (checkcast (class "java.lang.String"))) 
                                      (82 (new (class "clojure.inspector$fn__6856$fn__6857"))) 
                                      (85 (dup)) 
                                      (86 (invokespecial (methodCP "<init>" "clojure.inspector$fn__6856$fn__6857" () void))) 
                                      (89 (checkcast (class "clojure.lang.IFn"))) 
                                      (92 (getstatic (fieldCP "const__10" "clojure.inspector$fn__6856" (class "clojure.lang.Keyword")))) 
                                      (95 (getstatic (fieldCP "const__11" "clojure.inspector$fn__6856" (class "clojure.lang.Var")))) 
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


(defconst *inspector$fn__6856-class-table*
  (make-static-class-decls 
   *clojure.inspector$fn__6856*))

(defconst *package-name-map* 
  ("clojure.inspector$fn__6856" . "clojure"))
