; reflect$fn__8987-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:57 CDT 2014.
;

(defconst *clojure.reflect$fn__8987*
 (make-class-def
      '(class "clojure.reflect$fn__8987"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.reflect")
                        (STRING  "Reflector")
                        (STRING  "file")
                        (STRING  "column")
                        (STRING  "line")
                        (STRING  "clojure/reflect.clj"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 5) (max_locals . 0) (code_length . 187)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.reflect"
                                      (2 (ldc 1))         ;;STRING:: "Reflector"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.reflect$fn__8987" (class "clojure.lang.Var"))))
                                      (13 (aconst_null))
                                      (14 (ldc 2))        ;;STRING:: "file"
                                      (16 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (19 (checkcast (class "clojure.lang.Keyword")))
                                      (22 (putstatic (fieldCP "const__1" "clojure.reflect$fn__8987" (class "clojure.lang.Keyword"))))
                                      (25 (aconst_null))
                                      (26 (ldc 3))        ;;STRING:: "column"
                                      (28 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (31 (checkcast (class "clojure.lang.Keyword")))
                                      (34 (putstatic (fieldCP "const__2" "clojure.reflect$fn__8987" (class "clojure.lang.Keyword"))))
                                      (37 (iconst_1))
                                      (38 (invokestatic
					(methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer"))))
                                      (41 (putstatic (fieldCP "const__3" "clojure.reflect$fn__8987" (class "java.lang.Object"))))
                                      (44 (aconst_null))
                                      (45 (ldc 4))        ;;STRING:: "line"
                                      (47 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (50 (checkcast (class "clojure.lang.Keyword")))
                                      (53 (putstatic (fieldCP "const__4" "clojure.reflect$fn__8987" (class "clojure.lang.Keyword"))))
                                      (56 (bipush 44))
                                      (58 (invokestatic
					(methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer"))))
                                      (61 (putstatic (fieldCP "const__5" "clojure.reflect$fn__8987" (class "java.lang.Object"))))
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
                                      (96 (bipush 44))
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
                                      (113 (ldc 5))       ;;STRING:: "clojure/reflect.clj"
                                      (115 (aastore))
                                      (116 (invokestatic
					(methodCP "map" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentMap"))))
                                      (119 (checkcast (class "clojure.lang.AFn")))
                                      (122 (putstatic (fieldCP "const__6" "clojure.reflect$fn__8987" (class "clojure.lang.AFn"))))
                                      (125 (bipush 6))
                                      (127 (anewarray (class "java.lang.Object")))
                                      (130 (dup))
                                      (131 (iconst_0))
                                      (132 (aconst_null))
                                      (133 (ldc 3))       ;;STRING:: "column"
                                      (135 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (138 (aastore))
                                      (139 (dup))
                                      (140 (iconst_1))
                                      (141 (iconst_1))
                                      (142 (invokestatic
					(methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer"))))
                                      (145 (aastore))
                                      (146 (dup))
                                      (147 (iconst_2))
                                      (148 (aconst_null))
                                      (149 (ldc 4))       ;;STRING:: "line"
                                      (151 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (154 (aastore))
                                      (155 (dup))
                                      (156 (iconst_3))
                                      (157 (bipush 44))
                                      (159 (invokestatic
					(methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer"))))
                                      (162 (aastore))
                                      (163 (dup))
                                      (164 (iconst_4))
                                      (165 (aconst_null))
                                      (166 (ldc 2))       ;;STRING:: "file"
                                      (168 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (171 (aastore))
                                      (172 (dup))
                                      (173 (iconst_5))
                                      (174 (ldc 5))       ;;STRING:: "clojure/reflect.clj"
                                      (176 (aastore))
                                      (177 (invokestatic
					(methodCP "map" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentMap"))))
                                      (180 (checkcast (class "clojure.lang.AFn")))
                                      (183 (putstatic (fieldCP "const__7" "clojure.reflect$fn__8987" (class "clojure.lang.AFn"))))
                                      (186 (return))
                                      (endofcode 187))
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
                                   (max_stack . 3) (max_locals . 2) (code_length . 52)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.reflect$fn__8987" (class "clojure.lang.Var")))) 
                                      (3 (dup)) 
                                      (4 (getstatic (fieldCP "const__6" "clojure.reflect$fn__8987" (class "clojure.lang.AFn")))) 
                                      (7 (checkcast (class "clojure.lang.IPersistentMap"))) 
                                      (10 (invokevirtual (methodCP "setMeta" "clojure.lang.Var" ((class "clojure.lang.IPersistentMap")) void))) 
                                      (13 (astore_1)) 
                                      (14 (aload_1)) 
                                      (15 (aconst_null)) 
                                      (16 (astore_1)) 
                                      (17 (checkcast (class "clojure.lang.Var"))) 
                                      (20 (invokevirtual (methodCP "hasRoot" "clojure.lang.Var" () boolean))) 
                                      (23 (ifeq 31))  ;;to TAG_0
                                      (26 (aconst_null)) 
                                      (27 (goto 51)) ;;to TAG_1
                                      (30 (pop)) 
                                      (31 (getstatic (fieldCP "const__0" "clojure.reflect$fn__8987" (class "clojure.lang.Var")))) ;;at TAG_0
                                      (34 (dup)) 
                                      (35 (getstatic (fieldCP "const__7" "clojure.reflect$fn__8987" (class "clojure.lang.AFn")))) 
                                      (38 (checkcast (class "clojure.lang.IPersistentMap"))) 
                                      (41 (invokevirtual (methodCP "setMeta" "clojure.lang.Var" ((class "clojure.lang.IPersistentMap")) void))) 
                                      (44 (dup)) 
                                      (45 (getstatic (fieldCP "EMPTY" "clojure.lang.PersistentArrayMap" (class "clojure.lang.PersistentArrayMap")))) 
                                      (48 (invokevirtual (methodCP "bindRoot" "clojure.lang.Var" ((class "java.lang.Object")) void))) 
                                      (51 (areturn)) ;;at TAG_1
                                      (endofcode 52))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *reflect$fn__8987-class-table*
  (make-static-class-decls 
   *clojure.reflect$fn__8987*))

(defconst *package-name-map* 
  ("clojure.reflect$fn__8987" . "clojure"))

