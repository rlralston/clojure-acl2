; core$proxy_name-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:45 CDT 2014.
;

(defconst *clojure.core$proxy_name*
 (make-class-def
      '(class "clojure.core$proxy_name"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "into1")
                        (STRING  "sorted-set")
                        (STRING  "map")
                        (STRING  "apply")
                        (STRING  "str")
                        (STRING  "*ns*")
                        (STRING  "interleave")
                        (STRING  "repeat")
                        (STRING  "concat")
                        (STRING  "hash")
                        (STRING  ".proxy")
                        (STRING  "$"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__9" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__10" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__11" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 147)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "into1"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$proxy_name" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "sorted-set"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$proxy_name" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "map"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.core$proxy_name" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 4))        ;;STRING:: "apply"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.core$proxy_name" (class "clojure.lang.Var"))))
                                      (52 (ldc 0))        ;;STRING:: "clojure.core"
                                      (54 (ldc 5))        ;;STRING:: "str"
                                      (56 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (59 (checkcast (class "clojure.lang.Var")))
                                      (62 (putstatic (fieldCP "const__4" "clojure.core$proxy_name" (class "clojure.lang.Var"))))
                                      (65 (ldc 0))        ;;STRING:: "clojure.core"
                                      (67 (ldc 6))        ;;STRING:: "*ns*"
                                      (69 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (72 (checkcast (class "clojure.lang.Var")))
                                      (75 (putstatic (fieldCP "const__5" "clojure.core$proxy_name" (class "clojure.lang.Var"))))
                                      (78 (bipush 45))
                                      (80 (invokestatic
					(methodCP "valueOf" "java.lang.Character" (char) (class "java.lang.Character"))))
                                      (83 (putstatic (fieldCP "const__6" "clojure.core$proxy_name" (class "java.lang.Object"))))
                                      (86 (bipush 95))
                                      (88 (invokestatic
					(methodCP "valueOf" "java.lang.Character" (char) (class "java.lang.Character"))))
                                      (91 (putstatic (fieldCP "const__7" "clojure.core$proxy_name" (class "java.lang.Object"))))
                                      (94 (ldc 0))        ;;STRING:: "clojure.core"
                                      (96 (ldc 7))        ;;STRING:: "interleave"
                                      (98 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (101 (checkcast (class "clojure.lang.Var")))
                                      (104 (putstatic (fieldCP "const__8" "clojure.core$proxy_name" (class "clojure.lang.Var"))))
                                      (107 (ldc 0))       ;;STRING:: "clojure.core"
                                      (109 (ldc 8))       ;;STRING:: "repeat"
                                      (111 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (114 (checkcast (class "clojure.lang.Var")))
                                      (117 (putstatic (fieldCP "const__9" "clojure.core$proxy_name" (class "clojure.lang.Var"))))
                                      (120 (ldc 0))       ;;STRING:: "clojure.core"
                                      (122 (ldc 9))       ;;STRING:: "concat"
                                      (124 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (127 (checkcast (class "clojure.lang.Var")))
                                      (130 (putstatic (fieldCP "const__10" "clojure.core$proxy_name" (class "clojure.lang.Var"))))
                                      (133 (ldc 0))       ;;STRING:: "clojure.core"
                                      (135 (ldc 10))      ;;STRING:: "hash"
                                      (137 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (140 (checkcast (class "clojure.lang.Var")))
                                      (143 (putstatic (fieldCP "const__11" "clojure.core$proxy_name" (class "clojure.lang.Var"))))
                                      (146 (return))
                                      (endofcode 147))
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
                                   (max_stack . 15) (max_locals . 4) (code_length . 241)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$proxy_name" (class "clojure.lang.Var"))))
                                      (3 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (6 (checkcast (class "clojure.lang.IFn")))
                                      (9 (getstatic (fieldCP "const__1" "clojure.core$proxy_name" (class "clojure.lang.Var"))))
                                      (12 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (15 (checkcast (class "clojure.lang.IFn")))
                                      (18 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1))
                                      (23 (getstatic (fieldCP "const__2" "clojure.core$proxy_name" (class "clojure.lang.Var"))))
                                      (26 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (29 (checkcast (class "clojure.lang.IFn")))
                                      (32 (new (class "clojure.core$proxy_name$fn__5180")))
                                      (35 (dup))
                                      (36 (invokespecial
					(methodCP "<init>" "clojure.core$proxy_name$fn__5180" () void)))
                                      (39 (aload_2))
                                      (40 (aconst_null))
                                      (41 (astore_2))
                                      (42 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (47 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (52 (astore_3))
                                      (53 (getstatic (fieldCP "const__3" "clojure.core$proxy_name" (class "clojure.lang.Var"))))
                                      (56 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (59 (checkcast (class "clojure.lang.IFn")))
                                      (62 (getstatic (fieldCP "const__4" "clojure.core$proxy_name" (class "clojure.lang.Var"))))
                                      (65 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (68 (getstatic (fieldCP "const__4" "clojure.core$proxy_name" (class "clojure.lang.Var"))))
                                      (71 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (74 (checkcast (class "clojure.lang.IFn")))
                                      (77 (getstatic (fieldCP "const__5" "clojure.core$proxy_name" (class "clojure.lang.Var"))))
                                      (80 (invokevirtual
					(methodCP "get" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (83 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (88 (checkcast (class "java.lang.String")))
                                      (91 (getstatic (fieldCP "const__6" "clojure.core$proxy_name" (class "java.lang.Object"))))
                                      (94 (checkcast (class "java.lang.Character")))
                                      (97 (invokevirtual
					(methodCP "charValue" "java.lang.Character" () char)))
                                      (100 (getstatic (fieldCP "const__7" "clojure.core$proxy_name" (class "java.lang.Object"))))
                                      (103 (checkcast (class "java.lang.Character")))
                                      (106 (invokevirtual
					(methodCP "charValue" "java.lang.Character" () char)))
                                      (109 (invokevirtual
					(methodCP "replace" "java.lang.String" (char char) (class "java.lang.String"))))
                                      (112 (ldc 11))      ;;STRING:: ".proxy"
                                      (114 (getstatic (fieldCP "const__8" "clojure.core$proxy_name" (class "clojure.lang.Var"))))
                                      (117 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (120 (checkcast (class "clojure.lang.IFn")))
                                      (123 (getstatic (fieldCP "const__9" "clojure.core$proxy_name" (class "clojure.lang.Var"))))
                                      (126 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (129 (checkcast (class "clojure.lang.IFn")))
                                      (132 (ldc 12))      ;;STRING:: "$"
                                      (134 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (139 (getstatic (fieldCP "const__10" "clojure.core$proxy_name" (class "clojure.lang.Var"))))
                                      (142 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (145 (checkcast (class "clojure.lang.IFn")))
                                      (148 (iconst_1))
                                      (149 (anewarray (class "java.lang.Object")))
                                      (152 (dup))
                                      (153 (iconst_0))
                                      (154 (aload_1))
                                      (155 (aconst_null))
                                      (156 (astore_1))
                                      (157 (checkcast (class "java.lang.Class")))
                                      (160 (invokevirtual
					(methodCP "getName" "java.lang.Class" () (class "java.lang.String"))))
                                      (163 (aastore))
                                      (164 (invokestatic
					(methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector"))))
                                      (167 (getstatic (fieldCP "const__2" "clojure.core$proxy_name" (class "clojure.lang.Var"))))
                                      (170 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (173 (checkcast (class "clojure.lang.IFn")))
                                      (176 (new (class "clojure.core$proxy_name$fn__5182")))
                                      (179 (dup))
                                      (180 (invokespecial
					(methodCP "<init>" "clojure.core$proxy_name$fn__5182" () void)))
                                      (183 (aload_3))
                                      (184 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (189 (iconst_1))
                                      (190 (anewarray (class "java.lang.Object")))
                                      (193 (dup))
                                      (194 (iconst_0))
                                      (195 (getstatic (fieldCP "const__11" "clojure.core$proxy_name" (class "clojure.lang.Var"))))
                                      (198 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (201 (checkcast (class "clojure.lang.IFn")))
                                      (204 (aload_3))
                                      (205 (aconst_null))
                                      (206 (astore_3))
                                      (207 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (212 (checkcast (class "java.lang.Number")))
                                      (215 (invokestatic
					(methodCP "intCast" "clojure.lang.RT" ((class "java.lang.Object")) int)))
                                      (218 (invokestatic
					(methodCP "toHexString" "java.lang.Integer" (int) (class "java.lang.String"))))
                                      (221 (aastore))
                                      (222 (invokestatic
					(methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector"))))
                                      (225 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4))
                                      (230 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (235 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 5))
                                      (240 (areturn))
                                      (endofcode 241))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$proxy_name-class-table*
  (make-static-class-decls 
   *clojure.core$proxy_name*))

(defconst *package-name-map* 
  ("clojure.core$proxy_name" . "clojure"))

