; pprint$pprint_ns_reference-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:56 CDT 2014.
;

(defconst *clojure.pprint$pprint_ns_reference*
 (make-class-def
      '(class "clojure.pprint$pprint_ns_reference"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "sequential?")
                        (STRING  "clojure.pprint")
                        (STRING  "brackets")
                        (STRING  "nth")
                        (STRING  "nthnext")
                        (STRING  "level-exceeded")
                        (STRING  "*out*")
                        (STRING  "push-thread-bindings")
                        (STRING  "*current-level*")
                        (STRING  "inc")
                        (STRING  "var-get")
                        (STRING  "*current-length*")
                        (STRING  "write-out")
                        (STRING  "#"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__9" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__10" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__11" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__12" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__13" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 171)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "sequential?"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.pprint$pprint_ns_reference" (class "clojure.lang.Var"))))
                                      (13 (ldc 2))        ;;STRING:: "clojure.pprint"
                                      (15 (ldc 3))        ;;STRING:: "brackets"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.pprint$pprint_ns_reference" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 4))        ;;STRING:: "nth"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.pprint$pprint_ns_reference" (class "clojure.lang.Var"))))
                                      (39 (lconst_0))
                                      (40 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (43 (putstatic (fieldCP "const__3" "clojure.pprint$pprint_ns_reference" (class "java.lang.Object"))))
                                      (46 (lconst_1))
                                      (47 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (50 (putstatic (fieldCP "const__4" "clojure.pprint$pprint_ns_reference" (class "java.lang.Object"))))
                                      (53 (ldc 0))        ;;STRING:: "clojure.core"
                                      (55 (ldc 5))        ;;STRING:: "nthnext"
                                      (57 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (60 (checkcast (class "clojure.lang.Var")))
                                      (63 (putstatic (fieldCP "const__5" "clojure.pprint$pprint_ns_reference" (class "clojure.lang.Var"))))
                                      (66 (ldc 2))        ;;STRING:: "clojure.pprint"
                                      (68 (ldc 6))        ;;STRING:: "level-exceeded"
                                      (70 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (73 (checkcast (class "clojure.lang.Var")))
                                      (76 (putstatic (fieldCP "const__6" "clojure.pprint$pprint_ns_reference" (class "clojure.lang.Var"))))
                                      (79 (ldc 0))        ;;STRING:: "clojure.core"
                                      (81 (ldc 7))        ;;STRING:: "*out*"
                                      (83 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (86 (checkcast (class "clojure.lang.Var")))
                                      (89 (putstatic (fieldCP "const__7" "clojure.pprint$pprint_ns_reference" (class "clojure.lang.Var"))))
                                      (92 (ldc 0))        ;;STRING:: "clojure.core"
                                      (94 (ldc 8))        ;;STRING:: "push-thread-bindings"
                                      (96 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (99 (checkcast (class "clojure.lang.Var")))
                                      (102 (putstatic (fieldCP "const__8" "clojure.pprint$pprint_ns_reference" (class "clojure.lang.Var"))))
                                      (105 (ldc 2))       ;;STRING:: "clojure.pprint"
                                      (107 (ldc 9))       ;;STRING:: "*current-level*"
                                      (109 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (112 (checkcast (class "clojure.lang.Var")))
                                      (115 (putstatic (fieldCP "const__9" "clojure.pprint$pprint_ns_reference" (class "clojure.lang.Var"))))
                                      (118 (ldc 0))       ;;STRING:: "clojure.core"
                                      (120 (ldc 10))      ;;STRING:: "inc"
                                      (122 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (125 (checkcast (class "clojure.lang.Var")))
                                      (128 (putstatic (fieldCP "const__10" "clojure.pprint$pprint_ns_reference" (class "clojure.lang.Var"))))
                                      (131 (ldc 0))       ;;STRING:: "clojure.core"
                                      (133 (ldc 11))      ;;STRING:: "var-get"
                                      (135 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (138 (checkcast (class "clojure.lang.Var")))
                                      (141 (putstatic (fieldCP "const__11" "clojure.pprint$pprint_ns_reference" (class "clojure.lang.Var"))))
                                      (144 (ldc 2))       ;;STRING:: "clojure.pprint"
                                      (146 (ldc 12))      ;;STRING:: "*current-length*"
                                      (148 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (151 (checkcast (class "clojure.lang.Var")))
                                      (154 (putstatic (fieldCP "const__12" "clojure.pprint$pprint_ns_reference" (class "clojure.lang.Var"))))
                                      (157 (ldc 2))       ;;STRING:: "clojure.pprint"
                                      (159 (ldc 13))      ;;STRING:: "write-out"
                                      (161 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (164 (checkcast (class "clojure.lang.Var")))
                                      (167 (putstatic (fieldCP "const__13" "clojure.pprint$pprint_ns_reference" (class "clojure.lang.Var"))))
                                      (170 (return))
                                      (endofcode 171))
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
                                   (max_stack . 7) (max_locals . 8) (code_length . 269)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.pprint$pprint_ns_reference" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (aload_1)) 
                                      (10 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (15 (dup)) 
                                      (16 (ifnull 250)) ;;to TAG_0
                                      (19 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (22 (if_acmpeq 251)) ;;to TAG_1
                                      (25 (getstatic (fieldCP "const__1" "clojure.pprint$pprint_ns_reference" (class "clojure.lang.Var")))) 
                                      (28 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (31 (checkcast (class "clojure.lang.IFn"))) 
                                      (34 (aload_1)) 
                                      (35 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (40 (astore_2)) 
                                      (41 (aload_2)) 
                                      (42 (lconst_0)) 
                                      (43 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (46 (aconst_null)) 
                                      (47 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (50 (astore_3)) 
                                      (51 (aload_2)) 
                                      (52 (aconst_null)) 
                                      (53 (astore_2)) 
                                      (54 (lconst_1)) 
                                      (55 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (58 (aconst_null)) 
                                      (59 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (62 (astore 4)) 
                                      (64 (aload_1)) 
                                      (65 (aconst_null)) 
                                      (66 (astore_1)) 
                                      (67 (astore 5)) 
                                      (69 (aload 5)) 
                                      (71 (lconst_0)) 
                                      (72 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (75 (aconst_null)) 
                                      (76 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (79 (astore 6)) 
                                      (81 (getstatic (fieldCP "const__5" "clojure.pprint$pprint_ns_reference" (class "clojure.lang.Var")))) 
                                      (84 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (87 (checkcast (class "clojure.lang.IFn"))) 
                                      (90 (aload 5)) 
                                      (92 (aconst_null)) 
                                      (93 (astore 5)) 
                                      (95 (getstatic (fieldCP "const__4" "clojure.pprint$pprint_ns_reference" (class "java.lang.Object")))) 
                                      (98 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (103 (astore 7)) 
                                      (105 (getstatic (fieldCP "const__6" "clojure.pprint$pprint_ns_reference" (class "clojure.lang.Var")))) 
                                      (108 (checkcast (class "clojure.lang.IFn"))) 
                                      (111 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1)) 
                                      (116 (dup)) 
                                      (117 (ifnull 148))  ;;to TAG_2
                                      (120 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (123 (if_acmpeq 149)) ;;to TAG_3
                                      (126 (getstatic (fieldCP "const__7" "clojure.pprint$pprint_ns_reference" (class "clojure.lang.Var")))) 
                                      (129 (invokevirtual (methodCP "get" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (132 (checkcast (class "java.io.Writer"))) 
                                      (135 (ldc 14)) ;;STRING:: "#"
                                      (137 (checkcast (class "java.lang.String"))) 
                                      (140 (invokevirtual (methodCP "write" "java.io.Writer" ((class "java.lang.String")) void))) 
                                      (143 (aconst_null)) 
                                      (144 (pop)) 
                                      (145 (goto 246)) ;;to TAG_4
                                      (148 (pop)) ;;at TAG_2
                                      (149 (getstatic (fieldCP "const__8" "clojure.pprint$pprint_ns_reference" (class "clojure.lang.Var")))) ;;at TAG_3
                                      (152 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (155 (checkcast (class "clojure.lang.IFn"))) 
                                      (158 (iconst_4)) 
                                      (159 (anewarray (class "java.lang.Object"))) 
                                      (162 (dup)) 
                                      (163 (iconst_0)) 
                                      (164 (getstatic (fieldCP "const__9" "clojure.pprint$pprint_ns_reference" (class "clojure.lang.Var")))) 
                                      (167 (aastore)) 
                                      (168 (dup)) 
                                      (169 (iconst_1)) 
                                      (170 (getstatic (fieldCP "const__11" "clojure.pprint$pprint_ns_reference" (class "clojure.lang.Var")))) 
                                      (173 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (176 (checkcast (class "clojure.lang.IFn"))) 
                                      (179 (getstatic (fieldCP "const__9" "clojure.pprint$pprint_ns_reference" (class "clojure.lang.Var")))) 
                                      (182 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (187 (invokestatic (methodCP "inc" "clojure.lang.Numbers" ((class "java.lang.Object")) (class "java.lang.Number")))) 
                                      (190 (aastore)) 
                                      (191 (dup)) 
                                      (192 (iconst_2)) 
                                      (193 (getstatic (fieldCP "const__12" "clojure.pprint$pprint_ns_reference" (class "clojure.lang.Var")))) 
                                      (196 (aastore)) 
                                      (197 (dup)) 
                                      (198 (iconst_3)) 
                                      (199 (getstatic (fieldCP "const__3" "clojure.pprint$pprint_ns_reference" (class "java.lang.Object")))) 
                                      (202 (aastore)) 
                                      (203 (invokestatic (methodCP "map" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentMap")))) 
                                      (206 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (211 (pop)) 
                                      (212 (new (class "clojure.pprint$pprint_ns_reference$fn__8236"))) 
                                      (215 (dup)) 
                                      (216 (aload 7)) 
                                      (218 (aconst_null)) 
                                      (219 (astore 7)) 
                                      (221 (aload_3)) 
                                      (222 (aconst_null)) 
                                      (223 (astore_3)) 
                                      (224 (aload 6)) 
                                      (226 (aconst_null)) 
                                      (227 (astore 6)) 
                                      (229 (aload 4)) 
                                      (231 (aconst_null)) 
                                      (232 (astore 4)) 
                                      (234 (invokespecial (methodCP "<init>" "clojure.pprint$pprint_ns_reference$fn__8236" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) void))) 
                                      (237 (checkcast (class "clojure.lang.IFn"))) 
                                      (240 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1)) 
                                      (245 (pop)) 
                                      (246 (aconst_null)) ;;at TAG_4
                                      (247 (goto 268)) ;;to TAG_5
                                      (250 (pop)) ;;at TAG_0
                                      (251 (getstatic (fieldCP "const__13" "clojure.pprint$pprint_ns_reference" (class "clojure.lang.Var")))) ;;at TAG_1
                                      (254 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (257 (checkcast (class "clojure.lang.IFn"))) 
                                      (260 (aload_1)) 
                                      (261 (aconst_null)) 
                                      (262 (astore_1)) 
                                      (263 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (268 (areturn)) ;;at TAG_5
                                      (endofcode 269))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *pprint$pprint_ns_reference-class-table*
  (make-static-class-decls 
   *clojure.pprint$pprint_ns_reference*))

(defconst *package-name-map* 
  ("clojure.pprint$pprint_ns_reference" . "clojure"))

