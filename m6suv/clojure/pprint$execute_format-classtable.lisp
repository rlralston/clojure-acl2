; pprint$execute_format-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:55 CDT 2014.
;

(defconst *clojure.pprint$execute_format*
 (make-class-def
      '(class "clojure.pprint$execute_format"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "not")
                        (STRING  "true?")
                        (STRING  "*out*")
                        (STRING  "else")
                        (STRING  "clojure.pprint")
                        (STRING  "needs-pretty")
                        (STRING  "pretty-writer?")
                        (STRING  "get-pretty-writer")
                        (STRING  "push-thread-bindings")
                        (STRING  "hash-map")
                        (STRING  "pop-thread-bindings")
                        (STRING  "map-passing-context"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__9" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__10" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 143)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "not"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.pprint$execute_format" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "true?"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.pprint$execute_format" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "*out*"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.pprint$execute_format" (class "clojure.lang.Var"))))
                                      (39 (aconst_null))
                                      (40 (ldc 4))        ;;STRING:: "else"
                                      (42 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (45 (checkcast (class "clojure.lang.Keyword")))
                                      (48 (putstatic (fieldCP "const__3" "clojure.pprint$execute_format" (class "clojure.lang.Keyword"))))
                                      (51 (ldc 5))        ;;STRING:: "clojure.pprint"
                                      (53 (ldc 6))        ;;STRING:: "needs-pretty"
                                      (55 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (58 (checkcast (class "clojure.lang.Var")))
                                      (61 (putstatic (fieldCP "const__4" "clojure.pprint$execute_format" (class "clojure.lang.Var"))))
                                      (64 (ldc 5))        ;;STRING:: "clojure.pprint"
                                      (66 (ldc 7))        ;;STRING:: "pretty-writer?"
                                      (68 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (71 (checkcast (class "clojure.lang.Var")))
                                      (74 (putstatic (fieldCP "const__5" "clojure.pprint$execute_format" (class "clojure.lang.Var"))))
                                      (77 (ldc 5))        ;;STRING:: "clojure.pprint"
                                      (79 (ldc 8))        ;;STRING:: "get-pretty-writer"
                                      (81 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (84 (checkcast (class "clojure.lang.Var")))
                                      (87 (putstatic (fieldCP "const__6" "clojure.pprint$execute_format" (class "clojure.lang.Var"))))
                                      (90 (ldc 0))        ;;STRING:: "clojure.core"
                                      (92 (ldc 9))        ;;STRING:: "push-thread-bindings"
                                      (94 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (97 (checkcast (class "clojure.lang.Var")))
                                      (100 (putstatic (fieldCP "const__7" "clojure.pprint$execute_format" (class "clojure.lang.Var"))))
                                      (103 (ldc 0))       ;;STRING:: "clojure.core"
                                      (105 (ldc 10))      ;;STRING:: "hash-map"
                                      (107 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (110 (checkcast (class "clojure.lang.Var")))
                                      (113 (putstatic (fieldCP "const__8" "clojure.pprint$execute_format" (class "clojure.lang.Var"))))
                                      (116 (ldc 0))       ;;STRING:: "clojure.core"
                                      (118 (ldc 11))      ;;STRING:: "pop-thread-bindings"
                                      (120 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (123 (checkcast (class "clojure.lang.Var")))
                                      (126 (putstatic (fieldCP "const__9" "clojure.pprint$execute_format" (class "clojure.lang.Var"))))
                                      (129 (ldc 5))       ;;STRING:: "clojure.pprint"
                                      (131 (ldc 12))      ;;STRING:: "map-passing-context"
                                      (133 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (136 (checkcast (class "clojure.lang.Var")))
                                      (139 (putstatic (fieldCP "const__10" "clojure.pprint$execute_format" (class "clojure.lang.Var"))))
                                      (142 (return))
                                      (endofcode 143))
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
                                   (max_stack . 7) (max_locals . 8) (code_length . 340)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.pprint$execute_format" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (aload_1)) 
                                      (10 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (15 (dup)) 
                                      (16 (ifnull 35)) ;;to TAG_0
                                      (19 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (22 (if_acmpeq 36)) ;;to TAG_1
                                      (25 (new (class "java.io.StringWriter"))) 
                                      (28 (dup)) 
                                      (29 (invokespecial (methodCP "<init>" "java.io.StringWriter" () void))) 
                                      (32 (goto 90)) ;;to TAG_2
                                      (35 (pop)) ;;at TAG_0
                                      (36 (getstatic (fieldCP "const__1" "clojure.pprint$execute_format" (class "clojure.lang.Var")))) ;;at TAG_1
                                      (39 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (42 (checkcast (class "clojure.lang.IFn"))) 
                                      (45 (aload_1)) 
                                      (46 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (51 (dup)) 
                                      (52 (ifnull 70)) ;;to TAG_3
                                      (55 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (58 (if_acmpeq 71)) ;;to TAG_4
                                      (61 (getstatic (fieldCP "const__2" "clojure.pprint$execute_format" (class "clojure.lang.Var")))) 
                                      (64 (invokevirtual (methodCP "get" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (67 (goto 90)) ;;to TAG_2
                                      (70 (pop)) ;;at TAG_3
                                      (71 (getstatic (fieldCP "const__3" "clojure.pprint$execute_format" (class "clojure.lang.Keyword")))) ;;at TAG_4
                                      (74 (dup)) 
                                      (75 (ifnull 88)) ;;to TAG_5
                                      (78 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (81 (if_acmpeq 89)) ;;to TAG_6
                                      (84 (aload_1)) 
                                      (85 (goto 90)) ;;to TAG_2
                                      (88 (pop)) ;;at TAG_5
                                      (89 (aconst_null)) ;;at TAG_6
                                      (90 (astore 4)) ;;at TAG_2
                                      (92 (getstatic (fieldCP "const__4" "clojure.pprint$execute_format" (class "clojure.lang.Var")))) 
                                      (95 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (98 (checkcast (class "clojure.lang.IFn"))) 
                                      (101 (aload_2)) 
                                      (102 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (107 (astore 5)) 
                                      (109 (aload 5)) 
                                      (111 (dup)) 
                                      (112 (ifnull 154)) ;;to TAG_7
                                      (115 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (118 (if_acmpeq 155)) ;;to TAG_8
                                      (121 (getstatic (fieldCP "const__0" "clojure.pprint$execute_format" (class "clojure.lang.Var")))) 
                                      (124 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (127 (checkcast (class "clojure.lang.IFn"))) 
                                      (130 (getstatic (fieldCP "const__5" "clojure.pprint$execute_format" (class "clojure.lang.Var")))) 
                                      (133 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (136 (checkcast (class "clojure.lang.IFn"))) 
                                      (139 (aload 4)) 
                                      (141 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (146 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (151 (goto 160)) ;;to TAG_9
                                      (154 (pop)) ;;at TAG_7
                                      (155 (aload 5)) ;;at TAG_8
                                      (157 (aconst_null)) 
                                      (158 (astore 5)) 
                                      (160 (dup)) ;;at TAG_9
                                      (161 (ifnull 189)) ;;to TAG_10
                                      (164 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (167 (if_acmpeq 190)) ;;to TAG_11
                                      (170 (getstatic (fieldCP "const__6" "clojure.pprint$execute_format" (class "clojure.lang.Var")))) 
                                      (173 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (176 (checkcast (class "clojure.lang.IFn"))) 
                                      (179 (aload 4)) 
                                      (181 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (186 (goto 192)) ;;to TAG_12
                                      (189 (pop)) ;;at TAG_10
                                      (190 (aload 4)) ;;at TAG_11
                                      (192 (astore 5)) ;;at TAG_12
                                      (194 (getstatic (fieldCP "const__7" "clojure.pprint$execute_format" (class "clojure.lang.Var")))) 
                                      (197 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (200 (checkcast (class "clojure.lang.IFn"))) 
                                      (203 (getstatic (fieldCP "const__8" "clojure.pprint$execute_format" (class "clojure.lang.Var")))) 
                                      (206 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (209 (checkcast (class "clojure.lang.IFn"))) 
                                      (212 (getstatic (fieldCP "const__2" "clojure.pprint$execute_format" (class "clojure.lang.Var")))) 
                                      (215 (aload 5)) 
                                      (217 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (222 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (227 (pop)) 
                                      (228 (new (class "clojure.pprint$execute_format$fn__8154"))) ;;at TAG_17
                                      (231 (dup)) 
                                      (232 (aload 5)) 
                                      (234 (aconst_null)) 
                                      (235 (astore 5)) 
                                      (237 (aload_3)) 
                                      (238 (aconst_null)) 
                                      (239 (astore_3)) 
                                      (240 (aload 4)) 
                                      (242 (aload_2)) 
                                      (243 (aconst_null)) 
                                      (244 (astore_2)) 
                                      (245 (invokespecial (methodCP "<init>" "clojure.pprint$execute_format$fn__8154" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) void))) 
                                      (248 (checkcast (class "clojure.lang.IFn"))) 
                                      (251 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1)) 
                                      (256 (pop)) 
                                      (257 (getstatic (fieldCP "const__0" "clojure.pprint$execute_format" (class "clojure.lang.Var")))) 
                                      (260 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (263 (checkcast (class "clojure.lang.IFn"))) 
                                      (266 (aload_1)) 
                                      (267 (aconst_null)) 
                                      (268 (astore_1)) 
                                      (269 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (274 (dup)) 
                                      (275 (ifnull 295))  ;;to TAG_13
                                      (278 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (281 (if_acmpeq 296)) ;;to TAG_14
                                      (284 (aload 4)) 
                                      (286 (aconst_null)) 
                                      (287 (astore 4)) 
                                      (289 (invokevirtual (methodCP "toString" "java.lang.Object" () (class "java.lang.String")))) 
                                      (292 (goto 297)) ;;to TAG_15
                                      (295 (pop)) ;;at TAG_13
                                      (296 (aconst_null)) ;;at TAG_14
                                      (297 (astore 6)) ;;at TAG_15
                                      (299 (getstatic (fieldCP "const__9" "clojure.pprint$execute_format" (class "clojure.lang.Var")))) ;;at TAG_18
                                      (302 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (305 (checkcast (class "clojure.lang.IFn"))) 
                                      (308 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1)) 
                                      (313 (pop)) 
                                      (314 (goto 337)) ;;to TAG_16
                                      (317 (astore 7)) ;;at TAG_19
                                      (319 (getstatic (fieldCP "const__9" "clojure.pprint$execute_format" (class "clojure.lang.Var")))) 
                                      (322 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (325 (checkcast (class "clojure.lang.IFn"))) 
                                      (328 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1)) 
                                      (333 (pop)) 
                                      (334 (aload 7)) 
                                      (336 (athrow)) 
                                      (337 (aload 6)) ;;at TAG_16
                                      (339 (areturn)) 
                                      (endofcode 340))
                                   (Exceptions 
                                     (handler 228 299  317 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 3) (code_length . 30)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__10" "clojure.pprint$execute_format" (class "clojure.lang.Var"))))
                                      (3 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (6 (checkcast (class "clojure.lang.IFn")))
                                      (9 (new (class "clojure.pprint$execute_format$fn__8156")))
                                      (12 (dup))
                                      (13 (invokespecial
					(methodCP "<init>" "clojure.pprint$execute_format$fn__8156" () void)))
                                      (16 (aload_2))
                                      (17 (aconst_null))
                                      (18 (astore_2))
                                      (19 (aload_1))
                                      (20 (aconst_null))
                                      (21 (astore_1))
                                      (22 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4))
                                      (27 (pop))
                                      (28 (aconst_null))
                                      (29 (areturn))
                                      (endofcode 30))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *pprint$execute_format-class-table*
  (make-static-class-decls 
   *clojure.pprint$execute_format*))

(defconst *package-name-map* 
  ("clojure.pprint$execute_format" . "clojure"))

