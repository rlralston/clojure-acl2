; pprint$pprint_ideref-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:56 CDT 2014.
;

(defconst *clojure.pprint$pprint_ideref*
 (make-class-def
      '(class "clojure.pprint$pprint_ideref"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "format")
                        (STRING  "clojure.pprint")
                        (STRING  "map-ref-type")
                        (STRING  "class")
                        (STRING  "instance?")
                        (STRING  "agent-error")
                        (STRING  "level-exceeded")
                        (STRING  "*out*")
                        (STRING  "push-thread-bindings")
                        (STRING  "*current-level*")
                        (STRING  "inc")
                        (STRING  "var-get")
                        (STRING  "*current-length*")
                        (STRING  "#<%s@%x%s: ")
                        (STRING  " FAILED")
                        (STRING  "")
                        (STRING  "#"))
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
                        (field "const__11" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__12" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 164)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "format"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.pprint$pprint_ideref" (class "clojure.lang.Var"))))
                                      (13 (ldc 2))        ;;STRING:: "clojure.pprint"
                                      (15 (ldc 3))        ;;STRING:: "map-ref-type"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.pprint$pprint_ideref" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 4))        ;;STRING:: "class"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.pprint$pprint_ideref" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 5))        ;;STRING:: "instance?"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.pprint$pprint_ideref" (class "clojure.lang.Var"))))
                                      (52 (ldc 0))        ;;STRING:: "clojure.core"
                                      (54 (ldc 6))        ;;STRING:: "agent-error"
                                      (56 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (59 (checkcast (class "clojure.lang.Var")))
                                      (62 (putstatic (fieldCP "const__4" "clojure.pprint$pprint_ideref" (class "clojure.lang.Var"))))
                                      (65 (ldc 2))        ;;STRING:: "clojure.pprint"
                                      (67 (ldc 7))        ;;STRING:: "level-exceeded"
                                      (69 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (72 (checkcast (class "clojure.lang.Var")))
                                      (75 (putstatic (fieldCP "const__5" "clojure.pprint$pprint_ideref" (class "clojure.lang.Var"))))
                                      (78 (ldc 0))        ;;STRING:: "clojure.core"
                                      (80 (ldc 8))        ;;STRING:: "*out*"
                                      (82 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (85 (checkcast (class "clojure.lang.Var")))
                                      (88 (putstatic (fieldCP "const__6" "clojure.pprint$pprint_ideref" (class "clojure.lang.Var"))))
                                      (91 (ldc 0))        ;;STRING:: "clojure.core"
                                      (93 (ldc 9))        ;;STRING:: "push-thread-bindings"
                                      (95 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (98 (checkcast (class "clojure.lang.Var")))
                                      (101 (putstatic (fieldCP "const__7" "clojure.pprint$pprint_ideref" (class "clojure.lang.Var"))))
                                      (104 (ldc 2))       ;;STRING:: "clojure.pprint"
                                      (106 (ldc 10))      ;;STRING:: "*current-level*"
                                      (108 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (111 (checkcast (class "clojure.lang.Var")))
                                      (114 (putstatic (fieldCP "const__8" "clojure.pprint$pprint_ideref" (class "clojure.lang.Var"))))
                                      (117 (ldc 0))       ;;STRING:: "clojure.core"
                                      (119 (ldc 11))      ;;STRING:: "inc"
                                      (121 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (124 (checkcast (class "clojure.lang.Var")))
                                      (127 (putstatic (fieldCP "const__9" "clojure.pprint$pprint_ideref" (class "clojure.lang.Var"))))
                                      (130 (ldc 0))       ;;STRING:: "clojure.core"
                                      (132 (ldc 12))      ;;STRING:: "var-get"
                                      (134 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (137 (checkcast (class "clojure.lang.Var")))
                                      (140 (putstatic (fieldCP "const__10" "clojure.pprint$pprint_ideref" (class "clojure.lang.Var"))))
                                      (143 (ldc 2))       ;;STRING:: "clojure.pprint"
                                      (145 (ldc 13))      ;;STRING:: "*current-length*"
                                      (147 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (150 (checkcast (class "clojure.lang.Var")))
                                      (153 (putstatic (fieldCP "const__11" "clojure.pprint$pprint_ideref" (class "clojure.lang.Var"))))
                                      (156 (lconst_0))
                                      (157 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (160 (putstatic (fieldCP "const__12" "clojure.pprint$pprint_ideref" (class "java.lang.Object"))))
                                      (163 (return))
                                      (endofcode 164))
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
                                   (max_stack . 6) (max_locals . 3) (code_length . 249)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.pprint$pprint_ideref" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (ldc 14)) ;;STRING:: "#<%s@%x%s: "
                                      (11 (getstatic (fieldCP "const__1" "clojure.pprint$pprint_ideref" (class "clojure.lang.Var")))) 
                                      (14 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (17 (checkcast (class "clojure.lang.IFn"))) 
                                      (20 (getstatic (fieldCP "const__2" "clojure.pprint$pprint_ideref" (class "clojure.lang.Var")))) 
                                      (23 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (26 (checkcast (class "clojure.lang.IFn"))) 
                                      (29 (aload_1)) 
                                      (30 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (35 (checkcast (class "java.lang.Class"))) 
                                      (38 (invokevirtual (methodCP "getSimpleName" "java.lang.Class" () (class "java.lang.String")))) 
                                      (41 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (46 (aload_1)) 
                                      (47 (invokestatic (methodCP "identityHashCode" "java.lang.System" ((class "java.lang.Object")) int))) 
                                      (50 (invokestatic (methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer")))) 
                                      (53 (aload_1)) 
                                      (54 (instanceof (class "clojure.lang.Agent"))) 
                                      (57 (istore_2)) 
                                      (58 (iload_2)) 
                                      (59 (ifeq 81)) ;;to TAG_0
                                      (62 (getstatic (fieldCP "const__4" "clojure.pprint$pprint_ideref" (class "clojure.lang.Var")))) 
                                      (65 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (68 (checkcast (class "clojure.lang.IFn"))) 
                                      (71 (aload_1)) 
                                      (72 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (77 (goto 94))  ;;to TAG_1
                                      (80 (pop)) 
                                      (81 (iload_2)) ;;at TAG_0
                                      (82 (ifeq 91)) ;;to TAG_2
                                      (85 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (88 (goto 94))  ;;to TAG_1
                                      (91 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) ;;at TAG_2
                                      (94 (dup)) ;;at TAG_1
                                      (95 (ifnull 109)) ;;to TAG_3
                                      (98 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (101 (if_acmpeq 110)) ;;to TAG_4
                                      (104 (ldc 15)) ;;STRING:: " FAILED"
                                      (106 (goto 112)) ;;to TAG_5
                                      (109 (pop)) ;;at TAG_3
                                      (110 (ldc 16)) ;;at TAG_4;;STRING:: ""
                                      (112 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 5)) ;;at TAG_5
                                      (117 (astore_2)) 
                                      (118 (getstatic (fieldCP "const__5" "clojure.pprint$pprint_ideref" (class "clojure.lang.Var")))) 
                                      (121 (checkcast (class "clojure.lang.IFn"))) 
                                      (124 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1)) 
                                      (129 (dup)) 
                                      (130 (ifnull 161)) ;;to TAG_6
                                      (133 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (136 (if_acmpeq 162)) ;;to TAG_7
                                      (139 (getstatic (fieldCP "const__6" "clojure.pprint$pprint_ideref" (class "clojure.lang.Var")))) 
                                      (142 (invokevirtual (methodCP "get" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (145 (checkcast (class "java.io.Writer"))) 
                                      (148 (ldc 17)) ;;STRING:: "#"
                                      (150 (checkcast (class "java.lang.String"))) 
                                      (153 (invokevirtual (methodCP "write" "java.io.Writer" ((class "java.lang.String")) void))) 
                                      (156 (aconst_null)) 
                                      (157 (pop)) 
                                      (158 (goto 247)) ;;to TAG_8
                                      (161 (pop)) ;;at TAG_6
                                      (162 (getstatic (fieldCP "const__7" "clojure.pprint$pprint_ideref" (class "clojure.lang.Var")))) ;;at TAG_7
                                      (165 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (168 (checkcast (class "clojure.lang.IFn"))) 
                                      (171 (iconst_4)) 
                                      (172 (anewarray (class "java.lang.Object"))) 
                                      (175 (dup)) 
                                      (176 (iconst_0)) 
                                      (177 (getstatic (fieldCP "const__8" "clojure.pprint$pprint_ideref" (class "clojure.lang.Var")))) 
                                      (180 (aastore)) 
                                      (181 (dup)) 
                                      (182 (iconst_1)) 
                                      (183 (getstatic (fieldCP "const__10" "clojure.pprint$pprint_ideref" (class "clojure.lang.Var")))) 
                                      (186 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (189 (checkcast (class "clojure.lang.IFn"))) 
                                      (192 (getstatic (fieldCP "const__8" "clojure.pprint$pprint_ideref" (class "clojure.lang.Var")))) 
                                      (195 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (200 (invokestatic (methodCP "inc" "clojure.lang.Numbers" ((class "java.lang.Object")) (class "java.lang.Number")))) 
                                      (203 (aastore)) 
                                      (204 (dup)) 
                                      (205 (iconst_2)) 
                                      (206 (getstatic (fieldCP "const__11" "clojure.pprint$pprint_ideref" (class "clojure.lang.Var")))) 
                                      (209 (aastore)) 
                                      (210 (dup)) 
                                      (211 (iconst_3)) 
                                      (212 (getstatic (fieldCP "const__12" "clojure.pprint$pprint_ideref" (class "java.lang.Object")))) 
                                      (215 (aastore)) 
                                      (216 (invokestatic (methodCP "map" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentMap")))) 
                                      (219 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (224 (pop)) 
                                      (225 (new (class "clojure.pprint$pprint_ideref$fn__8214"))) 
                                      (228 (dup)) 
                                      (229 (aload_1)) 
                                      (230 (aconst_null)) 
                                      (231 (astore_1)) 
                                      (232 (aload_2)) 
                                      (233 (aconst_null)) 
                                      (234 (astore_2)) 
                                      (235 (invokespecial (methodCP "<init>" "clojure.pprint$pprint_ideref$fn__8214" ((class "java.lang.Object") (class "java.lang.Object")) void))) 
                                      (238 (checkcast (class "clojure.lang.IFn"))) 
                                      (241 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1)) 
                                      (246 (pop)) 
                                      (247 (aconst_null)) ;;at TAG_8
                                      (248 (areturn)) 
                                      (endofcode 249))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *pprint$pprint_ideref-class-table*
  (make-static-class-decls 
   *clojure.pprint$pprint_ideref*))

(defconst *package-name-map* 
  ("clojure.pprint$pprint_ideref" . "clojure"))

