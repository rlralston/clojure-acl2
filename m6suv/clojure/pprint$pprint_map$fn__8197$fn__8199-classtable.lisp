; pprint$pprint_map$fn__8197$fn__8199-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:56 CDT 2014.
;

(defconst *clojure.pprint$pprint_map$fn__8197$fn__8199*
 (make-class-def
      '(class "clojure.pprint$pprint_map$fn__8197$fn__8199"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.pprint")
                        (STRING  "start-block")
                        (STRING  "clojure.core")
                        (STRING  "*out*")
                        (STRING  "write-out")
                        (STRING  "ffirst")
                        (STRING  "pprint-newline")
                        (STRING  "linear")
                        (STRING  "*current-length*")
                        (STRING  "fnext")
                        (STRING  "first")
                        (STRING  "end-block")
                        (STRING  "pop-thread-bindings")
                        (STRING  " "))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__9" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__10" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__11" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "aseq" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 150)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.pprint"
                                      (2 (ldc 1))         ;;STRING:: "start-block"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.pprint$pprint_map$fn__8197$fn__8199" (class "clojure.lang.Var"))))
                                      (13 (ldc 2))        ;;STRING:: "clojure.core"
                                      (15 (ldc 3))        ;;STRING:: "*out*"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.pprint$pprint_map$fn__8197$fn__8199" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.pprint"
                                      (28 (ldc 4))        ;;STRING:: "write-out"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.pprint$pprint_map$fn__8197$fn__8199" (class "clojure.lang.Var"))))
                                      (39 (ldc 2))        ;;STRING:: "clojure.core"
                                      (41 (ldc 5))        ;;STRING:: "ffirst"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.pprint$pprint_map$fn__8197$fn__8199" (class "clojure.lang.Var"))))
                                      (52 (ldc 0))        ;;STRING:: "clojure.pprint"
                                      (54 (ldc 6))        ;;STRING:: "pprint-newline"
                                      (56 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (59 (checkcast (class "clojure.lang.Var")))
                                      (62 (putstatic (fieldCP "const__4" "clojure.pprint$pprint_map$fn__8197$fn__8199" (class "clojure.lang.Var"))))
                                      (65 (aconst_null))
                                      (66 (ldc 7))        ;;STRING:: "linear"
                                      (68 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (71 (checkcast (class "clojure.lang.Keyword")))
                                      (74 (putstatic (fieldCP "const__5" "clojure.pprint$pprint_map$fn__8197$fn__8199" (class "clojure.lang.Keyword"))))
                                      (77 (ldc 0))        ;;STRING:: "clojure.pprint"
                                      (79 (ldc 8))        ;;STRING:: "*current-length*"
                                      (81 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (84 (checkcast (class "clojure.lang.Var")))
                                      (87 (putstatic (fieldCP "const__6" "clojure.pprint$pprint_map$fn__8197$fn__8199" (class "clojure.lang.Var"))))
                                      (90 (lconst_0))
                                      (91 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (94 (putstatic (fieldCP "const__7" "clojure.pprint$pprint_map$fn__8197$fn__8199" (class "java.lang.Object"))))
                                      (97 (ldc 2))        ;;STRING:: "clojure.core"
                                      (99 (ldc 9))        ;;STRING:: "fnext"
                                      (101 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (104 (checkcast (class "clojure.lang.Var")))
                                      (107 (putstatic (fieldCP "const__8" "clojure.pprint$pprint_map$fn__8197$fn__8199" (class "clojure.lang.Var"))))
                                      (110 (ldc 2))       ;;STRING:: "clojure.core"
                                      (112 (ldc 10))      ;;STRING:: "first"
                                      (114 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (117 (checkcast (class "clojure.lang.Var")))
                                      (120 (putstatic (fieldCP "const__9" "clojure.pprint$pprint_map$fn__8197$fn__8199" (class "clojure.lang.Var"))))
                                      (123 (ldc 0))       ;;STRING:: "clojure.pprint"
                                      (125 (ldc 11))      ;;STRING:: "end-block"
                                      (127 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (130 (checkcast (class "clojure.lang.Var")))
                                      (133 (putstatic (fieldCP "const__10" "clojure.pprint$pprint_map$fn__8197$fn__8199" (class "clojure.lang.Var"))))
                                      (136 (ldc 2))       ;;STRING:: "clojure.core"
                                      (138 (ldc 12))      ;;STRING:: "pop-thread-bindings"
                                      (140 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (143 (checkcast (class "clojure.lang.Var")))
                                      (146 (putstatic (fieldCP "const__11" "clojure.pprint$pprint_map$fn__8197$fn__8199" (class "clojure.lang.Var"))))
                                      (149 (return))
                                      (endofcode 150))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.AFunction" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "aseq" "clojure.pprint$pprint_map$fn__8197$fn__8199" (class "java.lang.Object"))))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 3) (code_length . 204)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.pprint$pprint_map$fn__8197$fn__8199" (class "clojure.lang.Var")))) ;;at TAG_1
                                      (3 (checkcast (class "clojure.lang.IFn"))) 
                                      (6 (getstatic (fieldCP "const__1" "clojure.pprint$pprint_map$fn__8197$fn__8199" (class "clojure.lang.Var")))) 
                                      (9 (invokevirtual (methodCP "get" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (12 (aconst_null)) 
                                      (13 (aconst_null)) 
                                      (14 (aconst_null)) 
                                      (15 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 5)) 
                                      (20 (pop)) 
                                      (21 (getstatic (fieldCP "const__2" "clojure.pprint$pprint_map$fn__8197$fn__8199" (class "clojure.lang.Var")))) 
                                      (24 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (27 (checkcast (class "clojure.lang.IFn"))) 
                                      (30 (getstatic (fieldCP "const__3" "clojure.pprint$pprint_map$fn__8197$fn__8199" (class "clojure.lang.Var")))) 
                                      (33 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (36 (checkcast (class "clojure.lang.IFn"))) 
                                      (39 (aload_0)) 
                                      (40 (getfield (fieldCP "aseq" "clojure.pprint$pprint_map$fn__8197$fn__8199" (class "java.lang.Object")))) 
                                      (43 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (48 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (53 (pop)) 
                                      (54 (getstatic (fieldCP "const__1" "clojure.pprint$pprint_map$fn__8197$fn__8199" (class "clojure.lang.Var")))) 
                                      (57 (invokevirtual (methodCP "get" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (60 (checkcast (class "java.io.Writer"))) 
                                      (63 (ldc 13)) ;;STRING:: " "
                                      (65 (checkcast (class "java.lang.String"))) 
                                      (68 (invokevirtual (methodCP "write" "java.io.Writer" ((class "java.lang.String")) void))) 
                                      (71 (aconst_null)) 
                                      (72 (pop)) 
                                      (73 (getstatic (fieldCP "const__4" "clojure.pprint$pprint_map$fn__8197$fn__8199" (class "clojure.lang.Var")))) 
                                      (76 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (79 (checkcast (class "clojure.lang.IFn"))) 
                                      (82 (getstatic (fieldCP "const__5" "clojure.pprint$pprint_map$fn__8197$fn__8199" (class "clojure.lang.Keyword")))) 
                                      (85 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (90 (pop)) 
                                      (91 (getstatic (fieldCP "const__6" "clojure.pprint$pprint_map$fn__8197$fn__8199" (class "clojure.lang.Var")))) 
                                      (94 (getstatic (fieldCP "const__7" "clojure.pprint$pprint_map$fn__8197$fn__8199" (class "java.lang.Object")))) 
                                      (97 (invokevirtual (methodCP "set" "clojure.lang.Var" ((class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (100 (pop)) 
                                      (101 (getstatic (fieldCP "const__2" "clojure.pprint$pprint_map$fn__8197$fn__8199" (class "clojure.lang.Var")))) 
                                      (104 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (107 (checkcast (class "clojure.lang.IFn"))) 
                                      (110 (getstatic (fieldCP "const__8" "clojure.pprint$pprint_map$fn__8197$fn__8199" (class "clojure.lang.Var")))) 
                                      (113 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (116 (checkcast (class "clojure.lang.IFn"))) 
                                      (119 (getstatic (fieldCP "const__9" "clojure.pprint$pprint_map$fn__8197$fn__8199" (class "clojure.lang.Var")))) 
                                      (122 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (125 (checkcast (class "clojure.lang.IFn"))) 
                                      (128 (aload_0)) 
                                      (129 (getfield (fieldCP "aseq" "clojure.pprint$pprint_map$fn__8197$fn__8199" (class "java.lang.Object")))) 
                                      (132 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (137 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (142 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (147 (pop)) 
                                      (148 (getstatic (fieldCP "const__10" "clojure.pprint$pprint_map$fn__8197$fn__8199" (class "clojure.lang.Var")))) 
                                      (151 (checkcast (class "clojure.lang.IFn"))) 
                                      (154 (getstatic (fieldCP "const__1" "clojure.pprint$pprint_map$fn__8197$fn__8199" (class "clojure.lang.Var")))) 
                                      (157 (invokevirtual (methodCP "get" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (160 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (165 (astore_1)) 
                                      (166 (getstatic (fieldCP "const__11" "clojure.pprint$pprint_map$fn__8197$fn__8199" (class "clojure.lang.Var")))) ;;at TAG_2
                                      (169 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (172 (checkcast (class "clojure.lang.IFn"))) 
                                      (175 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1)) 
                                      (180 (pop)) 
                                      (181 (goto 202)) ;;to TAG_0
                                      (184 (astore_2)) ;;at TAG_3
                                      (185 (getstatic (fieldCP "const__11" "clojure.pprint$pprint_map$fn__8197$fn__8199" (class "clojure.lang.Var")))) 
                                      (188 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (191 (checkcast (class "clojure.lang.IFn"))) 
                                      (194 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1)) 
                                      (199 (pop)) 
                                      (200 (aload_2)) 
                                      (201 (athrow)) 
                                      (202 (aload_1)) ;;at TAG_0
                                      (203 (areturn)) 
                                      (endofcode 204))
                                   (Exceptions 
                                     (handler 0 166  184 (class "java.lang.Throwable")))
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *pprint$pprint_map$fn__8197$fn__8199-class-table*
  (make-static-class-decls 
   *clojure.pprint$pprint_map$fn__8197$fn__8199*))

(defconst *package-name-map* 
  ("clojure.pprint$pprint_map$fn__8197$fn__8199" . "clojure"))

