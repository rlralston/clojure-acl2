; core$to_array_2d-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:45 CDT 2014.
;

(defconst *clojure.core$to_array_2d*
 (make-class-def
      '(class "clojure.core$to_array_2d"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "make-array")
                        (STRING  "seq")
                        (STRING  "aset")
                        (STRING  "int")
                        (STRING  "to-array")
                        (STRING  "first")
                        (STRING  "inc")
                        (STRING  "next")
                        (STRING  "[Ljava.lang.Object;")
                        (STRING  "clojure.lang.RT"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 112)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "make-array"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$to_array_2d" (class "clojure.lang.Var"))))
                                      (13 (lconst_0))
                                      (14 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (17 (putstatic (fieldCP "const__1" "clojure.core$to_array_2d" (class "java.lang.Object"))))
                                      (20 (ldc 0))        ;;STRING:: "clojure.core"
                                      (22 (ldc 2))        ;;STRING:: "seq"
                                      (24 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (27 (checkcast (class "clojure.lang.Var")))
                                      (30 (putstatic (fieldCP "const__2" "clojure.core$to_array_2d" (class "clojure.lang.Var"))))
                                      (33 (ldc 0))        ;;STRING:: "clojure.core"
                                      (35 (ldc 3))        ;;STRING:: "aset"
                                      (37 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (40 (checkcast (class "clojure.lang.Var")))
                                      (43 (putstatic (fieldCP "const__3" "clojure.core$to_array_2d" (class "clojure.lang.Var"))))
                                      (46 (ldc 0))        ;;STRING:: "clojure.core"
                                      (48 (ldc 4))        ;;STRING:: "int"
                                      (50 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (53 (checkcast (class "clojure.lang.Var")))
                                      (56 (putstatic (fieldCP "const__4" "clojure.core$to_array_2d" (class "clojure.lang.Var"))))
                                      (59 (ldc 0))        ;;STRING:: "clojure.core"
                                      (61 (ldc 5))        ;;STRING:: "to-array"
                                      (63 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (66 (checkcast (class "clojure.lang.Var")))
                                      (69 (putstatic (fieldCP "const__5" "clojure.core$to_array_2d" (class "clojure.lang.Var"))))
                                      (72 (ldc 0))        ;;STRING:: "clojure.core"
                                      (74 (ldc 6))        ;;STRING:: "first"
                                      (76 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (79 (checkcast (class "clojure.lang.Var")))
                                      (82 (putstatic (fieldCP "const__6" "clojure.core$to_array_2d" (class "clojure.lang.Var"))))
                                      (85 (ldc 0))        ;;STRING:: "clojure.core"
                                      (87 (ldc 7))        ;;STRING:: "inc"
                                      (89 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (92 (checkcast (class "clojure.lang.Var")))
                                      (95 (putstatic (fieldCP "const__7" "clojure.core$to_array_2d" (class "clojure.lang.Var"))))
                                      (98 (ldc 0))        ;;STRING:: "clojure.core"
                                      (100 (ldc 8))       ;;STRING:: "next"
                                      (102 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (105 (checkcast (class "clojure.lang.Var")))
                                      (108 (putstatic (fieldCP "const__8" "clojure.core$to_array_2d" (class "clojure.lang.Var"))))
                                      (111 (return))
                                      (endofcode 112))
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
                                   (max_stack . 8) (max_locals . 6) (code_length . 166)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$to_array_2d" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (ldc 9)) ;;STRING:: "[Ljava.lang.Object;"
                                      (11 (checkcast (class "java.lang.String"))) 
                                      (14 (invokestatic (methodCP "forName" "java.lang.Class" ((class "java.lang.String")) (class "java.lang.Class")))) 
                                      (17 (aload_1)) 
                                      (18 (checkcast (class "java.util.Collection"))) 
                                      (21 (invokeinterface (methodCP "size" "java.util.Collection" () int) 1)) 
                                      (26 (invokestatic (methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer")))) 
                                      (29 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (34 (astore_2)) 
                                      (35 (lconst_0)) 
                                      (36 (lstore_3)) 
                                      (37 (getstatic (fieldCP "const__2" "clojure.core$to_array_2d" (class "clojure.lang.Var")))) 
                                      (40 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (43 (checkcast (class "clojure.lang.IFn"))) 
                                      (46 (aload_1)) 
                                      (47 (aconst_null)) 
                                      (48 (astore_1)) 
                                      (49 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (54 (astore 5)) 
                                      (56 (aload 5)) ;;at TAG_2
                                      (58 (dup)) 
                                      (59 (ifnull 159)) ;;to TAG_0
                                      (62 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (65 (if_acmpeq 160)) ;;to TAG_1
                                      (68 (ldc 10)) ;;STRING:: "clojure.lang.RT"
                                      (70 (invokestatic (methodCP "forName" "java.lang.Class" ((class "java.lang.String")) (class "java.lang.Class")))) 
                                      (73 (ldc 3)) ;;STRING:: "aset"
                                      (75 (iconst_3)) 
                                      (76 (anewarray (class "java.lang.Object"))) 
                                      (79 (dup)) 
                                      (80 (iconst_0)) 
                                      (81 (aload_2)) 
                                      (82 (aastore)) 
                                      (83 (dup)) 
                                      (84 (iconst_1)) 
                                      (85 (lload_3)) 
                                      (86 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (89 (invokestatic (methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer")))) 
                                      (92 (aastore)) 
                                      (93 (dup)) 
                                      (94 (iconst_2)) 
                                      (95 (getstatic (fieldCP "const__5" "clojure.core$to_array_2d" (class "clojure.lang.Var")))) 
                                      (98 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (101 (checkcast (class "clojure.lang.IFn"))) 
                                      (104 (getstatic (fieldCP "const__6" "clojure.core$to_array_2d" (class "clojure.lang.Var")))) 
                                      (107 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (110 (checkcast (class "clojure.lang.IFn"))) 
                                      (113 (aload 5)) 
                                      (115 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (120 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (125 (aastore)) 
                                      (126 (invokestatic (methodCP "invokeStaticMethod" "clojure.lang.Reflector" ((class "java.lang.Class") (class "java.lang.String") (array (class "java.lang.Object"))) (class "java.lang.Object")))) 
                                      (129 (pop)) 
                                      (130 (lload_3)) 
                                      (131 (invokestatic (methodCP "inc" "clojure.lang.Numbers" (long) long))) 
                                      (134 (getstatic (fieldCP "const__8" "clojure.core$to_array_2d" (class "clojure.lang.Var")))) 
                                      (137 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (140 (checkcast (class "clojure.lang.IFn"))) 
                                      (143 (aload 5)) 
                                      (145 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (150 (astore 5)) 
                                      (152 (lstore_3)) 
                                      (153 (goto 56))  ;;to TAG_2
                                      (156 (goto 162)) ;;to TAG_3
                                      (159 (pop)) ;;at TAG_0
                                      (160 (aconst_null)) ;;at TAG_1
                                      (161 (pop)) 
                                      (162 (aload_2)) ;;at TAG_3
                                      (163 (aconst_null)) 
                                      (164 (astore_2)) 
                                      (165 (areturn)) 
                                      (endofcode 166))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$to_array_2d-class-table*
  (make-static-class-decls 
   *clojure.core$to_array_2d*))

(defconst *package-name-map* 
  ("clojure.core$to_array_2d" . "clojure"))
