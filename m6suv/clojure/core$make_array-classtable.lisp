; core$make_array-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:44 CDT 2014.
;

(defconst *clojure.core$make_array*
 (make-class-def
      '(class "clojure.core$make_array"
            "clojure.lang.RestFn"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "int")
                        (STRING  "cons")
                        (STRING  "make-array")
                        (STRING  "count")
                        (STRING  "long")
                        (STRING  "alength")
                        (STRING  "<")
                        (STRING  "aset-int")
                        (STRING  "nth")
                        (STRING  "unchecked-inc"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
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
                                   (max_stack . 2) (max_locals . 0) (code_length . 138)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "int"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$make_array" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "cons"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$make_array" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "make-array"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.core$make_array" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 4))        ;;STRING:: "count"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.core$make_array" (class "clojure.lang.Var"))))
                                      (52 (ldc 0))        ;;STRING:: "clojure.core"
                                      (54 (ldc 5))        ;;STRING:: "long"
                                      (56 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (59 (checkcast (class "clojure.lang.Var")))
                                      (62 (putstatic (fieldCP "const__4" "clojure.core$make_array" (class "clojure.lang.Var"))))
                                      (65 (ldc 0))        ;;STRING:: "clojure.core"
                                      (67 (ldc 6))        ;;STRING:: "alength"
                                      (69 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (72 (checkcast (class "clojure.lang.Var")))
                                      (75 (putstatic (fieldCP "const__5" "clojure.core$make_array" (class "clojure.lang.Var"))))
                                      (78 (lconst_0))
                                      (79 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (82 (putstatic (fieldCP "const__6" "clojure.core$make_array" (class "java.lang.Object"))))
                                      (85 (ldc 0))        ;;STRING:: "clojure.core"
                                      (87 (ldc 7))        ;;STRING:: "<"
                                      (89 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (92 (checkcast (class "clojure.lang.Var")))
                                      (95 (putstatic (fieldCP "const__7" "clojure.core$make_array" (class "clojure.lang.Var"))))
                                      (98 (ldc 0))        ;;STRING:: "clojure.core"
                                      (100 (ldc 8))       ;;STRING:: "aset-int"
                                      (102 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (105 (checkcast (class "clojure.lang.Var")))
                                      (108 (putstatic (fieldCP "const__8" "clojure.core$make_array" (class "clojure.lang.Var"))))
                                      (111 (ldc 0))       ;;STRING:: "clojure.core"
                                      (113 (ldc 9))       ;;STRING:: "nth"
                                      (115 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (118 (checkcast (class "clojure.lang.Var")))
                                      (121 (putstatic (fieldCP "const__9" "clojure.core$make_array" (class "clojure.lang.Var"))))
                                      (124 (ldc 0))       ;;STRING:: "clojure.core"
                                      (126 (ldc 10))      ;;STRING:: "unchecked-inc"
                                      (128 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (131 (checkcast (class "clojure.lang.Var")))
                                      (134 (putstatic (fieldCP "const__10" "clojure.core$make_array" (class "clojure.lang.Var"))))
                                      (137 (return))
                                      (endofcode 138))
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
					(methodCP "<init>" "clojure.lang.RestFn" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "doInvoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 10) (code_length . 134)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__1" "clojure.core$make_array" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (aload_2)) 
                                      (10 (aconst_null)) 
                                      (11 (astore_2)) 
                                      (12 (aload_3)) 
                                      (13 (aconst_null)) 
                                      (14 (astore_3)) 
                                      (15 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (20 (astore 4)) 
                                      (22 (getstatic (fieldCP "const__2" "clojure.core$make_array" (class "clojure.lang.Var")))) 
                                      (25 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (28 (checkcast (class "clojure.lang.IFn"))) 
                                      (31 (getstatic (fieldCP "TYPE" "java.lang.Integer" (class "java.lang.Class")))) 
                                      (34 (aload 4)) 
                                      (36 (invokestatic (methodCP "count" "clojure.lang.RT" ((class "java.lang.Object")) int))) 
                                      (39 (invokestatic (methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer")))) 
                                      (42 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (47 (astore 5)) 
                                      (49 (aload 5)) 
                                      (51 (checkcast (array int))) 
                                      (54 (arraylength)) 
                                      (55 (i2l)) 
                                      (56 (lstore 6)) 
                                      (58 (lconst_0)) 
                                      (59 (lstore 8)) 
                                      (61 (lload 8)) ;;at TAG_1
                                      (63 (lload 6)) 
                                      (65 (lcmp)) 
                                      (66 (ifge 114)) ;;to TAG_0
                                      (69 (getstatic (fieldCP "const__8" "clojure.core$make_array" (class "clojure.lang.Var")))) 
                                      (72 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (75 (checkcast (class "clojure.lang.IFn"))) 
                                      (78 (aload 5)) 
                                      (80 (lload 8)) 
                                      (82 (invokestatic (methodCP "num" "clojure.lang.Numbers" (long) (class "java.lang.Number")))) 
                                      (85 (aload 4)) 
                                      (87 (lload 8)) 
                                      (89 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (92 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int) (class "java.lang.Object")))) 
                                      (95 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) 
                                      (100 (pop)) 
                                      (101 (lload 8)) 
                                      (103 (lconst_1)) 
                                      (104 (ladd)) 
                                      (105 (lstore 8)) 
                                      (107 (goto 61)) ;;to TAG_1
                                      (110 (goto 116))  ;;to TAG_2
                                      (113 (pop)) 
                                      (114 (aconst_null)) ;;at TAG_0
                                      (115 (pop)) 
                                      (116 (aload_1)) ;;at TAG_2
                                      (117 (aconst_null)) 
                                      (118 (astore_1)) 
                                      (119 (checkcast (class "java.lang.Class"))) 
                                      (122 (aload 5)) 
                                      (124 (aconst_null)) 
                                      (125 (astore 5)) 
                                      (127 (checkcast (array int))) 
                                      (130 (invokestatic (methodCP "newInstance" "java.lang.reflect.Array" ((class "java.lang.Class") (array int)) (class "java.lang.Object")))) 
                                      (133 (areturn)) 
                                      (endofcode 134))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 16)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (aconst_null))
                                      (2 (astore_1))
                                      (3 (checkcast (class "java.lang.Class")))
                                      (6 (aload_2))
                                      (7 (aconst_null))
                                      (8 (astore_2))
                                      (9 (invokestatic
					(methodCP "intCast" "clojure.lang.RT" ((class "java.lang.Object")) int)))
                                      (12 (invokestatic
					(methodCP "newInstance" "java.lang.reflect.Array" ((class "java.lang.Class") int) (class "java.lang.Object"))))
                                      (15 (areturn))
                                      (endofcode 16))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getRequiredArity"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 2)
                                   (parsedcode
                                      (0 (iconst_2))
                                      (1 (ireturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$make_array-class-table*
  (make-static-class-decls 
   *clojure.core$make_array*))

(defconst *package-name-map* 
  ("clojure.core$make_array" . "clojure"))

