; pprint$two_forms$iter__8418__8422$fn__8423$fn__8424-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:57 CDT 2014.
;

(defconst *clojure.pprint$two_forms$iter__8418__8422$fn__8423$fn__8424*
 (make-class-def
      '(class "clojure.pprint$two_forms$iter__8418__8422$fn__8423$fn__8424"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "int")
                        (STRING  "<")
                        (STRING  "chunk-append")
                        (STRING  "symbol")
                        (STRING  "name")
                        (STRING  "first")
                        (STRING  "second")
                        (STRING  "unchecked-inc"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "size__4589__auto__" int (accessflags  *class* ) -1)
                        (field "c__4588__auto__" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "b__8421" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 112)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "int"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.pprint$two_forms$iter__8418__8422$fn__8423$fn__8424" (class "clojure.lang.Var"))))
                                      (13 (lconst_0))
                                      (14 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (17 (putstatic (fieldCP "const__1" "clojure.pprint$two_forms$iter__8418__8422$fn__8423$fn__8424" (class "java.lang.Object"))))
                                      (20 (ldc 0))        ;;STRING:: "clojure.core"
                                      (22 (ldc 2))        ;;STRING:: "<"
                                      (24 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (27 (checkcast (class "clojure.lang.Var")))
                                      (30 (putstatic (fieldCP "const__2" "clojure.pprint$two_forms$iter__8418__8422$fn__8423$fn__8424" (class "clojure.lang.Var"))))
                                      (33 (ldc 0))        ;;STRING:: "clojure.core"
                                      (35 (ldc 3))        ;;STRING:: "chunk-append"
                                      (37 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (40 (checkcast (class "clojure.lang.Var")))
                                      (43 (putstatic (fieldCP "const__3" "clojure.pprint$two_forms$iter__8418__8422$fn__8423$fn__8424" (class "clojure.lang.Var"))))
                                      (46 (ldc 0))        ;;STRING:: "clojure.core"
                                      (48 (ldc 4))        ;;STRING:: "symbol"
                                      (50 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (53 (checkcast (class "clojure.lang.Var")))
                                      (56 (putstatic (fieldCP "const__4" "clojure.pprint$two_forms$iter__8418__8422$fn__8423$fn__8424" (class "clojure.lang.Var"))))
                                      (59 (ldc 0))        ;;STRING:: "clojure.core"
                                      (61 (ldc 5))        ;;STRING:: "name"
                                      (63 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (66 (checkcast (class "clojure.lang.Var")))
                                      (69 (putstatic (fieldCP "const__5" "clojure.pprint$two_forms$iter__8418__8422$fn__8423$fn__8424" (class "clojure.lang.Var"))))
                                      (72 (ldc 0))        ;;STRING:: "clojure.core"
                                      (74 (ldc 6))        ;;STRING:: "first"
                                      (76 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (79 (checkcast (class "clojure.lang.Var")))
                                      (82 (putstatic (fieldCP "const__6" "clojure.pprint$two_forms$iter__8418__8422$fn__8423$fn__8424" (class "clojure.lang.Var"))))
                                      (85 (ldc 0))        ;;STRING:: "clojure.core"
                                      (87 (ldc 7))        ;;STRING:: "second"
                                      (89 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (92 (checkcast (class "clojure.lang.Var")))
                                      (95 (putstatic (fieldCP "const__7" "clojure.pprint$two_forms$iter__8418__8422$fn__8423$fn__8424" (class "clojure.lang.Var"))))
                                      (98 (ldc 0))        ;;STRING:: "clojure.core"
                                      (100 (ldc 8))       ;;STRING:: "unchecked-inc"
                                      (102 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (105 (checkcast (class "clojure.lang.Var")))
                                      (108 (putstatic (fieldCP "const__8" "clojure.pprint$two_forms$iter__8418__8422$fn__8423$fn__8424" (class "clojure.lang.Var"))))
                                      (111 (return))
                                      (endofcode 112))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters int (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 20)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.AFunction" () void)))
                                      (4 (aload_0))
                                      (5 (iload_1))
                                      (6 (putfield (fieldCP "size__4589__auto__" "clojure.pprint$two_forms$iter__8418__8422$fn__8423$fn__8424" int)))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "c__4588__auto__" "clojure.pprint$two_forms$iter__8418__8422$fn__8423$fn__8424" (class "java.lang.Object"))))
                                      (14 (aload_0))
                                      (15 (aload_3))
                                      (16 (putfield (fieldCP "b__8421" "clojure.pprint$two_forms$iter__8418__8422$fn__8423$fn__8424" (class "java.lang.Object"))))
                                      (19 (return))
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 12) (max_locals . 4) (code_length . 154)
                                   (parsedcode
                                      (0 (lconst_0)) 
                                      (1 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (4 (i2l)) 
                                      (5 (lstore_1)) 
                                      (6 (lload_1)) ;;at TAG_1
                                      (7 (aload_0)) 
                                      (8 (getfield (fieldCP "size__4589__auto__" "clojure.pprint$two_forms$iter__8418__8422$fn__8423$fn__8424" int))) 
                                      (11 (i2l)) 
                                      (12 (lcmp)) 
                                      (13 (ifge 150)) ;;to TAG_0
                                      (16 (aload_0)) 
                                      (17 (getfield (fieldCP "c__4588__auto__" "clojure.pprint$two_forms$iter__8418__8422$fn__8423$fn__8424" (class "java.lang.Object")))) 
                                      (20 (checkcast (class "clojure.lang.Indexed"))) 
                                      (23 (lload_1)) 
                                      (24 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (27 (invokeinterface (methodCP "nth" "clojure.lang.Indexed" (int) (class "java.lang.Object")) 2)) 
                                      (32 (astore_3)) 
                                      (33 (getstatic (fieldCP "const__3" "clojure.pprint$two_forms$iter__8418__8422$fn__8423$fn__8424" (class "clojure.lang.Var")))) 
                                      (36 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (39 (checkcast (class "clojure.lang.IFn"))) 
                                      (42 (aload_0)) 
                                      (43 (getfield (fieldCP "b__8421" "clojure.pprint$two_forms$iter__8418__8422$fn__8423$fn__8424" (class "java.lang.Object")))) 
                                      (46 (iconst_2)) 
                                      (47 (anewarray (class "java.lang.Object"))) 
                                      (50 (dup)) 
                                      (51 (iconst_0)) 
                                      (52 (aload_3)) 
                                      (53 (aastore)) 
                                      (54 (dup)) 
                                      (55 (iconst_1)) 
                                      (56 (iconst_2)) 
                                      (57 (anewarray (class "java.lang.Object"))) 
                                      (60 (dup)) 
                                      (61 (iconst_0)) 
                                      (62 (getstatic (fieldCP "const__4" "clojure.pprint$two_forms$iter__8418__8422$fn__8423$fn__8424" (class "clojure.lang.Var")))) 
                                      (65 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (68 (checkcast (class "clojure.lang.IFn"))) 
                                      (71 (getstatic (fieldCP "const__5" "clojure.pprint$two_forms$iter__8418__8422$fn__8423$fn__8424" (class "clojure.lang.Var")))) 
                                      (74 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (77 (checkcast (class "clojure.lang.IFn"))) 
                                      (80 (getstatic (fieldCP "const__6" "clojure.pprint$two_forms$iter__8418__8422$fn__8423$fn__8424" (class "clojure.lang.Var")))) 
                                      (83 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (86 (checkcast (class "clojure.lang.IFn"))) 
                                      (89 (aload_3)) 
                                      (90 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (95 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (100 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (105 (aastore)) 
                                      (106 (dup)) 
                                      (107 (iconst_1)) 
                                      (108 (getstatic (fieldCP "const__7" "clojure.pprint$two_forms$iter__8418__8422$fn__8423$fn__8424" (class "clojure.lang.Var")))) 
                                      (111 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (114 (checkcast (class "clojure.lang.IFn"))) 
                                      (117 (aload_3)) 
                                      (118 (aconst_null)) 
                                      (119 (astore_3)) 
                                      (120 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (125 (aastore)) 
                                      (126 (invokestatic (methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector")))) 
                                      (129 (aastore)) 
                                      (130 (invokestatic (methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector")))) 
                                      (133 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (138 (pop)) 
                                      (139 (lload_1)) 
                                      (140 (lconst_1)) 
                                      (141 (ladd)) 
                                      (142 (lstore_1)) 
                                      (143 (goto 6)) ;;to TAG_1
                                      (146 (goto 153))  ;;to TAG_2
                                      (149 (pop)) 
                                      (150 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) ;;at TAG_0
                                      (153 (areturn)) ;;at TAG_2
                                      (endofcode 154))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *pprint$two_forms$iter__8418__8422$fn__8423$fn__8424-class-table*
  (make-static-class-decls 
   *clojure.pprint$two_forms$iter__8418__8422$fn__8423$fn__8424*))

(defconst *package-name-map* 
  ("clojure.pprint$two_forms$iter__8418__8422$fn__8423$fn__8424" . "clojure"))

