; pprint$tuple_map$iter__7314__7318$fn__7319$fn__7320-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:57 CDT 2014.
;

(defconst *clojure.pprint$tuple_map$iter__7314__7318$fn__7319$fn__7320*
 (make-class-def
      '(class "clojure.pprint$tuple_map$iter__7314__7318$fn__7319$fn__7320"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "int")
                        (STRING  "<")
                        (STRING  "nth")
                        (STRING  "chunk-append")
                        (STRING  "unchecked-inc"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "size__4589__auto__" int (accessflags  *class* ) -1)
                        (field "v1" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "c__4588__auto__" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "b__7317" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 80)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "int"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.pprint$tuple_map$iter__7314__7318$fn__7319$fn__7320" (class "clojure.lang.Var"))))
                                      (13 (lconst_0))
                                      (14 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (17 (putstatic (fieldCP "const__1" "clojure.pprint$tuple_map$iter__7314__7318$fn__7319$fn__7320" (class "java.lang.Object"))))
                                      (20 (ldc 0))        ;;STRING:: "clojure.core"
                                      (22 (ldc 2))        ;;STRING:: "<"
                                      (24 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (27 (checkcast (class "clojure.lang.Var")))
                                      (30 (putstatic (fieldCP "const__2" "clojure.pprint$tuple_map$iter__7314__7318$fn__7319$fn__7320" (class "clojure.lang.Var"))))
                                      (33 (ldc 0))        ;;STRING:: "clojure.core"
                                      (35 (ldc 3))        ;;STRING:: "nth"
                                      (37 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (40 (checkcast (class "clojure.lang.Var")))
                                      (43 (putstatic (fieldCP "const__3" "clojure.pprint$tuple_map$iter__7314__7318$fn__7319$fn__7320" (class "clojure.lang.Var"))))
                                      (46 (lconst_1))
                                      (47 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (50 (putstatic (fieldCP "const__4" "clojure.pprint$tuple_map$iter__7314__7318$fn__7319$fn__7320" (class "java.lang.Object"))))
                                      (53 (ldc 0))        ;;STRING:: "clojure.core"
                                      (55 (ldc 4))        ;;STRING:: "chunk-append"
                                      (57 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (60 (checkcast (class "clojure.lang.Var")))
                                      (63 (putstatic (fieldCP "const__5" "clojure.pprint$tuple_map$iter__7314__7318$fn__7319$fn__7320" (class "clojure.lang.Var"))))
                                      (66 (ldc 0))        ;;STRING:: "clojure.core"
                                      (68 (ldc 5))        ;;STRING:: "unchecked-inc"
                                      (70 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (73 (checkcast (class "clojure.lang.Var")))
                                      (76 (putstatic (fieldCP "const__6" "clojure.pprint$tuple_map$iter__7314__7318$fn__7319$fn__7320" (class "clojure.lang.Var"))))
                                      (79 (return))
                                      (endofcode 80))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters int (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 5) (code_length . 26)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.AFunction" () void)))
                                      (4 (aload_0))
                                      (5 (iload_1))
                                      (6 (putfield (fieldCP "size__4589__auto__" "clojure.pprint$tuple_map$iter__7314__7318$fn__7319$fn__7320" int)))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "v1" "clojure.pprint$tuple_map$iter__7314__7318$fn__7319$fn__7320" (class "java.lang.Object"))))
                                      (14 (aload_0))
                                      (15 (aload_3))
                                      (16 (putfield (fieldCP "c__4588__auto__" "clojure.pprint$tuple_map$iter__7314__7318$fn__7319$fn__7320" (class "java.lang.Object"))))
                                      (19 (aload_0))
                                      (20 (aload 4))
                                      (22 (putfield (fieldCP "b__7317" "clojure.pprint$tuple_map$iter__7314__7318$fn__7319$fn__7320" (class "java.lang.Object"))))
                                      (25 (return))
                                      (endofcode 26))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 10) (max_locals . 6) (code_length . 131)
                                   (parsedcode
                                      (0 (lconst_0)) 
                                      (1 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (4 (i2l)) 
                                      (5 (lstore_1)) 
                                      (6 (lload_1)) ;;at TAG_1
                                      (7 (aload_0)) 
                                      (8 (getfield (fieldCP "size__4589__auto__" "clojure.pprint$tuple_map$iter__7314__7318$fn__7319$fn__7320" int))) 
                                      (11 (i2l)) 
                                      (12 (lcmp)) 
                                      (13 (ifge 127)) ;;to TAG_0
                                      (16 (aload_0)) 
                                      (17 (getfield (fieldCP "c__4588__auto__" "clojure.pprint$tuple_map$iter__7314__7318$fn__7319$fn__7320" (class "java.lang.Object")))) 
                                      (20 (checkcast (class "clojure.lang.Indexed"))) 
                                      (23 (lload_1)) 
                                      (24 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (27 (invokeinterface (methodCP "nth" "clojure.lang.Indexed" (int) (class "java.lang.Object")) 2)) 
                                      (32 (astore_3)) 
                                      (33 (aload_3)) 
                                      (34 (lconst_0)) 
                                      (35 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (38 (aconst_null)) 
                                      (39 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (42 (astore 4)) 
                                      (44 (aload_3)) 
                                      (45 (aconst_null)) 
                                      (46 (astore_3)) 
                                      (47 (lconst_1)) 
                                      (48 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (51 (aconst_null)) 
                                      (52 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (55 (astore 5)) 
                                      (57 (getstatic (fieldCP "const__5" "clojure.pprint$tuple_map$iter__7314__7318$fn__7319$fn__7320" (class "clojure.lang.Var")))) 
                                      (60 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (63 (checkcast (class "clojure.lang.IFn"))) 
                                      (66 (aload_0)) 
                                      (67 (getfield (fieldCP "b__7317" "clojure.pprint$tuple_map$iter__7314__7318$fn__7319$fn__7320" (class "java.lang.Object")))) 
                                      (70 (iconst_2)) 
                                      (71 (anewarray (class "java.lang.Object"))) 
                                      (74 (dup)) 
                                      (75 (iconst_0)) 
                                      (76 (aload 4)) 
                                      (78 (aconst_null)) 
                                      (79 (astore 4)) 
                                      (81 (aastore)) 
                                      (82 (dup)) 
                                      (83 (iconst_1)) 
                                      (84 (iconst_2)) 
                                      (85 (anewarray (class "java.lang.Object"))) 
                                      (88 (dup)) 
                                      (89 (iconst_0)) 
                                      (90 (aload 5)) 
                                      (92 (aconst_null)) 
                                      (93 (astore 5)) 
                                      (95 (aastore)) 
                                      (96 (dup)) 
                                      (97 (iconst_1)) 
                                      (98 (aload_0)) 
                                      (99 (getfield (fieldCP "v1" "clojure.pprint$tuple_map$iter__7314__7318$fn__7319$fn__7320" (class "java.lang.Object")))) 
                                      (102 (aastore)) 
                                      (103 (invokestatic (methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector")))) 
                                      (106 (aastore)) 
                                      (107 (invokestatic (methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector")))) 
                                      (110 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (115 (pop)) 
                                      (116 (lload_1)) 
                                      (117 (lconst_1)) 
                                      (118 (ladd)) 
                                      (119 (lstore_1)) 
                                      (120 (goto 6)) ;;to TAG_1
                                      (123 (goto 130))  ;;to TAG_2
                                      (126 (pop)) 
                                      (127 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) ;;at TAG_0
                                      (130 (areturn)) ;;at TAG_2
                                      (endofcode 131))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *pprint$tuple_map$iter__7314__7318$fn__7319$fn__7320-class-table*
  (make-static-class-decls 
   *clojure.pprint$tuple_map$iter__7314__7318$fn__7319$fn__7320*))

(defconst *package-name-map* 
  ("clojure.pprint$tuple_map$iter__7314__7318$fn__7319$fn__7320" . "clojure"))
