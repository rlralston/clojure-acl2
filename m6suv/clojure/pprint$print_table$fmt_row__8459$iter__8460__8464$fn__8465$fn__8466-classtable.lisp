; pprint$print_table$fmt_row__8459$iter__8460__8464$fn__8465$fn__8466-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:57 CDT 2014.
;

(defconst *clojure.pprint$print_table$fmt_row__8459$iter__8460__8464$fn__8465$fn__8466*
 (make-class-def
      '(class "clojure.pprint$print_table$fmt_row__8459$iter__8460__8464$fn__8465$fn__8466"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "int")
                        (STRING  "<")
                        (STRING  "nth")
                        (STRING  "chunk-append")
                        (STRING  "format")
                        (STRING  "str")
                        (STRING  "unchecked-inc"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "b__8463" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "size__4589__auto__" int (accessflags  *class* ) -1)
                        (field "c__4588__auto__" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 106)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "int"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.pprint$print_table$fmt_row__8459$iter__8460__8464$fn__8465$fn__8466" (class "clojure.lang.Var"))))
                                      (13 (lconst_0))
                                      (14 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (17 (putstatic (fieldCP "const__1" "clojure.pprint$print_table$fmt_row__8459$iter__8460__8464$fn__8465$fn__8466" (class "java.lang.Object"))))
                                      (20 (ldc 0))        ;;STRING:: "clojure.core"
                                      (22 (ldc 2))        ;;STRING:: "<"
                                      (24 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (27 (checkcast (class "clojure.lang.Var")))
                                      (30 (putstatic (fieldCP "const__2" "clojure.pprint$print_table$fmt_row__8459$iter__8460__8464$fn__8465$fn__8466" (class "clojure.lang.Var"))))
                                      (33 (ldc 0))        ;;STRING:: "clojure.core"
                                      (35 (ldc 3))        ;;STRING:: "nth"
                                      (37 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (40 (checkcast (class "clojure.lang.Var")))
                                      (43 (putstatic (fieldCP "const__3" "clojure.pprint$print_table$fmt_row__8459$iter__8460__8464$fn__8465$fn__8466" (class "clojure.lang.Var"))))
                                      (46 (lconst_1))
                                      (47 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (50 (putstatic (fieldCP "const__4" "clojure.pprint$print_table$fmt_row__8459$iter__8460__8464$fn__8465$fn__8466" (class "java.lang.Object"))))
                                      (53 (ldc 0))        ;;STRING:: "clojure.core"
                                      (55 (ldc 4))        ;;STRING:: "chunk-append"
                                      (57 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (60 (checkcast (class "clojure.lang.Var")))
                                      (63 (putstatic (fieldCP "const__5" "clojure.pprint$print_table$fmt_row__8459$iter__8460__8464$fn__8465$fn__8466" (class "clojure.lang.Var"))))
                                      (66 (ldc 0))        ;;STRING:: "clojure.core"
                                      (68 (ldc 5))        ;;STRING:: "format"
                                      (70 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (73 (checkcast (class "clojure.lang.Var")))
                                      (76 (putstatic (fieldCP "const__6" "clojure.pprint$print_table$fmt_row__8459$iter__8460__8464$fn__8465$fn__8466" (class "clojure.lang.Var"))))
                                      (79 (ldc 0))        ;;STRING:: "clojure.core"
                                      (81 (ldc 6))        ;;STRING:: "str"
                                      (83 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (86 (checkcast (class "clojure.lang.Var")))
                                      (89 (putstatic (fieldCP "const__7" "clojure.pprint$print_table$fmt_row__8459$iter__8460__8464$fn__8465$fn__8466" (class "clojure.lang.Var"))))
                                      (92 (ldc 0))        ;;STRING:: "clojure.core"
                                      (94 (ldc 7))        ;;STRING:: "unchecked-inc"
                                      (96 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (99 (checkcast (class "clojure.lang.Var")))
                                      (102 (putstatic (fieldCP "const__8" "clojure.pprint$print_table$fmt_row__8459$iter__8460__8464$fn__8465$fn__8466" (class "clojure.lang.Var"))))
                                      (105 (return))
                                      (endofcode 106))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.Object") int (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 20)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.AFunction" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "b__8463" "clojure.pprint$print_table$fmt_row__8459$iter__8460__8464$fn__8465$fn__8466" (class "java.lang.Object"))))
                                      (9 (aload_0))
                                      (10 (iload_2))
                                      (11 (putfield (fieldCP "size__4589__auto__" "clojure.pprint$print_table$fmt_row__8459$iter__8460__8464$fn__8465$fn__8466" int)))
                                      (14 (aload_0))
                                      (15 (aload_3))
                                      (16 (putfield (fieldCP "c__4588__auto__" "clojure.pprint$print_table$fmt_row__8459$iter__8460__8464$fn__8465$fn__8466" (class "java.lang.Object"))))
                                      (19 (return))
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 7) (max_locals . 6) (code_length . 129)
                                   (parsedcode
                                      (0 (lconst_0)) 
                                      (1 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (4 (i2l)) 
                                      (5 (lstore_1)) 
                                      (6 (lload_1)) ;;at TAG_1
                                      (7 (aload_0)) 
                                      (8 (getfield (fieldCP "size__4589__auto__" "clojure.pprint$print_table$fmt_row__8459$iter__8460__8464$fn__8465$fn__8466" int))) 
                                      (11 (i2l)) 
                                      (12 (lcmp)) 
                                      (13 (ifge 125)) ;;to TAG_0
                                      (16 (aload_0)) 
                                      (17 (getfield (fieldCP "c__4588__auto__" "clojure.pprint$print_table$fmt_row__8459$iter__8460__8464$fn__8465$fn__8466" (class "java.lang.Object")))) 
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
                                      (57 (getstatic (fieldCP "const__5" "clojure.pprint$print_table$fmt_row__8459$iter__8460__8464$fn__8465$fn__8466" (class "clojure.lang.Var")))) 
                                      (60 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (63 (checkcast (class "clojure.lang.IFn"))) 
                                      (66 (aload_0)) 
                                      (67 (getfield (fieldCP "b__8463" "clojure.pprint$print_table$fmt_row__8459$iter__8460__8464$fn__8465$fn__8466" (class "java.lang.Object")))) 
                                      (70 (getstatic (fieldCP "const__6" "clojure.pprint$print_table$fmt_row__8459$iter__8460__8464$fn__8465$fn__8466" (class "clojure.lang.Var")))) 
                                      (73 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (76 (checkcast (class "clojure.lang.IFn"))) 
                                      (79 (aload 5)) 
                                      (81 (aconst_null)) 
                                      (82 (astore 5)) 
                                      (84 (getstatic (fieldCP "const__7" "clojure.pprint$print_table$fmt_row__8459$iter__8460__8464$fn__8465$fn__8466" (class "clojure.lang.Var")))) 
                                      (87 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (90 (checkcast (class "clojure.lang.IFn"))) 
                                      (93 (aload 4)) 
                                      (95 (aconst_null)) 
                                      (96 (astore 4)) 
                                      (98 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (103 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (108 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (113 (pop)) 
                                      (114 (lload_1)) 
                                      (115 (lconst_1)) 
                                      (116 (ladd)) 
                                      (117 (lstore_1)) 
                                      (118 (goto 6)) ;;to TAG_1
                                      (121 (goto 128))  ;;to TAG_2
                                      (124 (pop)) 
                                      (125 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) ;;at TAG_0
                                      (128 (areturn)) ;;at TAG_2
                                      (endofcode 129))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *pprint$print_table$fmt_row__8459$iter__8460__8464$fn__8465$fn__8466-class-table*
  (make-static-class-decls 
   *clojure.pprint$print_table$fmt_row__8459$iter__8460__8464$fn__8465$fn__8466*))

(defconst *package-name-map* 
  ("clojure.pprint$print_table$fmt_row__8459$iter__8460__8464$fn__8465$fn__8466" . "clojure"))

