; string$escape-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:58 CDT 2014.
;

(defconst *clojure.string$escape*
 (make-class-def
      '(class "clojure.string$escape"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "int")
                        (STRING  "=")
                        (STRING  "inc"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 47)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "int"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.string$escape" (class "clojure.lang.Var"))))
                                      (13 (lconst_0))
                                      (14 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (17 (putstatic (fieldCP "const__1" "clojure.string$escape" (class "java.lang.Object"))))
                                      (20 (ldc 0))        ;;STRING:: "clojure.core"
                                      (22 (ldc 2))        ;;STRING:: "="
                                      (24 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (27 (checkcast (class "clojure.lang.Var")))
                                      (30 (putstatic (fieldCP "const__2" "clojure.string$escape" (class "clojure.lang.Var"))))
                                      (33 (ldc 0))        ;;STRING:: "clojure.core"
                                      (35 (ldc 3))        ;;STRING:: "inc"
                                      (37 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (40 (checkcast (class "clojure.lang.Var")))
                                      (43 (putstatic (fieldCP "const__3" "clojure.string$escape" (class "clojure.lang.Var"))))
                                      (46 (return))
                                      (endofcode 47))
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
                                   (max_stack . 4) (max_locals . 9) (code_length . 143)
                                   (parsedcode
                                      (0 (lconst_0)) 
                                      (1 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (4 (i2l)) 
                                      (5 (lstore_3)) 
                                      (6 (new (class "java.lang.StringBuilder"))) 
                                      (9 (dup)) 
                                      (10 (aload_1)) 
                                      (11 (checkcast (class "java.lang.CharSequence"))) 
                                      (14 (invokeinterface (methodCP "length" "java.lang.CharSequence" () int) 1)) 
                                      (19 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" (int) void))) 
                                      (22 (astore 5)) 
                                      (24 (aload_1)) ;;at TAG_5
                                      (25 (checkcast (class "java.lang.CharSequence"))) 
                                      (28 (invokeinterface (methodCP "length" "java.lang.CharSequence" () int) 1)) 
                                      (33 (i2l)) 
                                      (34 (lload_3)) 
                                      (35 (lcmp)) 
                                      (36 (ifne 51)) ;;to TAG_0
                                      (39 (aload 5)) 
                                      (41 (checkcast (class "java.lang.StringBuilder"))) 
                                      (44 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (47 (goto 142)) ;;to TAG_1
                                      (50 (pop)) 
                                      (51 (aload_1)) ;;at TAG_0
                                      (52 (checkcast (class "java.lang.CharSequence"))) 
                                      (55 (lload_3)) 
                                      (56 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (59 (invokeinterface (methodCP "charAt" "java.lang.CharSequence" (int) char) 2)) 
                                      (64 (istore 6)) 
                                      (66 (aload_2)) 
                                      (67 (checkcast (class "clojure.lang.IFn"))) 
                                      (70 (iload 6)) 
                                      (72 (invokestatic (methodCP "valueOf" "java.lang.Character" (char) (class "java.lang.Character")))) 
                                      (75 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (80 (astore 7)) 
                                      (82 (aload 7)) 
                                      (84 (dup)) 
                                      (85 (ifnull 118))  ;;to TAG_2
                                      (88 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (91 (if_acmpeq 119)) ;;to TAG_3
                                      (94 (aload 7)) 
                                      (96 (aconst_null)) 
                                      (97 (astore 7)) 
                                      (99 (astore 8)) 
                                      (101 (aload 5)) 
                                      (103 (checkcast (class "java.lang.StringBuilder"))) 
                                      (106 (aload 8)) 
                                      (108 (aconst_null)) 
                                      (109 (astore 8)) 
                                      (111 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.Object")) (class "java.lang.StringBuilder")))) 
                                      (114 (pop)) 
                                      (115 (goto 130)) ;;to TAG_4
                                      (118 (pop)) ;;at TAG_2
                                      (119 (aload 5)) ;;at TAG_3
                                      (121 (checkcast (class "java.lang.StringBuilder"))) 
                                      (124 (iload 6)) 
                                      (126 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (char) (class "java.lang.StringBuilder")))) 
                                      (129 (pop)) 
                                      (130 (lload_3)) ;;at TAG_4
                                      (131 (invokestatic (methodCP "inc" "clojure.lang.Numbers" (long) long))) 
                                      (134 (aload 5)) 
                                      (136 (astore 5)) 
                                      (138 (lstore_3)) 
                                      (139 (goto 24)) ;;to TAG_5
                                      (142 (areturn)) ;;at TAG_1
                                      (endofcode 143))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *string$escape-class-table*
  (make-static-class-decls 
   *clojure.string$escape*))

(defconst *package-name-map* 
  ("clojure.string$escape" . "clojure"))
