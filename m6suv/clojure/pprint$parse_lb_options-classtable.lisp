; pprint$parse_lb_options-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:56 CDT 2014.
;

(defconst *clojure.pprint$parse_lb_options*
 (make-class-def
      '(class "clojure.pprint$parse_lb_options"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "first")
                        (STRING  "drop")
                        (LONG 2)
                        (STRING  "concat")
                        (STRING  "take")
                        (STRING  "apply")
                        (STRING  "hash-map"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 88)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "first"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.pprint$parse_lb_options" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "drop"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.pprint$parse_lb_options" (class "clojure.lang.Var"))))
                                      (26 (ldc2_w 3))     ;; LONG:: "2"
                                      (29 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (32 (putstatic (fieldCP "const__2" "clojure.pprint$parse_lb_options" (class "java.lang.Object"))))
                                      (35 (ldc 0))        ;;STRING:: "clojure.core"
                                      (37 (ldc 4))        ;;STRING:: "concat"
                                      (39 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (42 (checkcast (class "clojure.lang.Var")))
                                      (45 (putstatic (fieldCP "const__3" "clojure.pprint$parse_lb_options" (class "clojure.lang.Var"))))
                                      (48 (ldc 0))        ;;STRING:: "clojure.core"
                                      (50 (ldc 5))        ;;STRING:: "take"
                                      (52 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (55 (checkcast (class "clojure.lang.Var")))
                                      (58 (putstatic (fieldCP "const__4" "clojure.pprint$parse_lb_options" (class "clojure.lang.Var"))))
                                      (61 (ldc 0))        ;;STRING:: "clojure.core"
                                      (63 (ldc 6))        ;;STRING:: "apply"
                                      (65 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (68 (checkcast (class "clojure.lang.Var")))
                                      (71 (putstatic (fieldCP "const__5" "clojure.pprint$parse_lb_options" (class "clojure.lang.Var"))))
                                      (74 (ldc 0))        ;;STRING:: "clojure.core"
                                      (76 (ldc 7))        ;;STRING:: "hash-map"
                                      (78 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (81 (checkcast (class "clojure.lang.Var")))
                                      (84 (putstatic (fieldCP "const__6" "clojure.pprint$parse_lb_options" (class "clojure.lang.Var"))))
                                      (87 (return))
                                      (endofcode 88))
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
                                   (max_stack . 6) (max_locals . 5) (code_length . 142)
                                   (parsedcode
                                      (0 (aload_2)) 
                                      (1 (aconst_null)) 
                                      (2 (astore_2)) 
                                      (3 (astore_3)) 
                                      (4 (getstatic (fieldCP "EMPTY" "clojure.lang.PersistentVector" (class "clojure.lang.PersistentVector")))) 
                                      (7 (astore 4)) 
                                      (9 (aload_1)) ;;at TAG_2
                                      (10 (checkcast (class "clojure.lang.IFn"))) 
                                      (13 (getstatic (fieldCP "const__0" "clojure.pprint$parse_lb_options" (class "clojure.lang.Var")))) 
                                      (16 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (19 (checkcast (class "clojure.lang.IFn"))) 
                                      (22 (aload_3)) 
                                      (23 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (28 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (33 (dup)) 
                                      (34 (ifnull 104)) ;;to TAG_0
                                      (37 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (40 (if_acmpeq 105)) ;;to TAG_1
                                      (43 (getstatic (fieldCP "const__1" "clojure.pprint$parse_lb_options" (class "clojure.lang.Var")))) 
                                      (46 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (49 (checkcast (class "clojure.lang.IFn"))) 
                                      (52 (getstatic (fieldCP "const__2" "clojure.pprint$parse_lb_options" (class "java.lang.Object")))) 
                                      (55 (aload_3)) 
                                      (56 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (61 (getstatic (fieldCP "const__3" "clojure.pprint$parse_lb_options" (class "clojure.lang.Var")))) 
                                      (64 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (67 (checkcast (class "clojure.lang.IFn"))) 
                                      (70 (aload 4)) 
                                      (72 (getstatic (fieldCP "const__4" "clojure.pprint$parse_lb_options" (class "clojure.lang.Var")))) 
                                      (75 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (78 (checkcast (class "clojure.lang.IFn"))) 
                                      (81 (getstatic (fieldCP "const__2" "clojure.pprint$parse_lb_options" (class "java.lang.Object")))) 
                                      (84 (aload_3)) 
                                      (85 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (90 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (95 (astore 4)) 
                                      (97 (astore_3)) 
                                      (98 (goto 9))  ;;to TAG_2
                                      (101 (goto 141)) ;;to TAG_3
                                      (104 (pop)) ;;at TAG_0
                                      (105 (iconst_2)) ;;at TAG_1
                                      (106 (anewarray (class "java.lang.Object"))) 
                                      (109 (dup)) 
                                      (110 (iconst_0)) 
                                      (111 (getstatic (fieldCP "const__5" "clojure.pprint$parse_lb_options" (class "clojure.lang.Var")))) 
                                      (114 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (117 (checkcast (class "clojure.lang.IFn"))) 
                                      (120 (getstatic (fieldCP "const__6" "clojure.pprint$parse_lb_options" (class "clojure.lang.Var")))) 
                                      (123 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (126 (aload 4)) 
                                      (128 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (133 (aastore)) 
                                      (134 (dup)) 
                                      (135 (iconst_1)) 
                                      (136 (aload_3)) 
                                      (137 (aastore)) 
                                      (138 (invokestatic (methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector")))) 
                                      (141 (areturn)) ;;at TAG_3
                                      (endofcode 142))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *pprint$parse_lb_options-class-table*
  (make-static-class-decls 
   *clojure.pprint$parse_lb_options*))

(defconst *package-name-map* 
  ("clojure.pprint$parse_lb_options" . "clojure"))
