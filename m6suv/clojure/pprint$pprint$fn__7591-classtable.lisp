; pprint$pprint$fn__7591-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:56 CDT 2014.
;

(defconst *clojure.pprint$pprint$fn__7591*
 (make-class-def
      '(class "clojure.pprint$pprint$fn__7591"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "not")
                        (STRING  "=")
                        (STRING  "clojure.pprint")
                        (STRING  "*print-base*")
                        (LONG 10)
                        (STRING  "*print-radix*")
                        (STRING  "pr")
                        (STRING  "pr-with-base")
                        (STRING  "write-out")
                        (STRING  "pop-thread-bindings"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "object" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 114)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "not"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.pprint$pprint$fn__7591" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "="
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.pprint$pprint$fn__7591" (class "clojure.lang.Var"))))
                                      (26 (ldc 3))        ;;STRING:: "clojure.pprint"
                                      (28 (ldc 4))        ;;STRING:: "*print-base*"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.pprint$pprint$fn__7591" (class "clojure.lang.Var"))))
                                      (39 (ldc2_w 5))     ;; LONG:: "10"
                                      (42 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (45 (putstatic (fieldCP "const__3" "clojure.pprint$pprint$fn__7591" (class "java.lang.Object"))))
                                      (48 (ldc 3))        ;;STRING:: "clojure.pprint"
                                      (50 (ldc 6))        ;;STRING:: "*print-radix*"
                                      (52 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (55 (checkcast (class "clojure.lang.Var")))
                                      (58 (putstatic (fieldCP "const__4" "clojure.pprint$pprint$fn__7591" (class "clojure.lang.Var"))))
                                      (61 (ldc 0))        ;;STRING:: "clojure.core"
                                      (63 (ldc 7))        ;;STRING:: "pr"
                                      (65 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (68 (checkcast (class "clojure.lang.Var")))
                                      (71 (putstatic (fieldCP "const__5" "clojure.pprint$pprint$fn__7591" (class "clojure.lang.Var"))))
                                      (74 (ldc 3))        ;;STRING:: "clojure.pprint"
                                      (76 (ldc 8))        ;;STRING:: "pr-with-base"
                                      (78 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (81 (checkcast (class "clojure.lang.Var")))
                                      (84 (putstatic (fieldCP "const__6" "clojure.pprint$pprint$fn__7591" (class "clojure.lang.Var"))))
                                      (87 (ldc 3))        ;;STRING:: "clojure.pprint"
                                      (89 (ldc 9))        ;;STRING:: "write-out"
                                      (91 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (94 (checkcast (class "clojure.lang.Var")))
                                      (97 (putstatic (fieldCP "const__7" "clojure.pprint$pprint$fn__7591" (class "clojure.lang.Var"))))
                                      (100 (ldc 0))       ;;STRING:: "clojure.core"
                                      (102 (ldc 10))      ;;STRING:: "pop-thread-bindings"
                                      (104 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (107 (checkcast (class "clojure.lang.Var")))
                                      (110 (putstatic (fieldCP "const__8" "clojure.pprint$pprint$fn__7591" (class "clojure.lang.Var"))))
                                      (113 (return))
                                      (endofcode 114))
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
                                      (6 (putfield (fieldCP "object" "clojure.pprint$pprint$fn__7591" (class "java.lang.Object"))))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 5) (code_length . 186)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.pprint$pprint$fn__7591" (class "clojure.lang.Var")))) ;;at TAG_13
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (getstatic (fieldCP "const__2" "clojure.pprint$pprint$fn__7591" (class "clojure.lang.Var")))) 
                                      (12 (invokevirtual (methodCP "get" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (15 (ldc2_w 5)) ;; LONG:: "10"
                                      (18 (invokestatic (methodCP "equiv" "clojure.lang.Util" ((class "java.lang.Object") long) boolean))) 
                                      (21 (ifeq 30)) ;;to TAG_0
                                      (24 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (27 (goto 33))  ;;to TAG_1
                                      (30 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) ;;at TAG_0
                                      (33 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) ;;at TAG_1
                                      (38 (astore_1)) 
                                      (39 (aload_1)) 
                                      (40 (dup)) 
                                      (41 (ifnull 56)) ;;to TAG_2
                                      (44 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (47 (if_acmpeq 57)) ;;to TAG_3
                                      (50 (aload_1)) 
                                      (51 (aconst_null)) 
                                      (52 (astore_1)) 
                                      (53 (goto 63)) ;;to TAG_4
                                      (56 (pop)) ;;at TAG_2
                                      (57 (getstatic (fieldCP "const__4" "clojure.pprint$pprint$fn__7591" (class "clojure.lang.Var")))) ;;at TAG_3
                                      (60 (invokevirtual (methodCP "get" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (63 (dup)) ;;at TAG_4
                                      (64 (ifnull 98)) ;;to TAG_5
                                      (67 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (70 (if_acmpeq 99)) ;;to TAG_6
                                      (73 (iconst_2)) 
                                      (74 (anewarray (class "java.lang.Object"))) 
                                      (77 (dup)) 
                                      (78 (iconst_0)) 
                                      (79 (getstatic (fieldCP "const__5" "clojure.pprint$pprint$fn__7591" (class "clojure.lang.Var")))) 
                                      (82 (aastore)) 
                                      (83 (dup)) 
                                      (84 (iconst_1)) 
                                      (85 (getstatic (fieldCP "const__6" "clojure.pprint$pprint$fn__7591" (class "clojure.lang.Var")))) 
                                      (88 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (91 (aastore)) 
                                      (92 (invokestatic (methodCP "mapUniqueKeys" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentMap")))) 
                                      (95 (goto 102)) ;;to TAG_7
                                      (98 (pop)) ;;at TAG_5
                                      (99 (getstatic (fieldCP "EMPTY" "clojure.lang.PersistentArrayMap" (class "clojure.lang.PersistentArrayMap")))) ;;at TAG_6
                                      (102 (checkcast (class "clojure.lang.Associative"))) ;;at TAG_7
                                      (105 (invokestatic (methodCP "pushThreadBindings" "clojure.lang.Var" ((class "clojure.lang.Associative")) void))) 
                                      (108 (getstatic (fieldCP "const__7" "clojure.pprint$pprint$fn__7591" (class "clojure.lang.Var")))) ;;at TAG_10
                                      (111 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (114 (checkcast (class "clojure.lang.IFn"))) 
                                      (117 (aload_0)) 
                                      (118 (getfield (fieldCP "object" "clojure.pprint$pprint$fn__7591" (class "java.lang.Object")))) 
                                      (121 (aload_0)) 
                                      (122 (aconst_null)) 
                                      (123 (putfield (fieldCP "object" "clojure.pprint$pprint$fn__7591" (class "java.lang.Object")))) 
                                      (126 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (131 (astore_1)) 
                                      (132 (invokestatic (methodCP "popThreadBindings" "clojure.lang.Var" () void))) ;;at TAG_11
                                      (135 (goto 144)) ;;to TAG_8
                                      (138 (astore_2)) ;;at TAG_12
                                      (139 (invokestatic (methodCP "popThreadBindings" "clojure.lang.Var" () void))) 
                                      (142 (aload_2)) 
                                      (143 (athrow)) 
                                      (144 (aload_1)) ;;at TAG_8
                                      (145 (astore_3)) 
                                      (146 (getstatic (fieldCP "const__8" "clojure.pprint$pprint$fn__7591" (class "clojure.lang.Var")))) ;;at TAG_14
                                      (149 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (152 (checkcast (class "clojure.lang.IFn"))) 
                                      (155 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1)) 
                                      (160 (pop)) 
                                      (161 (goto 184)) ;;to TAG_9
                                      (164 (astore 4)) ;;at TAG_15
                                      (166 (getstatic (fieldCP "const__8" "clojure.pprint$pprint$fn__7591" (class "clojure.lang.Var")))) 
                                      (169 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (172 (checkcast (class "clojure.lang.IFn"))) 
                                      (175 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1)) 
                                      (180 (pop)) 
                                      (181 (aload 4)) 
                                      (183 (athrow)) 
                                      (184 (aload_3)) ;;at TAG_9
                                      (185 (areturn)) 
                                      (endofcode 186))
                                   (Exceptions 
                                     (handler 108 132  138 (class "java.lang.Throwable"))
                                     (handler 0 146  164 (class "java.lang.Throwable")))
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *pprint$pprint$fn__7591-class-table*
  (make-static-class-decls 
   *clojure.pprint$pprint$fn__7591*))

(defconst *package-name-map* 
  ("clojure.pprint$pprint$fn__7591" . "clojure"))

