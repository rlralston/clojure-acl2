; zip$next-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:59 CDT 2014.
;

(defconst *clojure.zip$next*
 (make-class-def
      '(class "clojure.zip$next"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "=")
                        (STRING  "end")
                        (STRING  "clojure.zip")
                        (STRING  "branch?")
                        (STRING  "down")
                        (STRING  "right")
                        (STRING  "up")
                        (STRING  "node"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 98)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "="
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.zip$next" (class "clojure.lang.Var"))))
                                      (13 (aconst_null))
                                      (14 (ldc 2))        ;;STRING:: "end"
                                      (16 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (19 (checkcast (class "clojure.lang.Keyword")))
                                      (22 (putstatic (fieldCP "const__1" "clojure.zip$next" (class "clojure.lang.Keyword"))))
                                      (25 (lconst_1))
                                      (26 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (29 (putstatic (fieldCP "const__2" "clojure.zip$next" (class "java.lang.Object"))))
                                      (32 (ldc 3))        ;;STRING:: "clojure.zip"
                                      (34 (ldc 4))        ;;STRING:: "branch?"
                                      (36 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (39 (checkcast (class "clojure.lang.Var")))
                                      (42 (putstatic (fieldCP "const__3" "clojure.zip$next" (class "clojure.lang.Var"))))
                                      (45 (ldc 3))        ;;STRING:: "clojure.zip"
                                      (47 (ldc 5))        ;;STRING:: "down"
                                      (49 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (52 (checkcast (class "clojure.lang.Var")))
                                      (55 (putstatic (fieldCP "const__4" "clojure.zip$next" (class "clojure.lang.Var"))))
                                      (58 (ldc 3))        ;;STRING:: "clojure.zip"
                                      (60 (ldc 6))        ;;STRING:: "right"
                                      (62 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (65 (checkcast (class "clojure.lang.Var")))
                                      (68 (putstatic (fieldCP "const__5" "clojure.zip$next" (class "clojure.lang.Var"))))
                                      (71 (ldc 3))        ;;STRING:: "clojure.zip"
                                      (73 (ldc 7))        ;;STRING:: "up"
                                      (75 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (78 (checkcast (class "clojure.lang.Var")))
                                      (81 (putstatic (fieldCP "const__6" "clojure.zip$next" (class "clojure.lang.Var"))))
                                      (84 (ldc 3))        ;;STRING:: "clojure.zip"
                                      (86 (ldc 8))        ;;STRING:: "node"
                                      (88 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (91 (checkcast (class "clojure.lang.Var")))
                                      (94 (putstatic (fieldCP "const__7" "clojure.zip$next" (class "clojure.lang.Var"))))
                                      (97 (return))
                                      (endofcode 98))
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
                                   (max_stack . 5) (max_locals . 6) (code_length . 272)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__1" "clojure.zip$next" (class "clojure.lang.Keyword")))) 
                                      (3 (aload_1)) 
                                      (4 (checkcast (class "clojure.lang.IFn"))) 
                                      (7 (getstatic (fieldCP "const__2" "clojure.zip$next" (class "java.lang.Object")))) 
                                      (10 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (15 (invokestatic (methodCP "equiv" "clojure.lang.Util" ((class "java.lang.Object") (class "java.lang.Object")) boolean))) 
                                      (18 (ifeq 28)) ;;to TAG_0
                                      (21 (aload_1)) 
                                      (22 (aconst_null)) 
                                      (23 (astore_1)) 
                                      (24 (goto 271)) ;;to TAG_1
                                      (27 (pop)) 
                                      (28 (getstatic (fieldCP "const__3" "clojure.zip$next" (class "clojure.lang.Var")))) ;;at TAG_0
                                      (31 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (34 (checkcast (class "clojure.lang.IFn"))) 
                                      (37 (aload_1)) 
                                      (38 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (43 (astore_2)) 
                                      (44 (aload_2)) 
                                      (45 (dup)) 
                                      (46 (ifnull 73)) ;;to TAG_2
                                      (49 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (52 (if_acmpeq 74)) ;;to TAG_3
                                      (55 (getstatic (fieldCP "const__4" "clojure.zip$next" (class "clojure.lang.Var")))) 
                                      (58 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (61 (checkcast (class "clojure.lang.IFn"))) 
                                      (64 (aload_1)) 
                                      (65 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (70 (goto 77)) ;;to TAG_4
                                      (73 (pop)) ;;at TAG_2
                                      (74 (aload_2)) ;;at TAG_3
                                      (75 (aconst_null)) 
                                      (76 (astore_2)) 
                                      (77 (astore_2)) ;;at TAG_4
                                      (78 (aload_2)) 
                                      (79 (dup)) 
                                      (80 (ifnull 95)) ;;to TAG_5
                                      (83 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (86 (if_acmpeq 96)) ;;to TAG_6
                                      (89 (aload_2)) 
                                      (90 (aconst_null)) 
                                      (91 (astore_2)) 
                                      (92 (goto 271)) ;;to TAG_1
                                      (95 (pop)) ;;at TAG_5
                                      (96 (getstatic (fieldCP "const__5" "clojure.zip$next" (class "clojure.lang.Var")))) ;;at TAG_6
                                      (99 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (102 (checkcast (class "clojure.lang.IFn"))) 
                                      (105 (aload_1)) 
                                      (106 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (111 (astore_3)) 
                                      (112 (aload_3)) 
                                      (113 (dup)) 
                                      (114 (ifnull 129)) ;;to TAG_7
                                      (117 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (120 (if_acmpeq 130)) ;;to TAG_8
                                      (123 (aload_3)) 
                                      (124 (aconst_null)) 
                                      (125 (astore_3)) 
                                      (126 (goto 271)) ;;to TAG_1
                                      (129 (pop)) ;;at TAG_7
                                      (130 (aload_1)) ;;at TAG_8
                                      (131 (aconst_null)) 
                                      (132 (astore_1)) 
                                      (133 (astore 4)) 
                                      (135 (getstatic (fieldCP "const__6" "clojure.zip$next" (class "clojure.lang.Var")))) ;;at TAG_14
                                      (138 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (141 (checkcast (class "clojure.lang.IFn"))) 
                                      (144 (aload 4)) 
                                      (146 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (151 (dup)) 
                                      (152 (ifnull 238)) ;;to TAG_9
                                      (155 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (158 (if_acmpeq 239)) ;;to TAG_10
                                      (161 (getstatic (fieldCP "const__5" "clojure.zip$next" (class "clojure.lang.Var")))) 
                                      (164 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (167 (checkcast (class "clojure.lang.IFn"))) 
                                      (170 (getstatic (fieldCP "const__6" "clojure.zip$next" (class "clojure.lang.Var")))) 
                                      (173 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (176 (checkcast (class "clojure.lang.IFn"))) 
                                      (179 (aload 4)) 
                                      (181 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (186 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (191 (astore 5)) 
                                      (193 (aload 5)) 
                                      (195 (dup)) 
                                      (196 (ifnull 213)) ;;to TAG_11
                                      (199 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (202 (if_acmpeq 214)) ;;to TAG_12
                                      (205 (aload 5)) 
                                      (207 (aconst_null)) 
                                      (208 (astore 5)) 
                                      (210 (goto 235)) ;;to TAG_13
                                      (213 (pop)) ;;at TAG_11
                                      (214 (getstatic (fieldCP "const__6" "clojure.zip$next" (class "clojure.lang.Var")))) ;;at TAG_12
                                      (217 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (220 (checkcast (class "clojure.lang.IFn"))) 
                                      (223 (aload 4)) 
                                      (225 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (230 (astore 4)) 
                                      (232 (goto 135))  ;;to TAG_14
                                      (235 (goto 271)) ;;to TAG_1;;at TAG_13
                                      (238 (pop)) ;;at TAG_9
                                      (239 (iconst_2)) ;;at TAG_10
                                      (240 (anewarray (class "java.lang.Object"))) 
                                      (243 (dup)) 
                                      (244 (iconst_0)) 
                                      (245 (getstatic (fieldCP "const__7" "clojure.zip$next" (class "clojure.lang.Var")))) 
                                      (248 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (251 (checkcast (class "clojure.lang.IFn"))) 
                                      (254 (aload 4)) 
                                      (256 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (261 (aastore)) 
                                      (262 (dup)) 
                                      (263 (iconst_1)) 
                                      (264 (getstatic (fieldCP "const__1" "clojure.zip$next" (class "clojure.lang.Keyword")))) 
                                      (267 (aastore)) 
                                      (268 (invokestatic (methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector")))) 
                                      (271 (areturn)) ;;at TAG_1
                                      (endofcode 272))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *zip$next-class-table*
  (make-static-class-decls 
   *clojure.zip$next*))

(defconst *package-name-map* 
  ("clojure.zip$next" . "clojure"))

