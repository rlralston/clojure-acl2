; core$the_class-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:45 CDT 2014.
;

(defconst *clojure.core$the_class*
 (make-class-def
      '(class "clojure.core$the_class"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "class?")
                        (STRING  "contains?")
                        (STRING  "prim->class")
                        (STRING  "else")
                        (STRING  "str")
                        (STRING  "some")
                        (STRING  "java.lang."))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 0) (code_length . 123)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "class?"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$the_class" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "contains?"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$the_class" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "prim->class"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.core$the_class" (class "clojure.lang.Var"))))
                                      (39 (aconst_null))
                                      (40 (ldc 4))        ;;STRING:: "else"
                                      (42 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (45 (checkcast (class "clojure.lang.Keyword")))
                                      (48 (putstatic (fieldCP "const__3" "clojure.core$the_class" (class "clojure.lang.Keyword"))))
                                      (51 (ldc 0))        ;;STRING:: "clojure.core"
                                      (53 (ldc 5))        ;;STRING:: "str"
                                      (55 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (58 (checkcast (class "clojure.lang.Var")))
                                      (61 (putstatic (fieldCP "const__4" "clojure.core$the_class" (class "clojure.lang.Var"))))
                                      (64 (ldc 0))        ;;STRING:: "clojure.core"
                                      (66 (ldc 6))        ;;STRING:: "some"
                                      (68 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (71 (checkcast (class "clojure.lang.Var")))
                                      (74 (putstatic (fieldCP "const__5" "clojure.core$the_class" (class "clojure.lang.Var"))))
                                      (77 (bipush 46))
                                      (79 (invokestatic
					(methodCP "valueOf" "java.lang.Character" (char) (class "java.lang.Character"))))
                                      (82 (putstatic (fieldCP "const__6" "clojure.core$the_class" (class "java.lang.Object"))))
                                      (85 (bipush 91))
                                      (87 (invokestatic
					(methodCP "valueOf" "java.lang.Character" (char) (class "java.lang.Character"))))
                                      (90 (putstatic (fieldCP "const__7" "clojure.core$the_class" (class "java.lang.Object"))))
                                      (93 (iconst_2))
                                      (94 (anewarray (class "java.lang.Object")))
                                      (97 (dup))
                                      (98 (iconst_0))
                                      (99 (bipush 46))
                                      (101 (invokestatic
					(methodCP "valueOf" "java.lang.Character" (char) (class "java.lang.Character"))))
                                      (104 (aastore))
                                      (105 (dup))
                                      (106 (iconst_1))
                                      (107 (bipush 91))
                                      (109 (invokestatic
					(methodCP "valueOf" "java.lang.Character" (char) (class "java.lang.Character"))))
                                      (112 (aastore))
                                      (113 (invokestatic
					(methodCP "create" "clojure.lang.PersistentHashSet" ((array (class "java.lang.Object"))) (class "clojure.lang.PersistentHashSet"))))
                                      (116 (checkcast (class "clojure.lang.AFn")))
                                      (119 (putstatic (fieldCP "const__8" "clojure.core$the_class" (class "clojure.lang.AFn"))))
                                      (122 (return))
                                      (endofcode 123))
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
                                   (max_stack . 4) (max_locals . 3) (code_length . 181)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$the_class" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (aload_1)) 
                                      (10 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (15 (dup)) 
                                      (16 (ifnull 31)) ;;to TAG_0
                                      (19 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (22 (if_acmpeq 32))  ;;to TAG_1
                                      (25 (aload_1)) 
                                      (26 (aconst_null)) 
                                      (27 (astore_1)) 
                                      (28 (goto 180)) ;;to TAG_2
                                      (31 (pop)) ;;at TAG_0
                                      (32 (getstatic (fieldCP "const__1" "clojure.core$the_class" (class "clojure.lang.Var")))) ;;at TAG_1
                                      (35 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (38 (checkcast (class "clojure.lang.IFn"))) 
                                      (41 (getstatic (fieldCP "const__2" "clojure.core$the_class" (class "clojure.lang.Var")))) 
                                      (44 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (47 (aload_1)) 
                                      (48 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (53 (dup)) 
                                      (54 (ifnull 83)) ;;to TAG_3
                                      (57 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (60 (if_acmpeq 84)) ;;to TAG_4
                                      (63 (getstatic (fieldCP "const__2" "clojure.core$the_class" (class "clojure.lang.Var")))) 
                                      (66 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (69 (checkcast (class "clojure.lang.IFn"))) 
                                      (72 (aload_1)) 
                                      (73 (aconst_null)) 
                                      (74 (astore_1)) 
                                      (75 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (80 (goto 180)) ;;to TAG_2
                                      (83 (pop)) ;;at TAG_3
                                      (84 (getstatic (fieldCP "const__3" "clojure.core$the_class" (class "clojure.lang.Keyword")))) ;;at TAG_4
                                      (87 (dup)) 
                                      (88 (ifnull 178)) ;;to TAG_5
                                      (91 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (94 (if_acmpeq 179)) ;;to TAG_6
                                      (97 (getstatic (fieldCP "const__4" "clojure.core$the_class" (class "clojure.lang.Var")))) 
                                      (100 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (103 (checkcast (class "clojure.lang.IFn"))) 
                                      (106 (aload_1)) 
                                      (107 (aconst_null)) 
                                      (108 (astore_1)) 
                                      (109 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (114 (astore_2)) 
                                      (115 (getstatic (fieldCP "const__5" "clojure.core$the_class" (class "clojure.lang.Var")))) 
                                      (118 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (121 (checkcast (class "clojure.lang.IFn"))) 
                                      (124 (getstatic (fieldCP "const__8" "clojure.core$the_class" (class "clojure.lang.AFn")))) 
                                      (127 (aload_2)) 
                                      (128 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (133 (dup)) 
                                      (134 (ifnull 149)) ;;to TAG_7
                                      (137 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (140 (if_acmpeq 150)) ;;to TAG_8
                                      (143 (aload_2)) 
                                      (144 (aconst_null)) 
                                      (145 (astore_2)) 
                                      (146 (goto 169)) ;;to TAG_9
                                      (149 (pop)) ;;at TAG_7
                                      (150 (getstatic (fieldCP "const__4" "clojure.core$the_class" (class "clojure.lang.Var")))) ;;at TAG_8
                                      (153 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (156 (checkcast (class "clojure.lang.IFn"))) 
                                      (159 (ldc 7)) ;;STRING:: "java.lang."
                                      (161 (aload_2)) 
                                      (162 (aconst_null)) 
                                      (163 (astore_2)) 
                                      (164 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (169 (checkcast (class "java.lang.String"))) ;;at TAG_9
                                      (172 (invokestatic (methodCP "classForName" "clojure.lang.RT" ((class "java.lang.String")) (class "java.lang.Class")))) 
                                      (175 (goto 180)) ;;to TAG_2
                                      (178 (pop)) ;;at TAG_5
                                      (179 (aconst_null)) ;;at TAG_6
                                      (180 (areturn)) ;;at TAG_2
                                      (endofcode 181))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$the_class-class-table*
  (make-static-class-decls 
   *clojure.core$the_class*))

(defconst *package-name-map* 
  ("clojure.core$the_class" . "clojure"))

