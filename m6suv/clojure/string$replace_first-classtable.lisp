; string$replace_first-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:58 CDT 2014.
;

(defconst *clojure.string$replace_first*
 (make-class-def
      '(class "clojure.string$replace_first"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "instance?")
                        (STRING  "clojure.string")
                        (STRING  "replace-first-char")
                        (STRING  "replace-first-str")
                        (STRING  "re-matcher")
                        (STRING  "replace-first-by")
                        (STRING  "else")
                        (STRING  "str")
                        (STRING  "Invalid match arg: "))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 91)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "instance?"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.string$replace_first" (class "clojure.lang.Var"))))
                                      (13 (ldc 2))        ;;STRING:: "clojure.string"
                                      (15 (ldc 3))        ;;STRING:: "replace-first-char"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.string$replace_first" (class "clojure.lang.Var"))))
                                      (26 (ldc 2))        ;;STRING:: "clojure.string"
                                      (28 (ldc 4))        ;;STRING:: "replace-first-str"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.string$replace_first" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 5))        ;;STRING:: "re-matcher"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.string$replace_first" (class "clojure.lang.Var"))))
                                      (52 (ldc 2))        ;;STRING:: "clojure.string"
                                      (54 (ldc 6))        ;;STRING:: "replace-first-by"
                                      (56 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (59 (checkcast (class "clojure.lang.Var")))
                                      (62 (putstatic (fieldCP "const__4" "clojure.string$replace_first" (class "clojure.lang.Var"))))
                                      (65 (aconst_null))
                                      (66 (ldc 7))        ;;STRING:: "else"
                                      (68 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (71 (checkcast (class "clojure.lang.Keyword")))
                                      (74 (putstatic (fieldCP "const__5" "clojure.string$replace_first" (class "clojure.lang.Keyword"))))
                                      (77 (ldc 0))        ;;STRING:: "clojure.core"
                                      (79 (ldc 8))        ;;STRING:: "str"
                                      (81 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (84 (checkcast (class "clojure.lang.Var")))
                                      (87 (putstatic (fieldCP "const__6" "clojure.string$replace_first" (class "clojure.lang.Var"))))
                                      (90 (return))
                                      (endofcode 91))
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
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 5) (code_length . 222)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (aconst_null)) 
                                      (2 (astore_1)) 
                                      (3 (invokevirtual (methodCP "toString" "java.lang.Object" () (class "java.lang.String")))) 
                                      (6 (astore 4)) 
                                      (8 (aload_2)) 
                                      (9 (instanceof (class "java.lang.Character"))) 
                                      (12 (ifeq 44)) ;;to TAG_0
                                      (15 (getstatic (fieldCP "const__1" "clojure.string$replace_first" (class "clojure.lang.Var")))) 
                                      (18 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (21 (checkcast (class "clojure.lang.IFn"))) 
                                      (24 (aload 4)) 
                                      (26 (aconst_null)) 
                                      (27 (astore 4)) 
                                      (29 (aload_2)) 
                                      (30 (aconst_null)) 
                                      (31 (astore_2)) 
                                      (32 (aload_3)) 
                                      (33 (aconst_null)) 
                                      (34 (astore_3)) 
                                      (35 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) 
                                      (40 (goto 221)) ;;to TAG_1
                                      (43 (pop)) 
                                      (44 (aload_2)) ;;at TAG_0
                                      (45 (instanceof (class "java.lang.CharSequence"))) 
                                      (48 (ifeq 86))  ;;to TAG_2
                                      (51 (getstatic (fieldCP "const__2" "clojure.string$replace_first" (class "clojure.lang.Var")))) 
                                      (54 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (57 (checkcast (class "clojure.lang.IFn"))) 
                                      (60 (aload 4)) 
                                      (62 (aconst_null)) 
                                      (63 (astore 4)) 
                                      (65 (aload_2)) 
                                      (66 (aconst_null)) 
                                      (67 (astore_2)) 
                                      (68 (invokevirtual (methodCP "toString" "java.lang.Object" () (class "java.lang.String")))) 
                                      (71 (aload_3)) 
                                      (72 (aconst_null)) 
                                      (73 (astore_3)) 
                                      (74 (invokevirtual (methodCP "toString" "java.lang.Object" () (class "java.lang.String")))) 
                                      (77 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) 
                                      (82 (goto 221)) ;;to TAG_1
                                      (85 (pop)) 
                                      (86 (aload_2)) ;;at TAG_2
                                      (87 (instanceof (class "java.util.regex.Pattern"))) 
                                      (90 (ifeq 170)) ;;to TAG_3
                                      (93 (aload_3)) 
                                      (94 (instanceof (class "java.lang.CharSequence"))) 
                                      (97 (ifeq 141)) ;;to TAG_4
                                      (100 (getstatic (fieldCP "const__3" "clojure.string$replace_first" (class "clojure.lang.Var")))) 
                                      (103 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (106 (checkcast (class "clojure.lang.IFn"))) 
                                      (109 (aload_2)) 
                                      (110 (aconst_null)) 
                                      (111 (astore_2)) 
                                      (112 (aload 4)) 
                                      (114 (aconst_null)) 
                                      (115 (astore 4)) 
                                      (117 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (122 (checkcast (class "java.util.regex.Matcher"))) 
                                      (125 (aload_3)) 
                                      (126 (aconst_null)) 
                                      (127 (astore_3)) 
                                      (128 (invokevirtual (methodCP "toString" "java.lang.Object" () (class "java.lang.String")))) 
                                      (131 (checkcast (class "java.lang.String"))) 
                                      (134 (invokevirtual (methodCP "replaceFirst" "java.util.regex.Matcher" ((class "java.lang.String")) (class "java.lang.String")))) 
                                      (137 (goto 166)) ;;to TAG_5
                                      (140 (pop)) 
                                      (141 (getstatic (fieldCP "const__4" "clojure.string$replace_first" (class "clojure.lang.Var")))) ;;at TAG_4
                                      (144 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (147 (checkcast (class "clojure.lang.IFn"))) 
                                      (150 (aload 4)) 
                                      (152 (aconst_null)) 
                                      (153 (astore 4)) 
                                      (155 (aload_2)) 
                                      (156 (aconst_null)) 
                                      (157 (astore_2)) 
                                      (158 (aload_3)) 
                                      (159 (aconst_null)) 
                                      (160 (astore_3)) 
                                      (161 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) 
                                      (166 (goto 221)) ;;to TAG_1;;at TAG_5
                                      (169 (pop)) 
                                      (170 (getstatic (fieldCP "const__5" "clojure.string$replace_first" (class "clojure.lang.Keyword")))) ;;at TAG_3
                                      (173 (dup)) 
                                      (174 (ifnull 219)) ;;to TAG_6
                                      (177 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (180 (if_acmpeq 220)) ;;to TAG_7
                                      (183 (new (class "java.lang.IllegalArgumentException"))) 
                                      (186 (dup)) 
                                      (187 (getstatic (fieldCP "const__6" "clojure.string$replace_first" (class "clojure.lang.Var")))) 
                                      (190 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (193 (checkcast (class "clojure.lang.IFn"))) 
                                      (196 (ldc 9)) ;;STRING:: "Invalid match arg: "
                                      (198 (aload_2)) 
                                      (199 (aconst_null)) 
                                      (200 (astore_2)) 
                                      (201 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (206 (checkcast (class "java.lang.String"))) 
                                      (209 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (212 (checkcast (class "java.lang.Throwable"))) 
                                      (215 (athrow)) 
                                      (216 (goto 221)) ;;to TAG_1
                                      (219 (pop)) ;;at TAG_6
                                      (220 (aconst_null)) ;;at TAG_7
                                      (221 (areturn)) ;;at TAG_1
                                      (endofcode 222))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *string$replace_first-class-table*
  (make-static-class-decls 
   *clojure.string$replace_first*))

(defconst *package-name-map* 
  ("clojure.string$replace_first" . "clojure"))

