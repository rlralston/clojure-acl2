; string$replace-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:58 CDT 2014.
;

(defconst *clojure.string$replace*
 (make-class-def
      '(class "clojure.string$replace"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "instance?")
                        (STRING  "re-matcher")
                        (STRING  "clojure.string")
                        (STRING  "replace-by")
                        (STRING  "else")
                        (STRING  "str")
                        (STRING  "Invalid match arg: "))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 65)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "instance?"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.string$replace" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "re-matcher"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.string$replace" (class "clojure.lang.Var"))))
                                      (26 (ldc 3))        ;;STRING:: "clojure.string"
                                      (28 (ldc 4))        ;;STRING:: "replace-by"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.string$replace" (class "clojure.lang.Var"))))
                                      (39 (aconst_null))
                                      (40 (ldc 5))        ;;STRING:: "else"
                                      (42 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (45 (checkcast (class "clojure.lang.Keyword")))
                                      (48 (putstatic (fieldCP "const__3" "clojure.string$replace" (class "clojure.lang.Keyword"))))
                                      (51 (ldc 0))        ;;STRING:: "clojure.core"
                                      (53 (ldc 6))        ;;STRING:: "str"
                                      (55 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (58 (checkcast (class "clojure.lang.Var")))
                                      (61 (putstatic (fieldCP "const__4" "clojure.string$replace" (class "clojure.lang.Var"))))
                                      (64 (return))
                                      (endofcode 65))
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
                                   (max_stack . 6) (max_locals . 5) (code_length . 218)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (aconst_null)) 
                                      (2 (astore_1)) 
                                      (3 (invokevirtual (methodCP "toString" "java.lang.Object" () (class "java.lang.String")))) 
                                      (6 (astore 4)) 
                                      (8 (aload_2)) 
                                      (9 (instanceof (class "java.lang.Character"))) 
                                      (12 (ifeq 48)) ;;to TAG_0
                                      (15 (aload 4)) 
                                      (17 (aconst_null)) 
                                      (18 (astore 4)) 
                                      (20 (checkcast (class "java.lang.String"))) 
                                      (23 (aload_2)) 
                                      (24 (aconst_null)) 
                                      (25 (astore_2)) 
                                      (26 (checkcast (class "java.lang.Character"))) 
                                      (29 (invokevirtual (methodCP "charValue" "java.lang.Character" () char))) 
                                      (32 (aload_3)) 
                                      (33 (aconst_null)) 
                                      (34 (astore_3)) 
                                      (35 (checkcast (class "java.lang.Character"))) 
                                      (38 (invokevirtual (methodCP "charValue" "java.lang.Character" () char))) 
                                      (41 (invokevirtual (methodCP "replace" "java.lang.String" (char char) (class "java.lang.String")))) 
                                      (44 (goto 217)) ;;to TAG_1
                                      (47 (pop)) 
                                      (48 (aload_2)) ;;at TAG_0
                                      (49 (instanceof (class "java.lang.CharSequence"))) 
                                      (52 (ifeq 82))  ;;to TAG_2
                                      (55 (aload 4)) 
                                      (57 (aconst_null)) 
                                      (58 (astore 4)) 
                                      (60 (checkcast (class "java.lang.String"))) 
                                      (63 (aload_2)) 
                                      (64 (aconst_null)) 
                                      (65 (astore_2)) 
                                      (66 (checkcast (class "java.lang.CharSequence"))) 
                                      (69 (aload_3)) 
                                      (70 (aconst_null)) 
                                      (71 (astore_3)) 
                                      (72 (checkcast (class "java.lang.CharSequence"))) 
                                      (75 (invokevirtual (methodCP "replace" "java.lang.String" ((class "java.lang.CharSequence") (class "java.lang.CharSequence")) (class "java.lang.String")))) 
                                      (78 (goto 217)) ;;to TAG_1
                                      (81 (pop)) 
                                      (82 (aload_2)) ;;at TAG_2
                                      (83 (instanceof (class "java.util.regex.Pattern"))) 
                                      (86 (ifeq 166)) ;;to TAG_3
                                      (89 (aload_3)) 
                                      (90 (instanceof (class "java.lang.CharSequence"))) 
                                      (93 (ifeq 137)) ;;to TAG_4
                                      (96 (getstatic (fieldCP "const__1" "clojure.string$replace" (class "clojure.lang.Var")))) 
                                      (99 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (102 (checkcast (class "clojure.lang.IFn"))) 
                                      (105 (aload_2)) 
                                      (106 (aconst_null)) 
                                      (107 (astore_2)) 
                                      (108 (aload 4)) 
                                      (110 (aconst_null)) 
                                      (111 (astore 4)) 
                                      (113 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (118 (checkcast (class "java.util.regex.Matcher"))) 
                                      (121 (aload_3)) 
                                      (122 (aconst_null)) 
                                      (123 (astore_3)) 
                                      (124 (invokevirtual (methodCP "toString" "java.lang.Object" () (class "java.lang.String")))) 
                                      (127 (checkcast (class "java.lang.String"))) 
                                      (130 (invokevirtual (methodCP "replaceAll" "java.util.regex.Matcher" ((class "java.lang.String")) (class "java.lang.String")))) 
                                      (133 (goto 162)) ;;to TAG_5
                                      (136 (pop)) 
                                      (137 (getstatic (fieldCP "const__2" "clojure.string$replace" (class "clojure.lang.Var")))) ;;at TAG_4
                                      (140 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (143 (checkcast (class "clojure.lang.IFn"))) 
                                      (146 (aload 4)) 
                                      (148 (aconst_null)) 
                                      (149 (astore 4)) 
                                      (151 (aload_2)) 
                                      (152 (aconst_null)) 
                                      (153 (astore_2)) 
                                      (154 (aload_3)) 
                                      (155 (aconst_null)) 
                                      (156 (astore_3)) 
                                      (157 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) 
                                      (162 (goto 217)) ;;to TAG_1;;at TAG_5
                                      (165 (pop)) 
                                      (166 (getstatic (fieldCP "const__3" "clojure.string$replace" (class "clojure.lang.Keyword")))) ;;at TAG_3
                                      (169 (dup)) 
                                      (170 (ifnull 215)) ;;to TAG_6
                                      (173 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (176 (if_acmpeq 216)) ;;to TAG_7
                                      (179 (new (class "java.lang.IllegalArgumentException"))) 
                                      (182 (dup)) 
                                      (183 (getstatic (fieldCP "const__4" "clojure.string$replace" (class "clojure.lang.Var")))) 
                                      (186 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (189 (checkcast (class "clojure.lang.IFn"))) 
                                      (192 (ldc 7)) ;;STRING:: "Invalid match arg: "
                                      (194 (aload_2)) 
                                      (195 (aconst_null)) 
                                      (196 (astore_2)) 
                                      (197 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (202 (checkcast (class "java.lang.String"))) 
                                      (205 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (208 (checkcast (class "java.lang.Throwable"))) 
                                      (211 (athrow)) 
                                      (212 (goto 217)) ;;to TAG_1
                                      (215 (pop)) ;;at TAG_6
                                      (216 (aconst_null)) ;;at TAG_7
                                      (217 (areturn)) ;;at TAG_1
                                      (endofcode 218))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *string$replace-class-table*
  (make-static-class-decls 
   *clojure.string$replace*))

(defconst *package-name-map* 
  ("clojure.string$replace" . "clojure"))
