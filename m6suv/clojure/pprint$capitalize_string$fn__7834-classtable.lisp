; pprint$capitalize_string$fn__7834-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:55 CDT 2014.
;

(defconst *clojure.pprint$capitalize_string$fn__7834*
 (make-class-def
      '(class "clojure.pprint$capitalize_string$fn__7834"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "empty?")
                        (STRING  "re-matcher")
                        (STRING  "\\W\\w")
                        (STRING  "re-find")
                        (STRING  "inc")
                        (STRING  "str")
                        (STRING  "subs")
                        (STRING  "nth"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__9" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 0) (code_length . 128)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "empty?"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.pprint$capitalize_string$fn__7834" (class "clojure.lang.Var"))))
                                      (13 (iconst_2))
                                      (14 (anewarray (class "java.lang.Object")))
                                      (17 (dup))
                                      (18 (iconst_0))
                                      (19 (aconst_null))
                                      (20 (aastore))
                                      (21 (dup))
                                      (22 (iconst_1))
                                      (23 (aconst_null))
                                      (24 (aastore))
                                      (25 (invokestatic
					(methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector"))))
                                      (28 (checkcast (class "clojure.lang.AFn")))
                                      (31 (putstatic (fieldCP "const__1" "clojure.pprint$capitalize_string$fn__7834" (class "clojure.lang.AFn"))))
                                      (34 (ldc 0))        ;;STRING:: "clojure.core"
                                      (36 (ldc 2))        ;;STRING:: "re-matcher"
                                      (38 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (41 (checkcast (class "clojure.lang.Var")))
                                      (44 (putstatic (fieldCP "const__2" "clojure.pprint$capitalize_string$fn__7834" (class "clojure.lang.Var"))))
                                      (47 (ldc 3))        ;;STRING:: "\\W\\w"
                                      (49 (invokestatic
					(methodCP "compile" "java.util.regex.Pattern" ((class "java.lang.String")) (class "java.util.regex.Pattern"))))
                                      (52 (putstatic (fieldCP "const__3" "clojure.pprint$capitalize_string$fn__7834" (class "java.lang.Object"))))
                                      (55 (ldc 0))        ;;STRING:: "clojure.core"
                                      (57 (ldc 4))        ;;STRING:: "re-find"
                                      (59 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (62 (checkcast (class "clojure.lang.Var")))
                                      (65 (putstatic (fieldCP "const__4" "clojure.pprint$capitalize_string$fn__7834" (class "clojure.lang.Var"))))
                                      (68 (ldc 0))        ;;STRING:: "clojure.core"
                                      (70 (ldc 5))        ;;STRING:: "inc"
                                      (72 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (75 (checkcast (class "clojure.lang.Var")))
                                      (78 (putstatic (fieldCP "const__5" "clojure.pprint$capitalize_string$fn__7834" (class "clojure.lang.Var"))))
                                      (81 (ldc 0))        ;;STRING:: "clojure.core"
                                      (83 (ldc 6))        ;;STRING:: "str"
                                      (85 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (88 (checkcast (class "clojure.lang.Var")))
                                      (91 (putstatic (fieldCP "const__6" "clojure.pprint$capitalize_string$fn__7834" (class "clojure.lang.Var"))))
                                      (94 (ldc 0))        ;;STRING:: "clojure.core"
                                      (96 (ldc 7))        ;;STRING:: "subs"
                                      (98 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (101 (checkcast (class "clojure.lang.Var")))
                                      (104 (putstatic (fieldCP "const__7" "clojure.pprint$capitalize_string$fn__7834" (class "clojure.lang.Var"))))
                                      (107 (lconst_0))
                                      (108 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (111 (putstatic (fieldCP "const__8" "clojure.pprint$capitalize_string$fn__7834" (class "java.lang.Object"))))
                                      (114 (ldc 0))       ;;STRING:: "clojure.core"
                                      (116 (ldc 8))       ;;STRING:: "nth"
                                      (118 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (121 (checkcast (class "clojure.lang.Var")))
                                      (124 (putstatic (fieldCP "const__9" "clojure.pprint$capitalize_string$fn__7834" (class "clojure.lang.Var"))))
                                      (127 (return))
                                      (endofcode 128))
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
                                   (max_stack . 8) (max_locals . 5) (code_length . 241)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.pprint$capitalize_string$fn__7834" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (aload_1)) 
                                      (10 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (15 (dup)) 
                                      (16 (ifnull 31)) ;;to TAG_0
                                      (19 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (22 (if_acmpeq 32)) ;;to TAG_1
                                      (25 (getstatic (fieldCP "const__1" "clojure.pprint$capitalize_string$fn__7834" (class "clojure.lang.AFn")))) 
                                      (28 (goto 240))  ;;to TAG_2
                                      (31 (pop)) ;;at TAG_0
                                      (32 (getstatic (fieldCP "const__2" "clojure.pprint$capitalize_string$fn__7834" (class "clojure.lang.Var")))) ;;at TAG_1
                                      (35 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (38 (checkcast (class "clojure.lang.IFn"))) 
                                      (41 (getstatic (fieldCP "const__3" "clojure.pprint$capitalize_string$fn__7834" (class "java.lang.Object")))) 
                                      (44 (aload_1)) 
                                      (45 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (50 (astore_2)) 
                                      (51 (getstatic (fieldCP "const__4" "clojure.pprint$capitalize_string$fn__7834" (class "clojure.lang.Var")))) 
                                      (54 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (57 (checkcast (class "clojure.lang.IFn"))) 
                                      (60 (aload_2)) 
                                      (61 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (66 (astore_3)) 
                                      (67 (aload_3)) 
                                      (68 (aconst_null)) 
                                      (69 (astore_3)) 
                                      (70 (astore 4)) 
                                      (72 (aload 4)) 
                                      (74 (dup)) 
                                      (75 (ifnull 103)) ;;to TAG_3
                                      (78 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (81 (if_acmpeq 104)) ;;to TAG_4
                                      (84 (aload_2)) 
                                      (85 (aconst_null)) 
                                      (86 (astore_2)) 
                                      (87 (checkcast (class "java.util.regex.Matcher"))) 
                                      (90 (invokevirtual (methodCP "start" "java.util.regex.Matcher" () int))) 
                                      (93 (i2l)) 
                                      (94 (invokestatic (methodCP "inc" "clojure.lang.Numbers" (long) long))) 
                                      (97 (invokestatic (methodCP "num" "clojure.lang.Numbers" (long) (class "java.lang.Number")))) 
                                      (100 (goto 109)) ;;to TAG_5
                                      (103 (pop)) ;;at TAG_3
                                      (104 (aload 4)) ;;at TAG_4
                                      (106 (aconst_null)) 
                                      (107 (astore 4)) 
                                      (109 (astore 4)) ;;at TAG_5
                                      (111 (aload 4)) 
                                      (113 (dup)) 
                                      (114 (ifnull 222)) ;;to TAG_6
                                      (117 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (120 (if_acmpeq 223)) ;;to TAG_7
                                      (123 (iconst_2)) 
                                      (124 (anewarray (class "java.lang.Object"))) 
                                      (127 (dup)) 
                                      (128 (iconst_0)) 
                                      (129 (getstatic (fieldCP "const__6" "clojure.pprint$capitalize_string$fn__7834" (class "clojure.lang.Var")))) 
                                      (132 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (135 (checkcast (class "clojure.lang.IFn"))) 
                                      (138 (getstatic (fieldCP "const__7" "clojure.pprint$capitalize_string$fn__7834" (class "clojure.lang.Var")))) 
                                      (141 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (144 (checkcast (class "clojure.lang.IFn"))) 
                                      (147 (aload_1)) 
                                      (148 (getstatic (fieldCP "const__8" "clojure.pprint$capitalize_string$fn__7834" (class "java.lang.Object")))) 
                                      (151 (aload 4)) 
                                      (153 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) 
                                      (158 (aload_1)) 
                                      (159 (aload 4)) 
                                      (161 (checkcast (class "java.lang.Number"))) 
                                      (164 (invokestatic (methodCP "intCast" "clojure.lang.RT" ((class "java.lang.Object")) int))) 
                                      (167 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int) (class "java.lang.Object")))) 
                                      (170 (checkcast (class "java.lang.Character"))) 
                                      (173 (invokevirtual (methodCP "charValue" "java.lang.Character" () char))) 
                                      (176 (invokestatic (methodCP "toUpperCase" "java.lang.Character" (char) char))) 
                                      (179 (invokestatic (methodCP "valueOf" "java.lang.Character" (char) (class "java.lang.Character")))) 
                                      (182 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (187 (aastore)) 
                                      (188 (dup)) 
                                      (189 (iconst_1)) 
                                      (190 (getstatic (fieldCP "const__7" "clojure.pprint$capitalize_string$fn__7834" (class "clojure.lang.Var")))) 
                                      (193 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (196 (checkcast (class "clojure.lang.IFn"))) 
                                      (199 (aload_1)) 
                                      (200 (aconst_null)) 
                                      (201 (astore_1)) 
                                      (202 (aload 4)) 
                                      (204 (aconst_null)) 
                                      (205 (astore 4)) 
                                      (207 (invokestatic (methodCP "inc" "clojure.lang.Numbers" ((class "java.lang.Object")) (class "java.lang.Number")))) 
                                      (210 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (215 (aastore)) 
                                      (216 (invokestatic (methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector")))) 
                                      (219 (goto 240))  ;;to TAG_2
                                      (222 (pop)) ;;at TAG_6
                                      (223 (iconst_2)) ;;at TAG_7
                                      (224 (anewarray (class "java.lang.Object"))) 
                                      (227 (dup)) 
                                      (228 (iconst_0)) 
                                      (229 (aload_1)) 
                                      (230 (aconst_null)) 
                                      (231 (astore_1)) 
                                      (232 (aastore)) 
                                      (233 (dup)) 
                                      (234 (iconst_1)) 
                                      (235 (aconst_null)) 
                                      (236 (aastore)) 
                                      (237 (invokestatic (methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector")))) 
                                      (240 (areturn)) ;;at TAG_2
                                      (endofcode 241))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *pprint$capitalize_string$fn__7834-class-table*
  (make-static-class-decls 
   *clojure.pprint$capitalize_string$fn__7834*))

(defconst *package-name-map* 
  ("clojure.pprint$capitalize_string$fn__7834" . "clojure"))
