; core$distinct_QMARK_-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:41 CDT 2014.
;

(defconst *clojure.core$distinct_QMARK_*
 (make-class-def
      '(class "clojure.core$distinct_QMARK_"
            "clojure.lang.RestFn"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "not")
                        (STRING  "=")
                        (STRING  "not=")
                        (STRING  "nth")
                        (STRING  "nthnext")
                        (STRING  "contains?")
                        (STRING  "conj"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 106)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "not"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$distinct_QMARK_" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "="
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$distinct_QMARK_" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "not="
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.core$distinct_QMARK_" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 4))        ;;STRING:: "nth"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.core$distinct_QMARK_" (class "clojure.lang.Var"))))
                                      (52 (lconst_0))
                                      (53 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (56 (putstatic (fieldCP "const__4" "clojure.core$distinct_QMARK_" (class "java.lang.Object"))))
                                      (59 (ldc 0))        ;;STRING:: "clojure.core"
                                      (61 (ldc 5))        ;;STRING:: "nthnext"
                                      (63 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (66 (checkcast (class "clojure.lang.Var")))
                                      (69 (putstatic (fieldCP "const__5" "clojure.core$distinct_QMARK_" (class "clojure.lang.Var"))))
                                      (72 (lconst_1))
                                      (73 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (76 (putstatic (fieldCP "const__6" "clojure.core$distinct_QMARK_" (class "java.lang.Object"))))
                                      (79 (ldc 0))        ;;STRING:: "clojure.core"
                                      (81 (ldc 6))        ;;STRING:: "contains?"
                                      (83 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (86 (checkcast (class "clojure.lang.Var")))
                                      (89 (putstatic (fieldCP "const__7" "clojure.core$distinct_QMARK_" (class "clojure.lang.Var"))))
                                      (92 (ldc 0))        ;;STRING:: "clojure.core"
                                      (94 (ldc 7))        ;;STRING:: "conj"
                                      (96 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (99 (checkcast (class "clojure.lang.Var")))
                                      (102 (putstatic (fieldCP "const__8" "clojure.core$distinct_QMARK_" (class "clojure.lang.Var"))))
                                      (105 (return))
                                      (endofcode 106))
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
					(methodCP "<init>" "clojure.lang.RestFn" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "doInvoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 17) (code_length . 259)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__2" "clojure.core$distinct_QMARK_" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (aload_1)) 
                                      (10 (aload_2)) 
                                      (11 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (16 (dup)) 
                                      (17 (ifnull 254)) ;;to TAG_0
                                      (20 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (23 (if_acmpeq 255))  ;;to TAG_1
                                      (26 (iconst_2)) 
                                      (27 (anewarray (class "java.lang.Object"))) 
                                      (30 (dup)) 
                                      (31 (iconst_0)) 
                                      (32 (aload_2)) 
                                      (33 (aconst_null)) 
                                      (34 (astore_2)) 
                                      (35 (aastore)) 
                                      (36 (dup)) 
                                      (37 (iconst_1)) 
                                      (38 (aload_1)) 
                                      (39 (aconst_null)) 
                                      (40 (astore_1)) 
                                      (41 (aastore)) 
                                      (42 (invokestatic (methodCP "set" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentSet")))) 
                                      (45 (astore 4)) 
                                      (47 (aload_3)) 
                                      (48 (aconst_null)) 
                                      (49 (astore_3)) 
                                      (50 (astore 5)) 
                                      (52 (aload 5)) 
                                      (54 (astore 6)) 
                                      (56 (aload 6)) 
                                      (58 (lconst_0)) 
                                      (59 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (62 (aconst_null)) 
                                      (63 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (66 (astore 7)) 
                                      (68 (getstatic (fieldCP "const__5" "clojure.core$distinct_QMARK_" (class "clojure.lang.Var")))) 
                                      (71 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (74 (checkcast (class "clojure.lang.IFn"))) 
                                      (77 (aload 6)) 
                                      (79 (getstatic (fieldCP "const__6" "clojure.core$distinct_QMARK_" (class "java.lang.Object")))) 
                                      (82 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (87 (astore 8)) 
                                      (89 (aload 6)) 
                                      (91 (aconst_null)) 
                                      (92 (astore 6)) 
                                      (94 (astore 9)) 
                                      (96 (aload 4)) 
                                      (98 (aconst_null)) 
                                      (99 (astore 4)) 
                                      (101 (astore 10)) 
                                      (103 (aload 5)) 
                                      (105 (aconst_null)) 
                                      (106 (astore 5)) 
                                      (108 (astore 11)) 
                                      (110 (aload 10)) ;;at TAG_7
                                      (112 (astore 12)) 
                                      (114 (aload 11)) 
                                      (116 (astore 13)) 
                                      (118 (aload 13)) 
                                      (120 (lconst_0)) 
                                      (121 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (124 (aconst_null)) 
                                      (125 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (128 (astore 14)) 
                                      (130 (getstatic (fieldCP "const__5" "clojure.core$distinct_QMARK_" (class "clojure.lang.Var")))) 
                                      (133 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (136 (checkcast (class "clojure.lang.IFn"))) 
                                      (139 (aload 13)) 
                                      (141 (getstatic (fieldCP "const__6" "clojure.core$distinct_QMARK_" (class "java.lang.Object")))) 
                                      (144 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (149 (astore 15)) 
                                      (151 (aload 13)) 
                                      (153 (aconst_null)) 
                                      (154 (astore 13)) 
                                      (156 (astore 16)) 
                                      (158 (aload 16)) 
                                      (160 (aconst_null)) 
                                      (161 (astore 16)) 
                                      (163 (dup)) 
                                      (164 (ifnull 247)) ;;to TAG_2
                                      (167 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (170 (if_acmpeq 248)) ;;to TAG_3
                                      (173 (getstatic (fieldCP "const__7" "clojure.core$distinct_QMARK_" (class "clojure.lang.Var")))) 
                                      (176 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (179 (checkcast (class "clojure.lang.IFn"))) 
                                      (182 (aload 12)) 
                                      (184 (aload 14)) 
                                      (186 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (191 (dup)) 
                                      (192 (ifnull 207)) ;;to TAG_4
                                      (195 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (198 (if_acmpeq 208)) ;;to TAG_5
                                      (201 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (204 (goto 244)) ;;to TAG_6
                                      (207 (pop)) ;;at TAG_4
                                      (208 (getstatic (fieldCP "const__8" "clojure.core$distinct_QMARK_" (class "clojure.lang.Var")))) ;;at TAG_5
                                      (211 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (214 (checkcast (class "clojure.lang.IFn"))) 
                                      (217 (aload 12)) 
                                      (219 (aconst_null)) 
                                      (220 (astore 12)) 
                                      (222 (aload 14)) 
                                      (224 (aconst_null)) 
                                      (225 (astore 14)) 
                                      (227 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (232 (aload 15)) 
                                      (234 (aconst_null)) 
                                      (235 (astore 15)) 
                                      (237 (astore 11)) 
                                      (239 (astore 10)) 
                                      (241 (goto 110)) ;;to TAG_7
                                      (244 (goto 251)) ;;to TAG_8;;at TAG_6
                                      (247 (pop)) ;;at TAG_2
                                      (248 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) ;;at TAG_3
                                      (251 (goto 258)) ;;to TAG_9;;at TAG_8
                                      (254 (pop)) ;;at TAG_0
                                      (255 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) ;;at TAG_1
                                      (258 (areturn)) ;;at TAG_9
                                      (endofcode 259))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 36)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$distinct_QMARK_" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (aload_1)) 
                                      (10 (aconst_null)) 
                                      (11 (astore_1)) 
                                      (12 (aload_2)) 
                                      (13 (aconst_null)) 
                                      (14 (astore_2)) 
                                      (15 (invokestatic (methodCP "equiv" "clojure.lang.Util" ((class "java.lang.Object") (class "java.lang.Object")) boolean))) 
                                      (18 (ifeq 27))  ;;to TAG_0
                                      (21 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (24 (goto 30)) ;;to TAG_1
                                      (27 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) ;;at TAG_0
                                      (30 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) ;;at TAG_1
                                      (35 (areturn)) 
                                      (endofcode 36))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 4)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean"))))
                                      (3 (areturn))
                                      (endofcode 4))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getRequiredArity"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 2)
                                   (parsedcode
                                      (0 (iconst_2))
                                      (1 (ireturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$distinct_QMARK_-class-table*
  (make-static-class-decls 
   *clojure.core$distinct_QMARK_*))

(defconst *package-name-map* 
  ("clojure.core$distinct_QMARK_" . "clojure"))

