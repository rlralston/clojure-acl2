; pprint$capitalize_word_writer$fn__7840-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:55 CDT 2014.
;

(defconst *clojure.pprint$capitalize_word_writer$fn__7840*
 (make-class-def
      '(class "clojure.pprint$capitalize_word_writer$fn__7840"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "=")
                        (STRING  "class")
                        (STRING  "java.lang.String")
                        (STRING  "clojure.pprint")
                        (STRING  "capitalize-string")
                        (STRING  "deref")
                        (STRING  "pos?")
                        (STRING  "java.lang.Integer")
                        (STRING  "char")
                        (STRING  "int")
                        (STRING  "str")
                        (STRING  "No matching clause: "))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__9" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "writer" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "last_was_whitespace_QMARK_" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 121)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "="
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.pprint$capitalize_word_writer$fn__7840" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "class"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.pprint$capitalize_word_writer$fn__7840" (class "clojure.lang.Var"))))
                                      (26 (ldc 3))        ;;STRING:: "java.lang.String"
                                      (28 (invokestatic
					(methodCP "forName" "java.lang.Class" ((class "java.lang.String")) (class "java.lang.Class"))))
                                      (31 (putstatic (fieldCP "const__2" "clojure.pprint$capitalize_word_writer$fn__7840" (class "java.lang.Object"))))
                                      (34 (ldc 4))        ;;STRING:: "clojure.pprint"
                                      (36 (ldc 5))        ;;STRING:: "capitalize-string"
                                      (38 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (41 (checkcast (class "clojure.lang.Var")))
                                      (44 (putstatic (fieldCP "const__3" "clojure.pprint$capitalize_word_writer$fn__7840" (class "clojure.lang.Var"))))
                                      (47 (ldc 0))        ;;STRING:: "clojure.core"
                                      (49 (ldc 6))        ;;STRING:: "deref"
                                      (51 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (54 (checkcast (class "clojure.lang.Var")))
                                      (57 (putstatic (fieldCP "const__4" "clojure.pprint$capitalize_word_writer$fn__7840" (class "clojure.lang.Var"))))
                                      (60 (ldc 0))        ;;STRING:: "clojure.core"
                                      (62 (ldc 7))        ;;STRING:: "pos?"
                                      (64 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (67 (checkcast (class "clojure.lang.Var")))
                                      (70 (putstatic (fieldCP "const__5" "clojure.pprint$capitalize_word_writer$fn__7840" (class "clojure.lang.Var"))))
                                      (73 (ldc 8))        ;;STRING:: "java.lang.Integer"
                                      (75 (invokestatic
					(methodCP "forName" "java.lang.Class" ((class "java.lang.String")) (class "java.lang.Class"))))
                                      (78 (putstatic (fieldCP "const__6" "clojure.pprint$capitalize_word_writer$fn__7840" (class "java.lang.Object"))))
                                      (81 (ldc 0))        ;;STRING:: "clojure.core"
                                      (83 (ldc 9))        ;;STRING:: "char"
                                      (85 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (88 (checkcast (class "clojure.lang.Var")))
                                      (91 (putstatic (fieldCP "const__7" "clojure.pprint$capitalize_word_writer$fn__7840" (class "clojure.lang.Var"))))
                                      (94 (ldc 0))        ;;STRING:: "clojure.core"
                                      (96 (ldc 10))       ;;STRING:: "int"
                                      (98 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (101 (checkcast (class "clojure.lang.Var")))
                                      (104 (putstatic (fieldCP "const__8" "clojure.pprint$capitalize_word_writer$fn__7840" (class "clojure.lang.Var"))))
                                      (107 (ldc 0))       ;;STRING:: "clojure.core"
                                      (109 (ldc 11))      ;;STRING:: "str"
                                      (111 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (114 (checkcast (class "clojure.lang.Var")))
                                      (117 (putstatic (fieldCP "const__9" "clojure.pprint$capitalize_word_writer$fn__7840" (class "clojure.lang.Var"))))
                                      (120 (return))
                                      (endofcode 121))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.AFunction" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "writer" "clojure.pprint$capitalize_word_writer$fn__7840" (class "java.lang.Object"))))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "last_was_whitespace_QMARK_" "clojure.pprint$capitalize_word_writer$fn__7840" (class "java.lang.Object"))))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 5) (code_length . 38)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "writer" "clojure.pprint$capitalize_word_writer$fn__7840" (class "java.lang.Object"))))
                                      (4 (checkcast (class "java.io.Writer")))
                                      (7 (aload_2))
                                      (8 (aconst_null))
                                      (9 (astore_2))
                                      (10 (checkcast (array char)))
                                      (13 (aload_3))
                                      (14 (aconst_null))
                                      (15 (astore_3))
                                      (16 (checkcast (class "java.lang.Number")))
                                      (19 (invokestatic
					(methodCP "intCast" "clojure.lang.RT" ((class "java.lang.Object")) int)))
                                      (22 (aload 4))
                                      (24 (aconst_null))
                                      (25 (astore 4))
                                      (27 (checkcast (class "java.lang.Number")))
                                      (30 (invokestatic
					(methodCP "intCast" "clojure.lang.RT" ((class "java.lang.Object")) int)))
                                      (33 (invokevirtual
					(methodCP "write" "java.io.Writer" ((array char) int int) void)))
                                      (36 (aconst_null))
                                      (37 (areturn))
                                      (endofcode 38))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 7) (code_length . 305)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.pprint$capitalize_word_writer$fn__7840" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (astore_3)) 
                                      (7 (getstatic (fieldCP "const__1" "clojure.pprint$capitalize_word_writer$fn__7840" (class "clojure.lang.Var")))) 
                                      (10 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (13 (checkcast (class "clojure.lang.IFn"))) 
                                      (16 (aload_2)) 
                                      (17 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (22 (astore 4)) 
                                      (24 (aload_3)) 
                                      (25 (checkcast (class "clojure.lang.IFn"))) 
                                      (28 (getstatic (fieldCP "const__2" "clojure.pprint$capitalize_word_writer$fn__7840" (class "java.lang.Object")))) 
                                      (31 (aload 4)) 
                                      (33 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (38 (dup)) 
                                      (39 (ifnull 152)) ;;to TAG_0
                                      (42 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (45 (if_acmpeq 153))  ;;to TAG_1
                                      (48 (aload_2)) 
                                      (49 (aconst_null)) 
                                      (50 (astore_2)) 
                                      (51 (astore 5)) 
                                      (53 (aload_0)) 
                                      (54 (getfield (fieldCP "writer" "clojure.pprint$capitalize_word_writer$fn__7840" (class "java.lang.Object")))) 
                                      (57 (checkcast (class "java.io.Writer"))) 
                                      (60 (getstatic (fieldCP "const__3" "clojure.pprint$capitalize_word_writer$fn__7840" (class "clojure.lang.Var")))) 
                                      (63 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (66 (checkcast (class "clojure.lang.IFn"))) 
                                      (69 (aload 5)) 
                                      (71 (checkcast (class "java.lang.String"))) 
                                      (74 (invokevirtual (methodCP "toLowerCase" "java.lang.String" () (class "java.lang.String")))) 
                                      (77 (getstatic (fieldCP "const__4" "clojure.pprint$capitalize_word_writer$fn__7840" (class "clojure.lang.Var")))) 
                                      (80 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (83 (checkcast (class "clojure.lang.IFn"))) 
                                      (86 (aload_0)) 
                                      (87 (getfield (fieldCP "last_was_whitespace_QMARK_" "clojure.pprint$capitalize_word_writer$fn__7840" (class "java.lang.Object")))) 
                                      (90 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (95 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (100 (checkcast (class "java.lang.String"))) 
                                      (103 (invokevirtual (methodCP "write" "java.io.Writer" ((class "java.lang.String")) void))) 
                                      (106 (aconst_null)) 
                                      (107 (pop)) 
                                      (108 (aload 5)) 
                                      (110 (checkcast (class "java.lang.String"))) 
                                      (113 (invokevirtual (methodCP "length" "java.lang.String" () int))) 
                                      (116 (i2l)) 
                                      (117 (lconst_0)) 
                                      (118 (lcmp)) 
                                      (119 (ifle 148)) ;;to TAG_2
                                      (122 (new (class "clojure.pprint$capitalize_word_writer$fn__7840$fn__7844"))) 
                                      (125 (dup)) 
                                      (126 (aload_0)) 
                                      (127 (getfield (fieldCP "last_was_whitespace_QMARK_" "clojure.pprint$capitalize_word_writer$fn__7840" (class "java.lang.Object")))) 
                                      (130 (aload 5)) 
                                      (132 (aconst_null)) 
                                      (133 (astore 5)) 
                                      (135 (invokespecial (methodCP "<init>" "clojure.pprint$capitalize_word_writer$fn__7840$fn__7844" ((class "java.lang.Object") (class "java.lang.Object")) void))) 
                                      (138 (checkcast (class "java.util.concurrent.Callable"))) 
                                      (141 (invokestatic (methodCP "runInTransaction" "clojure.lang.LockingTransaction" ((class "java.util.concurrent.Callable")) (class "java.lang.Object")))) 
                                      (144 (goto 149)) ;;to TAG_3
                                      (147 (pop)) 
                                      (148 (aconst_null)) ;;at TAG_2
                                      (149 (goto 304)) ;;to TAG_4;;at TAG_3
                                      (152 (pop)) ;;at TAG_0
                                      (153 (aload_3)) ;;at TAG_1
                                      (154 (aconst_null)) 
                                      (155 (astore_3)) 
                                      (156 (checkcast (class "clojure.lang.IFn"))) 
                                      (159 (getstatic (fieldCP "const__6" "clojure.pprint$capitalize_word_writer$fn__7840" (class "java.lang.Object")))) 
                                      (162 (aload 4)) 
                                      (164 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (169 (dup)) 
                                      (170 (ifnull 268)) ;;to TAG_5
                                      (173 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (176 (if_acmpeq 269)) ;;to TAG_6
                                      (179 (aload_2)) 
                                      (180 (invokestatic (methodCP "charCast" "clojure.lang.RT" ((class "java.lang.Object")) char))) 
                                      (183 (istore 5)) 
                                      (185 (getstatic (fieldCP "const__4" "clojure.pprint$capitalize_word_writer$fn__7840" (class "clojure.lang.Var")))) 
                                      (188 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (191 (checkcast (class "clojure.lang.IFn"))) 
                                      (194 (aload_0)) 
                                      (195 (getfield (fieldCP "last_was_whitespace_QMARK_" "clojure.pprint$capitalize_word_writer$fn__7840" (class "java.lang.Object")))) 
                                      (198 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (203 (dup)) 
                                      (204 (ifnull 223)) ;;to TAG_7
                                      (207 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (210 (if_acmpeq 224)) ;;to TAG_8
                                      (213 (aload_2)) 
                                      (214 (invokestatic (methodCP "charCast" "clojure.lang.RT" ((class "java.lang.Object")) char))) 
                                      (217 (invokestatic (methodCP "toUpperCase" "java.lang.Character" (char) char))) 
                                      (220 (goto 226)) ;;to TAG_9
                                      (223 (pop)) ;;at TAG_7
                                      (224 (iload 5)) ;;at TAG_8
                                      (226 (istore 6)) ;;at TAG_9
                                      (228 (aload_0)) 
                                      (229 (getfield (fieldCP "writer" "clojure.pprint$capitalize_word_writer$fn__7840" (class "java.lang.Object")))) 
                                      (232 (checkcast (class "java.io.Writer"))) 
                                      (235 (iload 6)) 
                                      (237 (invokestatic (methodCP "intCast" "clojure.lang.RT" (char) int))) 
                                      (240 (invokevirtual (methodCP "write" "java.io.Writer" (int) void))) 
                                      (243 (aconst_null)) 
                                      (244 (pop)) 
                                      (245 (new (class "clojure.pprint$capitalize_word_writer$fn__7840$fn__7846"))) 
                                      (248 (dup)) 
                                      (249 (aload_0)) 
                                      (250 (getfield (fieldCP "last_was_whitespace_QMARK_" "clojure.pprint$capitalize_word_writer$fn__7840" (class "java.lang.Object")))) 
                                      (253 (aload_2)) 
                                      (254 (aconst_null)) 
                                      (255 (astore_2)) 
                                      (256 (invokespecial (methodCP "<init>" "clojure.pprint$capitalize_word_writer$fn__7840$fn__7846" ((class "java.lang.Object") (class "java.lang.Object")) void))) 
                                      (259 (checkcast (class "java.util.concurrent.Callable"))) 
                                      (262 (invokestatic (methodCP "runInTransaction" "clojure.lang.LockingTransaction" ((class "java.util.concurrent.Callable")) (class "java.lang.Object")))) 
                                      (265 (goto 304)) ;;to TAG_4
                                      (268 (pop)) ;;at TAG_5
                                      (269 (new (class "java.lang.IllegalArgumentException"))) ;;at TAG_6
                                      (272 (dup)) 
                                      (273 (getstatic (fieldCP "const__9" "clojure.pprint$capitalize_word_writer$fn__7840" (class "clojure.lang.Var")))) 
                                      (276 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (279 (checkcast (class "clojure.lang.IFn"))) 
                                      (282 (ldc 12)) ;;STRING:: "No matching clause: "
                                      (284 (aload 4)) 
                                      (286 (aconst_null)) 
                                      (287 (astore 4)) 
                                      (289 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (294 (checkcast (class "java.lang.String"))) 
                                      (297 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (300 (checkcast (class "java.lang.Throwable"))) 
                                      (303 (athrow)) 
                                      (304 (areturn)) ;;at TAG_4
                                      (endofcode 305))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *pprint$capitalize_word_writer$fn__7840-class-table*
  (make-static-class-decls 
   *clojure.pprint$capitalize_word_writer$fn__7840*))

(defconst *package-name-map* 
  ("clojure.pprint$capitalize_word_writer$fn__7840" . "clojure"))

