; core$partition$fn__4312-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:45 CDT 2014.
;

(defconst *clojure.core$partition$fn__4312*
 (make-class-def
      '(class "clojure.core$partition$fn__4312"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "seq")
                        (STRING  "doall")
                        (STRING  "take")
                        (STRING  "=")
                        (STRING  "count")
                        (STRING  "cons")
                        (STRING  "partition")
                        (STRING  "nthrest")
                        (STRING  "list")
                        (STRING  "concat"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__9" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "coll" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "n" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "pad" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "step" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 131)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "seq"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$partition$fn__4312" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "doall"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$partition$fn__4312" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "take"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.core$partition$fn__4312" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 4))        ;;STRING:: "="
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.core$partition$fn__4312" (class "clojure.lang.Var"))))
                                      (52 (ldc 0))        ;;STRING:: "clojure.core"
                                      (54 (ldc 5))        ;;STRING:: "count"
                                      (56 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (59 (checkcast (class "clojure.lang.Var")))
                                      (62 (putstatic (fieldCP "const__4" "clojure.core$partition$fn__4312" (class "clojure.lang.Var"))))
                                      (65 (ldc 0))        ;;STRING:: "clojure.core"
                                      (67 (ldc 6))        ;;STRING:: "cons"
                                      (69 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (72 (checkcast (class "clojure.lang.Var")))
                                      (75 (putstatic (fieldCP "const__5" "clojure.core$partition$fn__4312" (class "clojure.lang.Var"))))
                                      (78 (ldc 0))        ;;STRING:: "clojure.core"
                                      (80 (ldc 7))        ;;STRING:: "partition"
                                      (82 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (85 (checkcast (class "clojure.lang.Var")))
                                      (88 (putstatic (fieldCP "const__6" "clojure.core$partition$fn__4312" (class "clojure.lang.Var"))))
                                      (91 (ldc 0))        ;;STRING:: "clojure.core"
                                      (93 (ldc 8))        ;;STRING:: "nthrest"
                                      (95 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (98 (checkcast (class "clojure.lang.Var")))
                                      (101 (putstatic (fieldCP "const__7" "clojure.core$partition$fn__4312" (class "clojure.lang.Var"))))
                                      (104 (ldc 0))       ;;STRING:: "clojure.core"
                                      (106 (ldc 9))       ;;STRING:: "list"
                                      (108 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (111 (checkcast (class "clojure.lang.Var")))
                                      (114 (putstatic (fieldCP "const__8" "clojure.core$partition$fn__4312" (class "clojure.lang.Var"))))
                                      (117 (ldc 0))       ;;STRING:: "clojure.core"
                                      (119 (ldc 10))      ;;STRING:: "concat"
                                      (121 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (124 (checkcast (class "clojure.lang.Var")))
                                      (127 (putstatic (fieldCP "const__9" "clojure.core$partition$fn__4312" (class "clojure.lang.Var"))))
                                      (130 (return))
                                      (endofcode 131))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 5) (code_length . 26)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.AFunction" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "coll" "clojure.core$partition$fn__4312" (class "java.lang.Object"))))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "n" "clojure.core$partition$fn__4312" (class "java.lang.Object"))))
                                      (14 (aload_0))
                                      (15 (aload_3))
                                      (16 (putfield (fieldCP "pad" "clojure.core$partition$fn__4312" (class "java.lang.Object"))))
                                      (19 (aload_0))
                                      (20 (aload 4))
                                      (22 (putfield (fieldCP "step" "clojure.core$partition$fn__4312" (class "java.lang.Object"))))
                                      (25 (return))
                                      (endofcode 26))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 11) (max_locals . 4) (code_length . 240)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$partition$fn__4312" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (aload_0)) 
                                      (10 (getfield (fieldCP "coll" "clojure.core$partition$fn__4312" (class "java.lang.Object")))) 
                                      (13 (aload_0)) 
                                      (14 (aconst_null)) 
                                      (15 (putfield (fieldCP "coll" "clojure.core$partition$fn__4312" (class "java.lang.Object")))) 
                                      (18 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (23 (astore_1)) 
                                      (24 (aload_1)) 
                                      (25 (dup)) 
                                      (26 (ifnull 237)) ;;to TAG_0
                                      (29 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (32 (if_acmpeq 238)) ;;to TAG_1
                                      (35 (aload_1)) 
                                      (36 (aconst_null)) 
                                      (37 (astore_1)) 
                                      (38 (astore_2)) 
                                      (39 (getstatic (fieldCP "const__1" "clojure.core$partition$fn__4312" (class "clojure.lang.Var")))) 
                                      (42 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (45 (checkcast (class "clojure.lang.IFn"))) 
                                      (48 (getstatic (fieldCP "const__2" "clojure.core$partition$fn__4312" (class "clojure.lang.Var")))) 
                                      (51 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (54 (checkcast (class "clojure.lang.IFn"))) 
                                      (57 (aload_0)) 
                                      (58 (getfield (fieldCP "n" "clojure.core$partition$fn__4312" (class "java.lang.Object")))) 
                                      (61 (aload_2)) 
                                      (62 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (67 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (72 (astore_3)) 
                                      (73 (aload_0)) 
                                      (74 (getfield (fieldCP "n" "clojure.core$partition$fn__4312" (class "java.lang.Object")))) 
                                      (77 (aload_3)) 
                                      (78 (invokestatic (methodCP "count" "clojure.lang.RT" ((class "java.lang.Object")) int))) 
                                      (81 (i2l)) 
                                      (82 (invokestatic (methodCP "equiv" "clojure.lang.Util" ((class "java.lang.Object") long) boolean))) 
                                      (85 (ifeq 171))  ;;to TAG_2
                                      (88 (getstatic (fieldCP "const__5" "clojure.core$partition$fn__4312" (class "clojure.lang.Var")))) 
                                      (91 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (94 (checkcast (class "clojure.lang.IFn"))) 
                                      (97 (aload_3)) 
                                      (98 (aconst_null)) 
                                      (99 (astore_3)) 
                                      (100 (getstatic (fieldCP "const__6" "clojure.core$partition$fn__4312" (class "clojure.lang.Var")))) 
                                      (103 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (106 (checkcast (class "clojure.lang.IFn"))) 
                                      (109 (aload_0)) 
                                      (110 (getfield (fieldCP "n" "clojure.core$partition$fn__4312" (class "java.lang.Object")))) 
                                      (113 (aload_0)) 
                                      (114 (aconst_null)) 
                                      (115 (putfield (fieldCP "n" "clojure.core$partition$fn__4312" (class "java.lang.Object")))) 
                                      (118 (aload_0)) 
                                      (119 (getfield (fieldCP "step" "clojure.core$partition$fn__4312" (class "java.lang.Object")))) 
                                      (122 (aload_0)) 
                                      (123 (getfield (fieldCP "pad" "clojure.core$partition$fn__4312" (class "java.lang.Object")))) 
                                      (126 (aload_0)) 
                                      (127 (aconst_null)) 
                                      (128 (putfield (fieldCP "pad" "clojure.core$partition$fn__4312" (class "java.lang.Object")))) 
                                      (131 (getstatic (fieldCP "const__7" "clojure.core$partition$fn__4312" (class "clojure.lang.Var")))) 
                                      (134 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (137 (checkcast (class "clojure.lang.IFn"))) 
                                      (140 (aload_2)) 
                                      (141 (aconst_null)) 
                                      (142 (astore_2)) 
                                      (143 (aload_0)) 
                                      (144 (getfield (fieldCP "step" "clojure.core$partition$fn__4312" (class "java.lang.Object")))) 
                                      (147 (aload_0)) 
                                      (148 (aconst_null)) 
                                      (149 (putfield (fieldCP "step" "clojure.core$partition$fn__4312" (class "java.lang.Object")))) 
                                      (152 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (157 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 5)) 
                                      (162 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (167 (goto 234)) ;;to TAG_3
                                      (170 (pop)) 
                                      (171 (getstatic (fieldCP "const__8" "clojure.core$partition$fn__4312" (class "clojure.lang.Var")))) ;;at TAG_2
                                      (174 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (177 (checkcast (class "clojure.lang.IFn"))) 
                                      (180 (getstatic (fieldCP "const__2" "clojure.core$partition$fn__4312" (class "clojure.lang.Var")))) 
                                      (183 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (186 (checkcast (class "clojure.lang.IFn"))) 
                                      (189 (aload_0)) 
                                      (190 (getfield (fieldCP "n" "clojure.core$partition$fn__4312" (class "java.lang.Object")))) 
                                      (193 (aload_0)) 
                                      (194 (aconst_null)) 
                                      (195 (putfield (fieldCP "n" "clojure.core$partition$fn__4312" (class "java.lang.Object")))) 
                                      (198 (getstatic (fieldCP "const__9" "clojure.core$partition$fn__4312" (class "clojure.lang.Var")))) 
                                      (201 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (204 (checkcast (class "clojure.lang.IFn"))) 
                                      (207 (aload_3)) 
                                      (208 (aconst_null)) 
                                      (209 (astore_3)) 
                                      (210 (aload_0)) 
                                      (211 (getfield (fieldCP "pad" "clojure.core$partition$fn__4312" (class "java.lang.Object")))) 
                                      (214 (aload_0)) 
                                      (215 (aconst_null)) 
                                      (216 (putfield (fieldCP "pad" "clojure.core$partition$fn__4312" (class "java.lang.Object")))) 
                                      (219 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (224 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (229 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (234 (goto 239)) ;;to TAG_4;;at TAG_3
                                      (237 (pop)) ;;at TAG_0
                                      (238 (aconst_null)) ;;at TAG_1
                                      (239 (areturn)) ;;at TAG_4
                                      (endofcode 240))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$partition$fn__4312-class-table*
  (make-static-class-decls 
   *clojure.core$partition$fn__4312*))

(defconst *package-name-map* 
  ("clojure.core$partition$fn__4312" . "clojure"))

