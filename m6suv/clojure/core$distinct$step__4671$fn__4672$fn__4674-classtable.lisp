; core$distinct$step__4671$fn__4672$fn__4674-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:41 CDT 2014.
;

(defconst *clojure.core$distinct$step__4671$fn__4672$fn__4674*
 (make-class-def
      '(class "clojure.core$distinct$step__4671$fn__4672$fn__4674"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "nth")
                        (STRING  "seq")
                        (STRING  "contains?")
                        (STRING  "rest")
                        (STRING  "cons")
                        (STRING  "conj"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "step" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 86)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "nth"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$distinct$step__4671$fn__4672$fn__4674" (class "clojure.lang.Var"))))
                                      (13 (lconst_0))
                                      (14 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (17 (putstatic (fieldCP "const__1" "clojure.core$distinct$step__4671$fn__4672$fn__4674" (class "java.lang.Object"))))
                                      (20 (ldc 0))        ;;STRING:: "clojure.core"
                                      (22 (ldc 2))        ;;STRING:: "seq"
                                      (24 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (27 (checkcast (class "clojure.lang.Var")))
                                      (30 (putstatic (fieldCP "const__2" "clojure.core$distinct$step__4671$fn__4672$fn__4674" (class "clojure.lang.Var"))))
                                      (33 (ldc 0))        ;;STRING:: "clojure.core"
                                      (35 (ldc 3))        ;;STRING:: "contains?"
                                      (37 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (40 (checkcast (class "clojure.lang.Var")))
                                      (43 (putstatic (fieldCP "const__3" "clojure.core$distinct$step__4671$fn__4672$fn__4674" (class "clojure.lang.Var"))))
                                      (46 (ldc 0))        ;;STRING:: "clojure.core"
                                      (48 (ldc 4))        ;;STRING:: "rest"
                                      (50 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (53 (checkcast (class "clojure.lang.Var")))
                                      (56 (putstatic (fieldCP "const__4" "clojure.core$distinct$step__4671$fn__4672$fn__4674" (class "clojure.lang.Var"))))
                                      (59 (ldc 0))        ;;STRING:: "clojure.core"
                                      (61 (ldc 5))        ;;STRING:: "cons"
                                      (63 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (66 (checkcast (class "clojure.lang.Var")))
                                      (69 (putstatic (fieldCP "const__5" "clojure.core$distinct$step__4671$fn__4672$fn__4674" (class "clojure.lang.Var"))))
                                      (72 (ldc 0))        ;;STRING:: "clojure.core"
                                      (74 (ldc 6))        ;;STRING:: "conj"
                                      (76 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (79 (checkcast (class "clojure.lang.Var")))
                                      (82 (putstatic (fieldCP "const__6" "clojure.core$distinct$step__4671$fn__4672$fn__4674" (class "clojure.lang.Var"))))
                                      (85 (return))
                                      (endofcode 86))
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
                                      (6 (putfield (fieldCP "step" "clojure.core$distinct$step__4671$fn__4672$fn__4674" (class "java.lang.Object"))))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 8) (max_locals . 8) (code_length . 193)
                                   (parsedcode
                                      (0 (aload_1)) ;;at TAG_4
                                      (1 (aconst_null)) 
                                      (2 (astore_1)) 
                                      (3 (astore_3)) 
                                      (4 (aload_3)) 
                                      (5 (lconst_0)) 
                                      (6 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (9 (aconst_null)) 
                                      (10 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (13 (astore 4)) 
                                      (15 (aload_3)) 
                                      (16 (aconst_null)) 
                                      (17 (astore_3)) 
                                      (18 (astore 5)) 
                                      (20 (getstatic (fieldCP "const__2" "clojure.core$distinct$step__4671$fn__4672$fn__4674" (class "clojure.lang.Var")))) 
                                      (23 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (26 (checkcast (class "clojure.lang.IFn"))) 
                                      (29 (aload 5)) 
                                      (31 (aconst_null)) 
                                      (32 (astore 5)) 
                                      (34 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (39 (astore 6)) 
                                      (41 (aload 6)) 
                                      (43 (dup)) 
                                      (44 (ifnull 190)) ;;to TAG_0
                                      (47 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (50 (if_acmpeq 191)) ;;to TAG_1
                                      (53 (aload 6)) 
                                      (55 (aconst_null)) 
                                      (56 (astore 6)) 
                                      (58 (astore 7)) 
                                      (60 (getstatic (fieldCP "const__3" "clojure.core$distinct$step__4671$fn__4672$fn__4674" (class "clojure.lang.Var")))) 
                                      (63 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (66 (checkcast (class "clojure.lang.IFn"))) 
                                      (69 (aload_2)) 
                                      (70 (aload 4)) 
                                      (72 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (77 (dup)) 
                                      (78 (ifnull 117))  ;;to TAG_2
                                      (81 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (84 (if_acmpeq 118)) ;;to TAG_3
                                      (87 (getstatic (fieldCP "const__4" "clojure.core$distinct$step__4671$fn__4672$fn__4674" (class "clojure.lang.Var")))) 
                                      (90 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (93 (checkcast (class "clojure.lang.IFn"))) 
                                      (96 (aload 7)) 
                                      (98 (aconst_null)) 
                                      (99 (astore 7)) 
                                      (101 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (106 (aload_2)) 
                                      (107 (aconst_null)) 
                                      (108 (astore_2)) 
                                      (109 (astore_2)) 
                                      (110 (astore_1)) 
                                      (111 (goto 0)) ;;to TAG_4
                                      (114 (goto 187)) ;;to TAG_5
                                      (117 (pop)) ;;at TAG_2
                                      (118 (getstatic (fieldCP "const__5" "clojure.core$distinct$step__4671$fn__4672$fn__4674" (class "clojure.lang.Var")))) ;;at TAG_3
                                      (121 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (124 (checkcast (class "clojure.lang.IFn"))) 
                                      (127 (aload 4)) 
                                      (129 (aload_0)) 
                                      (130 (getfield (fieldCP "step" "clojure.core$distinct$step__4671$fn__4672$fn__4674" (class "java.lang.Object")))) 
                                      (133 (checkcast (class "clojure.lang.IFn"))) 
                                      (136 (getstatic (fieldCP "const__4" "clojure.core$distinct$step__4671$fn__4672$fn__4674" (class "clojure.lang.Var")))) 
                                      (139 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (142 (checkcast (class "clojure.lang.IFn"))) 
                                      (145 (aload 7)) 
                                      (147 (aconst_null)) 
                                      (148 (astore 7)) 
                                      (150 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (155 (getstatic (fieldCP "const__6" "clojure.core$distinct$step__4671$fn__4672$fn__4674" (class "clojure.lang.Var")))) 
                                      (158 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (161 (checkcast (class "clojure.lang.IFn"))) 
                                      (164 (aload_2)) 
                                      (165 (aconst_null)) 
                                      (166 (astore_2)) 
                                      (167 (aload 4)) 
                                      (169 (aconst_null)) 
                                      (170 (astore 4)) 
                                      (172 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (177 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (182 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (187 (goto 192)) ;;to TAG_6;;at TAG_5
                                      (190 (pop)) ;;at TAG_0
                                      (191 (aconst_null)) ;;at TAG_1
                                      (192 (areturn)) ;;at TAG_6
                                      (endofcode 193))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$distinct$step__4671$fn__4672$fn__4674-class-table*
  (make-static-class-decls 
   *clojure.core$distinct$step__4671$fn__4672$fn__4674*))

(defconst *package-name-map* 
  ("clojure.core$distinct$step__4671$fn__4672$fn__4674" . "clojure"))

