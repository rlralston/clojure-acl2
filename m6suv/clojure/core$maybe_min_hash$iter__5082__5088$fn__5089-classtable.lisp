; core$maybe_min_hash$iter__5082__5088$fn__5089-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:44 CDT 2014.
;

(defconst *clojure.core$maybe_min_hash$iter__5082__5088$fn__5089*
 (make-class-def
      '(class "clojure.core$maybe_min_hash$iter__5082__5088$fn__5089"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "seq")
                        (STRING  "first")
                        (STRING  "range")
                        (LONG 31)
                        (STRING  "concat")
                        (STRING  "rest"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "s__5083" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "iter__5082" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 82)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "seq"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$maybe_min_hash$iter__5082__5088$fn__5089" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "first"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$maybe_min_hash$iter__5082__5088$fn__5089" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "range"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.core$maybe_min_hash$iter__5082__5088$fn__5089" (class "clojure.lang.Var"))))
                                      (39 (lconst_0))
                                      (40 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (43 (putstatic (fieldCP "const__3" "clojure.core$maybe_min_hash$iter__5082__5088$fn__5089" (class "java.lang.Object"))))
                                      (46 (ldc2_w 4))     ;; LONG:: "31"
                                      (49 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (52 (putstatic (fieldCP "const__4" "clojure.core$maybe_min_hash$iter__5082__5088$fn__5089" (class "java.lang.Object"))))
                                      (55 (ldc 0))        ;;STRING:: "clojure.core"
                                      (57 (ldc 5))        ;;STRING:: "concat"
                                      (59 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (62 (checkcast (class "clojure.lang.Var")))
                                      (65 (putstatic (fieldCP "const__5" "clojure.core$maybe_min_hash$iter__5082__5088$fn__5089" (class "clojure.lang.Var"))))
                                      (68 (ldc 0))        ;;STRING:: "clojure.core"
                                      (70 (ldc 6))        ;;STRING:: "rest"
                                      (72 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (75 (checkcast (class "clojure.lang.Var")))
                                      (78 (putstatic (fieldCP "const__6" "clojure.core$maybe_min_hash$iter__5082__5088$fn__5089" (class "clojure.lang.Var"))))
                                      (81 (return))
                                      (endofcode 82))
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
                                      (6 (putfield (fieldCP "s__5083" "clojure.core$maybe_min_hash$iter__5082__5088$fn__5089" (class "java.lang.Object"))))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "iter__5082" "clojure.core$maybe_min_hash$iter__5082__5088$fn__5089" (class "java.lang.Object"))))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 7) (code_length . 210)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "s__5083" "clojure.core$maybe_min_hash$iter__5082__5088$fn__5089" (class "java.lang.Object")))) 
                                      (4 (aload_0)) 
                                      (5 (aconst_null)) 
                                      (6 (putfield (fieldCP "s__5083" "clojure.core$maybe_min_hash$iter__5082__5088$fn__5089" (class "java.lang.Object")))) 
                                      (9 (astore_1)) 
                                      (10 (getstatic (fieldCP "const__0" "clojure.core$maybe_min_hash$iter__5082__5088$fn__5089" (class "clojure.lang.Var")))) ;;at TAG_5
                                      (13 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (16 (checkcast (class "clojure.lang.IFn"))) 
                                      (19 (aload_1)) 
                                      (20 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (25 (astore_2)) 
                                      (26 (aload_2)) 
                                      (27 (dup)) 
                                      (28 (ifnull 207)) ;;to TAG_0
                                      (31 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (34 (if_acmpeq 208)) ;;to TAG_1
                                      (37 (aload_2)) 
                                      (38 (aconst_null)) 
                                      (39 (astore_2)) 
                                      (40 (astore_3)) 
                                      (41 (getstatic (fieldCP "const__1" "clojure.core$maybe_min_hash$iter__5082__5088$fn__5089" (class "clojure.lang.Var")))) 
                                      (44 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (47 (checkcast (class "clojure.lang.IFn"))) 
                                      (50 (aload_3)) 
                                      (51 (aconst_null)) 
                                      (52 (astore_3)) 
                                      (53 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (58 (astore 4)) 
                                      (60 (new (class "clojure.core$maybe_min_hash$iter__5082__5088$fn__5089$iter__5084__5090"))) 
                                      (63 (dup)) 
                                      (64 (aload 4)) 
                                      (66 (aconst_null)) 
                                      (67 (astore 4)) 
                                      (69 (invokespecial (methodCP "<init>" "clojure.core$maybe_min_hash$iter__5082__5088$fn__5089$iter__5084__5090" ((class "java.lang.Object")) void))) 
                                      (72 (astore 5)) 
                                      (74 (getstatic (fieldCP "const__0" "clojure.core$maybe_min_hash$iter__5082__5088$fn__5089" (class "clojure.lang.Var")))) 
                                      (77 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (80 (checkcast (class "clojure.lang.IFn"))) 
                                      (83 (aload 5)) 
                                      (85 (aconst_null)) 
                                      (86 (astore 5)) 
                                      (88 (checkcast (class "clojure.lang.IFn"))) 
                                      (91 (getstatic (fieldCP "const__2" "clojure.core$maybe_min_hash$iter__5082__5088$fn__5089" (class "clojure.lang.Var")))) 
                                      (94 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (97 (checkcast (class "clojure.lang.IFn"))) 
                                      (100 (getstatic (fieldCP "const__3" "clojure.core$maybe_min_hash$iter__5082__5088$fn__5089" (class "java.lang.Object")))) 
                                      (103 (getstatic (fieldCP "const__4" "clojure.core$maybe_min_hash$iter__5082__5088$fn__5089" (class "java.lang.Object")))) 
                                      (106 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (111 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (116 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (121 (astore 6)) 
                                      (123 (aload 6)) 
                                      (125 (dup)) 
                                      (126 (ifnull 184))  ;;to TAG_2
                                      (129 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (132 (if_acmpeq 185)) ;;to TAG_3
                                      (135 (getstatic (fieldCP "const__5" "clojure.core$maybe_min_hash$iter__5082__5088$fn__5089" (class "clojure.lang.Var")))) 
                                      (138 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (141 (checkcast (class "clojure.lang.IFn"))) 
                                      (144 (aload 6)) 
                                      (146 (aconst_null)) 
                                      (147 (astore 6)) 
                                      (149 (aload_0)) 
                                      (150 (getfield (fieldCP "iter__5082" "clojure.core$maybe_min_hash$iter__5082__5088$fn__5089" (class "java.lang.Object")))) 
                                      (153 (checkcast (class "clojure.lang.IFn"))) 
                                      (156 (getstatic (fieldCP "const__6" "clojure.core$maybe_min_hash$iter__5082__5088$fn__5089" (class "clojure.lang.Var")))) 
                                      (159 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (162 (checkcast (class "clojure.lang.IFn"))) 
                                      (165 (aload_1)) 
                                      (166 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (171 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (176 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (181 (goto 204)) ;;to TAG_4
                                      (184 (pop)) ;;at TAG_2
                                      (185 (getstatic (fieldCP "const__6" "clojure.core$maybe_min_hash$iter__5082__5088$fn__5089" (class "clojure.lang.Var")))) ;;at TAG_3
                                      (188 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (191 (checkcast (class "clojure.lang.IFn"))) 
                                      (194 (aload_1)) 
                                      (195 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (200 (astore_1)) 
                                      (201 (goto 10)) ;;to TAG_5
                                      (204 (goto 209)) ;;to TAG_6;;at TAG_4
                                      (207 (pop)) ;;at TAG_0
                                      (208 (aconst_null)) ;;at TAG_1
                                      (209 (areturn)) ;;at TAG_6
                                      (endofcode 210))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$maybe_min_hash$iter__5082__5088$fn__5089-class-table*
  (make-static-class-decls 
   *clojure.core$maybe_min_hash$iter__5082__5088$fn__5089*))

(defconst *package-name-map* 
  ("clojure.core$maybe_min_hash$iter__5082__5088$fn__5089" . "clojure"))

