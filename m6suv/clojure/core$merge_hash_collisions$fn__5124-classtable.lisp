; core$merge_hash_collisions$fn__5124-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:44 CDT 2014.
;

(defconst *clojure.core$merge_hash_collisions$fn__5124*
 (make-class-def
      '(class "clojure.core$merge_hash_collisions$fn__5124"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "nth")
                        (STRING  "==")
                        (STRING  "count")
                        (STRING  "assoc")
                        (STRING  "ffirst")
                        (STRING  "second")
                        (STRING  "first"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "assoc_multi" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 106)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "nth"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$merge_hash_collisions$fn__5124" (class "clojure.lang.Var"))))
                                      (13 (lconst_0))
                                      (14 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (17 (putstatic (fieldCP "const__1" "clojure.core$merge_hash_collisions$fn__5124" (class "java.lang.Object"))))
                                      (20 (lconst_1))
                                      (21 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (24 (putstatic (fieldCP "const__2" "clojure.core$merge_hash_collisions$fn__5124" (class "java.lang.Object"))))
                                      (27 (ldc 0))        ;;STRING:: "clojure.core"
                                      (29 (ldc 2))        ;;STRING:: "=="
                                      (31 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (34 (checkcast (class "clojure.lang.Var")))
                                      (37 (putstatic (fieldCP "const__3" "clojure.core$merge_hash_collisions$fn__5124" (class "clojure.lang.Var"))))
                                      (40 (ldc 0))        ;;STRING:: "clojure.core"
                                      (42 (ldc 3))        ;;STRING:: "count"
                                      (44 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (47 (checkcast (class "clojure.lang.Var")))
                                      (50 (putstatic (fieldCP "const__4" "clojure.core$merge_hash_collisions$fn__5124" (class "clojure.lang.Var"))))
                                      (53 (ldc 0))        ;;STRING:: "clojure.core"
                                      (55 (ldc 4))        ;;STRING:: "assoc"
                                      (57 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (60 (checkcast (class "clojure.lang.Var")))
                                      (63 (putstatic (fieldCP "const__5" "clojure.core$merge_hash_collisions$fn__5124" (class "clojure.lang.Var"))))
                                      (66 (ldc 0))        ;;STRING:: "clojure.core"
                                      (68 (ldc 5))        ;;STRING:: "ffirst"
                                      (70 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (73 (checkcast (class "clojure.lang.Var")))
                                      (76 (putstatic (fieldCP "const__6" "clojure.core$merge_hash_collisions$fn__5124" (class "clojure.lang.Var"))))
                                      (79 (ldc 0))        ;;STRING:: "clojure.core"
                                      (81 (ldc 6))        ;;STRING:: "second"
                                      (83 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (86 (checkcast (class "clojure.lang.Var")))
                                      (89 (putstatic (fieldCP "const__7" "clojure.core$merge_hash_collisions$fn__5124" (class "clojure.lang.Var"))))
                                      (92 (ldc 0))        ;;STRING:: "clojure.core"
                                      (94 (ldc 7))        ;;STRING:: "first"
                                      (96 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (99 (checkcast (class "clojure.lang.Var")))
                                      (102 (putstatic (fieldCP "const__8" "clojure.core$merge_hash_collisions$fn__5124" (class "clojure.lang.Var"))))
                                      (105 (return))
                                      (endofcode 106))
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
                                      (6 (putfield (fieldCP "assoc_multi" "clojure.core$merge_hash_collisions$fn__5124" (class "java.lang.Object"))))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 7) (max_locals . 6) (code_length . 135)
                                   (parsedcode
                                      (0 (aload_2)) 
                                      (1 (aconst_null)) 
                                      (2 (astore_2)) 
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
                                      (18 (lconst_1)) 
                                      (19 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (22 (aconst_null)) 
                                      (23 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (26 (astore 5)) 
                                      (28 (lconst_1)) 
                                      (29 (aload 5)) 
                                      (31 (invokestatic (methodCP "count" "clojure.lang.RT" ((class "java.lang.Object")) int))) 
                                      (34 (i2l)) 
                                      (35 (lcmp)) 
                                      (36 (ifne 109))  ;;to TAG_0
                                      (39 (getstatic (fieldCP "const__5" "clojure.core$merge_hash_collisions$fn__5124" (class "clojure.lang.Var")))) 
                                      (42 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (45 (checkcast (class "clojure.lang.IFn"))) 
                                      (48 (aload_1)) 
                                      (49 (aconst_null)) 
                                      (50 (astore_1)) 
                                      (51 (getstatic (fieldCP "const__6" "clojure.core$merge_hash_collisions$fn__5124" (class "clojure.lang.Var")))) 
                                      (54 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (57 (checkcast (class "clojure.lang.IFn"))) 
                                      (60 (aload 5)) 
                                      (62 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (67 (getstatic (fieldCP "const__7" "clojure.core$merge_hash_collisions$fn__5124" (class "clojure.lang.Var")))) 
                                      (70 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (73 (checkcast (class "clojure.lang.IFn"))) 
                                      (76 (getstatic (fieldCP "const__8" "clojure.core$merge_hash_collisions$fn__5124" (class "clojure.lang.Var")))) 
                                      (79 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (82 (checkcast (class "clojure.lang.IFn"))) 
                                      (85 (aload 5)) 
                                      (87 (aconst_null)) 
                                      (88 (astore 5)) 
                                      (90 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (95 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (100 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) 
                                      (105 (goto 134)) ;;to TAG_1
                                      (108 (pop)) 
                                      (109 (aload_0)) ;;at TAG_0
                                      (110 (getfield (fieldCP "assoc_multi" "clojure.core$merge_hash_collisions$fn__5124" (class "java.lang.Object")))) 
                                      (113 (checkcast (class "clojure.lang.IFn"))) 
                                      (116 (aload_1)) 
                                      (117 (aconst_null)) 
                                      (118 (astore_1)) 
                                      (119 (aload 4)) 
                                      (121 (aconst_null)) 
                                      (122 (astore 4)) 
                                      (124 (aload 5)) 
                                      (126 (aconst_null)) 
                                      (127 (astore 5)) 
                                      (129 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) 
                                      (134 (areturn)) ;;at TAG_1
                                      (endofcode 135))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$merge_hash_collisions$fn__5124-class-table*
  (make-static-class-decls 
   *clojure.core$merge_hash_collisions$fn__5124*))

(defconst *package-name-map* 
  ("clojure.core$merge_hash_collisions$fn__5124" . "clojure"))
