; core$merge_hash_collisions$fn__5118-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:44 CDT 2014.
;

(defconst *clojure.core$merge_hash_collisions$fn__5118*
 (make-class-def
      '(class "clojure.core$merge_hash_collisions$fn__5118"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "update-in")
                        (STRING  "hash")
                        (STRING  "first")
                        (STRING  "fnil")
                        (STRING  "conj")
                        (STRING  "next"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "thens" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "tests" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 79)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "update-in"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$merge_hash_collisions$fn__5118" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "hash"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$merge_hash_collisions$fn__5118" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "first"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.core$merge_hash_collisions$fn__5118" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 4))        ;;STRING:: "fnil"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.core$merge_hash_collisions$fn__5118" (class "clojure.lang.Var"))))
                                      (52 (ldc 0))        ;;STRING:: "clojure.core"
                                      (54 (ldc 5))        ;;STRING:: "conj"
                                      (56 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (59 (checkcast (class "clojure.lang.Var")))
                                      (62 (putstatic (fieldCP "const__4" "clojure.core$merge_hash_collisions$fn__5118" (class "clojure.lang.Var"))))
                                      (65 (ldc 0))        ;;STRING:: "clojure.core"
                                      (67 (ldc 6))        ;;STRING:: "next"
                                      (69 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (72 (checkcast (class "clojure.lang.Var")))
                                      (75 (putstatic (fieldCP "const__5" "clojure.core$merge_hash_collisions$fn__5118" (class "clojure.lang.Var"))))
                                      (78 (return))
                                      (endofcode 79))
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
                                      (6 (putfield (fieldCP "thens" "clojure.core$merge_hash_collisions$fn__5118" (class "java.lang.Object"))))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "tests" "clojure.core$merge_hash_collisions$fn__5118" (class "java.lang.Object"))))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 9) (max_locals . 5) (code_length . 221)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "EMPTY" "clojure.lang.PersistentArrayMap" (class "clojure.lang.PersistentArrayMap")))) 
                                      (3 (astore_1)) 
                                      (4 (aload_0)) 
                                      (5 (getfield (fieldCP "tests" "clojure.core$merge_hash_collisions$fn__5118" (class "java.lang.Object")))) 
                                      (8 (aload_0)) 
                                      (9 (aconst_null)) 
                                      (10 (putfield (fieldCP "tests" "clojure.core$merge_hash_collisions$fn__5118" (class "java.lang.Object")))) 
                                      (13 (astore_2)) 
                                      (14 (aload_0)) 
                                      (15 (getfield (fieldCP "thens" "clojure.core$merge_hash_collisions$fn__5118" (class "java.lang.Object")))) 
                                      (18 (aload_0)) 
                                      (19 (aconst_null)) 
                                      (20 (putfield (fieldCP "thens" "clojure.core$merge_hash_collisions$fn__5118" (class "java.lang.Object")))) 
                                      (23 (astore_3)) 
                                      (24 (aload_2)) ;;at TAG_5
                                      (25 (astore 4)) 
                                      (27 (aload 4)) 
                                      (29 (dup)) 
                                      (30 (ifnull 43)) ;;to TAG_0
                                      (33 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (36 (if_acmpeq 44)) ;;to TAG_1
                                      (39 (aload_3)) 
                                      (40 (goto 49))  ;;to TAG_2
                                      (43 (pop)) ;;at TAG_0
                                      (44 (aload 4)) ;;at TAG_1
                                      (46 (aconst_null)) 
                                      (47 (astore 4)) 
                                      (49 (dup)) ;;at TAG_2
                                      (50 (ifnull 218)) ;;to TAG_3
                                      (53 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (56 (if_acmpeq 219)) ;;to TAG_4
                                      (59 (getstatic (fieldCP "const__0" "clojure.core$merge_hash_collisions$fn__5118" (class "clojure.lang.Var")))) 
                                      (62 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (65 (checkcast (class "clojure.lang.IFn"))) 
                                      (68 (aload_1)) 
                                      (69 (iconst_1)) 
                                      (70 (anewarray (class "java.lang.Object"))) 
                                      (73 (dup)) 
                                      (74 (iconst_0)) 
                                      (75 (getstatic (fieldCP "const__1" "clojure.core$merge_hash_collisions$fn__5118" (class "clojure.lang.Var")))) 
                                      (78 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (81 (checkcast (class "clojure.lang.IFn"))) 
                                      (84 (getstatic (fieldCP "const__2" "clojure.core$merge_hash_collisions$fn__5118" (class "clojure.lang.Var")))) 
                                      (87 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (90 (checkcast (class "clojure.lang.IFn"))) 
                                      (93 (aload_2)) 
                                      (94 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (99 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (104 (aastore)) 
                                      (105 (invokestatic (methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector")))) 
                                      (108 (getstatic (fieldCP "const__3" "clojure.core$merge_hash_collisions$fn__5118" (class "clojure.lang.Var")))) 
                                      (111 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (114 (checkcast (class "clojure.lang.IFn"))) 
                                      (117 (getstatic (fieldCP "const__4" "clojure.core$merge_hash_collisions$fn__5118" (class "clojure.lang.Var")))) 
                                      (120 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (123 (getstatic (fieldCP "EMPTY" "clojure.lang.PersistentVector" (class "clojure.lang.PersistentVector")))) 
                                      (126 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (131 (iconst_2)) 
                                      (132 (anewarray (class "java.lang.Object"))) 
                                      (135 (dup)) 
                                      (136 (iconst_0)) 
                                      (137 (getstatic (fieldCP "const__2" "clojure.core$merge_hash_collisions$fn__5118" (class "clojure.lang.Var")))) 
                                      (140 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (143 (checkcast (class "clojure.lang.IFn"))) 
                                      (146 (aload_2)) 
                                      (147 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (152 (aastore)) 
                                      (153 (dup)) 
                                      (154 (iconst_1)) 
                                      (155 (getstatic (fieldCP "const__2" "clojure.core$merge_hash_collisions$fn__5118" (class "clojure.lang.Var")))) 
                                      (158 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (161 (checkcast (class "clojure.lang.IFn"))) 
                                      (164 (aload_3)) 
                                      (165 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (170 (aastore)) 
                                      (171 (invokestatic (methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector")))) 
                                      (174 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 5)) 
                                      (179 (getstatic (fieldCP "const__5" "clojure.core$merge_hash_collisions$fn__5118" (class "clojure.lang.Var")))) 
                                      (182 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (185 (checkcast (class "clojure.lang.IFn"))) 
                                      (188 (aload_2)) 
                                      (189 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (194 (getstatic (fieldCP "const__5" "clojure.core$merge_hash_collisions$fn__5118" (class "clojure.lang.Var")))) 
                                      (197 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (200 (checkcast (class "clojure.lang.IFn"))) 
                                      (203 (aload_3)) 
                                      (204 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (209 (astore_3)) 
                                      (210 (astore_2)) 
                                      (211 (astore_1)) 
                                      (212 (goto 24)) ;;to TAG_5
                                      (215 (goto 220)) ;;to TAG_6
                                      (218 (pop)) ;;at TAG_3
                                      (219 (aload_1)) ;;at TAG_4
                                      (220 (areturn)) ;;at TAG_6
                                      (endofcode 221))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$merge_hash_collisions$fn__5118-class-table*
  (make-static-class-decls 
   *clojure.core$merge_hash_collisions$fn__5118*))

(defconst *package-name-map* 
  ("clojure.core$merge_hash_collisions$fn__5118" . "clojure"))
