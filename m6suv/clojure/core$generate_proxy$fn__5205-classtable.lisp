; core$generate_proxy$fn__5205-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:43 CDT 2014.
;

(defconst *clojure.core$generate_proxy$fn__5205*
 (make-class-def
      '(class "clojure.core$generate_proxy$fn__5205"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "nth"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "super" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 28)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "nth"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$generate_proxy$fn__5205" (class "clojure.lang.Var"))))
                                      (13 (lconst_0))
                                      (14 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (17 (putstatic (fieldCP "const__1" "clojure.core$generate_proxy$fn__5205" (class "java.lang.Object"))))
                                      (20 (lconst_1))
                                      (21 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (24 (putstatic (fieldCP "const__2" "clojure.core$generate_proxy$fn__5205" (class "java.lang.Object"))))
                                      (27 (return))
                                      (endofcode 28))
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
                                      (6 (putfield (fieldCP "super" "clojure.core$generate_proxy$fn__5205" (class "java.lang.Object"))))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 7) (code_length . 119)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "EMPTY" "clojure.lang.PersistentArrayMap" (class "clojure.lang.PersistentArrayMap")))) 
                                      (3 (astore_1)) 
                                      (4 (getstatic (fieldCP "EMPTY" "clojure.lang.PersistentHashSet" (class "clojure.lang.PersistentHashSet")))) 
                                      (7 (astore_2)) 
                                      (8 (aload_0)) 
                                      (9 (getfield (fieldCP "super" "clojure.core$generate_proxy$fn__5205" (class "java.lang.Object")))) 
                                      (12 (aload_0)) 
                                      (13 (aconst_null)) 
                                      (14 (putfield (fieldCP "super" "clojure.core$generate_proxy$fn__5205" (class "java.lang.Object")))) 
                                      (17 (astore_3)) 
                                      (18 (aload_3)) ;;at TAG_2
                                      (19 (dup)) 
                                      (20 (ifnull 102)) ;;to TAG_0
                                      (23 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (26 (if_acmpeq 103)) ;;to TAG_1
                                      (29 (new (class "clojure.core$generate_proxy$fn__5205$fn__5207"))) 
                                      (32 (dup)) 
                                      (33 (aload_1)) 
                                      (34 (aload_3)) 
                                      (35 (aload_2)) 
                                      (36 (invokespecial (methodCP "<init>" "clojure.core$generate_proxy$fn__5205$fn__5207" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) void))) 
                                      (39 (checkcast (class "clojure.lang.IFn"))) 
                                      (42 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1)) 
                                      (47 (astore 4)) 
                                      (49 (aload 4)) 
                                      (51 (lconst_0)) 
                                      (52 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (55 (aconst_null)) 
                                      (56 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (59 (astore 5)) 
                                      (61 (aload 4)) 
                                      (63 (aconst_null)) 
                                      (64 (astore 4)) 
                                      (66 (lconst_1)) 
                                      (67 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (70 (aconst_null)) 
                                      (71 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (74 (astore 6)) 
                                      (76 (aload 5)) 
                                      (78 (aconst_null)) 
                                      (79 (astore 5)) 
                                      (81 (aload 6)) 
                                      (83 (aconst_null)) 
                                      (84 (astore 6)) 
                                      (86 (aload_3)) 
                                      (87 (checkcast (class "java.lang.Class"))) 
                                      (90 (invokevirtual (methodCP "getSuperclass" "java.lang.Class" () (class "java.lang.Class")))) 
                                      (93 (astore_3)) 
                                      (94 (astore_2)) 
                                      (95 (astore_1)) 
                                      (96 (goto 18))  ;;to TAG_2
                                      (99 (goto 118)) ;;to TAG_3
                                      (102 (pop)) ;;at TAG_0
                                      (103 (iconst_2)) ;;at TAG_1
                                      (104 (anewarray (class "java.lang.Object"))) 
                                      (107 (dup)) 
                                      (108 (iconst_0)) 
                                      (109 (aload_1)) 
                                      (110 (aastore)) 
                                      (111 (dup)) 
                                      (112 (iconst_1)) 
                                      (113 (aload_2)) 
                                      (114 (aastore)) 
                                      (115 (invokestatic (methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector")))) 
                                      (118 (areturn)) ;;at TAG_3
                                      (endofcode 119))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$generate_proxy$fn__5205-class-table*
  (make-static-class-decls 
   *clojure.core$generate_proxy$fn__5205*))

(defconst *package-name-map* 
  ("clojure.core$generate_proxy$fn__5205" . "clojure"))
