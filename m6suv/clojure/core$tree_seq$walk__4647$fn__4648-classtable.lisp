; core$tree_seq$walk__4647$fn__4648-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:45 CDT 2014.
;

(defconst *clojure.core$tree_seq$walk__4647$fn__4648*
 (make-class-def
      '(class "clojure.core$tree_seq$walk__4647$fn__4648"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "cons")
                        (STRING  "mapcat"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "branch_QMARK_" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "walk" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "node" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "children" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 27)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "cons"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$tree_seq$walk__4647$fn__4648" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "mapcat"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$tree_seq$walk__4647$fn__4648" (class "clojure.lang.Var"))))
                                      (26 (return))
                                      (endofcode 27))
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
                                      (6 (putfield (fieldCP "branch_QMARK_" "clojure.core$tree_seq$walk__4647$fn__4648" (class "java.lang.Object"))))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "walk" "clojure.core$tree_seq$walk__4647$fn__4648" (class "java.lang.Object"))))
                                      (14 (aload_0))
                                      (15 (aload_3))
                                      (16 (putfield (fieldCP "node" "clojure.core$tree_seq$walk__4647$fn__4648" (class "java.lang.Object"))))
                                      (19 (aload_0))
                                      (20 (aload 4))
                                      (22 (putfield (fieldCP "children" "clojure.core$tree_seq$walk__4647$fn__4648" (class "java.lang.Object"))))
                                      (25 (return))
                                      (endofcode 26))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 8) (max_locals . 1) (code_length . 99)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$tree_seq$walk__4647$fn__4648" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (aload_0)) 
                                      (10 (getfield (fieldCP "node" "clojure.core$tree_seq$walk__4647$fn__4648" (class "java.lang.Object")))) 
                                      (13 (aload_0)) 
                                      (14 (getfield (fieldCP "branch_QMARK_" "clojure.core$tree_seq$walk__4647$fn__4648" (class "java.lang.Object")))) 
                                      (17 (aload_0)) 
                                      (18 (aconst_null)) 
                                      (19 (putfield (fieldCP "branch_QMARK_" "clojure.core$tree_seq$walk__4647$fn__4648" (class "java.lang.Object")))) 
                                      (22 (checkcast (class "clojure.lang.IFn"))) 
                                      (25 (aload_0)) 
                                      (26 (getfield (fieldCP "node" "clojure.core$tree_seq$walk__4647$fn__4648" (class "java.lang.Object")))) 
                                      (29 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (34 (dup)) 
                                      (35 (ifnull 91)) ;;to TAG_0
                                      (38 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (41 (if_acmpeq 92)) ;;to TAG_1
                                      (44 (getstatic (fieldCP "const__1" "clojure.core$tree_seq$walk__4647$fn__4648" (class "clojure.lang.Var")))) 
                                      (47 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (50 (checkcast (class "clojure.lang.IFn"))) 
                                      (53 (aload_0)) 
                                      (54 (getfield (fieldCP "walk" "clojure.core$tree_seq$walk__4647$fn__4648" (class "java.lang.Object")))) 
                                      (57 (aload_0)) 
                                      (58 (getfield (fieldCP "children" "clojure.core$tree_seq$walk__4647$fn__4648" (class "java.lang.Object")))) 
                                      (61 (aload_0)) 
                                      (62 (aconst_null)) 
                                      (63 (putfield (fieldCP "children" "clojure.core$tree_seq$walk__4647$fn__4648" (class "java.lang.Object")))) 
                                      (66 (checkcast (class "clojure.lang.IFn"))) 
                                      (69 (aload_0)) 
                                      (70 (getfield (fieldCP "node" "clojure.core$tree_seq$walk__4647$fn__4648" (class "java.lang.Object")))) 
                                      (73 (aload_0)) 
                                      (74 (aconst_null)) 
                                      (75 (putfield (fieldCP "node" "clojure.core$tree_seq$walk__4647$fn__4648" (class "java.lang.Object")))) 
                                      (78 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (83 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (88 (goto 93))  ;;to TAG_2
                                      (91 (pop)) ;;at TAG_0
                                      (92 (aconst_null)) ;;at TAG_1
                                      (93 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) ;;at TAG_2
                                      (98 (areturn)) 
                                      (endofcode 99))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$tree_seq$walk__4647$fn__4648-class-table*
  (make-static-class-decls 
   *clojure.core$tree_seq$walk__4647$fn__4648*))

(defconst *package-name-map* 
  ("clojure.core$tree_seq$walk__4647$fn__4648" . "clojure"))

