; core$emit_hinted_impl-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:42 CDT 2014.
;

(defconst *clojure.core$emit_hinted_impl*
 (make-class-def
      '(class "clojure.core$emit_hinted_impl"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "nth")
                        (STRING  "zipmap")
                        (STRING  "map"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 54)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "nth"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$emit_hinted_impl" (class "clojure.lang.Var"))))
                                      (13 (lconst_0))
                                      (14 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (17 (putstatic (fieldCP "const__1" "clojure.core$emit_hinted_impl" (class "java.lang.Object"))))
                                      (20 (lconst_1))
                                      (21 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (24 (putstatic (fieldCP "const__2" "clojure.core$emit_hinted_impl" (class "java.lang.Object"))))
                                      (27 (ldc 0))        ;;STRING:: "clojure.core"
                                      (29 (ldc 2))        ;;STRING:: "zipmap"
                                      (31 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (34 (checkcast (class "clojure.lang.Var")))
                                      (37 (putstatic (fieldCP "const__3" "clojure.core$emit_hinted_impl" (class "clojure.lang.Var"))))
                                      (40 (ldc 0))        ;;STRING:: "clojure.core"
                                      (42 (ldc 3))        ;;STRING:: "map"
                                      (44 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (47 (checkcast (class "clojure.lang.Var")))
                                      (50 (putstatic (fieldCP "const__4" "clojure.core$emit_hinted_impl" (class "clojure.lang.Var"))))
                                      (53 (return))
                                      (endofcode 54))
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
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 10) (max_locals . 7) (code_length . 127)
                                   (parsedcode
                                      (0 (aload_2))
                                      (1 (aconst_null))
                                      (2 (astore_2))
                                      (3 (astore_3))
                                      (4 (aload_3))
                                      (5 (lconst_0))
                                      (6 (invokestatic
					(methodCP "intCast" "clojure.lang.RT" (long) int)))
                                      (9 (aconst_null))
                                      (10 (invokestatic
					(methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object"))))
                                      (13 (astore 4))
                                      (15 (aload_3))
                                      (16 (aconst_null))
                                      (17 (astore_3))
                                      (18 (lconst_1))
                                      (19 (invokestatic
					(methodCP "intCast" "clojure.lang.RT" (long) int)))
                                      (22 (aconst_null))
                                      (23 (invokestatic
					(methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object"))))
                                      (26 (astore 5))
                                      (28 (new (class "clojure.core$emit_hinted_impl$hint__5951")))
                                      (31 (dup))
                                      (32 (aload_1))
                                      (33 (aconst_null))
                                      (34 (astore_1))
                                      (35 (invokespecial
					(methodCP "<init>" "clojure.core$emit_hinted_impl$hint__5951" ((class "java.lang.Object")) void)))
                                      (38 (astore 6))
                                      (40 (iconst_2))
                                      (41 (anewarray (class "java.lang.Object")))
                                      (44 (dup))
                                      (45 (iconst_0))
                                      (46 (aload 4))
                                      (48 (aconst_null))
                                      (49 (astore 4))
                                      (51 (aastore))
                                      (52 (dup))
                                      (53 (iconst_1))
                                      (54 (getstatic (fieldCP "const__3" "clojure.core$emit_hinted_impl" (class "clojure.lang.Var"))))
                                      (57 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (60 (checkcast (class "clojure.lang.IFn")))
                                      (63 (getstatic (fieldCP "const__4" "clojure.core$emit_hinted_impl" (class "clojure.lang.Var"))))
                                      (66 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (69 (checkcast (class "clojure.lang.IFn")))
                                      (72 (new (class "clojure.core$emit_hinted_impl$fn__5958")))
                                      (75 (dup))
                                      (76 (invokespecial
					(methodCP "<init>" "clojure.core$emit_hinted_impl$fn__5958" () void)))
                                      (79 (aload 5))
                                      (81 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (86 (getstatic (fieldCP "const__4" "clojure.core$emit_hinted_impl" (class "clojure.lang.Var"))))
                                      (89 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (92 (checkcast (class "clojure.lang.IFn")))
                                      (95 (new (class "clojure.core$emit_hinted_impl$fn__5960")))
                                      (98 (dup))
                                      (99 (aload 6))
                                      (101 (aconst_null))
                                      (102 (astore 6))
                                      (104 (invokespecial
					(methodCP "<init>" "clojure.core$emit_hinted_impl$fn__5960" ((class "java.lang.Object")) void)))
                                      (107 (aload 5))
                                      (109 (aconst_null))
                                      (110 (astore 5))
                                      (112 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (117 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (122 (aastore))
                                      (123 (invokestatic
					(methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector"))))
                                      (126 (areturn))
                                      (endofcode 127))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$emit_hinted_impl-class-table*
  (make-static-class-decls 
   *clojure.core$emit_hinted_impl*))

(defconst *package-name-map* 
  ("clojure.core$emit_hinted_impl" . "clojure"))

