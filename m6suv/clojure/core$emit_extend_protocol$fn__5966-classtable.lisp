; core$emit_extend_protocol$fn__5966-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:42 CDT 2014.
;

(defconst *clojure.core$emit_extend_protocol$fn__5966*
 (make-class-def
      '(class "clojure.core$emit_extend_protocol$fn__5966"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "nth")
                        (STRING  "seq")
                        (STRING  "concat")
                        (STRING  "list")
                        (STRING  "extend-type"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "p" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 80)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "nth"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$emit_extend_protocol$fn__5966" (class "clojure.lang.Var"))))
                                      (13 (lconst_0))
                                      (14 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (17 (putstatic (fieldCP "const__1" "clojure.core$emit_extend_protocol$fn__5966" (class "java.lang.Object"))))
                                      (20 (lconst_1))
                                      (21 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (24 (putstatic (fieldCP "const__2" "clojure.core$emit_extend_protocol$fn__5966" (class "java.lang.Object"))))
                                      (27 (ldc 0))        ;;STRING:: "clojure.core"
                                      (29 (ldc 2))        ;;STRING:: "seq"
                                      (31 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (34 (checkcast (class "clojure.lang.Var")))
                                      (37 (putstatic (fieldCP "const__3" "clojure.core$emit_extend_protocol$fn__5966" (class "clojure.lang.Var"))))
                                      (40 (ldc 0))        ;;STRING:: "clojure.core"
                                      (42 (ldc 3))        ;;STRING:: "concat"
                                      (44 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (47 (checkcast (class "clojure.lang.Var")))
                                      (50 (putstatic (fieldCP "const__4" "clojure.core$emit_extend_protocol$fn__5966" (class "clojure.lang.Var"))))
                                      (53 (ldc 0))        ;;STRING:: "clojure.core"
                                      (55 (ldc 4))        ;;STRING:: "list"
                                      (57 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (60 (checkcast (class "clojure.lang.Var")))
                                      (63 (putstatic (fieldCP "const__5" "clojure.core$emit_extend_protocol$fn__5966" (class "clojure.lang.Var"))))
                                      (66 (ldc 0))        ;;STRING:: "clojure.core"
                                      (68 (ldc 5))        ;;STRING:: "extend-type"
                                      (70 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (73 (checkcast (class "clojure.lang.AFn")))
                                      (76 (putstatic (fieldCP "const__6" "clojure.core$emit_extend_protocol$fn__5966" (class "clojure.lang.AFn"))))
                                      (79 (return))
                                      (endofcode 80))
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
                                      (6 (putfield (fieldCP "p" "clojure.core$emit_extend_protocol$fn__5966" (class "java.lang.Object"))))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 7) (max_locals . 5) (code_length . 113)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (aconst_null))
                                      (2 (astore_1))
                                      (3 (astore_2))
                                      (4 (aload_2))
                                      (5 (lconst_0))
                                      (6 (invokestatic
					(methodCP "intCast" "clojure.lang.RT" (long) int)))
                                      (9 (aconst_null))
                                      (10 (invokestatic
					(methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object"))))
                                      (13 (astore_3))
                                      (14 (aload_2))
                                      (15 (aconst_null))
                                      (16 (astore_2))
                                      (17 (lconst_1))
                                      (18 (invokestatic
					(methodCP "intCast" "clojure.lang.RT" (long) int)))
                                      (21 (aconst_null))
                                      (22 (invokestatic
					(methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object"))))
                                      (25 (astore 4))
                                      (27 (getstatic (fieldCP "const__3" "clojure.core$emit_extend_protocol$fn__5966" (class "clojure.lang.Var"))))
                                      (30 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (33 (checkcast (class "clojure.lang.IFn")))
                                      (36 (getstatic (fieldCP "const__4" "clojure.core$emit_extend_protocol$fn__5966" (class "clojure.lang.Var"))))
                                      (39 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (42 (checkcast (class "clojure.lang.IFn")))
                                      (45 (getstatic (fieldCP "const__5" "clojure.core$emit_extend_protocol$fn__5966" (class "clojure.lang.Var"))))
                                      (48 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (51 (checkcast (class "clojure.lang.IFn")))
                                      (54 (getstatic (fieldCP "const__6" "clojure.core$emit_extend_protocol$fn__5966" (class "clojure.lang.AFn"))))
                                      (57 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (62 (getstatic (fieldCP "const__5" "clojure.core$emit_extend_protocol$fn__5966" (class "clojure.lang.Var"))))
                                      (65 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (68 (checkcast (class "clojure.lang.IFn")))
                                      (71 (aload_3))
                                      (72 (aconst_null))
                                      (73 (astore_3))
                                      (74 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (79 (getstatic (fieldCP "const__5" "clojure.core$emit_extend_protocol$fn__5966" (class "clojure.lang.Var"))))
                                      (82 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (85 (checkcast (class "clojure.lang.IFn")))
                                      (88 (aload_0))
                                      (89 (getfield (fieldCP "p" "clojure.core$emit_extend_protocol$fn__5966" (class "java.lang.Object"))))
                                      (92 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (97 (aload 4))
                                      (99 (aconst_null))
                                      (100 (astore 4))
                                      (102 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 5))
                                      (107 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (112 (areturn))
                                      (endofcode 113))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$emit_extend_protocol$fn__5966-class-table*
  (make-static-class-decls 
   *clojure.core$emit_extend_protocol$fn__5966*))

(defconst *package-name-map* 
  ("clojure.core$emit_extend_protocol$fn__5966" . "clojure"))

