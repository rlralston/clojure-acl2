; core$emit_extend_protocol-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:42 CDT 2014.
;

(defconst *clojure.core$emit_extend_protocol*
 (make-class-def
      '(class "clojure.core$emit_extend_protocol"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "parse-impls")
                        (STRING  "seq")
                        (STRING  "concat")
                        (STRING  "list")
                        (STRING  "do")
                        (STRING  "map"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 78)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "parse-impls"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$emit_extend_protocol" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "seq"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$emit_extend_protocol" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "concat"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.core$emit_extend_protocol" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 4))        ;;STRING:: "list"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.core$emit_extend_protocol" (class "clojure.lang.Var"))))
                                      (52 (aconst_null))
                                      (53 (ldc 5))        ;;STRING:: "do"
                                      (55 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (58 (checkcast (class "clojure.lang.AFn")))
                                      (61 (putstatic (fieldCP "const__4" "clojure.core$emit_extend_protocol" (class "clojure.lang.AFn"))))
                                      (64 (ldc 0))        ;;STRING:: "clojure.core"
                                      (66 (ldc 6))        ;;STRING:: "map"
                                      (68 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (71 (checkcast (class "clojure.lang.Var")))
                                      (74 (putstatic (fieldCP "const__5" "clojure.core$emit_extend_protocol" (class "clojure.lang.Var"))))
                                      (77 (return))
                                      (endofcode 78))
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
                                   (max_stack . 8) (max_locals . 4) (code_length . 91)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$emit_extend_protocol" (class "clojure.lang.Var"))))
                                      (3 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (6 (checkcast (class "clojure.lang.IFn")))
                                      (9 (aload_2))
                                      (10 (aconst_null))
                                      (11 (astore_2))
                                      (12 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (17 (astore_3))
                                      (18 (getstatic (fieldCP "const__1" "clojure.core$emit_extend_protocol" (class "clojure.lang.Var"))))
                                      (21 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (24 (checkcast (class "clojure.lang.IFn")))
                                      (27 (getstatic (fieldCP "const__2" "clojure.core$emit_extend_protocol" (class "clojure.lang.Var"))))
                                      (30 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (33 (checkcast (class "clojure.lang.IFn")))
                                      (36 (getstatic (fieldCP "const__3" "clojure.core$emit_extend_protocol" (class "clojure.lang.Var"))))
                                      (39 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (42 (checkcast (class "clojure.lang.IFn")))
                                      (45 (getstatic (fieldCP "const__4" "clojure.core$emit_extend_protocol" (class "clojure.lang.AFn"))))
                                      (48 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (53 (getstatic (fieldCP "const__5" "clojure.core$emit_extend_protocol" (class "clojure.lang.Var"))))
                                      (56 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (59 (checkcast (class "clojure.lang.IFn")))
                                      (62 (new (class "clojure.core$emit_extend_protocol$fn__5966")))
                                      (65 (dup))
                                      (66 (aload_1))
                                      (67 (aconst_null))
                                      (68 (astore_1))
                                      (69 (invokespecial
					(methodCP "<init>" "clojure.core$emit_extend_protocol$fn__5966" ((class "java.lang.Object")) void)))
                                      (72 (aload_3))
                                      (73 (aconst_null))
                                      (74 (astore_3))
                                      (75 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (80 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (85 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (90 (areturn))
                                      (endofcode 91))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$emit_extend_protocol-class-table*
  (make-static-class-decls 
   *clojure.core$emit_extend_protocol*))

(defconst *package-name-map* 
  ("clojure.core$emit_extend_protocol" . "clojure"))
