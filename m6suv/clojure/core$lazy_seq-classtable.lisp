; core$lazy_seq-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:44 CDT 2014.
;

(defconst *clojure.core$lazy_seq*
 (make-class-def
      '(class "clojure.core$lazy_seq"
            "clojure.lang.RestFn"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "list")
                        (STRING  "new")
                        (STRING  "clojure.lang.LazySeq")
                        (STRING  "list*")
                        (STRING  "fn*")
                        (STRING  "once"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 6) (max_locals . 0) (code_length . 96)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "list"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$lazy_seq" (class "clojure.lang.Var"))))
                                      (13 (aconst_null))
                                      (14 (ldc 2))        ;;STRING:: "new"
                                      (16 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (19 (checkcast (class "clojure.lang.AFn")))
                                      (22 (putstatic (fieldCP "const__1" "clojure.core$lazy_seq" (class "clojure.lang.AFn"))))
                                      (25 (aconst_null))
                                      (26 (ldc 3))        ;;STRING:: "clojure.lang.LazySeq"
                                      (28 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (31 (checkcast (class "clojure.lang.AFn")))
                                      (34 (putstatic (fieldCP "const__2" "clojure.core$lazy_seq" (class "clojure.lang.AFn"))))
                                      (37 (ldc 0))        ;;STRING:: "clojure.core"
                                      (39 (ldc 4))        ;;STRING:: "list*"
                                      (41 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (44 (checkcast (class "clojure.lang.Var")))
                                      (47 (putstatic (fieldCP "const__3" "clojure.core$lazy_seq" (class "clojure.lang.Var"))))
                                      (50 (aconst_null))
                                      (51 (ldc 5))        ;;STRING:: "fn*"
                                      (53 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (56 (checkcast (class "clojure.lang.IObj")))
                                      (59 (iconst_2))
                                      (60 (anewarray (class "java.lang.Object")))
                                      (63 (dup))
                                      (64 (iconst_0))
                                      (65 (aconst_null))
                                      (66 (ldc 6))        ;;STRING:: "once"
                                      (68 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (71 (aastore))
                                      (72 (dup))
                                      (73 (iconst_1))
                                      (74 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean"))))
                                      (77 (aastore))
                                      (78 (invokestatic
					(methodCP "map" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentMap"))))
                                      (81 (checkcast (class "clojure.lang.IPersistentMap")))
                                      (84 (invokeinterface
					(methodCP "withMeta" "clojure.lang.IObj" ((class "clojure.lang.IPersistentMap")) (class "clojure.lang.IObj")) 2))
                                      (89 (checkcast (class "clojure.lang.AFn")))
                                      (92 (putstatic (fieldCP "const__4" "clojure.core$lazy_seq" (class "clojure.lang.AFn"))))
                                      (95 (return))
                                      (endofcode 96))
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
					(methodCP "<init>" "clojure.lang.RestFn" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "doInvoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 8) (max_locals . 4) (code_length . 44)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$lazy_seq" (class "clojure.lang.Var"))))
                                      (3 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (6 (checkcast (class "clojure.lang.IFn")))
                                      (9 (getstatic (fieldCP "const__1" "clojure.core$lazy_seq" (class "clojure.lang.AFn"))))
                                      (12 (getstatic (fieldCP "const__2" "clojure.core$lazy_seq" (class "clojure.lang.AFn"))))
                                      (15 (getstatic (fieldCP "const__3" "clojure.core$lazy_seq" (class "clojure.lang.Var"))))
                                      (18 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (21 (checkcast (class "clojure.lang.IFn")))
                                      (24 (getstatic (fieldCP "const__4" "clojure.core$lazy_seq" (class "clojure.lang.AFn"))))
                                      (27 (getstatic (fieldCP "EMPTY" "clojure.lang.PersistentVector" (class "clojure.lang.PersistentVector"))))
                                      (30 (aload_3))
                                      (31 (aconst_null))
                                      (32 (astore_3))
                                      (33 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4))
                                      (38 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4))
                                      (43 (areturn))
                                      (endofcode 44))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getRequiredArity"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 2)
                                   (parsedcode
                                      (0 (iconst_2))
                                      (1 (ireturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$lazy_seq-class-table*
  (make-static-class-decls 
   *clojure.core$lazy_seq*))

(defconst *package-name-map* 
  ("clojure.core$lazy_seq" . "clojure"))

