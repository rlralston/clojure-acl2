; core$proxy$fn__5302$fn__5305-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:45 CDT 2014.
;

(defconst *clojure.core$proxy$fn__5302$fn__5305*
 (make-class-def
      '(class "clojure.core$proxy$fn__5302$fn__5305"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "nth")
                        (STRING  "nthnext")
                        (STRING  "cons")
                        (STRING  "apply")
                        (STRING  "vector")
                        (STRING  "this"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 92)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "nth"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$proxy$fn__5302$fn__5305" (class "clojure.lang.Var"))))
                                      (13 (lconst_0))
                                      (14 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (17 (putstatic (fieldCP "const__1" "clojure.core$proxy$fn__5302$fn__5305" (class "java.lang.Object"))))
                                      (20 (ldc 0))        ;;STRING:: "clojure.core"
                                      (22 (ldc 2))        ;;STRING:: "nthnext"
                                      (24 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (27 (checkcast (class "clojure.lang.Var")))
                                      (30 (putstatic (fieldCP "const__2" "clojure.core$proxy$fn__5302$fn__5305" (class "clojure.lang.Var"))))
                                      (33 (lconst_1))
                                      (34 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (37 (putstatic (fieldCP "const__3" "clojure.core$proxy$fn__5302$fn__5305" (class "java.lang.Object"))))
                                      (40 (ldc 0))        ;;STRING:: "clojure.core"
                                      (42 (ldc 3))        ;;STRING:: "cons"
                                      (44 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (47 (checkcast (class "clojure.lang.Var")))
                                      (50 (putstatic (fieldCP "const__4" "clojure.core$proxy$fn__5302$fn__5305" (class "clojure.lang.Var"))))
                                      (53 (ldc 0))        ;;STRING:: "clojure.core"
                                      (55 (ldc 4))        ;;STRING:: "apply"
                                      (57 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (60 (checkcast (class "clojure.lang.Var")))
                                      (63 (putstatic (fieldCP "const__5" "clojure.core$proxy$fn__5302$fn__5305" (class "clojure.lang.Var"))))
                                      (66 (ldc 0))        ;;STRING:: "clojure.core"
                                      (68 (ldc 5))        ;;STRING:: "vector"
                                      (70 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (73 (checkcast (class "clojure.lang.Var")))
                                      (76 (putstatic (fieldCP "const__6" "clojure.core$proxy$fn__5302$fn__5305" (class "clojure.lang.Var"))))
                                      (79 (aconst_null))
                                      (80 (ldc 6))        ;;STRING:: "this"
                                      (82 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (85 (checkcast (class "clojure.lang.AFn")))
                                      (88 (putstatic (fieldCP "const__7" "clojure.core$proxy$fn__5302$fn__5305" (class "clojure.lang.AFn"))))
                                      (91 (return))
                                      (endofcode 92))
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
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 5) (code_length . 82)
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
                                      (14 (getstatic (fieldCP "const__2" "clojure.core$proxy$fn__5302$fn__5305" (class "clojure.lang.Var"))))
                                      (17 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (20 (checkcast (class "clojure.lang.IFn")))
                                      (23 (aload_2))
                                      (24 (aconst_null))
                                      (25 (astore_2))
                                      (26 (getstatic (fieldCP "const__3" "clojure.core$proxy$fn__5302$fn__5305" (class "java.lang.Object"))))
                                      (29 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (34 (astore 4))
                                      (36 (getstatic (fieldCP "const__4" "clojure.core$proxy$fn__5302$fn__5305" (class "clojure.lang.Var"))))
                                      (39 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (42 (checkcast (class "clojure.lang.IFn")))
                                      (45 (getstatic (fieldCP "const__5" "clojure.core$proxy$fn__5302$fn__5305" (class "clojure.lang.Var"))))
                                      (48 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (51 (checkcast (class "clojure.lang.IFn")))
                                      (54 (getstatic (fieldCP "const__6" "clojure.core$proxy$fn__5302$fn__5305" (class "clojure.lang.Var"))))
                                      (57 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (60 (getstatic (fieldCP "const__7" "clojure.core$proxy$fn__5302$fn__5305" (class "clojure.lang.AFn"))))
                                      (63 (aload_3))
                                      (64 (aconst_null))
                                      (65 (astore_3))
                                      (66 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4))
                                      (71 (aload 4))
                                      (73 (aconst_null))
                                      (74 (astore 4))
                                      (76 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (81 (areturn))
                                      (endofcode 82))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$proxy$fn__5302$fn__5305-class-table*
  (make-static-class-decls 
   *clojure.core$proxy$fn__5302$fn__5305*))

(defconst *package-name-map* 
  ("clojure.core$proxy$fn__5302$fn__5305" . "clojure"))
