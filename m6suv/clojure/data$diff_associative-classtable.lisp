; data$diff_associative-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:48 CDT 2014.
;

(defconst *clojure.data$diff_associative*
 (make-class-def
      '(class "clojure.data$diff_associative"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "reduce")
                        (STRING  "map")
                        (STRING  "partial")
                        (STRING  "clojure.data")
                        (STRING  "diff-associative-key"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 0) (code_length . 78)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "reduce"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.data$diff_associative" (class "clojure.lang.Var"))))
                                      (13 (iconst_3))
                                      (14 (anewarray (class "java.lang.Object")))
                                      (17 (dup))
                                      (18 (iconst_0))
                                      (19 (aconst_null))
                                      (20 (aastore))
                                      (21 (dup))
                                      (22 (iconst_1))
                                      (23 (aconst_null))
                                      (24 (aastore))
                                      (25 (dup))
                                      (26 (iconst_2))
                                      (27 (aconst_null))
                                      (28 (aastore))
                                      (29 (invokestatic
					(methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector"))))
                                      (32 (checkcast (class "clojure.lang.AFn")))
                                      (35 (putstatic (fieldCP "const__1" "clojure.data$diff_associative" (class "clojure.lang.AFn"))))
                                      (38 (ldc 0))        ;;STRING:: "clojure.core"
                                      (40 (ldc 2))        ;;STRING:: "map"
                                      (42 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (45 (checkcast (class "clojure.lang.Var")))
                                      (48 (putstatic (fieldCP "const__2" "clojure.data$diff_associative" (class "clojure.lang.Var"))))
                                      (51 (ldc 0))        ;;STRING:: "clojure.core"
                                      (53 (ldc 3))        ;;STRING:: "partial"
                                      (55 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (58 (checkcast (class "clojure.lang.Var")))
                                      (61 (putstatic (fieldCP "const__3" "clojure.data$diff_associative" (class "clojure.lang.Var"))))
                                      (64 (ldc 4))        ;;STRING:: "clojure.data"
                                      (66 (ldc 5))        ;;STRING:: "diff-associative-key"
                                      (68 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (71 (checkcast (class "clojure.lang.Var")))
                                      (74 (putstatic (fieldCP "const__4" "clojure.data$diff_associative" (class "clojure.lang.Var"))))
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
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 9) (max_locals . 4) (code_length . 68)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.data$diff_associative" (class "clojure.lang.Var"))))
                                      (3 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (6 (checkcast (class "clojure.lang.IFn")))
                                      (9 (new (class "clojure.data$diff_associative$fn__8924")))
                                      (12 (dup))
                                      (13 (invokespecial
					(methodCP "<init>" "clojure.data$diff_associative$fn__8924" () void)))
                                      (16 (getstatic (fieldCP "const__1" "clojure.data$diff_associative" (class "clojure.lang.AFn"))))
                                      (19 (getstatic (fieldCP "const__2" "clojure.data$diff_associative" (class "clojure.lang.Var"))))
                                      (22 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (25 (checkcast (class "clojure.lang.IFn")))
                                      (28 (getstatic (fieldCP "const__3" "clojure.data$diff_associative" (class "clojure.lang.Var"))))
                                      (31 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (34 (checkcast (class "clojure.lang.IFn")))
                                      (37 (getstatic (fieldCP "const__4" "clojure.data$diff_associative" (class "clojure.lang.Var"))))
                                      (40 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (43 (aload_1))
                                      (44 (aconst_null))
                                      (45 (astore_1))
                                      (46 (aload_2))
                                      (47 (aconst_null))
                                      (48 (astore_2))
                                      (49 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4))
                                      (54 (aload_3))
                                      (55 (aconst_null))
                                      (56 (astore_3))
                                      (57 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (62 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4))
                                      (67 (areturn))
                                      (endofcode 68))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *data$diff_associative-class-table*
  (make-static-class-decls 
   *clojure.data$diff_associative*))

(defconst *package-name-map* 
  ("clojure.data$diff_associative" . "clojure"))
