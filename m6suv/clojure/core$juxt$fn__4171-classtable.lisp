; core$juxt$fn__4171-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:44 CDT 2014.
;

(defconst *clojure.core$juxt$fn__4171*
 (make-class-def
      '(class "clojure.core$juxt$fn__4171"
            "clojure.lang.RestFn"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "apply"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "f" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 14)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "apply"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$juxt$fn__4171" (class "clojure.lang.Var"))))
                                      (13 (return))
                                      (endofcode 14))
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
					(methodCP "<init>" "clojure.lang.RestFn" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "f" "clojure.core$juxt$fn__4171" (class "java.lang.Object"))))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "doInvoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 10) (max_locals . 5) (code_length . 43)
                                   (parsedcode
                                      (0 (iconst_1))
                                      (1 (anewarray (class "java.lang.Object")))
                                      (4 (dup))
                                      (5 (iconst_0))
                                      (6 (getstatic (fieldCP "const__0" "clojure.core$juxt$fn__4171" (class "clojure.lang.Var"))))
                                      (9 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (12 (checkcast (class "clojure.lang.IFn")))
                                      (15 (aload_0))
                                      (16 (getfield (fieldCP "f" "clojure.core$juxt$fn__4171" (class "java.lang.Object"))))
                                      (19 (aload_1))
                                      (20 (aconst_null))
                                      (21 (astore_1))
                                      (22 (aload_2))
                                      (23 (aconst_null))
                                      (24 (astore_2))
                                      (25 (aload_3))
                                      (26 (aconst_null))
                                      (27 (astore_3))
                                      (28 (aload 4))
                                      (30 (aconst_null))
                                      (31 (astore 4))
                                      (33 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 6))
                                      (38 (aastore))
                                      (39 (invokestatic
					(methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector"))))
                                      (42 (areturn))
                                      (endofcode 43))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 8) (max_locals . 4) (code_length . 32)
                                   (parsedcode
                                      (0 (iconst_1))
                                      (1 (anewarray (class "java.lang.Object")))
                                      (4 (dup))
                                      (5 (iconst_0))
                                      (6 (aload_0))
                                      (7 (getfield (fieldCP "f" "clojure.core$juxt$fn__4171" (class "java.lang.Object"))))
                                      (10 (checkcast (class "clojure.lang.IFn")))
                                      (13 (aload_1))
                                      (14 (aconst_null))
                                      (15 (astore_1))
                                      (16 (aload_2))
                                      (17 (aconst_null))
                                      (18 (astore_2))
                                      (19 (aload_3))
                                      (20 (aconst_null))
                                      (21 (astore_3))
                                      (22 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4))
                                      (27 (aastore))
                                      (28 (invokestatic
					(methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector"))))
                                      (31 (areturn))
                                      (endofcode 32))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 7) (max_locals . 3) (code_length . 29)
                                   (parsedcode
                                      (0 (iconst_1))
                                      (1 (anewarray (class "java.lang.Object")))
                                      (4 (dup))
                                      (5 (iconst_0))
                                      (6 (aload_0))
                                      (7 (getfield (fieldCP "f" "clojure.core$juxt$fn__4171" (class "java.lang.Object"))))
                                      (10 (checkcast (class "clojure.lang.IFn")))
                                      (13 (aload_1))
                                      (14 (aconst_null))
                                      (15 (astore_1))
                                      (16 (aload_2))
                                      (17 (aconst_null))
                                      (18 (astore_2))
                                      (19 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (24 (aastore))
                                      (25 (invokestatic
					(methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector"))))
                                      (28 (areturn))
                                      (endofcode 29))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 2) (code_length . 26)
                                   (parsedcode
                                      (0 (iconst_1))
                                      (1 (anewarray (class "java.lang.Object")))
                                      (4 (dup))
                                      (5 (iconst_0))
                                      (6 (aload_0))
                                      (7 (getfield (fieldCP "f" "clojure.core$juxt$fn__4171" (class "java.lang.Object"))))
                                      (10 (checkcast (class "clojure.lang.IFn")))
                                      (13 (aload_1))
                                      (14 (aconst_null))
                                      (15 (astore_1))
                                      (16 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (21 (aastore))
                                      (22 (invokestatic
					(methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector"))))
                                      (25 (areturn))
                                      (endofcode 26))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 1) (code_length . 23)
                                   (parsedcode
                                      (0 (iconst_1))
                                      (1 (anewarray (class "java.lang.Object")))
                                      (4 (dup))
                                      (5 (iconst_0))
                                      (6 (aload_0))
                                      (7 (getfield (fieldCP "f" "clojure.core$juxt$fn__4171" (class "java.lang.Object"))))
                                      (10 (checkcast (class "clojure.lang.IFn")))
                                      (13 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1))
                                      (18 (aastore))
                                      (19 (invokestatic
					(methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector"))))
                                      (22 (areturn))
                                      (endofcode 23))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getRequiredArity"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 2)
                                   (parsedcode
                                      (0 (iconst_3))
                                      (1 (ireturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$juxt$fn__4171-class-table*
  (make-static-class-decls 
   *clojure.core$juxt$fn__4171*))

(defconst *package-name-map* 
  ("clojure.core$juxt$fn__4171" . "clojure"))

