; core$juxt$fn__4173-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:44 CDT 2014.
;

(defconst *clojure.core$juxt$fn__4173*
 (make-class-def
      '(class "clojure.core$juxt$fn__4173"
            "clojure.lang.RestFn"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "apply"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "g" (class "java.lang.Object") (accessflags  *class* ) -1)
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
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$juxt$fn__4173" (class "clojure.lang.Var"))))
                                      (13 (return))
                                      (endofcode 14))
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
					(methodCP "<init>" "clojure.lang.RestFn" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "g" "clojure.core$juxt$fn__4173" (class "java.lang.Object"))))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "f" "clojure.core$juxt$fn__4173" (class "java.lang.Object"))))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "doInvoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 10) (max_locals . 5) (code_length . 69)
                                   (parsedcode
                                      (0 (iconst_2))
                                      (1 (anewarray (class "java.lang.Object")))
                                      (4 (dup))
                                      (5 (iconst_0))
                                      (6 (getstatic (fieldCP "const__0" "clojure.core$juxt$fn__4173" (class "clojure.lang.Var"))))
                                      (9 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (12 (checkcast (class "clojure.lang.IFn")))
                                      (15 (aload_0))
                                      (16 (getfield (fieldCP "f" "clojure.core$juxt$fn__4173" (class "java.lang.Object"))))
                                      (19 (aload_1))
                                      (20 (aload_2))
                                      (21 (aload_3))
                                      (22 (aload 4))
                                      (24 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 6))
                                      (29 (aastore))
                                      (30 (dup))
                                      (31 (iconst_1))
                                      (32 (getstatic (fieldCP "const__0" "clojure.core$juxt$fn__4173" (class "clojure.lang.Var"))))
                                      (35 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (38 (checkcast (class "clojure.lang.IFn")))
                                      (41 (aload_0))
                                      (42 (getfield (fieldCP "g" "clojure.core$juxt$fn__4173" (class "java.lang.Object"))))
                                      (45 (aload_1))
                                      (46 (aconst_null))
                                      (47 (astore_1))
                                      (48 (aload_2))
                                      (49 (aconst_null))
                                      (50 (astore_2))
                                      (51 (aload_3))
                                      (52 (aconst_null))
                                      (53 (astore_3))
                                      (54 (aload 4))
                                      (56 (aconst_null))
                                      (57 (astore 4))
                                      (59 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 6))
                                      (64 (aastore))
                                      (65 (invokestatic
					(methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector"))))
                                      (68 (areturn))
                                      (endofcode 69))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 8) (max_locals . 4) (code_length . 50)
                                   (parsedcode
                                      (0 (iconst_2))
                                      (1 (anewarray (class "java.lang.Object")))
                                      (4 (dup))
                                      (5 (iconst_0))
                                      (6 (aload_0))
                                      (7 (getfield (fieldCP "f" "clojure.core$juxt$fn__4173" (class "java.lang.Object"))))
                                      (10 (checkcast (class "clojure.lang.IFn")))
                                      (13 (aload_1))
                                      (14 (aload_2))
                                      (15 (aload_3))
                                      (16 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4))
                                      (21 (aastore))
                                      (22 (dup))
                                      (23 (iconst_1))
                                      (24 (aload_0))
                                      (25 (getfield (fieldCP "g" "clojure.core$juxt$fn__4173" (class "java.lang.Object"))))
                                      (28 (checkcast (class "clojure.lang.IFn")))
                                      (31 (aload_1))
                                      (32 (aconst_null))
                                      (33 (astore_1))
                                      (34 (aload_2))
                                      (35 (aconst_null))
                                      (36 (astore_2))
                                      (37 (aload_3))
                                      (38 (aconst_null))
                                      (39 (astore_3))
                                      (40 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4))
                                      (45 (aastore))
                                      (46 (invokestatic
					(methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector"))))
                                      (49 (areturn))
                                      (endofcode 50))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 7) (max_locals . 3) (code_length . 46)
                                   (parsedcode
                                      (0 (iconst_2))
                                      (1 (anewarray (class "java.lang.Object")))
                                      (4 (dup))
                                      (5 (iconst_0))
                                      (6 (aload_0))
                                      (7 (getfield (fieldCP "f" "clojure.core$juxt$fn__4173" (class "java.lang.Object"))))
                                      (10 (checkcast (class "clojure.lang.IFn")))
                                      (13 (aload_1))
                                      (14 (aload_2))
                                      (15 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (20 (aastore))
                                      (21 (dup))
                                      (22 (iconst_1))
                                      (23 (aload_0))
                                      (24 (getfield (fieldCP "g" "clojure.core$juxt$fn__4173" (class "java.lang.Object"))))
                                      (27 (checkcast (class "clojure.lang.IFn")))
                                      (30 (aload_1))
                                      (31 (aconst_null))
                                      (32 (astore_1))
                                      (33 (aload_2))
                                      (34 (aconst_null))
                                      (35 (astore_2))
                                      (36 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (41 (aastore))
                                      (42 (invokestatic
					(methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector"))))
                                      (45 (areturn))
                                      (endofcode 46))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 2) (code_length . 42)
                                   (parsedcode
                                      (0 (iconst_2))
                                      (1 (anewarray (class "java.lang.Object")))
                                      (4 (dup))
                                      (5 (iconst_0))
                                      (6 (aload_0))
                                      (7 (getfield (fieldCP "f" "clojure.core$juxt$fn__4173" (class "java.lang.Object"))))
                                      (10 (checkcast (class "clojure.lang.IFn")))
                                      (13 (aload_1))
                                      (14 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (19 (aastore))
                                      (20 (dup))
                                      (21 (iconst_1))
                                      (22 (aload_0))
                                      (23 (getfield (fieldCP "g" "clojure.core$juxt$fn__4173" (class "java.lang.Object"))))
                                      (26 (checkcast (class "clojure.lang.IFn")))
                                      (29 (aload_1))
                                      (30 (aconst_null))
                                      (31 (astore_1))
                                      (32 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (37 (aastore))
                                      (38 (invokestatic
					(methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector"))))
                                      (41 (areturn))
                                      (endofcode 42))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 1) (code_length . 38)
                                   (parsedcode
                                      (0 (iconst_2))
                                      (1 (anewarray (class "java.lang.Object")))
                                      (4 (dup))
                                      (5 (iconst_0))
                                      (6 (aload_0))
                                      (7 (getfield (fieldCP "f" "clojure.core$juxt$fn__4173" (class "java.lang.Object"))))
                                      (10 (checkcast (class "clojure.lang.IFn")))
                                      (13 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1))
                                      (18 (aastore))
                                      (19 (dup))
                                      (20 (iconst_1))
                                      (21 (aload_0))
                                      (22 (getfield (fieldCP "g" "clojure.core$juxt$fn__4173" (class "java.lang.Object"))))
                                      (25 (checkcast (class "clojure.lang.IFn")))
                                      (28 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1))
                                      (33 (aastore))
                                      (34 (invokestatic
					(methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector"))))
                                      (37 (areturn))
                                      (endofcode 38))
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


(defconst *core$juxt$fn__4173-class-table*
  (make-static-class-decls 
   *clojure.core$juxt$fn__4173*))

(defconst *package-name-map* 
  ("clojure.core$juxt$fn__4173" . "clojure"))

