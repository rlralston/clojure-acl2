; core$apply-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:40 CDT 2014.
;

(defconst *clojure.core$apply*
 (make-class-def
      '(class "clojure.core$apply"
            "clojure.lang.RestFn"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "seq")
                        (STRING  "list*")
                        (STRING  "cons")
                        (STRING  "spread"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 53)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "seq"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$apply" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "list*"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$apply" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "cons"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.core$apply" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 4))        ;;STRING:: "spread"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.core$apply" (class "clojure.lang.Var"))))
                                      (52 (return))
                                      (endofcode 53))
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
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 12) (max_locals . 7) (code_length . 106)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (aconst_null))
                                      (2 (astore_1))
                                      (3 (checkcast (class "clojure.lang.IFn")))
                                      (6 (getstatic (fieldCP "const__2" "clojure.core$apply" (class "clojure.lang.Var"))))
                                      (9 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (12 (checkcast (class "clojure.lang.IFn")))
                                      (15 (aload_2))
                                      (16 (aconst_null))
                                      (17 (astore_2))
                                      (18 (getstatic (fieldCP "const__2" "clojure.core$apply" (class "clojure.lang.Var"))))
                                      (21 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (24 (checkcast (class "clojure.lang.IFn")))
                                      (27 (aload_3))
                                      (28 (aconst_null))
                                      (29 (astore_3))
                                      (30 (getstatic (fieldCP "const__2" "clojure.core$apply" (class "clojure.lang.Var"))))
                                      (33 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (36 (checkcast (class "clojure.lang.IFn")))
                                      (39 (aload 4))
                                      (41 (aconst_null))
                                      (42 (astore 4))
                                      (44 (getstatic (fieldCP "const__2" "clojure.core$apply" (class "clojure.lang.Var"))))
                                      (47 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (50 (checkcast (class "clojure.lang.IFn")))
                                      (53 (aload 5))
                                      (55 (aconst_null))
                                      (56 (astore 5))
                                      (58 (getstatic (fieldCP "const__3" "clojure.core$apply" (class "clojure.lang.Var"))))
                                      (61 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (64 (checkcast (class "clojure.lang.IFn")))
                                      (67 (aload 6))
                                      (69 (aconst_null))
                                      (70 (astore 6))
                                      (72 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (77 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (82 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (87 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (92 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (97 (checkcast (class "clojure.lang.ISeq")))
                                      (100 (invokeinterface
					(methodCP "applyTo" "clojure.lang.IFn" ((class "clojure.lang.ISeq")) (class "java.lang.Object")) 2))
                                      (105 (areturn))
                                      (endofcode 106))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 7) (max_locals . 6) (code_length . 45)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (aconst_null))
                                      (2 (astore_1))
                                      (3 (checkcast (class "clojure.lang.IFn")))
                                      (6 (getstatic (fieldCP "const__1" "clojure.core$apply" (class "clojure.lang.Var"))))
                                      (9 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (12 (checkcast (class "clojure.lang.IFn")))
                                      (15 (aload_2))
                                      (16 (aconst_null))
                                      (17 (astore_2))
                                      (18 (aload_3))
                                      (19 (aconst_null))
                                      (20 (astore_3))
                                      (21 (aload 4))
                                      (23 (aconst_null))
                                      (24 (astore 4))
                                      (26 (aload 5))
                                      (28 (aconst_null))
                                      (29 (astore 5))
                                      (31 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 5))
                                      (36 (checkcast (class "clojure.lang.ISeq")))
                                      (39 (invokeinterface
					(methodCP "applyTo" "clojure.lang.IFn" ((class "clojure.lang.ISeq")) (class "java.lang.Object")) 2))
                                      (44 (areturn))
                                      (endofcode 45))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 5) (code_length . 40)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (aconst_null))
                                      (2 (astore_1))
                                      (3 (checkcast (class "clojure.lang.IFn")))
                                      (6 (getstatic (fieldCP "const__1" "clojure.core$apply" (class "clojure.lang.Var"))))
                                      (9 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (12 (checkcast (class "clojure.lang.IFn")))
                                      (15 (aload_2))
                                      (16 (aconst_null))
                                      (17 (astore_2))
                                      (18 (aload_3))
                                      (19 (aconst_null))
                                      (20 (astore_3))
                                      (21 (aload 4))
                                      (23 (aconst_null))
                                      (24 (astore 4))
                                      (26 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4))
                                      (31 (checkcast (class "clojure.lang.ISeq")))
                                      (34 (invokeinterface
					(methodCP "applyTo" "clojure.lang.IFn" ((class "clojure.lang.ISeq")) (class "java.lang.Object")) 2))
                                      (39 (areturn))
                                      (endofcode 40))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 4) (code_length . 35)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (aconst_null))
                                      (2 (astore_1))
                                      (3 (checkcast (class "clojure.lang.IFn")))
                                      (6 (getstatic (fieldCP "const__1" "clojure.core$apply" (class "clojure.lang.Var"))))
                                      (9 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (12 (checkcast (class "clojure.lang.IFn")))
                                      (15 (aload_2))
                                      (16 (aconst_null))
                                      (17 (astore_2))
                                      (18 (aload_3))
                                      (19 (aconst_null))
                                      (20 (astore_3))
                                      (21 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (26 (checkcast (class "clojure.lang.ISeq")))
                                      (29 (invokeinterface
					(methodCP "applyTo" "clojure.lang.IFn" ((class "clojure.lang.ISeq")) (class "java.lang.Object")) 2))
                                      (34 (areturn))
                                      (endofcode 35))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 32)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (aconst_null))
                                      (2 (astore_1))
                                      (3 (checkcast (class "clojure.lang.IFn")))
                                      (6 (getstatic (fieldCP "const__0" "clojure.core$apply" (class "clojure.lang.Var"))))
                                      (9 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (12 (checkcast (class "clojure.lang.IFn")))
                                      (15 (aload_2))
                                      (16 (aconst_null))
                                      (17 (astore_2))
                                      (18 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (23 (checkcast (class "clojure.lang.ISeq")))
                                      (26 (invokeinterface
					(methodCP "applyTo" "clojure.lang.IFn" ((class "clojure.lang.ISeq")) (class "java.lang.Object")) 2))
                                      (31 (areturn))
                                      (endofcode 32))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getRequiredArity"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 2)
                                   (parsedcode
                                      (0 (iconst_5))
                                      (1 (ireturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$apply-class-table*
  (make-static-class-decls 
   *clojure.core$apply*))

(defconst *package-name-map* 
  ("clojure.core$apply" . "clojure"))
