; core$comp$fn__4156-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:41 CDT 2014.
;

(defconst *clojure.core$comp$fn__4156*
 (make-class-def
      '(class "clojure.core$comp$fn__4156"
            "clojure.lang.RestFn"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "apply"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "g" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "f" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "h" (class "java.lang.Object") (accessflags  *class* ) -1))
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
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$comp$fn__4156" (class "clojure.lang.Var"))))
                                      (13 (return))
                                      (endofcode 14))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 20)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.RestFn" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "g" "clojure.core$comp$fn__4156" (class "java.lang.Object"))))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "f" "clojure.core$comp$fn__4156" (class "java.lang.Object"))))
                                      (14 (aload_0))
                                      (15 (aload_3))
                                      (16 (putfield (fieldCP "h" "clojure.core$comp$fn__4156" (class "java.lang.Object"))))
                                      (19 (return))
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap )))
                        (method "doInvoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 9) (max_locals . 5) (code_length . 57)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "f" "clojure.core$comp$fn__4156" (class "java.lang.Object"))))
                                      (4 (checkcast (class "clojure.lang.IFn")))
                                      (7 (aload_0))
                                      (8 (getfield (fieldCP "g" "clojure.core$comp$fn__4156" (class "java.lang.Object"))))
                                      (11 (checkcast (class "clojure.lang.IFn")))
                                      (14 (getstatic (fieldCP "const__0" "clojure.core$comp$fn__4156" (class "clojure.lang.Var"))))
                                      (17 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (20 (checkcast (class "clojure.lang.IFn")))
                                      (23 (aload_0))
                                      (24 (getfield (fieldCP "h" "clojure.core$comp$fn__4156" (class "java.lang.Object"))))
                                      (27 (aload_1))
                                      (28 (aconst_null))
                                      (29 (astore_1))
                                      (30 (aload_2))
                                      (31 (aconst_null))
                                      (32 (astore_2))
                                      (33 (aload_3))
                                      (34 (aconst_null))
                                      (35 (astore_3))
                                      (36 (aload 4))
                                      (38 (aconst_null))
                                      (39 (astore 4))
                                      (41 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 6))
                                      (46 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (51 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (56 (areturn))
                                      (endofcode 57))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 7) (max_locals . 4) (code_length . 46)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "f" "clojure.core$comp$fn__4156" (class "java.lang.Object"))))
                                      (4 (checkcast (class "clojure.lang.IFn")))
                                      (7 (aload_0))
                                      (8 (getfield (fieldCP "g" "clojure.core$comp$fn__4156" (class "java.lang.Object"))))
                                      (11 (checkcast (class "clojure.lang.IFn")))
                                      (14 (aload_0))
                                      (15 (getfield (fieldCP "h" "clojure.core$comp$fn__4156" (class "java.lang.Object"))))
                                      (18 (checkcast (class "clojure.lang.IFn")))
                                      (21 (aload_1))
                                      (22 (aconst_null))
                                      (23 (astore_1))
                                      (24 (aload_2))
                                      (25 (aconst_null))
                                      (26 (astore_2))
                                      (27 (aload_3))
                                      (28 (aconst_null))
                                      (29 (astore_3))
                                      (30 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4))
                                      (35 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (40 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (45 (areturn))
                                      (endofcode 46))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 3) (code_length . 43)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "f" "clojure.core$comp$fn__4156" (class "java.lang.Object"))))
                                      (4 (checkcast (class "clojure.lang.IFn")))
                                      (7 (aload_0))
                                      (8 (getfield (fieldCP "g" "clojure.core$comp$fn__4156" (class "java.lang.Object"))))
                                      (11 (checkcast (class "clojure.lang.IFn")))
                                      (14 (aload_0))
                                      (15 (getfield (fieldCP "h" "clojure.core$comp$fn__4156" (class "java.lang.Object"))))
                                      (18 (checkcast (class "clojure.lang.IFn")))
                                      (21 (aload_1))
                                      (22 (aconst_null))
                                      (23 (astore_1))
                                      (24 (aload_2))
                                      (25 (aconst_null))
                                      (26 (astore_2))
                                      (27 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (32 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (37 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (42 (areturn))
                                      (endofcode 43))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 2) (code_length . 40)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "f" "clojure.core$comp$fn__4156" (class "java.lang.Object"))))
                                      (4 (checkcast (class "clojure.lang.IFn")))
                                      (7 (aload_0))
                                      (8 (getfield (fieldCP "g" "clojure.core$comp$fn__4156" (class "java.lang.Object"))))
                                      (11 (checkcast (class "clojure.lang.IFn")))
                                      (14 (aload_0))
                                      (15 (getfield (fieldCP "h" "clojure.core$comp$fn__4156" (class "java.lang.Object"))))
                                      (18 (checkcast (class "clojure.lang.IFn")))
                                      (21 (aload_1))
                                      (22 (aconst_null))
                                      (23 (astore_1))
                                      (24 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (29 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (34 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (39 (areturn))
                                      (endofcode 40))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 37)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "f" "clojure.core$comp$fn__4156" (class "java.lang.Object"))))
                                      (4 (checkcast (class "clojure.lang.IFn")))
                                      (7 (aload_0))
                                      (8 (getfield (fieldCP "g" "clojure.core$comp$fn__4156" (class "java.lang.Object"))))
                                      (11 (checkcast (class "clojure.lang.IFn")))
                                      (14 (aload_0))
                                      (15 (getfield (fieldCP "h" "clojure.core$comp$fn__4156" (class "java.lang.Object"))))
                                      (18 (checkcast (class "clojure.lang.IFn")))
                                      (21 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1))
                                      (26 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (31 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (36 (areturn))
                                      (endofcode 37))
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


(defconst *core$comp$fn__4156-class-table*
  (make-static-class-decls 
   *clojure.core$comp$fn__4156*))

(defconst *package-name-map* 
  ("clojure.core$comp$fn__4156" . "clojure"))
