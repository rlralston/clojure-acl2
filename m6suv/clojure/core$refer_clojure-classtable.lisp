; core$refer_clojure-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:45 CDT 2014.
;

(defconst *clojure.core$refer_clojure*
 (make-class-def
      '(class "clojure.core$refer_clojure"
            "clojure.lang.RestFn"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "seq")
                        (STRING  "concat")
                        (STRING  "list")
                        (STRING  "refer")
                        (STRING  "quote"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 77)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "seq"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$refer_clojure" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "concat"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$refer_clojure" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "list"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.core$refer_clojure" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 4))        ;;STRING:: "refer"
                                      (43 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (46 (checkcast (class "clojure.lang.AFn")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.core$refer_clojure" (class "clojure.lang.AFn"))))
                                      (52 (aconst_null))
                                      (53 (ldc 5))        ;;STRING:: "quote"
                                      (55 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (58 (checkcast (class "clojure.lang.AFn")))
                                      (61 (putstatic (fieldCP "const__4" "clojure.core$refer_clojure" (class "clojure.lang.AFn"))))
                                      (64 (aconst_null))
                                      (65 (ldc 0))        ;;STRING:: "clojure.core"
                                      (67 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (70 (checkcast (class "clojure.lang.AFn")))
                                      (73 (putstatic (fieldCP "const__5" "clojure.core$refer_clojure" (class "clojure.lang.AFn"))))
                                      (76 (return))
                                      (endofcode 77))
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
                                   (max_stack . 9) (max_locals . 4) (code_length . 125)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$refer_clojure" (class "clojure.lang.Var"))))
                                      (3 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (6 (checkcast (class "clojure.lang.IFn")))
                                      (9 (getstatic (fieldCP "const__1" "clojure.core$refer_clojure" (class "clojure.lang.Var"))))
                                      (12 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (15 (checkcast (class "clojure.lang.IFn")))
                                      (18 (getstatic (fieldCP "const__2" "clojure.core$refer_clojure" (class "clojure.lang.Var"))))
                                      (21 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (24 (checkcast (class "clojure.lang.IFn")))
                                      (27 (getstatic (fieldCP "const__3" "clojure.core$refer_clojure" (class "clojure.lang.AFn"))))
                                      (30 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (35 (getstatic (fieldCP "const__2" "clojure.core$refer_clojure" (class "clojure.lang.Var"))))
                                      (38 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (41 (checkcast (class "clojure.lang.IFn")))
                                      (44 (getstatic (fieldCP "const__0" "clojure.core$refer_clojure" (class "clojure.lang.Var"))))
                                      (47 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (50 (checkcast (class "clojure.lang.IFn")))
                                      (53 (getstatic (fieldCP "const__1" "clojure.core$refer_clojure" (class "clojure.lang.Var"))))
                                      (56 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (59 (checkcast (class "clojure.lang.IFn")))
                                      (62 (getstatic (fieldCP "const__2" "clojure.core$refer_clojure" (class "clojure.lang.Var"))))
                                      (65 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (68 (checkcast (class "clojure.lang.IFn")))
                                      (71 (getstatic (fieldCP "const__4" "clojure.core$refer_clojure" (class "clojure.lang.AFn"))))
                                      (74 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (79 (getstatic (fieldCP "const__2" "clojure.core$refer_clojure" (class "clojure.lang.Var"))))
                                      (82 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (85 (checkcast (class "clojure.lang.IFn")))
                                      (88 (getstatic (fieldCP "const__5" "clojure.core$refer_clojure" (class "clojure.lang.AFn"))))
                                      (91 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (96 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (101 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (106 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (111 (aload_3))
                                      (112 (aconst_null))
                                      (113 (astore_3))
                                      (114 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4))
                                      (119 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (124 (areturn))
                                      (endofcode 125))
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


(defconst *core$refer_clojure-class-table*
  (make-static-class-decls 
   *clojure.core$refer_clojure*))

(defconst *package-name-map* 
  ("clojure.core$refer_clojure" . "clojure"))

