; core$_LT___inliner-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:46 CDT 2014.
;

(defconst *clojure.core$_LT___inliner*
 (make-class-def
      '(class "clojure.core$_LT___inliner"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "seq")
                        (STRING  "concat")
                        (STRING  "list")
                        (STRING  ".")
                        (STRING  "clojure.lang.Numbers")
                        (STRING  "lt"))
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
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$_LT___inliner" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "concat"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$_LT___inliner" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "list"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.core$_LT___inliner" (class "clojure.lang.Var"))))
                                      (39 (aconst_null))
                                      (40 (ldc 4))        ;;STRING:: "."
                                      (42 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (45 (checkcast (class "clojure.lang.AFn")))
                                      (48 (putstatic (fieldCP "const__3" "clojure.core$_LT___inliner" (class "clojure.lang.AFn"))))
                                      (51 (aconst_null))
                                      (52 (ldc 5))        ;;STRING:: "clojure.lang.Numbers"
                                      (54 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (57 (checkcast (class "clojure.lang.AFn")))
                                      (60 (putstatic (fieldCP "const__4" "clojure.core$_LT___inliner" (class "clojure.lang.AFn"))))
                                      (63 (ldc 0))        ;;STRING:: "clojure.core"
                                      (65 (ldc 6))        ;;STRING:: "lt"
                                      (67 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (70 (checkcast (class "clojure.lang.AFn")))
                                      (73 (putstatic (fieldCP "const__5" "clojure.core$_LT___inliner" (class "clojure.lang.AFn"))))
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
                                   (max_stack . 12) (max_locals . 3) (code_length . 156)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$_LT___inliner" (class "clojure.lang.Var"))))
                                      (3 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (6 (checkcast (class "clojure.lang.IFn")))
                                      (9 (getstatic (fieldCP "const__1" "clojure.core$_LT___inliner" (class "clojure.lang.Var"))))
                                      (12 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (15 (checkcast (class "clojure.lang.IFn")))
                                      (18 (getstatic (fieldCP "const__2" "clojure.core$_LT___inliner" (class "clojure.lang.Var"))))
                                      (21 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (24 (checkcast (class "clojure.lang.IFn")))
                                      (27 (getstatic (fieldCP "const__3" "clojure.core$_LT___inliner" (class "clojure.lang.AFn"))))
                                      (30 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (35 (getstatic (fieldCP "const__2" "clojure.core$_LT___inliner" (class "clojure.lang.Var"))))
                                      (38 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (41 (checkcast (class "clojure.lang.IFn")))
                                      (44 (getstatic (fieldCP "const__4" "clojure.core$_LT___inliner" (class "clojure.lang.AFn"))))
                                      (47 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (52 (getstatic (fieldCP "const__2" "clojure.core$_LT___inliner" (class "clojure.lang.Var"))))
                                      (55 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (58 (checkcast (class "clojure.lang.IFn")))
                                      (61 (getstatic (fieldCP "const__0" "clojure.core$_LT___inliner" (class "clojure.lang.Var"))))
                                      (64 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (67 (checkcast (class "clojure.lang.IFn")))
                                      (70 (getstatic (fieldCP "const__1" "clojure.core$_LT___inliner" (class "clojure.lang.Var"))))
                                      (73 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (76 (checkcast (class "clojure.lang.IFn")))
                                      (79 (getstatic (fieldCP "const__2" "clojure.core$_LT___inliner" (class "clojure.lang.Var"))))
                                      (82 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (85 (checkcast (class "clojure.lang.IFn")))
                                      (88 (getstatic (fieldCP "const__5" "clojure.core$_LT___inliner" (class "clojure.lang.AFn"))))
                                      (91 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (96 (getstatic (fieldCP "const__2" "clojure.core$_LT___inliner" (class "clojure.lang.Var"))))
                                      (99 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (102 (checkcast (class "clojure.lang.IFn")))
                                      (105 (aload_1))
                                      (106 (aconst_null))
                                      (107 (astore_1))
                                      (108 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (113 (getstatic (fieldCP "const__2" "clojure.core$_LT___inliner" (class "clojure.lang.Var"))))
                                      (116 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (119 (checkcast (class "clojure.lang.IFn")))
                                      (122 (aload_2))
                                      (123 (aconst_null))
                                      (124 (astore_2))
                                      (125 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (130 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4))
                                      (135 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (140 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (145 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4))
                                      (150 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (155 (areturn))
                                      (endofcode 156))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$_LT___inliner-class-table*
  (make-static-class-decls 
   *clojure.core$_LT___inliner*))

(defconst *package-name-map* 
  ("clojure.core$_LT___inliner" . "clojure"))

