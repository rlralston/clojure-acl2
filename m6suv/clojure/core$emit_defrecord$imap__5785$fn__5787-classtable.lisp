; core$emit_defrecord$imap__5785$fn__5787-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:41 CDT 2014.
;

(defconst *clojure.core$emit_defrecord$imap__5785$fn__5787*
 (make-class-def
      '(class "clojure.core$emit_defrecord$imap__5785$fn__5787"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "seq")
                        (STRING  "concat")
                        (STRING  "list")
                        (STRING  "=")
                        (STRING  ".")
                        (STRING  "symbol")
                        (STRING  "str")
                        (STRING  "-"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "gs" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 91)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "seq"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$emit_defrecord$imap__5785$fn__5787" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "concat"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$emit_defrecord$imap__5785$fn__5787" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "list"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.core$emit_defrecord$imap__5785$fn__5787" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 4))        ;;STRING:: "="
                                      (43 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (46 (checkcast (class "clojure.lang.AFn")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.core$emit_defrecord$imap__5785$fn__5787" (class "clojure.lang.AFn"))))
                                      (52 (aconst_null))
                                      (53 (ldc 5))        ;;STRING:: "."
                                      (55 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (58 (checkcast (class "clojure.lang.AFn")))
                                      (61 (putstatic (fieldCP "const__4" "clojure.core$emit_defrecord$imap__5785$fn__5787" (class "clojure.lang.AFn"))))
                                      (64 (ldc 0))        ;;STRING:: "clojure.core"
                                      (66 (ldc 6))        ;;STRING:: "symbol"
                                      (68 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (71 (checkcast (class "clojure.lang.Var")))
                                      (74 (putstatic (fieldCP "const__5" "clojure.core$emit_defrecord$imap__5785$fn__5787" (class "clojure.lang.Var"))))
                                      (77 (ldc 0))        ;;STRING:: "clojure.core"
                                      (79 (ldc 7))        ;;STRING:: "str"
                                      (81 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (84 (checkcast (class "clojure.lang.Var")))
                                      (87 (putstatic (fieldCP "const__6" "clojure.core$emit_defrecord$imap__5785$fn__5787" (class "clojure.lang.Var"))))
                                      (90 (return))
                                      (endofcode 91))
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
					(methodCP "<init>" "clojure.lang.AFunction" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "gs" "clojure.core$emit_defrecord$imap__5785$fn__5787" (class "java.lang.Object"))))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 15) (max_locals . 2) (code_length . 185)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$emit_defrecord$imap__5785$fn__5787" (class "clojure.lang.Var"))))
                                      (3 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (6 (checkcast (class "clojure.lang.IFn")))
                                      (9 (getstatic (fieldCP "const__1" "clojure.core$emit_defrecord$imap__5785$fn__5787" (class "clojure.lang.Var"))))
                                      (12 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (15 (checkcast (class "clojure.lang.IFn")))
                                      (18 (getstatic (fieldCP "const__2" "clojure.core$emit_defrecord$imap__5785$fn__5787" (class "clojure.lang.Var"))))
                                      (21 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (24 (checkcast (class "clojure.lang.IFn")))
                                      (27 (getstatic (fieldCP "const__3" "clojure.core$emit_defrecord$imap__5785$fn__5787" (class "clojure.lang.AFn"))))
                                      (30 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (35 (getstatic (fieldCP "const__2" "clojure.core$emit_defrecord$imap__5785$fn__5787" (class "clojure.lang.Var"))))
                                      (38 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (41 (checkcast (class "clojure.lang.IFn")))
                                      (44 (aload_1))
                                      (45 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (50 (getstatic (fieldCP "const__2" "clojure.core$emit_defrecord$imap__5785$fn__5787" (class "clojure.lang.Var"))))
                                      (53 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (56 (checkcast (class "clojure.lang.IFn")))
                                      (59 (getstatic (fieldCP "const__0" "clojure.core$emit_defrecord$imap__5785$fn__5787" (class "clojure.lang.Var"))))
                                      (62 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (65 (checkcast (class "clojure.lang.IFn")))
                                      (68 (getstatic (fieldCP "const__1" "clojure.core$emit_defrecord$imap__5785$fn__5787" (class "clojure.lang.Var"))))
                                      (71 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (74 (checkcast (class "clojure.lang.IFn")))
                                      (77 (getstatic (fieldCP "const__2" "clojure.core$emit_defrecord$imap__5785$fn__5787" (class "clojure.lang.Var"))))
                                      (80 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (83 (checkcast (class "clojure.lang.IFn")))
                                      (86 (getstatic (fieldCP "const__4" "clojure.core$emit_defrecord$imap__5785$fn__5787" (class "clojure.lang.AFn"))))
                                      (89 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (94 (getstatic (fieldCP "const__2" "clojure.core$emit_defrecord$imap__5785$fn__5787" (class "clojure.lang.Var"))))
                                      (97 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (100 (checkcast (class "clojure.lang.IFn")))
                                      (103 (aload_0))
                                      (104 (getfield (fieldCP "gs" "clojure.core$emit_defrecord$imap__5785$fn__5787" (class "java.lang.Object"))))
                                      (107 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (112 (getstatic (fieldCP "const__2" "clojure.core$emit_defrecord$imap__5785$fn__5787" (class "clojure.lang.Var"))))
                                      (115 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (118 (checkcast (class "clojure.lang.IFn")))
                                      (121 (getstatic (fieldCP "const__5" "clojure.core$emit_defrecord$imap__5785$fn__5787" (class "clojure.lang.Var"))))
                                      (124 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (127 (checkcast (class "clojure.lang.IFn")))
                                      (130 (getstatic (fieldCP "const__6" "clojure.core$emit_defrecord$imap__5785$fn__5787" (class "clojure.lang.Var"))))
                                      (133 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (136 (checkcast (class "clojure.lang.IFn")))
                                      (139 (ldc 8))       ;;STRING:: "-"
                                      (141 (aload_1))
                                      (142 (aconst_null))
                                      (143 (astore_1))
                                      (144 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (149 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (154 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (159 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4))
                                      (164 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (169 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (174 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4))
                                      (179 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (184 (areturn))
                                      (endofcode 185))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$emit_defrecord$imap__5785$fn__5787-class-table*
  (make-static-class-decls 
   *clojure.core$emit_defrecord$imap__5785$fn__5787*))

(defconst *package-name-map* 
  ("clojure.core$emit_defrecord$imap__5785$fn__5787" . "clojure"))
