; stacktrace$fn__7011-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:58 CDT 2014.
;

(defconst *clojure.stacktrace$fn__7011*
 (make-class-def
      '(class "clojure.stacktrace$fn__7011"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "commute")
                        (STRING  "deref")
                        (STRING  "*loaded-libs*")
                        (STRING  "conj")
                        (STRING  "clojure.stacktrace")
                        (STRING  "author")
                        (STRING  "Stuart Sierra")
                        (STRING  "doc")
                        (STRING  "Print stack traces oriented towards Clojure, not Java."))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 6) (max_locals . 0) (code_length . 111)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "commute"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.stacktrace$fn__7011" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "deref"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.stacktrace$fn__7011" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "*loaded-libs*"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.stacktrace$fn__7011" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 4))        ;;STRING:: "conj"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.stacktrace$fn__7011" (class "clojure.lang.Var"))))
                                      (52 (aconst_null))
                                      (53 (ldc 5))        ;;STRING:: "clojure.stacktrace"
                                      (55 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (58 (checkcast (class "clojure.lang.IObj")))
                                      (61 (iconst_4))
                                      (62 (anewarray (class "java.lang.Object")))
                                      (65 (dup))
                                      (66 (iconst_0))
                                      (67 (aconst_null))
                                      (68 (ldc 6))        ;;STRING:: "author"
                                      (70 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (73 (aastore))
                                      (74 (dup))
                                      (75 (iconst_1))
                                      (76 (ldc 7))        ;;STRING:: "Stuart Sierra"
                                      (78 (aastore))
                                      (79 (dup))
                                      (80 (iconst_2))
                                      (81 (aconst_null))
                                      (82 (ldc 8))        ;;STRING:: "doc"
                                      (84 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (87 (aastore))
                                      (88 (dup))
                                      (89 (iconst_3))
                                      (90 (ldc 9))        ;;STRING:: "Print stack traces oriented towards Clojure, not Java."
                                      (92 (aastore))
                                      (93 (invokestatic
					(methodCP "map" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentMap"))))
                                      (96 (checkcast (class "clojure.lang.IPersistentMap")))
                                      (99 (invokeinterface
					(methodCP "withMeta" "clojure.lang.IObj" ((class "clojure.lang.IPersistentMap")) (class "clojure.lang.IObj")) 2))
                                      (104 (checkcast (class "clojure.lang.AFn")))
                                      (107 (putstatic (fieldCP "const__4" "clojure.stacktrace$fn__7011" (class "clojure.lang.AFn"))))
                                      (110 (return))
                                      (endofcode 111))
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
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 1) (code_length . 41)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.stacktrace$fn__7011" (class "clojure.lang.Var"))))
                                      (3 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (6 (checkcast (class "clojure.lang.IFn")))
                                      (9 (getstatic (fieldCP "const__1" "clojure.stacktrace$fn__7011" (class "clojure.lang.Var"))))
                                      (12 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (15 (checkcast (class "clojure.lang.IFn")))
                                      (18 (getstatic (fieldCP "const__2" "clojure.stacktrace$fn__7011" (class "clojure.lang.Var"))))
                                      (21 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (26 (getstatic (fieldCP "const__3" "clojure.stacktrace$fn__7011" (class "clojure.lang.Var"))))
                                      (29 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (32 (getstatic (fieldCP "const__4" "clojure.stacktrace$fn__7011" (class "clojure.lang.AFn"))))
                                      (35 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4))
                                      (40 (areturn))
                                      (endofcode 41))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *stacktrace$fn__7011-class-table*
  (make-static-class-decls 
   *clojure.stacktrace$fn__7011*))

(defconst *package-name-map* 
  ("clojure.stacktrace$fn__7011" . "clojure"))

