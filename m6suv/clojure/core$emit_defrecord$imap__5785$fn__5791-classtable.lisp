; core$emit_defrecord$imap__5785$fn__5791-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:41 CDT 2014.
;

(defconst *clojure.core$emit_defrecord$imap__5785$fn__5791*
 (make-class-def
      '(class "clojure.core$emit_defrecord$imap__5785$fn__5791"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "keyword")
                        (STRING  "list*")
                        (STRING  "new")
                        (STRING  "replace"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "tagname" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "gs" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "fields" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 52)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "keyword"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$emit_defrecord$imap__5785$fn__5791" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "list*"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$emit_defrecord$imap__5785$fn__5791" (class "clojure.lang.Var"))))
                                      (26 (aconst_null))
                                      (27 (ldc 3))        ;;STRING:: "new"
                                      (29 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (32 (checkcast (class "clojure.lang.AFn")))
                                      (35 (putstatic (fieldCP "const__2" "clojure.core$emit_defrecord$imap__5785$fn__5791" (class "clojure.lang.AFn"))))
                                      (38 (ldc 0))        ;;STRING:: "clojure.core"
                                      (40 (ldc 4))        ;;STRING:: "replace"
                                      (42 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (45 (checkcast (class "clojure.lang.Var")))
                                      (48 (putstatic (fieldCP "const__3" "clojure.core$emit_defrecord$imap__5785$fn__5791" (class "clojure.lang.Var"))))
                                      (51 (return))
                                      (endofcode 52))
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
					(methodCP "<init>" "clojure.lang.AFunction" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "tagname" "clojure.core$emit_defrecord$imap__5785$fn__5791" (class "java.lang.Object"))))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "gs" "clojure.core$emit_defrecord$imap__5785$fn__5791" (class "java.lang.Object"))))
                                      (14 (aload_0))
                                      (15 (aload_3))
                                      (16 (putfield (fieldCP "fields" "clojure.core$emit_defrecord$imap__5785$fn__5791" (class "java.lang.Object"))))
                                      (19 (return))
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 12) (max_locals . 2) (code_length . 88)
                                   (parsedcode
                                      (0 (iconst_2))
                                      (1 (anewarray (class "java.lang.Object")))
                                      (4 (dup))
                                      (5 (iconst_0))
                                      (6 (getstatic (fieldCP "const__0" "clojure.core$emit_defrecord$imap__5785$fn__5791" (class "clojure.lang.Var"))))
                                      (9 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (12 (checkcast (class "clojure.lang.IFn")))
                                      (15 (aload_1))
                                      (16 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (21 (aastore))
                                      (22 (dup))
                                      (23 (iconst_1))
                                      (24 (getstatic (fieldCP "const__1" "clojure.core$emit_defrecord$imap__5785$fn__5791" (class "clojure.lang.Var"))))
                                      (27 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (30 (checkcast (class "clojure.lang.IFn")))
                                      (33 (getstatic (fieldCP "const__2" "clojure.core$emit_defrecord$imap__5785$fn__5791" (class "clojure.lang.AFn"))))
                                      (36 (aload_0))
                                      (37 (getfield (fieldCP "tagname" "clojure.core$emit_defrecord$imap__5785$fn__5791" (class "java.lang.Object"))))
                                      (40 (getstatic (fieldCP "const__3" "clojure.core$emit_defrecord$imap__5785$fn__5791" (class "clojure.lang.Var"))))
                                      (43 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (46 (checkcast (class "clojure.lang.IFn")))
                                      (49 (iconst_2))
                                      (50 (anewarray (class "java.lang.Object")))
                                      (53 (dup))
                                      (54 (iconst_0))
                                      (55 (aload_1))
                                      (56 (aconst_null))
                                      (57 (astore_1))
                                      (58 (aastore))
                                      (59 (dup))
                                      (60 (iconst_1))
                                      (61 (aload_0))
                                      (62 (getfield (fieldCP "gs" "clojure.core$emit_defrecord$imap__5785$fn__5791" (class "java.lang.Object"))))
                                      (65 (aastore))
                                      (66 (invokestatic
					(methodCP "mapUniqueKeys" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentMap"))))
                                      (69 (aload_0))
                                      (70 (getfield (fieldCP "fields" "clojure.core$emit_defrecord$imap__5785$fn__5791" (class "java.lang.Object"))))
                                      (73 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (78 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4))
                                      (83 (aastore))
                                      (84 (invokestatic
					(methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector"))))
                                      (87 (areturn))
                                      (endofcode 88))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$emit_defrecord$imap__5785$fn__5791-class-table*
  (make-static-class-decls 
   *clojure.core$emit_defrecord$imap__5785$fn__5791*))

(defconst *package-name-map* 
  ("clojure.core$emit_defrecord$imap__5785$fn__5791" . "clojure"))
