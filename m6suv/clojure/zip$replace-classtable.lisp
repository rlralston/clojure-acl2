; zip$replace-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:59 CDT 2014.
;

(defconst *clojure.zip$replace*
 (make-class-def
      '(class "clojure.zip$replace"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "nth")
                        (STRING  "with-meta")
                        (STRING  "assoc")
                        (STRING  "changed?")
                        (STRING  "meta"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 79)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "nth"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.zip$replace" (class "clojure.lang.Var"))))
                                      (13 (lconst_0))
                                      (14 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (17 (putstatic (fieldCP "const__1" "clojure.zip$replace" (class "java.lang.Object"))))
                                      (20 (lconst_1))
                                      (21 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (24 (putstatic (fieldCP "const__2" "clojure.zip$replace" (class "java.lang.Object"))))
                                      (27 (ldc 0))        ;;STRING:: "clojure.core"
                                      (29 (ldc 2))        ;;STRING:: "with-meta"
                                      (31 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (34 (checkcast (class "clojure.lang.Var")))
                                      (37 (putstatic (fieldCP "const__3" "clojure.zip$replace" (class "clojure.lang.Var"))))
                                      (40 (ldc 0))        ;;STRING:: "clojure.core"
                                      (42 (ldc 3))        ;;STRING:: "assoc"
                                      (44 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (47 (checkcast (class "clojure.lang.Var")))
                                      (50 (putstatic (fieldCP "const__4" "clojure.zip$replace" (class "clojure.lang.Var"))))
                                      (53 (aconst_null))
                                      (54 (ldc 4))        ;;STRING:: "changed?"
                                      (56 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (59 (checkcast (class "clojure.lang.Keyword")))
                                      (62 (putstatic (fieldCP "const__5" "clojure.zip$replace" (class "clojure.lang.Keyword"))))
                                      (65 (ldc 0))        ;;STRING:: "clojure.core"
                                      (67 (ldc 5))        ;;STRING:: "meta"
                                      (69 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (72 (checkcast (class "clojure.lang.Var")))
                                      (75 (putstatic (fieldCP "const__6" "clojure.zip$replace" (class "clojure.lang.Var"))))
                                      (78 (return))
                                      (endofcode 79))
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
                                   (max_stack . 8) (max_locals . 6) (code_length . 99)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (astore_3))
                                      (2 (aload_3))
                                      (3 (lconst_0))
                                      (4 (invokestatic
					(methodCP "intCast" "clojure.lang.RT" (long) int)))
                                      (7 (aconst_null))
                                      (8 (invokestatic
					(methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object"))))
                                      (11 (astore 4))
                                      (13 (aload_3))
                                      (14 (aconst_null))
                                      (15 (astore_3))
                                      (16 (lconst_1))
                                      (17 (invokestatic
					(methodCP "intCast" "clojure.lang.RT" (long) int)))
                                      (20 (aconst_null))
                                      (21 (invokestatic
					(methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object"))))
                                      (24 (astore 5))
                                      (26 (getstatic (fieldCP "const__3" "clojure.zip$replace" (class "clojure.lang.Var"))))
                                      (29 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (32 (checkcast (class "clojure.lang.IFn")))
                                      (35 (iconst_2))
                                      (36 (anewarray (class "java.lang.Object")))
                                      (39 (dup))
                                      (40 (iconst_0))
                                      (41 (aload_2))
                                      (42 (aconst_null))
                                      (43 (astore_2))
                                      (44 (aastore))
                                      (45 (dup))
                                      (46 (iconst_1))
                                      (47 (getstatic (fieldCP "const__4" "clojure.zip$replace" (class "clojure.lang.Var"))))
                                      (50 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (53 (checkcast (class "clojure.lang.IFn")))
                                      (56 (aload 5))
                                      (58 (aconst_null))
                                      (59 (astore 5))
                                      (61 (getstatic (fieldCP "const__5" "clojure.zip$replace" (class "clojure.lang.Keyword"))))
                                      (64 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean"))))
                                      (67 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4))
                                      (72 (aastore))
                                      (73 (invokestatic
					(methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector"))))
                                      (76 (getstatic (fieldCP "const__6" "clojure.zip$replace" (class "clojure.lang.Var"))))
                                      (79 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (82 (checkcast (class "clojure.lang.IFn")))
                                      (85 (aload_1))
                                      (86 (aconst_null))
                                      (87 (astore_1))
                                      (88 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (93 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (98 (areturn))
                                      (endofcode 99))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *zip$replace-class-table*
  (make-static-class-decls 
   *clojure.zip$replace*))

(defconst *package-name-map* 
  ("clojure.zip$replace" . "clojure"))

