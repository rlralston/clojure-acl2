; data$vectorize$fn__8910-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:48 CDT 2014.
;

(defconst *clojure.data$vectorize$fn__8910*
 (make-class-def
      '(class "clojure.data$vectorize$fn__8910"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "nth")
                        (STRING  "assoc"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 41)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "nth"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.data$vectorize$fn__8910" (class "clojure.lang.Var"))))
                                      (13 (lconst_0))
                                      (14 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (17 (putstatic (fieldCP "const__1" "clojure.data$vectorize$fn__8910" (class "java.lang.Object"))))
                                      (20 (lconst_1))
                                      (21 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (24 (putstatic (fieldCP "const__2" "clojure.data$vectorize$fn__8910" (class "java.lang.Object"))))
                                      (27 (ldc 0))        ;;STRING:: "clojure.core"
                                      (29 (ldc 2))        ;;STRING:: "assoc"
                                      (31 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (34 (checkcast (class "clojure.lang.Var")))
                                      (37 (putstatic (fieldCP "const__3" "clojure.data$vectorize$fn__8910" (class "clojure.lang.Var"))))
                                      (40 (return))
                                      (endofcode 41))
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
                                   (max_stack . 5) (max_locals . 6) (code_length . 56)
                                   (parsedcode
                                      (0 (aload_2))
                                      (1 (aconst_null))
                                      (2 (astore_2))
                                      (3 (astore_3))
                                      (4 (aload_3))
                                      (5 (lconst_0))
                                      (6 (invokestatic
					(methodCP "intCast" "clojure.lang.RT" (long) int)))
                                      (9 (aconst_null))
                                      (10 (invokestatic
					(methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object"))))
                                      (13 (astore 4))
                                      (15 (aload_3))
                                      (16 (aconst_null))
                                      (17 (astore_3))
                                      (18 (lconst_1))
                                      (19 (invokestatic
					(methodCP "intCast" "clojure.lang.RT" (long) int)))
                                      (22 (aconst_null))
                                      (23 (invokestatic
					(methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object"))))
                                      (26 (astore 5))
                                      (28 (getstatic (fieldCP "const__3" "clojure.data$vectorize$fn__8910" (class "clojure.lang.Var"))))
                                      (31 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (34 (checkcast (class "clojure.lang.IFn")))
                                      (37 (aload_1))
                                      (38 (aconst_null))
                                      (39 (astore_1))
                                      (40 (aload 4))
                                      (42 (aconst_null))
                                      (43 (astore 4))
                                      (45 (aload 5))
                                      (47 (aconst_null))
                                      (48 (astore 5))
                                      (50 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4))
                                      (55 (areturn))
                                      (endofcode 56))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *data$vectorize$fn__8910-class-table*
  (make-static-class-decls 
   *clojure.data$vectorize$fn__8910*))

(defconst *package-name-map* 
  ("clojure.data$vectorize$fn__8910" . "clojure"))

