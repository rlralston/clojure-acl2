; core$derive$tf__4869-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:41 CDT 2014.
;

(defconst *clojure.core$derive$tf__4869*
 (make-class-def
      '(class "clojure.core$derive$tf__4869"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "reduce1")
                        (STRING  "cons"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 27)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "reduce1"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$derive$tf__4869" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "cons"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$derive$tf__4869" (class "clojure.lang.Var"))))
                                      (26 (return))
                                      (endofcode 27))
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
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 8) (max_locals . 6) (code_length . 64)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$derive$tf__4869" (class "clojure.lang.Var"))))
                                      (3 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (6 (checkcast (class "clojure.lang.IFn")))
                                      (9 (new (class "clojure.core$derive$tf__4869$fn__4870")))
                                      (12 (dup))
                                      (13 (aload 5))
                                      (15 (aconst_null))
                                      (16 (astore 5))
                                      (18 (aload 4))
                                      (20 (aconst_null))
                                      (21 (astore 4))
                                      (23 (invokespecial
					(methodCP "<init>" "clojure.core$derive$tf__4869$fn__4870" ((class "java.lang.Object") (class "java.lang.Object")) void)))
                                      (26 (aload_1))
                                      (27 (aconst_null))
                                      (28 (astore_1))
                                      (29 (getstatic (fieldCP "const__1" "clojure.core$derive$tf__4869" (class "clojure.lang.Var"))))
                                      (32 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (35 (checkcast (class "clojure.lang.IFn")))
                                      (38 (aload_2))
                                      (39 (aload_3))
                                      (40 (aconst_null))
                                      (41 (astore_3))
                                      (42 (checkcast (class "clojure.lang.IFn")))
                                      (45 (aload_2))
                                      (46 (aconst_null))
                                      (47 (astore_2))
                                      (48 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (53 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (58 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4))
                                      (63 (areturn))
                                      (endofcode 64))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$derive$tf__4869-class-table*
  (make-static-class-decls 
   *clojure.core$derive$tf__4869*))

(defconst *package-name-map* 
  ("clojure.core$derive$tf__4869" . "clojure"))

