; core$destructure$pb__4541$pmap__4544$fn__4547-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:41 CDT 2014.
;

(defconst *clojure.core$destructure$pb__4541$pmap__4544$fn__4547*
 (make-class-def
      '(class "clojure.core$destructure$pb__4541$pmap__4544$fn__4547"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "reduce1")
                        (STRING  "dissoc")
                        (STRING  "key"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 40)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "reduce1"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$destructure$pb__4541$pmap__4544$fn__4547" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "dissoc"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$destructure$pb__4541$pmap__4544$fn__4547" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "key"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.core$destructure$pb__4541$pmap__4544$fn__4547" (class "clojure.lang.Var"))))
                                      (39 (return))
                                      (endofcode 40))
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
                                   (max_stack . 6) (max_locals . 3) (code_length . 81)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$destructure$pb__4541$pmap__4544$fn__4547" (class "clojure.lang.Var"))))
                                      (3 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (6 (checkcast (class "clojure.lang.IFn")))
                                      (9 (new (class "clojure.core$destructure$pb__4541$pmap__4544$fn__4547$fn__4548")))
                                      (12 (dup))
                                      (13 (aload_2))
                                      (14 (invokespecial
					(methodCP "<init>" "clojure.core$destructure$pb__4541$pmap__4544$fn__4547$fn__4548" ((class "java.lang.Object")) void)))
                                      (17 (getstatic (fieldCP "const__1" "clojure.core$destructure$pb__4541$pmap__4544$fn__4547" (class "clojure.lang.Var"))))
                                      (20 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (23 (checkcast (class "clojure.lang.IFn")))
                                      (26 (aload_1))
                                      (27 (getstatic (fieldCP "const__2" "clojure.core$destructure$pb__4541$pmap__4544$fn__4547" (class "clojure.lang.Var"))))
                                      (30 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (33 (checkcast (class "clojure.lang.IFn")))
                                      (36 (aload_2))
                                      (37 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (42 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (47 (getstatic (fieldCP "const__2" "clojure.core$destructure$pb__4541$pmap__4544$fn__4547" (class "clojure.lang.Var"))))
                                      (50 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (53 (checkcast (class "clojure.lang.IFn")))
                                      (56 (aload_2))
                                      (57 (aconst_null))
                                      (58 (astore_2))
                                      (59 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (64 (checkcast (class "clojure.lang.IFn")))
                                      (67 (aload_1))
                                      (68 (aconst_null))
                                      (69 (astore_1))
                                      (70 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (75 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4))
                                      (80 (areturn))
                                      (endofcode 81))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$destructure$pb__4541$pmap__4544$fn__4547-class-table*
  (make-static-class-decls 
   *clojure.core$destructure$pb__4541$pmap__4544$fn__4547*))

(defconst *package-name-map* 
  ("clojure.core$destructure$pb__4541$pmap__4544$fn__4547" . "clojure"))

