; core$bound_fn_STAR_-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:40 CDT 2014.
;

(defconst *clojure.core$bound_fn_STAR_*
 (make-class-def
      '(class "clojure.core$bound_fn_STAR_"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "get-thread-bindings"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 14)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "get-thread-bindings"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$bound_fn_STAR_" (class "clojure.lang.Var"))))
                                      (13 (return))
                                      (endofcode 14))
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
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 3) (code_length . 29)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$bound_fn_STAR_" (class "clojure.lang.Var"))))
                                      (3 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (6 (checkcast (class "clojure.lang.IFn")))
                                      (9 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1))
                                      (14 (astore_2))
                                      (15 (new (class "clojure.core$bound_fn_STAR_$fn__4102")))
                                      (18 (dup))
                                      (19 (aload_2))
                                      (20 (aconst_null))
                                      (21 (astore_2))
                                      (22 (aload_1))
                                      (23 (aconst_null))
                                      (24 (astore_1))
                                      (25 (invokespecial
					(methodCP "<init>" "clojure.core$bound_fn_STAR_$fn__4102" ((class "java.lang.Object") (class "java.lang.Object")) void)))
                                      (28 (areturn))
                                      (endofcode 29))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$bound_fn_STAR_-class-table*
  (make-static-class-decls 
   *clojure.core$bound_fn_STAR_*))

(defconst *package-name-map* 
  ("clojure.core$bound_fn_STAR_" . "clojure"))
