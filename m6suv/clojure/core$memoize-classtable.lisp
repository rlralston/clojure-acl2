; core$memoize-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:44 CDT 2014.
;

(defconst *clojure.core$memoize*
 (make-class-def
      '(class "clojure.core$memoize"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "atom"))
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
                                      (2 (ldc 1))         ;;STRING:: "atom"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$memoize" (class "clojure.lang.Var"))))
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
                                   (max_stack . 5) (max_locals . 3) (code_length . 32)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$memoize" (class "clojure.lang.Var"))))
                                      (3 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (6 (checkcast (class "clojure.lang.IFn")))
                                      (9 (getstatic (fieldCP "EMPTY" "clojure.lang.PersistentArrayMap" (class "clojure.lang.PersistentArrayMap"))))
                                      (12 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (17 (astore_2))
                                      (18 (new (class "clojure.core$memoize$fn__5049")))
                                      (21 (dup))
                                      (22 (aload_2))
                                      (23 (aconst_null))
                                      (24 (astore_2))
                                      (25 (aload_1))
                                      (26 (aconst_null))
                                      (27 (astore_1))
                                      (28 (invokespecial
					(methodCP "<init>" "clojure.core$memoize$fn__5049" ((class "java.lang.Object") (class "java.lang.Object")) void)))
                                      (31 (areturn))
                                      (endofcode 32))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$memoize-class-table*
  (make-static-class-decls 
   *clojure.core$memoize*))

(defconst *package-name-map* 
  ("clojure.core$memoize" . "clojure"))

