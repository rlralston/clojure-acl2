; repl$pst$fn__8796-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:58 CDT 2014.
;

(defconst *clojure.repl$pst$fn__8796*
 (make-class-def
      '(class "clojure.repl$pst$fn__8796"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.lang.RestFn")
                        (STRING  "clojure.lang.AFn")
                        (STRING  "getClassName"))
            (fields
                        (field "const__0" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 0) (code_length . 24)
                                   (parsedcode
                                      (0 (iconst_2))
                                      (1 (anewarray (class "java.lang.Object")))
                                      (4 (dup))
                                      (5 (iconst_0))
                                      (6 (ldc 0))         ;;STRING:: "clojure.lang.RestFn"
                                      (8 (aastore))
                                      (9 (dup))
                                      (10 (iconst_1))
                                      (11 (ldc 1))        ;;STRING:: "clojure.lang.AFn"
                                      (13 (aastore))
                                      (14 (invokestatic
					(methodCP "create" "clojure.lang.PersistentHashSet" ((array (class "java.lang.Object"))) (class "clojure.lang.PersistentHashSet"))))
                                      (17 (checkcast (class "clojure.lang.AFn")))
                                      (20 (putstatic (fieldCP "const__0" "clojure.repl$pst$fn__8796" (class "clojure.lang.AFn"))))
                                      (23 (return))
                                      (endofcode 24))
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
                                   (max_stack . 3) (max_locals . 2) (code_length . 20)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.repl$pst$fn__8796" (class "clojure.lang.AFn"))))
                                      (3 (checkcast (class "clojure.lang.IFn")))
                                      (6 (aload_1))
                                      (7 (aconst_null))
                                      (8 (astore_1))
                                      (9 (ldc 2))         ;;STRING:: "getClassName"
                                      (11 (invokestatic
					(methodCP "invokeNoArgInstanceMember" "clojure.lang.Reflector" ((class "java.lang.Object") (class "java.lang.String")) (class "java.lang.Object"))))
                                      (14 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (19 (areturn))
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *repl$pst$fn__8796-class-table*
  (make-static-class-decls 
   *clojure.repl$pst$fn__8796*))

(defconst *package-name-map* 
  ("clojure.repl$pst$fn__8796" . "clojure"))

