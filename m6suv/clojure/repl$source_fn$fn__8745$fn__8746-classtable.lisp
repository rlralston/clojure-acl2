; repl$source_fn$fn__8745$fn__8746-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:58 CDT 2014.
;

(defconst *clojure.repl$source_fn$fn__8745$fn__8746*
 (make-class-def
      '(class "clojure.repl$source_fn$fn__8745$fn__8746"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "read"))
            (fields
                        (field "this" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 0) (max_locals . 0) (code_length . 1)
                                   (parsedcode
                                      (0 (return))
                                      (endofcode 1))
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
                                      (6 (putfield (fieldCP "this" "clojure.repl$source_fn$fn__8745$fn__8746" (class "java.lang.Object"))))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "this" "clojure.repl$source_fn$fn__8745$fn__8746" (class "java.lang.Object"))))
                                      (4 (ldc 0))         ;;STRING:: "read"
                                      (6 (invokestatic
					(methodCP "invokeNoArgInstanceMember" "clojure.lang.Reflector" ((class "java.lang.Object") (class "java.lang.String")) (class "java.lang.Object"))))
                                      (9 (areturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *repl$source_fn$fn__8745$fn__8746-class-table*
  (make-static-class-decls 
   *clojure.repl$source_fn$fn__8745$fn__8746*))

(defconst *package-name-map* 
  ("clojure.repl$source_fn$fn__8745$fn__8746" . "clojure"))

