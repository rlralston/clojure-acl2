; core$mk_bound_fn$fn__4692-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:44 CDT 2014.
;

(defconst *clojure.core$mk_bound_fn$fn__4692*
 (make-class-def
      '(class "clojure.core$mk_bound_fn$fn__4692"
            "clojure.lang.AFunction"
            (constant_pool)
            (fields
                        (field "const__0" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "key" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "sc" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "test" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 8)
                                   (parsedcode
                                      (0 (lconst_0))
                                      (1 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (4 (putstatic (fieldCP "const__0" "clojure.core$mk_bound_fn$fn__4692" (class "java.lang.Object"))))
                                      (7 (return))
                                      (endofcode 8))
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
                                      (6 (putfield (fieldCP "key" "clojure.core$mk_bound_fn$fn__4692" (class "java.lang.Object"))))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "sc" "clojure.core$mk_bound_fn$fn__4692" (class "java.lang.Object"))))
                                      (14 (aload_0))
                                      (15 (aload_3))
                                      (16 (putfield (fieldCP "test" "clojure.core$mk_bound_fn$fn__4692" (class "java.lang.Object"))))
                                      (19 (return))
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 2) (code_length . 58)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "test" "clojure.core$mk_bound_fn$fn__4692" (class "java.lang.Object"))))
                                      (4 (checkcast (class "clojure.lang.IFn")))
                                      (7 (aload_0))
                                      (8 (getfield (fieldCP "sc" "clojure.core$mk_bound_fn$fn__4692" (class "java.lang.Object"))))
                                      (11 (checkcast (class "clojure.lang.Sorted")))
                                      (14 (invokeinterface
					(methodCP "comparator" "clojure.lang.Sorted" () (class "java.util.Comparator")) 1))
                                      (19 (checkcast (class "java.util.Comparator")))
                                      (22 (aload_0))
                                      (23 (getfield (fieldCP "sc" "clojure.core$mk_bound_fn$fn__4692" (class "java.lang.Object"))))
                                      (26 (checkcast (class "clojure.lang.Sorted")))
                                      (29 (aload_1))
                                      (30 (aconst_null))
                                      (31 (astore_1))
                                      (32 (invokeinterface
					(methodCP "entryKey" "clojure.lang.Sorted" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (37 (aload_0))
                                      (38 (getfield (fieldCP "key" "clojure.core$mk_bound_fn$fn__4692" (class "java.lang.Object"))))
                                      (41 (invokeinterface
					(methodCP "compare" "java.util.Comparator" ((class "java.lang.Object") (class "java.lang.Object")) int) 3))
                                      (46 (invokestatic
					(methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer"))))
                                      (49 (getstatic (fieldCP "const__0" "clojure.core$mk_bound_fn$fn__4692" (class "java.lang.Object"))))
                                      (52 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (57 (areturn))
                                      (endofcode 58))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$mk_bound_fn$fn__4692-class-table*
  (make-static-class-decls 
   *clojure.core$mk_bound_fn$fn__4692*))

(defconst *package-name-map* 
  ("clojure.core$mk_bound_fn$fn__4692" . "clojure"))
