; core$tree_seq-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:45 CDT 2014.
;

(defconst *clojure.core$tree_seq*
 (make-class-def
      '(class "clojure.core$tree_seq"
            "clojure.lang.AFunction"
            (constant_pool)
            (fields)
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
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 5) (code_length . 32)
                                   (parsedcode
                                      (0 (new (class "clojure.core$tree_seq$walk__4647")))
                                      (3 (dup))
                                      (4 (aload_1))
                                      (5 (aconst_null))
                                      (6 (astore_1))
                                      (7 (aload_2))
                                      (8 (aconst_null))
                                      (9 (astore_2))
                                      (10 (invokespecial
					(methodCP "<init>" "clojure.core$tree_seq$walk__4647" ((class "java.lang.Object") (class "java.lang.Object")) void)))
                                      (13 (astore 4))
                                      (15 (aload 4))
                                      (17 (aconst_null))
                                      (18 (astore 4))
                                      (20 (checkcast (class "clojure.lang.IFn")))
                                      (23 (aload_3))
                                      (24 (aconst_null))
                                      (25 (astore_3))
                                      (26 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (31 (areturn))
                                      (endofcode 32))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$tree_seq-class-table*
  (make-static-class-decls 
   *clojure.core$tree_seq*))

(defconst *package-name-map* 
  ("clojure.core$tree_seq" . "clojure"))

