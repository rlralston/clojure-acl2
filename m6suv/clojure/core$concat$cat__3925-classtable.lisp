; core$concat$cat__3925-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:41 CDT 2014.
;

(defconst *clojure.core$concat$cat__3925*
 (make-class-def
      '(class "clojure.core$concat$cat__3925"
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
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 7) (max_locals . 3) (code_length . 25)
                                   (parsedcode
                                      (0 (new (class "clojure.lang.LazySeq")))
                                      (3 (dup))
                                      (4 (new (class "clojure.core$concat$cat__3925$fn__3926")))
                                      (7 (dup))
                                      (8 (aload_2))
                                      (9 (aconst_null))
                                      (10 (astore_2))
                                      (11 (aload_1))
                                      (12 (aconst_null))
                                      (13 (astore_1))
                                      (14 (aload_0))
                                      (15 (invokespecial
					(methodCP "<init>" "clojure.core$concat$cat__3925$fn__3926" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) void)))
                                      (18 (checkcast (class "clojure.lang.IFn")))
                                      (21 (invokespecial
					(methodCP "<init>" "clojure.lang.LazySeq" ((class "clojure.lang.IFn")) void)))
                                      (24 (areturn))
                                      (endofcode 25))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$concat$cat__3925-class-table*
  (make-static-class-decls 
   *clojure.core$concat$cat__3925*))

(defconst *package-name-map* 
  ("clojure.core$concat$cat__3925" . "clojure"))

