; core$cycle-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:41 CDT 2014.
;

(defconst *clojure.core$cycle*
 (make-class-def
      '(class "clojure.core$cycle"
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
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 2) (code_length . 21)
                                   (parsedcode
                                      (0 (new (class "clojure.lang.LazySeq")))
                                      (3 (dup))
                                      (4 (new (class "clojure.core$cycle$fn__4256")))
                                      (7 (dup))
                                      (8 (aload_1))
                                      (9 (aconst_null))
                                      (10 (astore_1))
                                      (11 (invokespecial
					(methodCP "<init>" "clojure.core$cycle$fn__4256" ((class "java.lang.Object")) void)))
                                      (14 (checkcast (class "clojure.lang.IFn")))
                                      (17 (invokespecial
					(methodCP "<init>" "clojure.lang.LazySeq" ((class "clojure.lang.IFn")) void)))
                                      (20 (areturn))
                                      (endofcode 21))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$cycle-class-table*
  (make-static-class-decls 
   *clojure.core$cycle*))

(defconst *package-name-map* 
  ("clojure.core$cycle" . "clojure"))

