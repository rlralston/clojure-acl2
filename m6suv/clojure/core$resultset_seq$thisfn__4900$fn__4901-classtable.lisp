; core$resultset_seq$thisfn__4900$fn__4901-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:45 CDT 2014.
;

(defconst *clojure.core$resultset_seq$thisfn__4900$fn__4901*
 (make-class-def
      '(class "clojure.core$resultset_seq$thisfn__4900$fn__4901"
            "clojure.lang.AFunction"
            (constant_pool)
            (fields
                        (field "thisfn" (class "java.lang.Object") (accessflags  *class* ) -1))
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
                                      (6 (putfield (fieldCP "thisfn" "clojure.core$resultset_seq$thisfn__4900$fn__4901" (class "java.lang.Object"))))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 13)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "thisfn" "clojure.core$resultset_seq$thisfn__4900$fn__4901" (class "java.lang.Object"))))
                                      (4 (checkcast (class "clojure.lang.IFn")))
                                      (7 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1))
                                      (12 (areturn))
                                      (endofcode 13))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$resultset_seq$thisfn__4900$fn__4901-class-table*
  (make-static-class-decls 
   *clojure.core$resultset_seq$thisfn__4900$fn__4901*))

(defconst *package-name-map* 
  ("clojure.core$resultset_seq$thisfn__4900$fn__4901" . "clojure"))

