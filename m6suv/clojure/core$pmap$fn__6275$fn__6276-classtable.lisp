; core$pmap$fn__6275$fn__6276-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:45 CDT 2014.
;

(defconst *clojure.core$pmap$fn__6275$fn__6276*
 (make-class-def
      '(class "clojure.core$pmap$fn__6275$fn__6276"
            "clojure.lang.AFunction"
            (constant_pool)
            (fields
                        (field "f" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "p1__6273_SHARP_" (class "java.lang.Object") (accessflags  *class* ) -1))
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
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.AFunction" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "f" "clojure.core$pmap$fn__6275$fn__6276" (class "java.lang.Object"))))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "p1__6273_SHARP_" "clojure.core$pmap$fn__6275$fn__6276" (class "java.lang.Object"))))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 1) (code_length . 27)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "f" "clojure.core$pmap$fn__6275$fn__6276" (class "java.lang.Object"))))
                                      (4 (aload_0))
                                      (5 (aconst_null))
                                      (6 (putfield (fieldCP "f" "clojure.core$pmap$fn__6275$fn__6276" (class "java.lang.Object"))))
                                      (9 (checkcast (class "clojure.lang.IFn")))
                                      (12 (aload_0))
                                      (13 (getfield (fieldCP "p1__6273_SHARP_" "clojure.core$pmap$fn__6275$fn__6276" (class "java.lang.Object"))))
                                      (16 (aload_0))
                                      (17 (aconst_null))
                                      (18 (putfield (fieldCP "p1__6273_SHARP_" "clojure.core$pmap$fn__6275$fn__6276" (class "java.lang.Object"))))
                                      (21 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (26 (areturn))
                                      (endofcode 27))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$pmap$fn__6275$fn__6276-class-table*
  (make-static-class-decls 
   *clojure.core$pmap$fn__6275$fn__6276*))

(defconst *package-name-map* 
  ("clojure.core$pmap$fn__6275$fn__6276" . "clojure"))

