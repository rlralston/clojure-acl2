; reflect$fn__8990-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:57 CDT 2014.
;

(defconst *clojure.reflect$fn__8990*
 (make-class-def
      '(class "clojure.reflect$fn__8990"
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
                                   (max_stack . 4) (max_locals . 4) (code_length . 38)
                                   (parsedcode
                                      (0 (new (class "clojure.reflect$fn__8990$G__8986__8993")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "clojure.reflect$fn__8990$G__8986__8993" () void)))
                                      (7 (astore_2))
                                      (8 (new (class "clojure.reflect$fn__8990$G__8985__8997")))
                                      (11 (dup))
                                      (12 (aload_2))
                                      (13 (aconst_null))
                                      (14 (astore_2))
                                      (15 (invokespecial
					(methodCP "<init>" "clojure.reflect$fn__8990$G__8985__8997" ((class "java.lang.Object")) void)))
                                      (18 (astore_3))
                                      (19 (aload_3))
                                      (20 (checkcast (class "clojure.lang.AFunction")))
                                      (23 (aload_1))
                                      (24 (aconst_null))
                                      (25 (astore_1))
                                      (26 (dup_x1))
                                      (27 (checkcast (class "clojure.lang.MethodImplCache")))
                                      (30 (putfield (fieldCP "__methodImplCache" "clojure.lang.AFunction" (class "clojure.lang.MethodImplCache"))))
                                      (33 (pop))
                                      (34 (aload_3))
                                      (35 (aconst_null))
                                      (36 (astore_3))
                                      (37 (areturn))
                                      (endofcode 38))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *reflect$fn__8990-class-table*
  (make-static-class-decls 
   *clojure.reflect$fn__8990*))

(defconst *package-name-map* 
  ("clojure.reflect$fn__8990" . "clojure"))

