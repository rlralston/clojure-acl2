; core$sort_by$fn__4299-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:45 CDT 2014.
;

(defconst *clojure.core$sort_by$fn__4299*
 (make-class-def
      '(class "clojure.core$sort_by$fn__4299"
            "clojure.lang.AFunction"
            (constant_pool)
            (fields
                        (field "keyfn" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "comp" (class "java.lang.Object") (accessflags  *class* ) -1))
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
                                      (6 (putfield (fieldCP "keyfn" "clojure.core$sort_by$fn__4299" (class "java.lang.Object"))))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "comp" "clojure.core$sort_by$fn__4299" (class "java.lang.Object"))))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 3) (code_length . 46)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "comp" "clojure.core$sort_by$fn__4299" (class "java.lang.Object"))))
                                      (4 (checkcast (class "java.util.Comparator")))
                                      (7 (aload_0))
                                      (8 (getfield (fieldCP "keyfn" "clojure.core$sort_by$fn__4299" (class "java.lang.Object"))))
                                      (11 (checkcast (class "clojure.lang.IFn")))
                                      (14 (aload_1))
                                      (15 (aconst_null))
                                      (16 (astore_1))
                                      (17 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (22 (aload_0))
                                      (23 (getfield (fieldCP "keyfn" "clojure.core$sort_by$fn__4299" (class "java.lang.Object"))))
                                      (26 (checkcast (class "clojure.lang.IFn")))
                                      (29 (aload_2))
                                      (30 (aconst_null))
                                      (31 (astore_2))
                                      (32 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (37 (invokeinterface
					(methodCP "compare" "java.util.Comparator" ((class "java.lang.Object") (class "java.lang.Object")) int) 3))
                                      (42 (invokestatic
					(methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer"))))
                                      (45 (areturn))
                                      (endofcode 46))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$sort_by$fn__4299-class-table*
  (make-static-class-decls 
   *clojure.core$sort_by$fn__4299*))

(defconst *package-name-map* 
  ("clojure.core$sort_by$fn__4299" . "clojure"))

