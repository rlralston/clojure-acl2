; core$drop_while$fn__4253-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:41 CDT 2014.
;

(defconst *clojure.core$drop_while$fn__4253*
 (make-class-def
      '(class "clojure.core$drop_while$fn__4253"
            "clojure.lang.AFunction"
            (constant_pool)
            (fields
                        (field "pred" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "step" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "coll" (class "java.lang.Object") (accessflags  *class* ) -1))
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
                                      (6 (putfield (fieldCP "pred" "clojure.core$drop_while$fn__4253" (class "java.lang.Object"))))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "step" "clojure.core$drop_while$fn__4253" (class "java.lang.Object"))))
                                      (14 (aload_0))
                                      (15 (aload_3))
                                      (16 (putfield (fieldCP "coll" "clojure.core$drop_while$fn__4253" (class "java.lang.Object"))))
                                      (19 (return))
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 1) (code_length . 36)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "step" "clojure.core$drop_while$fn__4253" (class "java.lang.Object"))))
                                      (4 (aload_0))
                                      (5 (aconst_null))
                                      (6 (putfield (fieldCP "step" "clojure.core$drop_while$fn__4253" (class "java.lang.Object"))))
                                      (9 (checkcast (class "clojure.lang.IFn")))
                                      (12 (aload_0))
                                      (13 (getfield (fieldCP "pred" "clojure.core$drop_while$fn__4253" (class "java.lang.Object"))))
                                      (16 (aload_0))
                                      (17 (aconst_null))
                                      (18 (putfield (fieldCP "pred" "clojure.core$drop_while$fn__4253" (class "java.lang.Object"))))
                                      (21 (aload_0))
                                      (22 (getfield (fieldCP "coll" "clojure.core$drop_while$fn__4253" (class "java.lang.Object"))))
                                      (25 (aload_0))
                                      (26 (aconst_null))
                                      (27 (putfield (fieldCP "coll" "clojure.core$drop_while$fn__4253" (class "java.lang.Object"))))
                                      (30 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (35 (areturn))
                                      (endofcode 36))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$drop_while$fn__4253-class-table*
  (make-static-class-decls 
   *clojure.core$drop_while$fn__4253*))

(defconst *package-name-map* 
  ("clojure.core$drop_while$fn__4253" . "clojure"))

