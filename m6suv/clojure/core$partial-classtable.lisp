; core$partial-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:45 CDT 2014.
;

(defconst *clojure.core$partial*
 (make-class-def
      '(class "clojure.core$partial"
            "clojure.lang.RestFn"
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
					(methodCP "<init>" "clojure.lang.RestFn" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "doInvoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 8) (max_locals . 6) (code_length . 27)
                                   (parsedcode
                                      (0 (new (class "clojure.core$partial$fn__4196")))
                                      (3 (dup))
                                      (4 (aload 5))
                                      (6 (aconst_null))
                                      (7 (astore 5))
                                      (9 (aload_1))
                                      (10 (aconst_null))
                                      (11 (astore_1))
                                      (12 (aload_2))
                                      (13 (aconst_null))
                                      (14 (astore_2))
                                      (15 (aload_3))
                                      (16 (aconst_null))
                                      (17 (astore_3))
                                      (18 (aload 4))
                                      (20 (aconst_null))
                                      (21 (astore 4))
                                      (23 (invokespecial
					(methodCP "<init>" "clojure.core$partial$fn__4196" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) void)))
                                      (26 (areturn))
                                      (endofcode 27))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 7) (max_locals . 5) (code_length . 22)
                                   (parsedcode
                                      (0 (new (class "clojure.core$partial$fn__4194")))
                                      (3 (dup))
                                      (4 (aload_1))
                                      (5 (aconst_null))
                                      (6 (astore_1))
                                      (7 (aload_2))
                                      (8 (aconst_null))
                                      (9 (astore_2))
                                      (10 (aload 4))
                                      (12 (aconst_null))
                                      (13 (astore 4))
                                      (15 (aload_3))
                                      (16 (aconst_null))
                                      (17 (astore_3))
                                      (18 (invokespecial
					(methodCP "<init>" "clojure.core$partial$fn__4194" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) void)))
                                      (21 (areturn))
                                      (endofcode 22))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 4) (code_length . 17)
                                   (parsedcode
                                      (0 (new (class "clojure.core$partial$fn__4192")))
                                      (3 (dup))
                                      (4 (aload_2))
                                      (5 (aconst_null))
                                      (6 (astore_2))
                                      (7 (aload_3))
                                      (8 (aconst_null))
                                      (9 (astore_3))
                                      (10 (aload_1))
                                      (11 (aconst_null))
                                      (12 (astore_1))
                                      (13 (invokespecial
					(methodCP "<init>" "clojure.core$partial$fn__4192" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) void)))
                                      (16 (areturn))
                                      (endofcode 17))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 3) (code_length . 14)
                                   (parsedcode
                                      (0 (new (class "clojure.core$partial$fn__4190")))
                                      (3 (dup))
                                      (4 (aload_1))
                                      (5 (aconst_null))
                                      (6 (astore_1))
                                      (7 (aload_2))
                                      (8 (aconst_null))
                                      (9 (astore_2))
                                      (10 (invokespecial
					(methodCP "<init>" "clojure.core$partial$fn__4190" ((class "java.lang.Object") (class "java.lang.Object")) void)))
                                      (13 (areturn))
                                      (endofcode 14))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 4)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (aconst_null))
                                      (2 (astore_1))
                                      (3 (areturn))
                                      (endofcode 4))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getRequiredArity"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 2)
                                   (parsedcode
                                      (0 (iconst_4))
                                      (1 (ireturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$partial-class-table*
  (make-static-class-decls 
   *clojure.core$partial*))

(defconst *package-name-map* 
  ("clojure.core$partial" . "clojure"))

