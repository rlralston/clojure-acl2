; LispReader$DiscardReader-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:52 CDT 2014.
;

(defconst *clojure.lang.LispReader$DiscardReader*
 (make-class-def
      '(class "clojure.lang.LispReader$DiscardReader"
            "clojure.lang.AFn"
            (constant_pool)
            (fields)
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.AFn" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 4) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (checkcast (class "java.io.PushbackReader")))
                                      (4 (astore_3))
                                      (5 (aload_3))
                                      (6 (iconst_1))
                                      (7 (aconst_null))
                                      (8 (iconst_1))
                                      (9 (invokestatic
					(methodCP "read" "clojure.lang.LispReader" ((class "java.io.PushbackReader") boolean (class "java.lang.Object") boolean) (class "java.lang.Object"))))
                                      (12 (pop))
                                      (13 (aload_3))
                                      (14 (areturn))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *LispReader$DiscardReader-class-table*
  (make-static-class-decls 
   *clojure.lang.LispReader$DiscardReader*))

(defconst *package-name-map* 
  ("clojure.lang.LispReader$DiscardReader" . "clojure.lang"))

