; EdnReader$VectorReader-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:51 CDT 2014.
;

(defconst *clojure.lang.EdnReader$VectorReader*
 (make-class-def
      '(class "clojure.lang.EdnReader$VectorReader"
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
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 5) (code_length . 19)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (checkcast (class "java.io.PushbackReader")))
                                      (4 (astore 4))
                                      (6 (bipush 93))
                                      (8 (aload 4))
                                      (10 (iconst_1))
                                      (11 (aload_3))
                                      (12 (invokestatic
					(methodCP "readDelimitedList" "clojure.lang.EdnReader" (char (class "java.io.PushbackReader") boolean (class "java.lang.Object")) (class "java.util.List"))))
                                      (15 (invokestatic
					(methodCP "create" "clojure.lang.LazilyPersistentVector" ((class "java.util.Collection")) (class "clojure.lang.IPersistentVector"))))
                                      (18 (areturn))
                                      (endofcode 19))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *EdnReader$VectorReader-class-table*
  (make-static-class-decls 
   *clojure.lang.EdnReader$VectorReader*))

(defconst *package-name-map* 
  ("clojure.lang.EdnReader$VectorReader" . "clojure.lang"))

