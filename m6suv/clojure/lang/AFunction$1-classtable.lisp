; AFunction$1-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:50 CDT 2014.
;

(defconst *clojure.lang.AFunction$1*
 (make-class-def
      '(class "clojure.lang.AFunction$1"
            "clojure.lang.RestFn"
            (constant_pool)
            (fields
                        (field "val$meta" (class "clojure.lang.IPersistentMap") (accessflags  *class*  *final* ) -1)
                        (field "this$0" (class "clojure.lang.AFunction") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "clojure.lang.AFunction") (class "clojure.lang.IPersistentMap"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "clojure.lang.AFunction$1" (class "clojure.lang.AFunction"))))
                                      (5 (aload_0))
                                      (6 (aload_2))
                                      (7 (putfield (fieldCP "val$meta" "clojure.lang.AFunction$1" (class "clojure.lang.IPersistentMap"))))
                                      (10 (aload_0))
                                      (11 (invokespecial
					(methodCP "<init>" "clojure.lang.RestFn" () void)))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "doInvoke"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 12)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "this$0" "clojure.lang.AFunction$1" (class "clojure.lang.AFunction"))))
                                      (4 (aload_1))
                                      (5 (checkcast (class "clojure.lang.ISeq")))
                                      (8 (invokevirtual
					(methodCP "applyTo" "clojure.lang.AFunction" ((class "clojure.lang.ISeq")) (class "java.lang.Object"))))
                                      (11 (areturn))
                                      (endofcode 12))
                                   (Exceptions )
                                   (StackMap )))
                        (method "meta"
                              (parameters )
                              (returntype . (class "clojure.lang.IPersistentMap"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "val$meta" "clojure.lang.AFunction$1" (class "clojure.lang.IPersistentMap"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "withMeta"
                              (parameters (class "clojure.lang.IPersistentMap"))
                              (returntype . (class "clojure.lang.IObj"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "this$0" "clojure.lang.AFunction$1" (class "clojure.lang.AFunction"))))
                                      (4 (aload_1))
                                      (5 (invokevirtual
					(methodCP "withMeta" "clojure.lang.AFunction" ((class "clojure.lang.IPersistentMap")) (class "clojure.lang.IObj"))))
                                      (8 (areturn))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getRequiredArity"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 2)
                                   (parsedcode
                                      (0 (iconst_0))
                                      (1 (ireturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "InnerClasses")))))


(defconst *AFunction$1-class-table*
  (make-static-class-decls 
   *clojure.lang.AFunction$1*))

(defconst *package-name-map* 
  ("clojure.lang.AFunction$1" . "clojure.lang"))

