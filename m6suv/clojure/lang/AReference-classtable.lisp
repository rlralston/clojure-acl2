; AReference-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:50 CDT 2014.
;

(defconst *clojure.lang.AReference*
 (make-class-def
      '(class "clojure.lang.AReference"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "_meta" (class "clojure.lang.IPersistentMap") (accessflags  *class*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aconst_null))
                                      (2 (invokespecial
					(methodCP "<init>" "clojure.lang.AReference" ((class "clojure.lang.IPersistentMap")) void)))
                                      (5 (return))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "clojure.lang.IPersistentMap"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "_meta" "clojure.lang.AReference" (class "clojure.lang.IPersistentMap"))))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "meta"
                              (parameters )
                              (returntype . (class "clojure.lang.IPersistentMap"))
                              (accessflags  *class*  *public*  *super*  *synchronized* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "_meta" "clojure.lang.AReference" (class "clojure.lang.IPersistentMap"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "alterMeta"
                              (parameters (class "clojure.lang.IFn") (class "clojure.lang.ISeq"))
                              (returntype . (class "clojure.lang.IPersistentMap"))
                              (accessflags  *class*  *public*  *super*  *synchronized* )
                              (code
                                   (max_stack . 6) (max_locals . 3) (code_length . 30)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (new (class "clojure.lang.Cons")))
                                      (5 (dup))
                                      (6 (aload_0))
                                      (7 (getfield (fieldCP "_meta" "clojure.lang.AReference" (class "clojure.lang.IPersistentMap"))))
                                      (10 (aload_2))
                                      (11 (invokespecial
					(methodCP "<init>" "clojure.lang.Cons" ((class "java.lang.Object") (class "clojure.lang.ISeq")) void)))
                                      (14 (invokeinterface
					(methodCP "applyTo" "clojure.lang.IFn" ((class "clojure.lang.ISeq")) (class "java.lang.Object")) 2))
                                      (19 (checkcast (class "clojure.lang.IPersistentMap")))
                                      (22 (putfield (fieldCP "_meta" "clojure.lang.AReference" (class "clojure.lang.IPersistentMap"))))
                                      (25 (aload_0))
                                      (26 (getfield (fieldCP "_meta" "clojure.lang.AReference" (class "clojure.lang.IPersistentMap"))))
                                      (29 (areturn))
                                      (endofcode 30))
                                   (Exceptions )
                                   (StackMap )))
                        (method "resetMeta"
                              (parameters (class "clojure.lang.IPersistentMap"))
                              (returntype . (class "clojure.lang.IPersistentMap"))
                              (accessflags  *class*  *public*  *super*  *synchronized* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 7)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "_meta" "clojure.lang.AReference" (class "clojure.lang.IPersistentMap"))))
                                      (5 (aload_1))
                                      (6 (areturn))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "clojure.lang.IReference")
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *AReference-class-table*
  (make-static-class-decls 
   *clojure.lang.AReference*))

(defconst *package-name-map* 
  ("clojure.lang.AReference" . "clojure.lang"))

