; APersistentMap$3-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:50 CDT 2014.
;

(defconst *clojure.lang.APersistentMap$3*
 (make-class-def
      '(class "clojure.lang.APersistentMap$3"
            "java.util.AbstractCollection"
            (constant_pool)
            (fields
                        (field "this$0" (class "clojure.lang.APersistentMap") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "clojure.lang.APersistentMap"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "clojure.lang.APersistentMap$3" (class "clojure.lang.APersistentMap"))))
                                      (5 (aload_0))
                                      (6 (invokespecial
					(methodCP "<init>" "java.util.AbstractCollection" () void)))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "iterator"
                              (parameters )
                              (returntype . (class "java.util.Iterator"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 18)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "this$0" "clojure.lang.APersistentMap$3" (class "clojure.lang.APersistentMap"))))
                                      (4 (invokevirtual
					(methodCP "iterator" "clojure.lang.APersistentMap" () (class "java.util.Iterator"))))
                                      (7 (astore_1))
                                      (8 (new (class "clojure.lang.APersistentMap$3$1")))
                                      (11 (dup))
                                      (12 (aload_0))
                                      (13 (aload_1))
                                      (14 (invokespecial
					(methodCP "<init>" "clojure.lang.APersistentMap$3$1" ((class "clojure.lang.APersistentMap$3") (class "java.util.Iterator")) void)))
                                      (17 (areturn))
                                      (endofcode 18))
                                   (Exceptions )
                                   (StackMap )))
                        (method "size"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "this$0" "clojure.lang.APersistentMap$3" (class "clojure.lang.APersistentMap"))))
                                      (4 (invokevirtual
					(methodCP "count" "clojure.lang.APersistentMap" () int)))
                                      (7 (ireturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "InnerClasses")))))


(defconst *APersistentMap$3-class-table*
  (make-static-class-decls 
   *clojure.lang.APersistentMap$3*))

(defconst *package-name-map* 
  ("clojure.lang.APersistentMap$3" . "clojure.lang"))

