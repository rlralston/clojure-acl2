; APersistentMap$3$1-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:50 CDT 2014.
;

(defconst *clojure.lang.APersistentMap$3$1*
 (make-class-def
      '(class "clojure.lang.APersistentMap$3$1"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "val$mi" (class "java.util.Iterator") (accessflags  *class*  *final* ) -1)
                        (field "this$1" (class "clojure.lang.APersistentMap$3") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "clojure.lang.APersistentMap$3") (class "java.util.Iterator"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$1" "clojure.lang.APersistentMap$3$1" (class "clojure.lang.APersistentMap$3"))))
                                      (5 (aload_0))
                                      (6 (aload_2))
                                      (7 (putfield (fieldCP "val$mi" "clojure.lang.APersistentMap$3$1" (class "java.util.Iterator"))))
                                      (10 (aload_0))
                                      (11 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hasNext"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "val$mi" "clojure.lang.APersistentMap$3$1" (class "java.util.Iterator"))))
                                      (4 (invokeinterface
					(methodCP "hasNext" "java.util.Iterator" () boolean) 1))
                                      (9 (ireturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "next"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 20)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "val$mi" "clojure.lang.APersistentMap$3$1" (class "java.util.Iterator"))))
                                      (4 (invokeinterface
					(methodCP "next" "java.util.Iterator" () (class "java.lang.Object")) 1))
                                      (9 (checkcast (class "java.util.Map$Entry")))
                                      (12 (astore_1))
                                      (13 (aload_1))
                                      (14 (invokeinterface
					(methodCP "getValue" "java.util.Map$Entry" () (class "java.lang.Object")) 1))
                                      (19 (areturn))
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap )))
                        (method "remove"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (new (class "java.lang.UnsupportedOperationException")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.lang.UnsupportedOperationException" () void)))
                                      (7 (athrow))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.util.Iterator")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "InnerClasses")))))


(defconst *APersistentMap$3$1-class-table*
  (make-static-class-decls 
   *clojure.lang.APersistentMap$3$1*))

(defconst *package-name-map* 
  ("clojure.lang.APersistentMap$3$1" . "clojure.lang"))

