; APersistentMap$KeySeq-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:50 CDT 2014.
;

(defconst *clojure.lang.APersistentMap$KeySeq*
 (make-class-def
      '(class "clojure.lang.APersistentMap$KeySeq"
            "clojure.lang.ASeq"
            (constant_pool)
            (fields
                        (field "seq" (class "clojure.lang.ISeq") (accessflags  *class* ) -1))
            (methods
                        (method "create"
                              (parameters (class "clojure.lang.ISeq"))
                              (returntype . (class "clojure.lang.APersistentMap$KeySeq"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (ifnonnull 6))  ;;to TAG_0
                                      (4 (aconst_null)) 
                                      (5 (areturn)) 
                                      (6 (new (class "clojure.lang.APersistentMap$KeySeq"))) ;;at TAG_0
                                      (9 (dup)) 
                                      (10 (aload_0)) 
                                      (11 (invokespecial (methodCP "<init>" "clojure.lang.APersistentMap$KeySeq" ((class "clojure.lang.ISeq")) void))) 
                                      (14 (areturn)) 
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "clojure.lang.ISeq"))
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.ASeq" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "seq" "clojure.lang.APersistentMap$KeySeq" (class "clojure.lang.ISeq"))))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "clojure.lang.IPersistentMap") (class "clojure.lang.ISeq"))
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokespecial
					(methodCP "<init>" "clojure.lang.ASeq" ((class "clojure.lang.IPersistentMap")) void)))
                                      (5 (aload_0))
                                      (6 (aload_2))
                                      (7 (putfield (fieldCP "seq" "clojure.lang.APersistentMap$KeySeq" (class "clojure.lang.ISeq"))))
                                      (10 (return))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "first"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 18)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "seq" "clojure.lang.APersistentMap$KeySeq" (class "clojure.lang.ISeq"))))
                                      (4 (invokeinterface
					(methodCP "first" "clojure.lang.ISeq" () (class "java.lang.Object")) 1))
                                      (9 (checkcast (class "java.util.Map$Entry")))
                                      (12 (invokeinterface
					(methodCP "getKey" "java.util.Map$Entry" () (class "java.lang.Object")) 1))
                                      (17 (areturn))
                                      (endofcode 18))
                                   (Exceptions )
                                   (StackMap )))
                        (method "next"
                              (parameters )
                              (returntype . (class "clojure.lang.ISeq"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 13)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "seq" "clojure.lang.APersistentMap$KeySeq" (class "clojure.lang.ISeq"))))
                                      (4 (invokeinterface
					(methodCP "next" "clojure.lang.ISeq" () (class "clojure.lang.ISeq")) 1))
                                      (9 (invokestatic
					(methodCP "create" "clojure.lang.APersistentMap$KeySeq" ((class "clojure.lang.ISeq")) (class "clojure.lang.APersistentMap$KeySeq"))))
                                      (12 (areturn))
                                      (endofcode 13))
                                   (Exceptions )
                                   (StackMap )))
                        (method "withMeta"
                              (parameters (class "clojure.lang.IPersistentMap"))
                              (returntype . (class "clojure.lang.APersistentMap$KeySeq"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 13)
                                   (parsedcode
                                      (0 (new (class "clojure.lang.APersistentMap$KeySeq")))
                                      (3 (dup))
                                      (4 (aload_1))
                                      (5 (aload_0))
                                      (6 (getfield (fieldCP "seq" "clojure.lang.APersistentMap$KeySeq" (class "clojure.lang.ISeq"))))
                                      (9 (invokespecial
					(methodCP "<init>" "clojure.lang.APersistentMap$KeySeq" ((class "clojure.lang.IPersistentMap") (class "clojure.lang.ISeq")) void)))
                                      (12 (areturn))
                                      (endofcode 13))
                                   (Exceptions )
                                   (StackMap )))
                        (method "withMeta"
                              (parameters (class "clojure.lang.IPersistentMap"))
                              (returntype . (class "clojure.lang.Obj"))
                              (accessflags  *class*  *public*  *volatile* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokevirtual
					(methodCP "withMeta" "clojure.lang.APersistentMap$KeySeq" ((class "clojure.lang.IPersistentMap")) (class "clojure.lang.APersistentMap$KeySeq"))))
                                      (5 (areturn))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "withMeta"
                              (parameters (class "clojure.lang.IPersistentMap"))
                              (returntype . (class "clojure.lang.IObj"))
                              (accessflags  *class*  *public*  *volatile* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokevirtual
					(methodCP "withMeta" "clojure.lang.APersistentMap$KeySeq" ((class "clojure.lang.IPersistentMap")) (class "clojure.lang.APersistentMap$KeySeq"))))
                                      (5 (areturn))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *APersistentMap$KeySeq-class-table*
  (make-static-class-decls 
   *clojure.lang.APersistentMap$KeySeq*))

(defconst *package-name-map* 
  ("clojure.lang.APersistentMap$KeySeq" . "clojure.lang"))

