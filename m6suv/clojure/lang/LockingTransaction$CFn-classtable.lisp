; LockingTransaction$CFn-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:52 CDT 2014.
;

(defconst *clojure.lang.LockingTransaction$CFn*
 (make-class-def
      '(class "clojure.lang.LockingTransaction$CFn"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "fn" (class "clojure.lang.IFn") (accessflags  *class*  *final* ) -1)
                        (field "args" (class "clojure.lang.ISeq") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "clojure.lang.IFn") (class "clojure.lang.ISeq"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "fn" "clojure.lang.LockingTransaction$CFn" (class "clojure.lang.IFn"))))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "args" "clojure.lang.LockingTransaction$CFn" (class "clojure.lang.ISeq"))))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *LockingTransaction$CFn-class-table*
  (make-static-class-decls 
   *clojure.lang.LockingTransaction$CFn*))

(defconst *package-name-map* 
  ("clojure.lang.LockingTransaction$CFn" . "clojure.lang"))
