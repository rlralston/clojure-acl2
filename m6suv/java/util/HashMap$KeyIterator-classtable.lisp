; HashMap$KeyIterator-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:45 CDT 2014.
;

(defconst *java.util.HashMap$KeyIterator*
 (make-class-def
      '(class "java.util.HashMap$KeyIterator"
            "java.util.HashMap$HashIterator"
            (constant_pool)
            (fields
                        (field "this$0" (class "java.util.HashMap") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.HashMap"))
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.util.HashMap$KeyIterator" (class "java.util.HashMap"))))
                                      (5 (aload_0))
                                      (6 (aload_1))
                                      (7 (invokespecial
					(methodCP "<init>" "java.util.HashMap$HashIterator" ((class "java.util.HashMap")) void)))
                                      (10 (return))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "next"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokevirtual
					(methodCP "nextEntry" "java.util.HashMap$KeyIterator" () (class "java.util.HashMap$Entry"))))
                                      (4 (invokevirtual
					(methodCP "getKey" "java.util.HashMap$Entry" () (class "java.lang.Object"))))
                                      (7 (areturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.util.HashMap") (class "java.util.HashMap$1"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokespecial
					(methodCP "<init>" "java.util.HashMap$KeyIterator" ((class "java.util.HashMap")) void)))
                                      (5 (return))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *HashMap$KeyIterator-class-table*
  (make-static-class-decls 
   *java.util.HashMap$KeyIterator*))

(defconst *package-name-map* 
  ("java.util.HashMap$KeyIterator" . "java.util"))

