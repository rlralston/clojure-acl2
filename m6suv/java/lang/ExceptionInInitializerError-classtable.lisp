; ExceptionInInitializerError-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:33 CDT 2014.
;

(defconst *java.lang.ExceptionInInitializerError*
 (make-class-def
      '(class "java.lang.ExceptionInInitializerError"
            "java.lang.LinkageError"
            (constant_pool
                        (LONG 1521711792217232256))
            (fields
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0)
                        (field "exception" (class "java.lang.Throwable") (accessflags  *class*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.LinkageError" () void)))
                                      (4 (aload_0))
                                      (5 (aconst_null))
                                      (6 (invokevirtual
					(methodCP "initCause" "java.lang.ExceptionInInitializerError" ((class "java.lang.Throwable")) (class "java.lang.Throwable"))))
                                      (9 (pop))
                                      (10 (return))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.Throwable"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 16)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.LinkageError" () void)))
                                      (4 (aload_0))
                                      (5 (aconst_null))
                                      (6 (invokevirtual
					(methodCP "initCause" "java.lang.ExceptionInInitializerError" ((class "java.lang.Throwable")) (class "java.lang.Throwable"))))
                                      (9 (pop))
                                      (10 (aload_0))
                                      (11 (aload_1))
                                      (12 (putfield (fieldCP "exception" "java.lang.ExceptionInInitializerError" (class "java.lang.Throwable"))))
                                      (15 (return))
                                      (endofcode 16))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 12)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokespecial
					(methodCP "<init>" "java.lang.LinkageError" ((class "java.lang.String")) void)))
                                      (5 (aload_0))
                                      (6 (aconst_null))
                                      (7 (invokevirtual
					(methodCP "initCause" "java.lang.ExceptionInInitializerError" ((class "java.lang.Throwable")) (class "java.lang.Throwable"))))
                                      (10 (pop))
                                      (11 (return))
                                      (endofcode 12))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getException"
                              (parameters )
                              (returntype . (class "java.lang.Throwable"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "exception" "java.lang.ExceptionInInitializerError" (class "java.lang.Throwable"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getCause"
                              (parameters )
                              (returntype . (class "java.lang.Throwable"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "exception" "java.lang.ExceptionInInitializerError" (class "java.lang.Throwable"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *ExceptionInInitializerError-class-table*
  (make-static-class-decls 
   *java.lang.ExceptionInInitializerError*))

(defconst *package-name-map* 
  ("java.lang.ExceptionInInitializerError" . "java.lang"))

