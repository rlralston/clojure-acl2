; RejectedExecutionException-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:44 CDT 2014.
;

(defconst *java.util.concurrent.RejectedExecutionException*
 (make-class-def
      '(class "java.util.concurrent.RejectedExecutionException"
            "java.lang.RuntimeException"
            (constant_pool
                        (LONG -375805702767069545))
            (fields
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0))
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
					(methodCP "<init>" "java.lang.RuntimeException" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokespecial
					(methodCP "<init>" "java.lang.RuntimeException" ((class "java.lang.String")) void)))
                                      (5 (return))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.String") (class "java.lang.Throwable"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 7)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (aload_2))
                                      (3 (invokespecial
					(methodCP "<init>" "java.lang.RuntimeException" ((class "java.lang.String") (class "java.lang.Throwable")) void)))
                                      (6 (return))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.Throwable"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokespecial
					(methodCP "<init>" "java.lang.RuntimeException" ((class "java.lang.Throwable")) void)))
                                      (5 (return))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *RejectedExecutionException-class-table*
  (make-static-class-decls 
   *java.util.concurrent.RejectedExecutionException*))

(defconst *package-name-map* 
  ("java.util.concurrent.RejectedExecutionException" . "java.util.concurrent"))

