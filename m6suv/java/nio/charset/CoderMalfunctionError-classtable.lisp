; CoderMalfunctionError-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:38 CDT 2014.
;

(defconst *java.nio.charset.CoderMalfunctionError*
 (make-class-def
      '(class "java.nio.charset.CoderMalfunctionError"
            "java.lang.Error"
            (constant_pool
                        (LONG -1151412348057794301))
            (fields
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0))
            (methods
                        (method "<init>"
                              (parameters (class "java.lang.Exception"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokespecial
					(methodCP "<init>" "java.lang.Error" ((class "java.lang.Throwable")) void)))
                                      (5 (return))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *CoderMalfunctionError-class-table*
  (make-static-class-decls 
   *java.nio.charset.CoderMalfunctionError*))

(defconst *package-name-map* 
  ("java.nio.charset.CoderMalfunctionError" . "java.nio.charset"))
