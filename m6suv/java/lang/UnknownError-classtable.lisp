; UnknownError-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:36 CDT 2014.
;

(defconst *java.lang.UnknownError*
 (make-class-def
      '(class "java.lang.UnknownError"
            "java.lang.VirtualMachineError"
            (constant_pool
                        (LONG 2524784860676771849))
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
					(methodCP "<init>" "java.lang.VirtualMachineError" () void)))
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
					(methodCP "<init>" "java.lang.VirtualMachineError" ((class "java.lang.String")) void)))
                                      (5 (return))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *UnknownError-class-table*
  (make-static-class-decls 
   *java.lang.UnknownError*))

(defconst *package-name-map* 
  ("java.lang.UnknownError" . "java.lang"))

