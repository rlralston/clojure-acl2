; LastOwnerException-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:40 CDT 2014.
;

(defconst *java.security.acl.LastOwnerException*
 (make-class-def
      '(class "java.security.acl.LastOwnerException"
            "java.lang.Exception"
            (constant_pool
                        (LONG -5141997548211140359))
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
					(methodCP "<init>" "java.lang.Exception" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *LastOwnerException-class-table*
  (make-static-class-decls 
   *java.security.acl.LastOwnerException*))

(defconst *package-name-map* 
  ("java.security.acl.LastOwnerException" . "java.security.acl"))
