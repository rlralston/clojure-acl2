; ReadOnlyBufferException-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:39 CDT 2014.
;

(defconst *java.nio.ReadOnlyBufferException*
 (make-class-def
      '(class "java.nio.ReadOnlyBufferException"
            "java.lang.UnsupportedOperationException"
            (constant_pool
                        (LONG -1210063976496234090))
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
					(methodCP "<init>" "java.lang.UnsupportedOperationException" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *ReadOnlyBufferException-class-table*
  (make-static-class-decls 
   *java.nio.ReadOnlyBufferException*))

(defconst *package-name-map* 
  ("java.nio.ReadOnlyBufferException" . "java.nio"))

