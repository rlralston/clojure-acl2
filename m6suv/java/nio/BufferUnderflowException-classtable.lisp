; BufferUnderflowException-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:38 CDT 2014.
;

(defconst *java.nio.BufferUnderflowException*
 (make-class-def
      '(class "java.nio.BufferUnderflowException"
            "java.lang.RuntimeException"
            (constant_pool
                        (LONG -1713313658691622206))
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
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *BufferUnderflowException-class-table*
  (make-static-class-decls 
   *java.nio.BufferUnderflowException*))

(defconst *package-name-map* 
  ("java.nio.BufferUnderflowException" . "java.nio"))
