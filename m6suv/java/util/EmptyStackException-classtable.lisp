; EmptyStackException-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:45 CDT 2014.
;

(defconst *java.util.EmptyStackException*
 (make-class-def
      '(class "java.util.EmptyStackException"
            "java.lang.RuntimeException"
            (constant_pool
                        (LONG 5084686378493302095))
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


(defconst *EmptyStackException-class-table*
  (make-static-class-decls 
   *java.util.EmptyStackException*))

(defconst *package-name-map* 
  ("java.util.EmptyStackException" . "java.util"))

