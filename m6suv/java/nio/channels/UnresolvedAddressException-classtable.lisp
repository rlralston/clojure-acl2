; UnresolvedAddressException-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:38 CDT 2014.
;

(defconst *java.nio.channels.UnresolvedAddressException*
 (make-class-def
      '(class "java.nio.channels.UnresolvedAddressException"
            "java.lang.IllegalArgumentException"
            (constant_pool
                        (LONG 6136959093620794148))
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
					(methodCP "<init>" "java.lang.IllegalArgumentException" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *UnresolvedAddressException-class-table*
  (make-static-class-decls 
   *java.nio.channels.UnresolvedAddressException*))

(defconst *package-name-map* 
  ("java.nio.channels.UnresolvedAddressException" . "java.nio.channels"))
