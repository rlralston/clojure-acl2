; NotYetConnectedException-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:38 CDT 2014.
;

(defconst *java.nio.channels.NotYetConnectedException*
 (make-class-def
      '(class "java.nio.channels.NotYetConnectedException"
            "java.lang.IllegalStateException"
            (constant_pool
                        (LONG 4697316551909513464))
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
					(methodCP "<init>" "java.lang.IllegalStateException" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *NotYetConnectedException-class-table*
  (make-static-class-decls 
   *java.nio.channels.NotYetConnectedException*))

(defconst *package-name-map* 
  ("java.nio.channels.NotYetConnectedException" . "java.nio.channels"))
