; GenericSignatureFormatError-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:36 CDT 2014.
;

(defconst *java.lang.reflect.GenericSignatureFormatError*
 (make-class-def
      '(class "java.lang.reflect.GenericSignatureFormatError"
            "java.lang.ClassFormatError"
            (constant_pool
                        (LONG 6709919147137911034))
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
					(methodCP "<init>" "java.lang.ClassFormatError" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *GenericSignatureFormatError-class-table*
  (make-static-class-decls 
   *java.lang.reflect.GenericSignatureFormatError*))

(defconst *package-name-map* 
  ("java.lang.reflect.GenericSignatureFormatError" . "java.lang.reflect"))
