; Number-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:35 CDT 2014.
;

(defconst *java.lang.Number*
 (make-class-def
      '(class "java.lang.Number"
            "java.lang.Object"
            (constant_pool
                        (LONG -8742448824652078965))
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
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "intValue"
                              (parameters )
                              (returntype . int)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "longValue"
                              (parameters )
                              (returntype . long)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "floatValue"
                              (parameters )
                              (returntype . float)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "doubleValue"
                              (parameters )
                              (returntype . double)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "byteValue"
                              (parameters )
                              (returntype . byte)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokevirtual
					(methodCP "intValue" "java.lang.Number" () int)))
                                      (4 (i2b))
                                      (5 (ireturn))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "shortValue"
                              (parameters )
                              (returntype . short)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokevirtual
					(methodCP "intValue" "java.lang.Number" () int)))
                                      (4 (i2s))
                                      (5 (ireturn))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.io.Serializable")
            (accessflags  *abstract*  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *Number-class-table*
  (make-static-class-decls 
   *java.lang.Number*))

(defconst *package-name-map* 
  ("java.lang.Number" . "java.lang"))

