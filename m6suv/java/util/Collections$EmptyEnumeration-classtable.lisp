; Collections$EmptyEnumeration-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:42 CDT 2014.
;

(defconst *java.util.Collections$EmptyEnumeration*
 (make-class-def
      '(class "java.util.Collections$EmptyEnumeration"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "EMPTY_ENUMERATION" (class "java.util.Collections$EmptyEnumeration") (accessflags  *class*  *final*  *static* ) -1))
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *private* )
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
                        (method "hasMoreElements"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 2)
                                   (parsedcode
                                      (0 (iconst_0))
                                      (1 (ireturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap )))
                        (method "nextElement"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (new (class "java.util.NoSuchElementException")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.util.NoSuchElementException" () void)))
                                      (7 (athrow))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 11)
                                   (parsedcode
                                      (0 (new (class "java.util.Collections$EmptyEnumeration")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.util.Collections$EmptyEnumeration" () void)))
                                      (7 (putstatic (fieldCP "EMPTY_ENUMERATION" "java.util.Collections$EmptyEnumeration" (class "java.util.Collections$EmptyEnumeration"))))
                                      (10 (return))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.util.Enumeration")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *Collections$EmptyEnumeration-class-table*
  (make-static-class-decls 
   *java.util.Collections$EmptyEnumeration*))

(defconst *package-name-map* 
  ("java.util.Collections$EmptyEnumeration" . "java.util"))

