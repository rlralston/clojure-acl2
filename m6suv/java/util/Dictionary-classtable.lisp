; Dictionary-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:45 CDT 2014.
;

(defconst *java.util.Dictionary*
 (make-class-def
      '(class "java.util.Dictionary"
            "java.lang.Object"
            (constant_pool)
            (fields)
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
                        (method "size"
                              (parameters )
                              (returntype . int)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "isEmpty"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "keys"
                              (parameters )
                              (returntype . (class "java.util.Enumeration"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "elements"
                              (parameters )
                              (returntype . (class "java.util.Enumeration"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "get"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "put"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "remove"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces)
            (accessflags  *abstract*  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")))))


(defconst *Dictionary-class-table*
  (make-static-class-decls 
   *java.util.Dictionary*))

(defconst *package-name-map* 
  ("java.util.Dictionary" . "java.util"))

