; XMLUtils$EH-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:48 CDT 2014.
;

(defconst *java.util.XMLUtils$EH*
 (make-class-def
      '(class "java.util.XMLUtils$EH"
            "java.lang.Object"
            (constant_pool)
            (fields)
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
                        (method "error"
                              (parameters (class "org.xml.sax.SAXParseException"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 2)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (athrow))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap )))
                        (method "fatalError"
                              (parameters (class "org.xml.sax.SAXParseException"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 2)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (athrow))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap )))
                        (method "warning"
                              (parameters (class "org.xml.sax.SAXParseException"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 2)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (athrow))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.util.XMLUtils$1"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.util.XMLUtils$EH" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "org.xml.sax.ErrorHandler")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *XMLUtils$EH-class-table*
  (make-static-class-decls 
   *java.util.XMLUtils$EH*))

(defconst *package-name-map* 
  ("java.util.XMLUtils$EH" . "java.util"))

