; UnknownContentHandler-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:37 CDT 2014.
;

(defconst *java.net.UnknownContentHandler*
 (make-class-def
      '(class "java.net.UnknownContentHandler"
            "java.net.ContentHandler"
            (constant_pool)
            (fields
                        (field "INSTANCE" (class "java.net.ContentHandler") (accessflags  *class*  *final*  *static* ) -1))
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.net.ContentHandler" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getContent"
                              (parameters (class "java.net.URLConnection"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (invokevirtual
					(methodCP "getInputStream" "java.net.URLConnection" () (class "java.io.InputStream"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 11)
                                   (parsedcode
                                      (0 (new (class "java.net.UnknownContentHandler")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.net.UnknownContentHandler" () void)))
                                      (7 (putstatic (fieldCP "INSTANCE" "java.net.UnknownContentHandler" (class "java.net.ContentHandler"))))
                                      (10 (return))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *UnknownContentHandler-class-table*
  (make-static-class-decls 
   *java.net.UnknownContentHandler*))

(defconst *package-name-map* 
  ("java.net.UnknownContentHandler" . "java.net"))

