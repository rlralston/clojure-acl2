; AWTException-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:23 CDT 2014.
;

(defconst *java.awt.AWTException*
 (make-class-def
      '(class "java.awt.AWTException"
            "java.lang.Exception"
            (constant_pool
                        (LONG -1900414231151323879))
            (fields
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0))
            (methods
                        (method "<init>"
                              (parameters (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokespecial
					(methodCP "<init>" "java.lang.Exception" ((class "java.lang.String")) void)))
                                      (5 (return))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *AWTException-class-table*
  (make-static-class-decls 
   *java.awt.AWTException*))

(defconst *package-name-map* 
  ("java.awt.AWTException" . "java.awt"))
