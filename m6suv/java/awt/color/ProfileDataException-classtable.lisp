; ProfileDataException-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:24 CDT 2014.
;

(defconst *java.awt.color.ProfileDataException*
 (make-class-def
      '(class "java.awt.color.ProfileDataException"
            "java.lang.RuntimeException"
            (constant_pool)
            (fields)
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
					(methodCP "<init>" "java.lang.RuntimeException" ((class "java.lang.String")) void)))
                                      (5 (return))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *ProfileDataException-class-table*
  (make-static-class-decls 
   *java.awt.color.ProfileDataException*))

(defconst *package-name-map* 
  ("java.awt.color.ProfileDataException" . "java.awt.color"))

