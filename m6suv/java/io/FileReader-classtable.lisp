; FileReader-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:32 CDT 2014.
;

(defconst *java.io.FileReader*
 (make-class-def
      '(class "java.io.FileReader"
            "java.io.InputStreamReader"
            (constant_pool)
            (fields)
            (methods
                        (method "<init>"
                              (parameters (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 13)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (new (class "java.io.FileInputStream")))
                                      (4 (dup))
                                      (5 (aload_1))
                                      (6 (invokespecial
					(methodCP "<init>" "java.io.FileInputStream" ((class "java.lang.String")) void)))
                                      (9 (invokespecial
					(methodCP "<init>" "java.io.InputStreamReader" ((class "java.io.InputStream")) void)))
                                      (12 (return))
                                      (endofcode 13))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.io.File"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 13)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (new (class "java.io.FileInputStream")))
                                      (4 (dup))
                                      (5 (aload_1))
                                      (6 (invokespecial
					(methodCP "<init>" "java.io.FileInputStream" ((class "java.io.File")) void)))
                                      (9 (invokespecial
					(methodCP "<init>" "java.io.InputStreamReader" ((class "java.io.InputStream")) void)))
                                      (12 (return))
                                      (endofcode 13))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.io.FileDescriptor"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 13)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (new (class "java.io.FileInputStream")))
                                      (4 (dup))
                                      (5 (aload_1))
                                      (6 (invokespecial
					(methodCP "<init>" "java.io.FileInputStream" ((class "java.io.FileDescriptor")) void)))
                                      (9 (invokespecial
					(methodCP "<init>" "java.io.InputStreamReader" ((class "java.io.InputStream")) void)))
                                      (12 (return))
                                      (endofcode 13))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *FileReader-class-table*
  (make-static-class-decls 
   *java.io.FileReader*))

(defconst *package-name-map* 
  ("java.io.FileReader" . "java.io"))

