; NoSuchFileException-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:39 CDT 2014.
;

(defconst *java.nio.file.NoSuchFileException*
 (make-class-def
      '(class "java.nio.file.NoSuchFileException"
            "java.nio.file.FileSystemException"
            (constant_pool
                        (LONG -1390291775875351931))
            (fields
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *static* ) 0))
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
					(methodCP "<init>" "java.nio.file.FileSystemException" ((class "java.lang.String")) void)))
                                      (5 (return))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.String") (class "java.lang.String") (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 4) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (aload_2))
                                      (3 (aload_3))
                                      (4 (invokespecial
					(methodCP "<init>" "java.nio.file.FileSystemException" ((class "java.lang.String") (class "java.lang.String") (class "java.lang.String")) void)))
                                      (7 (return))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *NoSuchFileException-class-table*
  (make-static-class-decls 
   *java.nio.file.NoSuchFileException*))

(defconst *package-name-map* 
  ("java.nio.file.NoSuchFileException" . "java.nio.file"))

