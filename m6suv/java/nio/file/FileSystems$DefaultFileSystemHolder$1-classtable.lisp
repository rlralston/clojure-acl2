; FileSystems$DefaultFileSystemHolder$1-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:39 CDT 2014.
;

(defconst *java.nio.file.FileSystems$DefaultFileSystemHolder$1*
 (make-class-def
      '(class "java.nio.file.FileSystems$DefaultFileSystemHolder$1"
            "java.lang.Object"
            (constant_pool)
            (fields)
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
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "run"
                              (parameters )
                              (returntype . (class "java.nio.file.spi.FileSystemProvider"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 4)
                                   (parsedcode
                                      (0 (invokestatic
					(methodCP "access$000" "java.nio.file.FileSystems$DefaultFileSystemHolder" () (class "java.nio.file.spi.FileSystemProvider"))))
                                      (3 (areturn))
                                      (endofcode 4))
                                   (Exceptions )
                                   (StackMap )))
                        (method "run"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public*  *volatile* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokevirtual
					(methodCP "run" "java.nio.file.FileSystems$DefaultFileSystemHolder$1" () (class "java.nio.file.spi.FileSystemProvider"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.security.PrivilegedAction")
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *FileSystems$DefaultFileSystemHolder$1-class-table*
  (make-static-class-decls 
   *java.nio.file.FileSystems$DefaultFileSystemHolder$1*))

(defconst *package-name-map* 
  ("java.nio.file.FileSystems$DefaultFileSystemHolder$1" . "java.nio.file"))

