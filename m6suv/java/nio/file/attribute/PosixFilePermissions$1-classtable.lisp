; PosixFilePermissions$1-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:39 CDT 2014.
;

(defconst *java.nio.file.attribute.PosixFilePermissions$1*
 (make-class-def
      '(class "java.nio.file.attribute.PosixFilePermissions$1"
            "java.lang.Object"
            (constant_pool
                        (STRING  "posix:permissions"))
            (fields
                        (field "val$value" (class "java.util.Set") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.Set"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "val$value" "java.nio.file.attribute.PosixFilePermissions$1" (class "java.util.Set"))))
                                      (5 (aload_0))
                                      (6 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "name"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 3)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "posix:permissions"
                                      (2 (areturn))
                                      (endofcode 3))
                                   (Exceptions )
                                   (StackMap )))
                        (method "value"
                              (parameters )
                              (returntype . (class "java.util.Set"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "val$value" "java.nio.file.attribute.PosixFilePermissions$1" (class "java.util.Set"))))
                                      (4 (invokestatic
					(methodCP "unmodifiableSet" "java.util.Collections" ((class "java.util.Set")) (class "java.util.Set"))))
                                      (7 (areturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "value"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public*  *volatile* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokevirtual
					(methodCP "value" "java.nio.file.attribute.PosixFilePermissions$1" () (class "java.util.Set"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.nio.file.attribute.FileAttribute")
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *PosixFilePermissions$1-class-table*
  (make-static-class-decls 
   *java.nio.file.attribute.PosixFilePermissions$1*))

(defconst *package-name-map* 
  ("java.nio.file.attribute.PosixFilePermissions$1" . "java.nio.file.attribute"))

