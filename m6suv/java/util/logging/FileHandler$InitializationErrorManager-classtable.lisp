; FileHandler$InitializationErrorManager-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:46 CDT 2014.
;

(defconst *java.util.logging.FileHandler$InitializationErrorManager*
 (make-class-def
      '(class "java.util.logging.FileHandler$InitializationErrorManager"
            "java.util.logging.ErrorManager"
            (constant_pool)
            (fields
                        (field "lastException" (class "java.lang.Exception") (accessflags  *class* ) -1))
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
					(methodCP "<init>" "java.util.logging.ErrorManager" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "error"
                              (parameters (class "java.lang.String") (class "java.lang.Exception") int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_2))
                                      (2 (putfield (fieldCP "lastException" "java.util.logging.FileHandler$InitializationErrorManager" (class "java.lang.Exception"))))
                                      (5 (return))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.util.logging.FileHandler$1"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.util.logging.FileHandler$InitializationErrorManager" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *FileHandler$InitializationErrorManager-class-table*
  (make-static-class-decls 
   *java.util.logging.FileHandler$InitializationErrorManager*))

(defconst *package-name-map* 
  ("java.util.logging.FileHandler$InitializationErrorManager" . "java.util.logging"))

