; ProcessBuilder$NullOutputStream-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:35 CDT 2014.
;

(defconst *java.lang.ProcessBuilder$NullOutputStream*
 (make-class-def
      '(class "java.lang.ProcessBuilder$NullOutputStream"
            "java.io.OutputStream"
            (constant_pool
                        (STRING  "Stream closed"))
            (fields
                        (field "INSTANCE" (class "java.lang.ProcessBuilder$NullOutputStream") (accessflags  *class*  *final*  *static* ) -1))
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
					(methodCP "<init>" "java.io.OutputStream" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "write"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (new (class "java.io.IOException")))
                                      (3 (dup))
                                      (4 (ldc 0))         ;;STRING:: "Stream closed"
                                      (6 (invokespecial
					(methodCP "<init>" "java.io.IOException" ((class "java.lang.String")) void)))
                                      (9 (athrow))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 11)
                                   (parsedcode
                                      (0 (new (class "java.lang.ProcessBuilder$NullOutputStream")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.lang.ProcessBuilder$NullOutputStream" () void)))
                                      (7 (putstatic (fieldCP "INSTANCE" "java.lang.ProcessBuilder$NullOutputStream" (class "java.lang.ProcessBuilder$NullOutputStream"))))
                                      (10 (return))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *ProcessBuilder$NullOutputStream-class-table*
  (make-static-class-decls 
   *java.lang.ProcessBuilder$NullOutputStream*))

(defconst *package-name-map* 
  ("java.lang.ProcessBuilder$NullOutputStream" . "java.lang"))

