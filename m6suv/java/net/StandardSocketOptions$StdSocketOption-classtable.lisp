; StandardSocketOptions$StdSocketOption-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:37 CDT 2014.
;

(defconst *java.net.StandardSocketOptions$StdSocketOption*
 (make-class-def
      '(class "java.net.StandardSocketOptions$StdSocketOption"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "name" (class "java.lang.String") (accessflags  *class*  *final*  *private* ) -1)
                        (field "type" (class "java.lang.Class") (accessflags  *class*  *final*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.lang.String") (class "java.lang.Class"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "name" "java.net.StandardSocketOptions$StdSocketOption" (class "java.lang.String"))))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "type" "java.net.StandardSocketOptions$StdSocketOption" (class "java.lang.Class"))))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "name"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "name" "java.net.StandardSocketOptions$StdSocketOption" (class "java.lang.String"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "type"
                              (parameters )
                              (returntype . (class "java.lang.Class"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "type" "java.net.StandardSocketOptions$StdSocketOption" (class "java.lang.Class"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "toString"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "name" "java.net.StandardSocketOptions$StdSocketOption" (class "java.lang.String"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.net.SocketOption")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *StandardSocketOptions$StdSocketOption-class-table*
  (make-static-class-decls 
   *java.net.StandardSocketOptions$StdSocketOption*))

(defconst *package-name-map* 
  ("java.net.StandardSocketOptions$StdSocketOption" . "java.net"))
