; ServerCloneException-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:40 CDT 2014.
;

(defconst *java.rmi.server.ServerCloneException*
 (make-class-def
      '(class "java.rmi.server.ServerCloneException"
            "java.lang.CloneNotSupportedException"
            (constant_pool
                        (LONG 6617456357664815945)
                        (STRING  "; nested exception is: \n\t"))
            (fields
                        (field "detail" (class "java.lang.Exception") (accessflags  *class*  *public* ) -1)
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0))
            (methods
                        (method "<init>"
                              (parameters (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 12)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokespecial
					(methodCP "<init>" "java.lang.CloneNotSupportedException" ((class "java.lang.String")) void)))
                                      (5 (aload_0))
                                      (6 (aconst_null))
                                      (7 (invokevirtual
					(methodCP "initCause" "java.rmi.server.ServerCloneException" ((class "java.lang.Throwable")) (class "java.lang.Throwable"))))
                                      (10 (pop))
                                      (11 (return))
                                      (endofcode 12))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.String") (class "java.lang.Exception"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 17)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokespecial
					(methodCP "<init>" "java.lang.CloneNotSupportedException" ((class "java.lang.String")) void)))
                                      (5 (aload_0))
                                      (6 (aconst_null))
                                      (7 (invokevirtual
					(methodCP "initCause" "java.rmi.server.ServerCloneException" ((class "java.lang.Throwable")) (class "java.lang.Throwable"))))
                                      (10 (pop))
                                      (11 (aload_0))
                                      (12 (aload_2))
                                      (13 (putfield (fieldCP "detail" "java.rmi.server.ServerCloneException" (class "java.lang.Exception"))))
                                      (16 (return))
                                      (endofcode 17))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getMessage"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 45)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "detail" "java.rmi.server.ServerCloneException" (class "java.lang.Exception")))) 
                                      (4 (ifnonnull 12))  ;;to TAG_0
                                      (7 (aload_0)) 
                                      (8 (invokespecial (methodCP "getMessage" "java.lang.CloneNotSupportedException" () (class "java.lang.String")))) 
                                      (11 (areturn)) 
                                      (12 (new (class "java.lang.StringBuilder"))) ;;at TAG_0
                                      (15 (dup)) 
                                      (16 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (19 (aload_0)) 
                                      (20 (invokespecial (methodCP "getMessage" "java.lang.CloneNotSupportedException" () (class "java.lang.String")))) 
                                      (23 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (26 (ldc 1)) ;;STRING:: "; nested exception is: \n\t"
                                      (28 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (31 (aload_0)) 
                                      (32 (getfield (fieldCP "detail" "java.rmi.server.ServerCloneException" (class "java.lang.Exception")))) 
                                      (35 (invokevirtual (methodCP "toString" "java.lang.Exception" () (class "java.lang.String")))) 
                                      (38 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (41 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (44 (areturn)) 
                                      (endofcode 45))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getCause"
                              (parameters )
                              (returntype . (class "java.lang.Throwable"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "detail" "java.rmi.server.ServerCloneException" (class "java.lang.Exception"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *ServerCloneException-class-table*
  (make-static-class-decls 
   *java.rmi.server.ServerCloneException*))

(defconst *package-name-map* 
  ("java.rmi.server.ServerCloneException" . "java.rmi.server"))
