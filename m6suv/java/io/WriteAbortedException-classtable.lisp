; WriteAbortedException-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:32 CDT 2014.
;

(defconst *java.io.WriteAbortedException*
 (make-class-def
      '(class "java.io.WriteAbortedException"
            "java.io.ObjectStreamException"
            (constant_pool
                        (LONG -3326426625597282442)
                        (STRING  "; "))
            (fields
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0)
                        (field "detail" (class "java.lang.Exception") (accessflags  *class*  *public* ) -1))
            (methods
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
					(methodCP "<init>" "java.io.ObjectStreamException" ((class "java.lang.String")) void)))
                                      (5 (aload_0))
                                      (6 (aconst_null))
                                      (7 (invokevirtual
					(methodCP "initCause" "java.io.WriteAbortedException" ((class "java.lang.Throwable")) (class "java.lang.Throwable"))))
                                      (10 (pop))
                                      (11 (aload_0))
                                      (12 (aload_2))
                                      (13 (putfield (fieldCP "detail" "java.io.WriteAbortedException" (class "java.lang.Exception"))))
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
                                      (1 (getfield (fieldCP "detail" "java.io.WriteAbortedException" (class "java.lang.Exception")))) 
                                      (4 (ifnonnull 12))  ;;to TAG_0
                                      (7 (aload_0)) 
                                      (8 (invokespecial (methodCP "getMessage" "java.io.ObjectStreamException" () (class "java.lang.String")))) 
                                      (11 (areturn)) 
                                      (12 (new (class "java.lang.StringBuilder"))) ;;at TAG_0
                                      (15 (dup)) 
                                      (16 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (19 (aload_0)) 
                                      (20 (invokespecial (methodCP "getMessage" "java.io.ObjectStreamException" () (class "java.lang.String")))) 
                                      (23 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (26 (ldc 1)) ;;STRING:: "; "
                                      (28 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (31 (aload_0)) 
                                      (32 (getfield (fieldCP "detail" "java.io.WriteAbortedException" (class "java.lang.Exception")))) 
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
                                      (1 (getfield (fieldCP "detail" "java.io.WriteAbortedException" (class "java.lang.Exception"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *WriteAbortedException-class-table*
  (make-static-class-decls 
   *java.io.WriteAbortedException*))

(defconst *package-name-map* 
  ("java.io.WriteAbortedException" . "java.io"))

