; Executors$2-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:43 CDT 2014.
;

(defconst *java.util.concurrent.Executors$2*
 (make-class-def
      '(class "java.util.concurrent.Executors$2"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "val$action" (class "java.security.PrivilegedExceptionAction") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.security.PrivilegedExceptionAction"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "val$action" "java.util.concurrent.Executors$2" (class "java.security.PrivilegedExceptionAction"))))
                                      (5 (aload_0))
                                      (6 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "call"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "val$action" "java.util.concurrent.Executors$2" (class "java.security.PrivilegedExceptionAction"))))
                                      (4 (invokeinterface
					(methodCP "run" "java.security.PrivilegedExceptionAction" () (class "java.lang.Object")) 1))
                                      (9 (areturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.util.concurrent.Callable")
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *Executors$2-class-table*
  (make-static-class-decls 
   *java.util.concurrent.Executors$2*))

(defconst *package-name-map* 
  ("java.util.concurrent.Executors$2" . "java.util.concurrent"))

