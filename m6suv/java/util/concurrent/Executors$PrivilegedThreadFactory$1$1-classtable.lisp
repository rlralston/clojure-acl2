; Executors$PrivilegedThreadFactory$1$1-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:44 CDT 2014.
;

(defconst *java.util.concurrent.Executors$PrivilegedThreadFactory$1$1*
 (make-class-def
      '(class "java.util.concurrent.Executors$PrivilegedThreadFactory$1$1"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "this$1" (class "java.util.concurrent.Executors$PrivilegedThreadFactory$1") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.concurrent.Executors$PrivilegedThreadFactory$1"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$1" "java.util.concurrent.Executors$PrivilegedThreadFactory$1$1" (class "java.util.concurrent.Executors$PrivilegedThreadFactory$1"))))
                                      (5 (aload_0))
                                      (6 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "run"
                              (parameters )
                              (returntype . (class "java.lang.Void"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 30)
                                   (parsedcode
                                      (0 (invokestatic
					(methodCP "currentThread" "java.lang.Thread" () (class "java.lang.Thread"))))
                                      (3 (aload_0))
                                      (4 (getfield (fieldCP "this$1" "java.util.concurrent.Executors$PrivilegedThreadFactory$1$1" (class "java.util.concurrent.Executors$PrivilegedThreadFactory$1"))))
                                      (7 (getfield (fieldCP "this$0" "java.util.concurrent.Executors$PrivilegedThreadFactory$1" (class "java.util.concurrent.Executors$PrivilegedThreadFactory"))))
                                      (10 (invokestatic
					(methodCP "access$300" "java.util.concurrent.Executors$PrivilegedThreadFactory" ((class "java.util.concurrent.Executors$PrivilegedThreadFactory")) (class "java.lang.ClassLoader"))))
                                      (13 (invokevirtual
					(methodCP "setContextClassLoader" "java.lang.Thread" ((class "java.lang.ClassLoader")) void)))
                                      (16 (aload_0))
                                      (17 (getfield (fieldCP "this$1" "java.util.concurrent.Executors$PrivilegedThreadFactory$1$1" (class "java.util.concurrent.Executors$PrivilegedThreadFactory$1"))))
                                      (20 (getfield (fieldCP "val$r" "java.util.concurrent.Executors$PrivilegedThreadFactory$1" (class "java.lang.Runnable"))))
                                      (23 (invokeinterface
					(methodCP "run" "java.lang.Runnable" () void) 1))
                                      (28 (aconst_null))
                                      (29 (areturn))
                                      (endofcode 30))
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
					(methodCP "run" "java.util.concurrent.Executors$PrivilegedThreadFactory$1$1" () (class "java.lang.Void"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.security.PrivilegedAction")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *Executors$PrivilegedThreadFactory$1$1-class-table*
  (make-static-class-decls 
   *java.util.concurrent.Executors$PrivilegedThreadFactory$1$1*))

(defconst *package-name-map* 
  ("java.util.concurrent.Executors$PrivilegedThreadFactory$1$1" . "java.util.concurrent"))

