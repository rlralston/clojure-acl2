; Executors$PrivilegedThreadFactory$1-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:44 CDT 2014.
;

(defconst *java.util.concurrent.Executors$PrivilegedThreadFactory$1*
 (make-class-def
      '(class "java.util.concurrent.Executors$PrivilegedThreadFactory$1"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "val$r" (class "java.lang.Runnable") (accessflags  *class*  *final* ) -1)
                        (field "this$0" (class "java.util.concurrent.Executors$PrivilegedThreadFactory") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.concurrent.Executors$PrivilegedThreadFactory") (class "java.lang.Runnable"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.util.concurrent.Executors$PrivilegedThreadFactory$1" (class "java.util.concurrent.Executors$PrivilegedThreadFactory"))))
                                      (5 (aload_0))
                                      (6 (aload_2))
                                      (7 (putfield (fieldCP "val$r" "java.util.concurrent.Executors$PrivilegedThreadFactory$1" (class "java.lang.Runnable"))))
                                      (10 (aload_0))
                                      (11 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "run"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 20)
                                   (parsedcode
                                      (0 (new (class "java.util.concurrent.Executors$PrivilegedThreadFactory$1$1")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (invokespecial
					(methodCP "<init>" "java.util.concurrent.Executors$PrivilegedThreadFactory$1$1" ((class "java.util.concurrent.Executors$PrivilegedThreadFactory$1")) void)))
                                      (8 (aload_0))
                                      (9 (getfield (fieldCP "this$0" "java.util.concurrent.Executors$PrivilegedThreadFactory$1" (class "java.util.concurrent.Executors$PrivilegedThreadFactory"))))
                                      (12 (invokestatic
					(methodCP "access$400" "java.util.concurrent.Executors$PrivilegedThreadFactory" ((class "java.util.concurrent.Executors$PrivilegedThreadFactory")) (class "java.security.AccessControlContext"))))
                                      (15 (invokestatic
					(methodCP "doPrivileged" "java.security.AccessController" ((class "java.security.PrivilegedAction") (class "java.security.AccessControlContext")) (class "java.lang.Object"))))
                                      (18 (pop))
                                      (19 (return))
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.lang.Runnable")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "InnerClasses")))))


(defconst *Executors$PrivilegedThreadFactory$1-class-table*
  (make-static-class-decls 
   *java.util.concurrent.Executors$PrivilegedThreadFactory$1*))

(defconst *package-name-map* 
  ("java.util.concurrent.Executors$PrivilegedThreadFactory$1" . "java.util.concurrent"))

