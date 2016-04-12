; Executors$PrivilegedThreadFactory-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:44 CDT 2014.
;

(defconst *java.util.concurrent.Executors$PrivilegedThreadFactory*
 (make-class-def
      '(class "java.util.concurrent.Executors$PrivilegedThreadFactory"
            "java.util.concurrent.Executors$DefaultThreadFactory"
            (constant_pool
                        (STRING  "setContextClassLoader"))
            (fields
                        (field "acc" (class "java.security.AccessControlContext") (accessflags  *class*  *final*  *private* ) -1)
                        (field "ccl" (class "java.lang.ClassLoader") (accessflags  *class*  *final*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 50)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokespecial (methodCP "<init>" "java.util.concurrent.Executors$DefaultThreadFactory" () void))) 
                                      (4 (invokestatic (methodCP "getSecurityManager" "java.lang.System" () (class "java.lang.SecurityManager")))) 
                                      (7 (astore_1)) 
                                      (8 (aload_1)) 
                                      (9 (ifnull 32))  ;;to TAG_0
                                      (12 (aload_1)) 
                                      (13 (getstatic (fieldCP "GET_CLASSLOADER_PERMISSION" "sun.security.util.SecurityConstants" (class "java.lang.RuntimePermission")))) 
                                      (16 (invokevirtual (methodCP "checkPermission" "java.lang.SecurityManager" ((class "java.security.Permission")) void))) 
                                      (19 (aload_1)) 
                                      (20 (new (class "java.lang.RuntimePermission"))) 
                                      (23 (dup)) 
                                      (24 (ldc 0)) ;;STRING:: "setContextClassLoader"
                                      (26 (invokespecial (methodCP "<init>" "java.lang.RuntimePermission" ((class "java.lang.String")) void))) 
                                      (29 (invokevirtual (methodCP "checkPermission" "java.lang.SecurityManager" ((class "java.security.Permission")) void))) 
                                      (32 (aload_0)) ;;at TAG_0
                                      (33 (invokestatic (methodCP "getContext" "java.security.AccessController" () (class "java.security.AccessControlContext")))) 
                                      (36 (putfield (fieldCP "acc" "java.util.concurrent.Executors$PrivilegedThreadFactory" (class "java.security.AccessControlContext")))) 
                                      (39 (aload_0)) 
                                      (40 (invokestatic (methodCP "currentThread" "java.lang.Thread" () (class "java.lang.Thread")))) 
                                      (43 (invokevirtual (methodCP "getContextClassLoader" "java.lang.Thread" () (class "java.lang.ClassLoader")))) 
                                      (46 (putfield (fieldCP "ccl" "java.util.concurrent.Executors$PrivilegedThreadFactory" (class "java.lang.ClassLoader")))) 
                                      (49 (return)) 
                                      (endofcode 50))
                                   (Exceptions )
                                   (StackMap )))
                        (method "newThread"
                              (parameters (class "java.lang.Runnable"))
                              (returntype . (class "java.lang.Thread"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 2) (code_length . 14)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (new (class "java.util.concurrent.Executors$PrivilegedThreadFactory$1")))
                                      (4 (dup))
                                      (5 (aload_0))
                                      (6 (aload_1))
                                      (7 (invokespecial
					(methodCP "<init>" "java.util.concurrent.Executors$PrivilegedThreadFactory$1" ((class "java.util.concurrent.Executors$PrivilegedThreadFactory") (class "java.lang.Runnable")) void)))
                                      (10 (invokespecial
					(methodCP "newThread" "java.util.concurrent.Executors$DefaultThreadFactory" ((class "java.lang.Runnable")) (class "java.lang.Thread"))))
                                      (13 (areturn))
                                      (endofcode 14))
                                   (Exceptions )
                                   (StackMap )))
                        (method "access$300"
                              (parameters (class "java.util.concurrent.Executors$PrivilegedThreadFactory"))
                              (returntype . (class "java.lang.ClassLoader"))
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "ccl" "java.util.concurrent.Executors$PrivilegedThreadFactory" (class "java.lang.ClassLoader"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "access$400"
                              (parameters (class "java.util.concurrent.Executors$PrivilegedThreadFactory"))
                              (returntype . (class "java.security.AccessControlContext"))
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "acc" "java.util.concurrent.Executors$PrivilegedThreadFactory" (class "java.security.AccessControlContext"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *Executors$PrivilegedThreadFactory-class-table*
  (make-static-class-decls 
   *java.util.concurrent.Executors$PrivilegedThreadFactory*))

(defconst *package-name-map* 
  ("java.util.concurrent.Executors$PrivilegedThreadFactory" . "java.util.concurrent"))
