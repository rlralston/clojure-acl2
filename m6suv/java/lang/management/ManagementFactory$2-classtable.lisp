; ManagementFactory$2-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:35 CDT 2014.
;

(defconst *java.lang.management.ManagementFactory$2*
 (make-class-def
      '(class "java.lang.management.ManagementFactory$2"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "val$mbs" (class "javax.management.MBeanServer") (accessflags  *class*  *final* ) -1)
                        (field "val$dmbean" (class "javax.management.DynamicMBean") (accessflags  *class*  *final* ) -1)
                        (field "val$pmo" (class "java.lang.management.PlatformManagedObject") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "javax.management.MBeanServer") (class "javax.management.DynamicMBean") (class "java.lang.management.PlatformManagedObject"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 20)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "val$mbs" "java.lang.management.ManagementFactory$2" (class "javax.management.MBeanServer"))))
                                      (5 (aload_0))
                                      (6 (aload_2))
                                      (7 (putfield (fieldCP "val$dmbean" "java.lang.management.ManagementFactory$2" (class "javax.management.DynamicMBean"))))
                                      (10 (aload_0))
                                      (11 (aload_3))
                                      (12 (putfield (fieldCP "val$pmo" "java.lang.management.ManagementFactory$2" (class "java.lang.management.PlatformManagedObject"))))
                                      (15 (aload_0))
                                      (16 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (19 (return))
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap )))
                        (method "run"
                              (parameters )
                              (returntype . (class "java.lang.Void"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 25)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "val$mbs" "java.lang.management.ManagementFactory$2" (class "javax.management.MBeanServer"))))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "val$dmbean" "java.lang.management.ManagementFactory$2" (class "javax.management.DynamicMBean"))))
                                      (8 (aload_0))
                                      (9 (getfield (fieldCP "val$pmo" "java.lang.management.ManagementFactory$2" (class "java.lang.management.PlatformManagedObject"))))
                                      (12 (invokeinterface
					(methodCP "getObjectName" "java.lang.management.PlatformManagedObject" () (class "javax.management.ObjectName")) 1))
                                      (17 (invokeinterface
					(methodCP "registerMBean" "javax.management.MBeanServer" ((class "java.lang.Object") (class "javax.management.ObjectName")) (class "javax.management.ObjectInstance")) 3))
                                      (22 (pop))
                                      (23 (aconst_null))
                                      (24 (areturn))
                                      (endofcode 25))
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
					(methodCP "run" "java.lang.management.ManagementFactory$2" () (class "java.lang.Void"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.security.PrivilegedExceptionAction")
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *ManagementFactory$2-class-table*
  (make-static-class-decls 
   *java.lang.management.ManagementFactory$2*))

(defconst *package-name-map* 
  ("java.lang.management.ManagementFactory$2" . "java.lang.management"))

