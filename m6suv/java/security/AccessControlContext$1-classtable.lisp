; AccessControlContext$1-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:40 CDT 2014.
;

(defconst *java.security.AccessControlContext$1*
 (make-class-def
      '(class "java.security.AccessControlContext$1"
            "java.lang.Object"
            (constant_pool
                        (STRING  "domain that failed "))
            (fields
                        (field "val$db" (class "sun.security.util.Debug") (accessflags  *class*  *final* ) -1)
                        (field "val$pd" (class "java.security.ProtectionDomain") (accessflags  *class*  *final* ) -1)
                        (field "this$0" (class "java.security.AccessControlContext") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.security.AccessControlContext") (class "sun.security.util.Debug") (class "java.security.ProtectionDomain"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 20)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.security.AccessControlContext$1" (class "java.security.AccessControlContext"))))
                                      (5 (aload_0))
                                      (6 (aload_2))
                                      (7 (putfield (fieldCP "val$db" "java.security.AccessControlContext$1" (class "sun.security.util.Debug"))))
                                      (10 (aload_0))
                                      (11 (aload_3))
                                      (12 (putfield (fieldCP "val$pd" "java.security.AccessControlContext$1" (class "java.security.ProtectionDomain"))))
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
                                   (max_stack . 3) (max_locals . 1) (code_length . 31)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "val$db" "java.security.AccessControlContext$1" (class "sun.security.util.Debug"))))
                                      (4 (new (class "java.lang.StringBuilder")))
                                      (7 (dup))
                                      (8 (invokespecial
					(methodCP "<init>" "java.lang.StringBuilder" () void)))
                                      (11 (ldc 0))        ;;STRING:: "domain that failed "
                                      (13 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (16 (aload_0))
                                      (17 (getfield (fieldCP "val$pd" "java.security.AccessControlContext$1" (class "java.security.ProtectionDomain"))))
                                      (20 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.Object")) (class "java.lang.StringBuilder"))))
                                      (23 (invokevirtual
					(methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String"))))
                                      (26 (invokevirtual
					(methodCP "println" "sun.security.util.Debug" ((class "java.lang.String")) void)))
                                      (29 (aconst_null))
                                      (30 (areturn))
                                      (endofcode 31))
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
					(methodCP "run" "java.security.AccessControlContext$1" () (class "java.lang.Void"))))
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


(defconst *AccessControlContext$1-class-table*
  (make-static-class-decls 
   *java.security.AccessControlContext$1*))

(defconst *package-name-map* 
  ("java.security.AccessControlContext$1" . "java.security"))

