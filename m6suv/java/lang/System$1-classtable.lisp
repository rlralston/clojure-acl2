; System$1-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:36 CDT 2014.
;

(defconst *java.lang.System$1*
 (make-class-def
      '(class "java.lang.System$1"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "val$s" (class "java.lang.SecurityManager") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.lang.SecurityManager"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "val$s" "java.lang.System$1" (class "java.lang.SecurityManager"))))
                                      (5 (aload_0))
                                      (6 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "run"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 19)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "val$s" "java.lang.System$1" (class "java.lang.SecurityManager"))))
                                      (4 (invokevirtual
					(methodCP "getClass" "java.lang.Object" () (class "java.lang.Class"))))
                                      (7 (invokevirtual
					(methodCP "getProtectionDomain" "java.lang.Class" () (class "java.security.ProtectionDomain"))))
                                      (10 (getstatic (fieldCP "ALL_PERMISSION" "sun.security.util.SecurityConstants" (class "java.security.AllPermission"))))
                                      (13 (invokevirtual
					(methodCP "implies" "java.security.ProtectionDomain" ((class "java.security.Permission")) boolean)))
                                      (16 (pop))
                                      (17 (aconst_null))
                                      (18 (areturn))
                                      (endofcode 19))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.security.PrivilegedAction")
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *System$1-class-table*
  (make-static-class-decls 
   *java.lang.System$1*))

(defconst *package-name-map* 
  ("java.lang.System$1" . "java.lang"))

