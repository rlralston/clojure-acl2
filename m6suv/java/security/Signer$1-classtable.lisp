; Signer$1-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:41 CDT 2014.
;

(defconst *java.security.Signer$1*
 (make-class-def
      '(class "java.security.Signer$1"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "val$pub" (class "java.security.PublicKey") (accessflags  *class*  *final* ) -1)
                        (field "this$0" (class "java.security.Signer") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.security.Signer") (class "java.security.PublicKey"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.security.Signer$1" (class "java.security.Signer"))))
                                      (5 (aload_0))
                                      (6 (aload_2))
                                      (7 (putfield (fieldCP "val$pub" "java.security.Signer$1" (class "java.security.PublicKey"))))
                                      (10 (aload_0))
                                      (11 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "run"
                              (parameters )
                              (returntype . (class "java.lang.Void"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 13)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "this$0" "java.security.Signer$1" (class "java.security.Signer"))))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "val$pub" "java.security.Signer$1" (class "java.security.PublicKey"))))
                                      (8 (invokevirtual
					(methodCP "setPublicKey" "java.security.Signer" ((class "java.security.PublicKey")) void)))
                                      (11 (aconst_null))
                                      (12 (areturn))
                                      (endofcode 13))
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
					(methodCP "run" "java.security.Signer$1" () (class "java.lang.Void"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.security.PrivilegedExceptionAction")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *Signer$1-class-table*
  (make-static-class-decls 
   *java.security.Signer$1*))

(defconst *package-name-map* 
  ("java.security.Signer$1" . "java.security"))

