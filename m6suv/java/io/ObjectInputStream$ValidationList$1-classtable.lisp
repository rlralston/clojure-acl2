; ObjectInputStream$ValidationList$1-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:32 CDT 2014.
;

(defconst *java.io.ObjectInputStream$ValidationList$1*
 (make-class-def
      '(class "java.io.ObjectInputStream$ValidationList$1"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "this$0" (class "java.io.ObjectInputStream$ValidationList") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.io.ObjectInputStream$ValidationList"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.io.ObjectInputStream$ValidationList$1" (class "java.io.ObjectInputStream$ValidationList"))))
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
                                   (max_stack . 1) (max_locals . 1) (code_length . 17)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "this$0" "java.io.ObjectInputStream$ValidationList$1" (class "java.io.ObjectInputStream$ValidationList"))))
                                      (4 (invokestatic
					(methodCP "access$400" "java.io.ObjectInputStream$ValidationList" ((class "java.io.ObjectInputStream$ValidationList")) (class "java.io.ObjectInputStream$ValidationList$Callback"))))
                                      (7 (getfield (fieldCP "obj" "java.io.ObjectInputStream$ValidationList$Callback" (class "java.io.ObjectInputValidation"))))
                                      (10 (invokeinterface
					(methodCP "validateObject" "java.io.ObjectInputValidation" () void) 1))
                                      (15 (aconst_null))
                                      (16 (areturn))
                                      (endofcode 17))
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
					(methodCP "run" "java.io.ObjectInputStream$ValidationList$1" () (class "java.lang.Void"))))
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


(defconst *ObjectInputStream$ValidationList$1-class-table*
  (make-static-class-decls 
   *java.io.ObjectInputStream$ValidationList$1*))

(defconst *package-name-map* 
  ("java.io.ObjectInputStream$ValidationList$1" . "java.io"))

