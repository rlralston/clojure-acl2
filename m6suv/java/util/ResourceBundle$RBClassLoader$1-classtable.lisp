; ResourceBundle$RBClassLoader$1-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:47 CDT 2014.
;

(defconst *java.util.ResourceBundle$RBClassLoader$1*
 (make-class-def
      '(class "java.util.ResourceBundle$RBClassLoader$1"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "run"
                              (parameters )
                              (returntype . (class "java.util.ResourceBundle$RBClassLoader"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 9)
                                   (parsedcode
                                      (0 (new (class "java.util.ResourceBundle$RBClassLoader")))
                                      (3 (dup))
                                      (4 (aconst_null))
                                      (5 (invokespecial
					(methodCP "<init>" "java.util.ResourceBundle$RBClassLoader" ((class "java.util.ResourceBundle$1")) void)))
                                      (8 (areturn))
                                      (endofcode 9))
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
					(methodCP "run" "java.util.ResourceBundle$RBClassLoader$1" () (class "java.util.ResourceBundle$RBClassLoader"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.security.PrivilegedAction")
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *ResourceBundle$RBClassLoader$1-class-table*
  (make-static-class-decls 
   *java.util.ResourceBundle$RBClassLoader$1*))

(defconst *package-name-map* 
  ("java.util.ResourceBundle$RBClassLoader$1" . "java.util"))

