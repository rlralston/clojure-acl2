; URLClassLoader$6-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:38 CDT 2014.
;

(defconst *java.net.URLClassLoader$6*
 (make-class-def
      '(class "java.net.URLClassLoader$6"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "val$urls" (array (class "java.net.URL")) (accessflags  *class*  *final* ) -1)
                        (field "val$acc" (class "java.security.AccessControlContext") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (array (class "java.net.URL")) (class "java.security.AccessControlContext"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "val$urls" "java.net.URLClassLoader$6" (array (class "java.net.URL")))))
                                      (5 (aload_0))
                                      (6 (aload_2))
                                      (7 (putfield (fieldCP "val$acc" "java.net.URLClassLoader$6" (class "java.security.AccessControlContext"))))
                                      (10 (aload_0))
                                      (11 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "run"
                              (parameters )
                              (returntype . (class "java.net.URLClassLoader"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 1) (code_length . 16)
                                   (parsedcode
                                      (0 (new (class "java.net.FactoryURLClassLoader")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "val$urls" "java.net.URLClassLoader$6" (array (class "java.net.URL")))))
                                      (8 (aload_0))
                                      (9 (getfield (fieldCP "val$acc" "java.net.URLClassLoader$6" (class "java.security.AccessControlContext"))))
                                      (12 (invokespecial
					(methodCP "<init>" "java.net.FactoryURLClassLoader" ((array (class "java.net.URL")) (class "java.security.AccessControlContext")) void)))
                                      (15 (areturn))
                                      (endofcode 16))
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
					(methodCP "run" "java.net.URLClassLoader$6" () (class "java.net.URLClassLoader"))))
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


(defconst *URLClassLoader$6-class-table*
  (make-static-class-decls 
   *java.net.URLClassLoader$6*))

(defconst *package-name-map* 
  ("java.net.URLClassLoader$6" . "java.net"))

