; URLClassLoader$5-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:38 CDT 2014.
;

(defconst *java.net.URLClassLoader$5*
 (make-class-def
      '(class "java.net.URLClassLoader$5"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "val$urls" (array (class "java.net.URL")) (accessflags  *class*  *final* ) -1)
                        (field "val$parent" (class "java.lang.ClassLoader") (accessflags  *class*  *final* ) -1)
                        (field "val$acc" (class "java.security.AccessControlContext") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (array (class "java.net.URL")) (class "java.lang.ClassLoader") (class "java.security.AccessControlContext"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 20)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "val$urls" "java.net.URLClassLoader$5" (array (class "java.net.URL")))))
                                      (5 (aload_0))
                                      (6 (aload_2))
                                      (7 (putfield (fieldCP "val$parent" "java.net.URLClassLoader$5" (class "java.lang.ClassLoader"))))
                                      (10 (aload_0))
                                      (11 (aload_3))
                                      (12 (putfield (fieldCP "val$acc" "java.net.URLClassLoader$5" (class "java.security.AccessControlContext"))))
                                      (15 (aload_0))
                                      (16 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (19 (return))
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap )))
                        (method "run"
                              (parameters )
                              (returntype . (class "java.net.URLClassLoader"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 1) (code_length . 20)
                                   (parsedcode
                                      (0 (new (class "java.net.FactoryURLClassLoader")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "val$urls" "java.net.URLClassLoader$5" (array (class "java.net.URL")))))
                                      (8 (aload_0))
                                      (9 (getfield (fieldCP "val$parent" "java.net.URLClassLoader$5" (class "java.lang.ClassLoader"))))
                                      (12 (aload_0))
                                      (13 (getfield (fieldCP "val$acc" "java.net.URLClassLoader$5" (class "java.security.AccessControlContext"))))
                                      (16 (invokespecial
					(methodCP "<init>" "java.net.FactoryURLClassLoader" ((array (class "java.net.URL")) (class "java.lang.ClassLoader") (class "java.security.AccessControlContext")) void)))
                                      (19 (areturn))
                                      (endofcode 20))
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
					(methodCP "run" "java.net.URLClassLoader$5" () (class "java.net.URLClassLoader"))))
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


(defconst *URLClassLoader$5-class-table*
  (make-static-class-decls 
   *java.net.URLClassLoader$5*))

(defconst *package-name-map* 
  ("java.net.URLClassLoader$5" . "java.net"))

