; RMIClassLoader$2-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:40 CDT 2014.
;

(defconst *java.rmi.server.RMIClassLoader$2*
 (make-class-def
      '(class "java.rmi.server.RMIClassLoader$2"
            "java.rmi.server.RMIClassLoaderSpi"
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
					(methodCP "<init>" "java.rmi.server.RMIClassLoaderSpi" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "loadClass"
                              (parameters (class "java.lang.String") (class "java.lang.String") (class "java.lang.ClassLoader"))
                              (returntype . (class "java.lang.Class"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 4) (code_length . 7)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (aload_2))
                                      (2 (aload_3))
                                      (3 (invokestatic
					(methodCP "loadClass" "sun.rmi.server.LoaderHandler" ((class "java.lang.String") (class "java.lang.String") (class "java.lang.ClassLoader")) (class "java.lang.Class"))))
                                      (6 (areturn))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap )))
                        (method "loadProxyClass"
                              (parameters (class "java.lang.String") (array (class "java.lang.String")) (class "java.lang.ClassLoader"))
                              (returntype . (class "java.lang.Class"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 4) (code_length . 7)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (aload_2))
                                      (2 (aload_3))
                                      (3 (invokestatic
					(methodCP "loadProxyClass" "sun.rmi.server.LoaderHandler" ((class "java.lang.String") (array (class "java.lang.String")) (class "java.lang.ClassLoader")) (class "java.lang.Class"))))
                                      (6 (areturn))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getClassLoader"
                              (parameters (class "java.lang.String"))
                              (returntype . (class "java.lang.ClassLoader"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (invokestatic
					(methodCP "getClassLoader" "sun.rmi.server.LoaderHandler" ((class "java.lang.String")) (class "java.lang.ClassLoader"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getClassAnnotation"
                              (parameters (class "java.lang.Class"))
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (invokestatic
					(methodCP "getClassAnnotation" "sun.rmi.server.LoaderHandler" ((class "java.lang.Class")) (class "java.lang.String"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "InnerClasses")))))


(defconst *RMIClassLoader$2-class-table*
  (make-static-class-decls 
   *java.rmi.server.RMIClassLoader$2*))

(defconst *package-name-map* 
  ("java.rmi.server.RMIClassLoader$2" . "java.rmi.server"))

