; ResourceBundle$RBClassLoader-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:47 CDT 2014.
;

(defconst *java.util.ResourceBundle$RBClassLoader*
 (make-class-def
      '(class "java.util.ResourceBundle$RBClassLoader"
            "java.lang.ClassLoader"
            (constant_pool)
            (fields
                        (field "INSTANCE" (class "java.util.ResourceBundle$RBClassLoader") (accessflags  *class*  *final*  *private*  *static* ) -1)
                        (field "loader" (class "java.lang.ClassLoader") (accessflags  *class*  *final*  *private*  *static* ) -1))
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.ClassLoader" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "loadClass"
                              (parameters (class "java.lang.String"))
                              (returntype . (class "java.lang.Class"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 19)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "loader" "java.util.ResourceBundle$RBClassLoader" (class "java.lang.ClassLoader")))) 
                                      (3 (ifnull 14))  ;;to TAG_0
                                      (6 (getstatic (fieldCP "loader" "java.util.ResourceBundle$RBClassLoader" (class "java.lang.ClassLoader")))) 
                                      (9 (aload_1)) 
                                      (10 (invokevirtual (methodCP "loadClass" "java.lang.ClassLoader" ((class "java.lang.String")) (class "java.lang.Class")))) 
                                      (13 (areturn)) 
                                      (14 (aload_1)) ;;at TAG_0
                                      (15 (invokestatic (methodCP "forName" "java.lang.Class" ((class "java.lang.String")) (class "java.lang.Class")))) 
                                      (18 (areturn)) 
                                      (endofcode 19))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getResource"
                              (parameters (class "java.lang.String"))
                              (returntype . (class "java.net.URL"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 19)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "loader" "java.util.ResourceBundle$RBClassLoader" (class "java.lang.ClassLoader")))) 
                                      (3 (ifnull 14))  ;;to TAG_0
                                      (6 (getstatic (fieldCP "loader" "java.util.ResourceBundle$RBClassLoader" (class "java.lang.ClassLoader")))) 
                                      (9 (aload_1)) 
                                      (10 (invokevirtual (methodCP "getResource" "java.lang.ClassLoader" ((class "java.lang.String")) (class "java.net.URL")))) 
                                      (13 (areturn)) 
                                      (14 (aload_1)) ;;at TAG_0
                                      (15 (invokestatic (methodCP "getSystemResource" "java.lang.ClassLoader" ((class "java.lang.String")) (class "java.net.URL")))) 
                                      (18 (areturn)) 
                                      (endofcode 19))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getResourceAsStream"
                              (parameters (class "java.lang.String"))
                              (returntype . (class "java.io.InputStream"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 19)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "loader" "java.util.ResourceBundle$RBClassLoader" (class "java.lang.ClassLoader")))) 
                                      (3 (ifnull 14))  ;;to TAG_0
                                      (6 (getstatic (fieldCP "loader" "java.util.ResourceBundle$RBClassLoader" (class "java.lang.ClassLoader")))) 
                                      (9 (aload_1)) 
                                      (10 (invokevirtual (methodCP "getResourceAsStream" "java.lang.ClassLoader" ((class "java.lang.String")) (class "java.io.InputStream")))) 
                                      (13 (areturn)) 
                                      (14 (aload_1)) ;;at TAG_0
                                      (15 (invokestatic (methodCP "getSystemResourceAsStream" "java.lang.ClassLoader" ((class "java.lang.String")) (class "java.io.InputStream")))) 
                                      (18 (areturn)) 
                                      (endofcode 19))
                                   (Exceptions )
                                   (StackMap )))
                        (method "access$000"
                              (parameters )
                              (returntype . (class "java.util.ResourceBundle$RBClassLoader"))
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 0) (code_length . 4)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "INSTANCE" "java.util.ResourceBundle$RBClassLoader" (class "java.util.ResourceBundle$RBClassLoader"))))
                                      (3 (areturn))
                                      (endofcode 4))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.util.ResourceBundle$1"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.util.ResourceBundle$RBClassLoader" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 23)
                                   (parsedcode
                                      (0 (new (class "java.util.ResourceBundle$RBClassLoader$1")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.util.ResourceBundle$RBClassLoader$1" () void)))
                                      (7 (invokestatic
					(methodCP "doPrivileged" "java.security.AccessController" ((class "java.security.PrivilegedAction")) (class "java.lang.Object"))))
                                      (10 (checkcast (class "java.util.ResourceBundle$RBClassLoader")))
                                      (13 (putstatic (fieldCP "INSTANCE" "java.util.ResourceBundle$RBClassLoader" (class "java.util.ResourceBundle$RBClassLoader"))))
                                      (16 (invokestatic
					(methodCP "getSystemClassLoader" "java.lang.ClassLoader" () (class "java.lang.ClassLoader"))))
                                      (19 (putstatic (fieldCP "loader" "java.util.ResourceBundle$RBClassLoader" (class "java.lang.ClassLoader"))))
                                      (22 (return))
                                      (endofcode 23))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *ResourceBundle$RBClassLoader-class-table*
  (make-static-class-decls 
   *java.util.ResourceBundle$RBClassLoader*))

(defconst *package-name-map* 
  ("java.util.ResourceBundle$RBClassLoader" . "java.util"))

