; ResourceBundle$SingleFormatControl-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:47 CDT 2014.
;

(defconst *java.util.ResourceBundle$SingleFormatControl*
 (make-class-def
      '(class "java.util.ResourceBundle$SingleFormatControl"
            "java.util.ResourceBundle$Control"
            (constant_pool)
            (fields
                        (field "PROPERTIES_ONLY" (class "java.util.ResourceBundle$Control") (accessflags  *class*  *final*  *private*  *static* ) -1)
                        (field "CLASS_ONLY" (class "java.util.ResourceBundle$Control") (accessflags  *class*  *final*  *private*  *static* ) -1)
                        (field "formats" (class "java.util.List") (accessflags  *class*  *final*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.List"))
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.util.ResourceBundle$Control" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "formats" "java.util.ResourceBundle$SingleFormatControl" (class "java.util.List"))))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getFormats"
                              (parameters (class "java.lang.String"))
                              (returntype . (class "java.util.List"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 17)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (ifnonnull 12))  ;;to TAG_0
                                      (4 (new (class "java.lang.NullPointerException"))) 
                                      (7 (dup)) 
                                      (8 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" () void))) 
                                      (11 (athrow)) 
                                      (12 (aload_0)) ;;at TAG_0
                                      (13 (getfield (fieldCP "formats" "java.util.ResourceBundle$SingleFormatControl" (class "java.util.List")))) 
                                      (16 (areturn)) 
                                      (endofcode 17))
                                   (Exceptions )
                                   (StackMap )))
                        (method "access$800"
                              (parameters )
                              (returntype . (class "java.util.ResourceBundle$Control"))
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 0) (code_length . 4)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "PROPERTIES_ONLY" "java.util.ResourceBundle$SingleFormatControl" (class "java.util.ResourceBundle$Control"))))
                                      (3 (areturn))
                                      (endofcode 4))
                                   (Exceptions )
                                   (StackMap )))
                        (method "access$900"
                              (parameters )
                              (returntype . (class "java.util.ResourceBundle$Control"))
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 0) (code_length . 4)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "CLASS_ONLY" "java.util.ResourceBundle$SingleFormatControl" (class "java.util.ResourceBundle$Control"))))
                                      (3 (areturn))
                                      (endofcode 4))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 0) (code_length . 27)
                                   (parsedcode
                                      (0 (new (class "java.util.ResourceBundle$SingleFormatControl")))
                                      (3 (dup))
                                      (4 (getstatic (fieldCP "FORMAT_PROPERTIES" "java.util.ResourceBundle$SingleFormatControl" (class "java.util.List"))))
                                      (7 (invokespecial
					(methodCP "<init>" "java.util.ResourceBundle$SingleFormatControl" ((class "java.util.List")) void)))
                                      (10 (putstatic (fieldCP "PROPERTIES_ONLY" "java.util.ResourceBundle$SingleFormatControl" (class "java.util.ResourceBundle$Control"))))
                                      (13 (new (class "java.util.ResourceBundle$SingleFormatControl")))
                                      (16 (dup))
                                      (17 (getstatic (fieldCP "FORMAT_CLASS" "java.util.ResourceBundle$SingleFormatControl" (class "java.util.List"))))
                                      (20 (invokespecial
					(methodCP "<init>" "java.util.ResourceBundle$SingleFormatControl" ((class "java.util.List")) void)))
                                      (23 (putstatic (fieldCP "CLASS_ONLY" "java.util.ResourceBundle$SingleFormatControl" (class "java.util.ResourceBundle$Control"))))
                                      (26 (return))
                                      (endofcode 27))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *ResourceBundle$SingleFormatControl-class-table*
  (make-static-class-decls 
   *java.util.ResourceBundle$SingleFormatControl*))

(defconst *package-name-map* 
  ("java.util.ResourceBundle$SingleFormatControl" . "java.util"))
