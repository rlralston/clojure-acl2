; Font$FontAccessImpl-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:27 CDT 2014.
;

(defconst *java.awt.Font$FontAccessImpl*
 (make-class-def
      '(class "java.awt.Font$FontAccessImpl"
            "sun.font.FontAccess"
            (constant_pool)
            (fields)
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
					(methodCP "<init>" "sun.font.FontAccess" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getFont2D"
                              (parameters (class "java.awt.Font"))
                              (returntype . (class "sun.font.Font2D"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (invokestatic
					(methodCP "access$000" "java.awt.Font" ((class "java.awt.Font")) (class "sun.font.Font2D"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "setFont2D"
                              (parameters (class "java.awt.Font") (class "sun.font.Font2DHandle"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 7)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (aload_2))
                                      (2 (invokestatic
					(methodCP "access$102" "java.awt.Font" ((class "java.awt.Font") (class "sun.font.Font2DHandle")) (class "sun.font.Font2DHandle"))))
                                      (5 (pop))
                                      (6 (return))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap )))
                        (method "setCreatedFont"
                              (parameters (class "java.awt.Font"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 7)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (iconst_1))
                                      (2 (invokestatic
					(methodCP "access$202" "java.awt.Font" ((class "java.awt.Font") boolean) boolean)))
                                      (5 (pop))
                                      (6 (return))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isCreatedFont"
                              (parameters (class "java.awt.Font"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (invokestatic
					(methodCP "access$200" "java.awt.Font" ((class "java.awt.Font")) boolean)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.awt.Font$1"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.awt.Font$FontAccessImpl" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *Font$FontAccessImpl-class-table*
  (make-static-class-decls 
   *java.awt.Font$FontAccessImpl*))

(defconst *package-name-map* 
  ("java.awt.Font$FontAccessImpl" . "java.awt"))
