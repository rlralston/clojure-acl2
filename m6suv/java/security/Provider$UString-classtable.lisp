; Provider$UString-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:40 CDT 2014.
;

(defconst *java.security.Provider$UString*
 (make-class-def
      '(class "java.security.Provider$UString"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "string" (class "java.lang.String") (accessflags  *class*  *final* ) -1)
                        (field "lowerString" (class "java.lang.String") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 21)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "string" "java.security.Provider$UString" (class "java.lang.String"))))
                                      (9 (aload_0))
                                      (10 (aload_1))
                                      (11 (getstatic (fieldCP "ENGLISH" "java.util.Locale" (class "java.util.Locale"))))
                                      (14 (invokevirtual
					(methodCP "toLowerCase" "java.lang.String" ((class "java.util.Locale")) (class "java.lang.String"))))
                                      (17 (putfield (fieldCP "lowerString" "java.security.Provider$UString" (class "java.lang.String"))))
                                      (20 (return))
                                      (endofcode 21))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hashCode"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "lowerString" "java.security.Provider$UString" (class "java.lang.String"))))
                                      (4 (invokevirtual
					(methodCP "hashCode" "java.lang.String" () int)))
                                      (7 (ireturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "equals"
                              (parameters (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 33)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (aload_1)) 
                                      (2 (if_acmpne 7))  ;;to TAG_0
                                      (5 (iconst_1)) 
                                      (6 (ireturn)) 
                                      (7 (aload_1)) ;;at TAG_0
                                      (8 (instanceof (class "java.security.Provider$UString"))) 
                                      (11 (ifne 16)) ;;to TAG_1
                                      (14 (iconst_0)) 
                                      (15 (ireturn)) 
                                      (16 (aload_1)) ;;at TAG_1
                                      (17 (checkcast (class "java.security.Provider$UString"))) 
                                      (20 (astore_2)) 
                                      (21 (aload_0)) 
                                      (22 (getfield (fieldCP "lowerString" "java.security.Provider$UString" (class "java.lang.String")))) 
                                      (25 (aload_2)) 
                                      (26 (getfield (fieldCP "lowerString" "java.security.Provider$UString" (class "java.lang.String")))) 
                                      (29 (invokevirtual (methodCP "equals" "java.lang.String" ((class "java.lang.Object")) boolean))) 
                                      (32 (ireturn)) 
                                      (endofcode 33))
                                   (Exceptions )
                                   (StackMap )))
                        (method "toString"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "string" "java.security.Provider$UString" (class "java.lang.String"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *Provider$UString-class-table*
  (make-static-class-decls 
   *java.security.Provider$UString*))

(defconst *package-name-map* 
  ("java.security.Provider$UString" . "java.security"))

