; PropertyResourceBundle-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:47 CDT 2014.
;

(defconst *java.util.PropertyResourceBundle*
 (make-class-def
      '(class "java.util.PropertyResourceBundle"
            "java.util.ResourceBundle"
            (constant_pool)
            (fields
                        (field "lookup" (class "java.util.Map") (accessflags  *class*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.io.InputStream"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 30)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.util.ResourceBundle" () void)))
                                      (4 (new (class "java.util.Properties")))
                                      (7 (dup))
                                      (8 (invokespecial
					(methodCP "<init>" "java.util.Properties" () void)))
                                      (11 (astore_2))
                                      (12 (aload_2))
                                      (13 (aload_1))
                                      (14 (invokevirtual
					(methodCP "load" "java.util.Properties" ((class "java.io.InputStream")) void)))
                                      (17 (aload_0))
                                      (18 (new (class "java.util.HashMap")))
                                      (21 (dup))
                                      (22 (aload_2))
                                      (23 (invokespecial
					(methodCP "<init>" "java.util.HashMap" ((class "java.util.Map")) void)))
                                      (26 (putfield (fieldCP "lookup" "java.util.PropertyResourceBundle" (class "java.util.Map"))))
                                      (29 (return))
                                      (endofcode 30))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.io.Reader"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 30)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.util.ResourceBundle" () void)))
                                      (4 (new (class "java.util.Properties")))
                                      (7 (dup))
                                      (8 (invokespecial
					(methodCP "<init>" "java.util.Properties" () void)))
                                      (11 (astore_2))
                                      (12 (aload_2))
                                      (13 (aload_1))
                                      (14 (invokevirtual
					(methodCP "load" "java.util.Properties" ((class "java.io.Reader")) void)))
                                      (17 (aload_0))
                                      (18 (new (class "java.util.HashMap")))
                                      (21 (dup))
                                      (22 (aload_2))
                                      (23 (invokespecial
					(methodCP "<init>" "java.util.HashMap" ((class "java.util.Map")) void)))
                                      (26 (putfield (fieldCP "lookup" "java.util.PropertyResourceBundle" (class "java.util.Map"))))
                                      (29 (return))
                                      (endofcode 30))
                                   (Exceptions )
                                   (StackMap )))
                        (method "handleGetObject"
                              (parameters (class "java.lang.String"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 23)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (ifnonnull 12))  ;;to TAG_0
                                      (4 (new (class "java.lang.NullPointerException"))) 
                                      (7 (dup)) 
                                      (8 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" () void))) 
                                      (11 (athrow)) 
                                      (12 (aload_0)) ;;at TAG_0
                                      (13 (getfield (fieldCP "lookup" "java.util.PropertyResourceBundle" (class "java.util.Map")))) 
                                      (16 (aload_1)) 
                                      (17 (invokeinterface (methodCP "get" "java.util.Map" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (22 (areturn)) 
                                      (endofcode 23))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getKeys"
                              (parameters )
                              (returntype . (class "java.util.Enumeration"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 34)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "parent" "java.util.PropertyResourceBundle" (class "java.util.ResourceBundle")))) 
                                      (4 (astore_1)) 
                                      (5 (new (class "sun.util.ResourceBundleEnumeration"))) 
                                      (8 (dup)) 
                                      (9 (aload_0)) 
                                      (10 (getfield (fieldCP "lookup" "java.util.PropertyResourceBundle" (class "java.util.Map")))) 
                                      (13 (invokeinterface (methodCP "keySet" "java.util.Map" () (class "java.util.Set")) 1)) 
                                      (18 (aload_1)) 
                                      (19 (ifnull 29))  ;;to TAG_0
                                      (22 (aload_1)) 
                                      (23 (invokevirtual (methodCP "getKeys" "java.util.ResourceBundle" () (class "java.util.Enumeration")))) 
                                      (26 (goto 30)) ;;to TAG_1
                                      (29 (aconst_null)) ;;at TAG_0
                                      (30 (invokespecial (methodCP "<init>" "sun.util.ResourceBundleEnumeration" ((class "java.util.Set") (class "java.util.Enumeration")) void))) ;;at TAG_1
                                      (33 (areturn)) 
                                      (endofcode 34))
                                   (Exceptions )
                                   (StackMap )))
                        (method "handleKeySet"
                              (parameters )
                              (returntype . (class "java.util.Set"))
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "lookup" "java.util.PropertyResourceBundle" (class "java.util.Map"))))
                                      (4 (invokeinterface
					(methodCP "keySet" "java.util.Map" () (class "java.util.Set")) 1))
                                      (9 (areturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *PropertyResourceBundle-class-table*
  (make-static-class-decls 
   *java.util.PropertyResourceBundle*))

(defconst *package-name-map* 
  ("java.util.PropertyResourceBundle" . "java.util"))

