; CollationKey-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:41 CDT 2014.
;

(defconst *java.text.CollationKey*
 (make-class-def
      '(class "java.text.CollationKey"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "source" (class "java.lang.String") (accessflags  *class*  *final*  *private* ) -1))
            (methods
                        (method "compareTo"
                              (parameters (class "java.text.CollationKey"))
                              (returntype . int)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getSourceString"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "source" "java.text.CollationKey" (class "java.lang.String"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "toByteArray"
                              (parameters )
                              (returntype . (array byte))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "<init>"
                              (parameters (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 22)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokespecial (methodCP "<init>" "java.lang.Object" () void))) 
                                      (4 (aload_1)) 
                                      (5 (ifnonnull 16))  ;;to TAG_0
                                      (8 (new (class "java.lang.NullPointerException"))) 
                                      (11 (dup)) 
                                      (12 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" () void))) 
                                      (15 (athrow)) 
                                      (16 (aload_0)) ;;at TAG_0
                                      (17 (aload_1)) 
                                      (18 (putfield (fieldCP "source" "java.text.CollationKey" (class "java.lang.String")))) 
                                      (21 (return)) 
                                      (endofcode 22))
                                   (Exceptions )
                                   (StackMap )))
                        (method "compareTo"
                              (parameters (class "java.lang.Object"))
                              (returntype . int)
                              (accessflags  *class*  *public*  *volatile* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (checkcast (class "java.text.CollationKey")))
                                      (5 (invokevirtual
					(methodCP "compareTo" "java.text.CollationKey" ((class "java.text.CollationKey")) int)))
                                      (8 (ireturn))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.lang.Comparable")
            (accessflags  *abstract*  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")))))


(defconst *CollationKey-class-table*
  (make-static-class-decls 
   *java.text.CollationKey*))

(defconst *package-name-map* 
  ("java.text.CollationKey" . "java.text"))
