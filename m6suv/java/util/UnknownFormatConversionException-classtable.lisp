; UnknownFormatConversionException-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:48 CDT 2014.
;

(defconst *java.util.UnknownFormatConversionException*
 (make-class-def
      '(class "java.util.UnknownFormatConversionException"
            "java.util.IllegalFormatException"
            (constant_pool
                        (LONG 19060418)
                        (STRING  "Conversion = \n%s\n"))
            (fields
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0)
                        (field "s" (class "java.lang.String") (accessflags  *class*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 22)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokespecial (methodCP "<init>" "java.util.IllegalFormatException" () void))) 
                                      (4 (aload_1)) 
                                      (5 (ifnonnull 16))  ;;to TAG_0
                                      (8 (new (class "java.lang.NullPointerException"))) 
                                      (11 (dup)) 
                                      (12 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" () void))) 
                                      (15 (athrow)) 
                                      (16 (aload_0)) ;;at TAG_0
                                      (17 (aload_1)) 
                                      (18 (putfield (fieldCP "s" "java.util.UnknownFormatConversionException" (class "java.lang.String")))) 
                                      (21 (return)) 
                                      (endofcode 22))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getConversion"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "s" "java.util.UnknownFormatConversionException" (class "java.lang.String"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getMessage"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 1) (code_length . 17)
                                   (parsedcode
                                      (0 (ldc 1))         ;;STRING:: "Conversion = \n%s\n"
                                      (2 (iconst_1))
                                      (3 (anewarray (class "java.lang.Object")))
                                      (6 (dup))
                                      (7 (iconst_0))
                                      (8 (aload_0))
                                      (9 (getfield (fieldCP "s" "java.util.UnknownFormatConversionException" (class "java.lang.String"))))
                                      (12 (aastore))
                                      (13 (invokestatic
					(methodCP "format" "java.lang.String" ((class "java.lang.String") (array (class "java.lang.Object"))) (class "java.lang.String"))))
                                      (16 (areturn))
                                      (endofcode 17))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *UnknownFormatConversionException-class-table*
  (make-static-class-decls 
   *java.util.UnknownFormatConversionException*))

(defconst *package-name-map* 
  ("java.util.UnknownFormatConversionException" . "java.util"))

