; UnknownFormatFlagsException-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:48 CDT 2014.
;

(defconst *java.util.UnknownFormatFlagsException*
 (make-class-def
      '(class "java.util.UnknownFormatFlagsException"
            "java.util.IllegalFormatException"
            (constant_pool
                        (LONG 19370506)
                        (STRING  "Flags = "))
            (fields
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0)
                        (field "flags" (class "java.lang.String") (accessflags  *class*  *private* ) -1))
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
                                      (18 (putfield (fieldCP "flags" "java.util.UnknownFormatFlagsException" (class "java.lang.String")))) 
                                      (21 (return)) 
                                      (endofcode 22))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getFlags"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "flags" "java.util.UnknownFormatFlagsException" (class "java.lang.String"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getMessage"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 23)
                                   (parsedcode
                                      (0 (new (class "java.lang.StringBuilder")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.lang.StringBuilder" () void)))
                                      (7 (ldc 1))         ;;STRING:: "Flags = "
                                      (9 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (12 (aload_0))
                                      (13 (getfield (fieldCP "flags" "java.util.UnknownFormatFlagsException" (class "java.lang.String"))))
                                      (16 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (19 (invokevirtual
					(methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String"))))
                                      (22 (areturn))
                                      (endofcode 23))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *UnknownFormatFlagsException-class-table*
  (make-static-class-decls 
   *java.util.UnknownFormatFlagsException*))

(defconst *package-name-map* 
  ("java.util.UnknownFormatFlagsException" . "java.util"))

