; Collections$UnmodifiableRandomAccessList-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:43 CDT 2014.
;

(defconst *java.util.Collections$UnmodifiableRandomAccessList*
 (make-class-def
      '(class "java.util.Collections$UnmodifiableRandomAccessList"
            "java.util.Collections$UnmodifiableList"
            (constant_pool
                        (LONG -2542308836966382001))
            (fields
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.List"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokespecial
					(methodCP "<init>" "java.util.Collections$UnmodifiableList" ((class "java.util.List")) void)))
                                      (5 (return))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "subList"
                              (parameters int int)
                              (returntype . (class "java.util.List"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 3) (code_length . 19)
                                   (parsedcode
                                      (0 (new (class "java.util.Collections$UnmodifiableRandomAccessList")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "list" "java.util.Collections$UnmodifiableRandomAccessList" (class "java.util.List"))))
                                      (8 (iload_1))
                                      (9 (iload_2))
                                      (10 (invokeinterface
					(methodCP "subList" "java.util.List" (int int) (class "java.util.List")) 3))
                                      (15 (invokespecial
					(methodCP "<init>" "java.util.Collections$UnmodifiableRandomAccessList" ((class "java.util.List")) void)))
                                      (18 (areturn))
                                      (endofcode 19))
                                   (Exceptions )
                                   (StackMap )))
                        (method "writeReplace"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 12)
                                   (parsedcode
                                      (0 (new (class "java.util.Collections$UnmodifiableList")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "list" "java.util.Collections$UnmodifiableRandomAccessList" (class "java.util.List"))))
                                      (8 (invokespecial
					(methodCP "<init>" "java.util.Collections$UnmodifiableList" ((class "java.util.List")) void)))
                                      (11 (areturn))
                                      (endofcode 12))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.util.RandomAccess")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *Collections$UnmodifiableRandomAccessList-class-table*
  (make-static-class-decls 
   *java.util.Collections$UnmodifiableRandomAccessList*))

(defconst *package-name-map* 
  ("java.util.Collections$UnmodifiableRandomAccessList" . "java.util"))

