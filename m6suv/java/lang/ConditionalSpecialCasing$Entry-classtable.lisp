; ConditionalSpecialCasing$Entry-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:33 CDT 2014.
;

(defconst *java.lang.ConditionalSpecialCasing$Entry*
 (make-class-def
      '(class "java.lang.ConditionalSpecialCasing$Entry"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "ch" int (accessflags  *class* ) -1)
                        (field "lower" (array char) (accessflags  *class* ) -1)
                        (field "upper" (array char) (accessflags  *class* ) -1)
                        (field "lang" (class "java.lang.String") (accessflags  *class* ) -1)
                        (field "condition" int (accessflags  *class* ) -1))
            (methods
                        (method "<init>"
                              (parameters int (array char) (array char) (class "java.lang.String") int)
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 6) (code_length . 32)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (iload_1))
                                      (6 (putfield (fieldCP "ch" "java.lang.ConditionalSpecialCasing$Entry" int)))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "lower" "java.lang.ConditionalSpecialCasing$Entry" (array char))))
                                      (14 (aload_0))
                                      (15 (aload_3))
                                      (16 (putfield (fieldCP "upper" "java.lang.ConditionalSpecialCasing$Entry" (array char))))
                                      (19 (aload_0))
                                      (20 (aload 4))
                                      (22 (putfield (fieldCP "lang" "java.lang.ConditionalSpecialCasing$Entry" (class "java.lang.String"))))
                                      (25 (aload_0))
                                      (26 (iload 5))
                                      (28 (putfield (fieldCP "condition" "java.lang.ConditionalSpecialCasing$Entry" int)))
                                      (31 (return))
                                      (endofcode 32))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getCodePoint"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "ch" "java.lang.ConditionalSpecialCasing$Entry" int)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getLowerCase"
                              (parameters )
                              (returntype . (array char))
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "lower" "java.lang.ConditionalSpecialCasing$Entry" (array char))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getUpperCase"
                              (parameters )
                              (returntype . (array char))
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "upper" "java.lang.ConditionalSpecialCasing$Entry" (array char))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getLanguage"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "lang" "java.lang.ConditionalSpecialCasing$Entry" (class "java.lang.String"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getCondition"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "condition" "java.lang.ConditionalSpecialCasing$Entry" int)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *ConditionalSpecialCasing$Entry-class-table*
  (make-static-class-decls 
   *java.lang.ConditionalSpecialCasing$Entry*))

(defconst *package-name-map* 
  ("java.lang.ConditionalSpecialCasing$Entry" . "java.lang"))

