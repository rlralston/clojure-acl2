; Pattern$CharPropertyNames$18-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:47 CDT 2014.
;

(defconst *java.util.regex.Pattern$CharPropertyNames$18*
 (make-class-def
      '(class "java.util.regex.Pattern$CharPropertyNames$18"
            "java.util.regex.Pattern$CharPropertyNames$CloneableProperty"
            (constant_pool)
            (fields)
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aconst_null))
                                      (2 (invokespecial
					(methodCP "<init>" "java.util.regex.Pattern$CharPropertyNames$CloneableProperty" ((class "java.util.regex.Pattern$1")) void)))
                                      (5 (return))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isSatisfiedBy"
                              (parameters int)
                              (returntype . boolean)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 5)
                                   (parsedcode
                                      (0 (iload_1))
                                      (1 (invokestatic
					(methodCP "isUnicodeIdentifierPart" "java.lang.Character" (int) boolean)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "InnerClasses")))))


(defconst *Pattern$CharPropertyNames$18-class-table*
  (make-static-class-decls 
   *java.util.regex.Pattern$CharPropertyNames$18*))

(defconst *package-name-map* 
  ("java.util.regex.Pattern$CharPropertyNames$18" . "java.util.regex"))

