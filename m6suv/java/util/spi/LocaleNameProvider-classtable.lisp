; LocaleNameProvider-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:48 CDT 2014.
;

(defconst *java.util.spi.LocaleNameProvider*
 (make-class-def
      '(class "java.util.spi.LocaleNameProvider"
            "java.util.spi.LocaleServiceProvider"
            (constant_pool)
            (fields)
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.util.spi.LocaleServiceProvider" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getDisplayLanguage"
                              (parameters (class "java.lang.String") (class "java.util.Locale"))
                              (returntype . (class "java.lang.String"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getDisplayScript"
                              (parameters (class "java.lang.String") (class "java.util.Locale"))
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 3) (code_length . 2)
                                   (parsedcode
                                      (0 (aconst_null))
                                      (1 (areturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getDisplayCountry"
                              (parameters (class "java.lang.String") (class "java.util.Locale"))
                              (returntype . (class "java.lang.String"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getDisplayVariant"
                              (parameters (class "java.lang.String") (class "java.util.Locale"))
                              (returntype . (class "java.lang.String"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces)
            (accessflags  *abstract*  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *LocaleNameProvider-class-table*
  (make-static-class-decls 
   *java.util.spi.LocaleNameProvider*))

(defconst *package-name-map* 
  ("java.util.spi.LocaleNameProvider" . "java.util.spi"))

