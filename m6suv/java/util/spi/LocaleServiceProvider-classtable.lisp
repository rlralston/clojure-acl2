; LocaleServiceProvider-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:48 CDT 2014.
;

(defconst *java.util.spi.LocaleServiceProvider*
 (make-class-def
      '(class "java.util.spi.LocaleServiceProvider"
            "java.lang.Object"
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
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getAvailableLocales"
                              (parameters )
                              (returntype . (array (class "java.util.Locale")))
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces)
            (accessflags  *abstract*  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *LocaleServiceProvider-class-table*
  (make-static-class-decls 
   *java.util.spi.LocaleServiceProvider*))

(defconst *package-name-map* 
  ("java.util.spi.LocaleServiceProvider" . "java.util.spi"))

