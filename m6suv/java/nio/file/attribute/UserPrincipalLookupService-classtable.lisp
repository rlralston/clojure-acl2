; UserPrincipalLookupService-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:39 CDT 2014.
;

(defconst *java.nio.file.attribute.UserPrincipalLookupService*
 (make-class-def
      '(class "java.nio.file.attribute.UserPrincipalLookupService"
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
                        (method "lookupPrincipalByName"
                              (parameters (class "java.lang.String"))
                              (returntype . (class "java.nio.file.attribute.UserPrincipal"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "lookupPrincipalByGroupName"
                              (parameters (class "java.lang.String"))
                              (returntype . (class "java.nio.file.attribute.GroupPrincipal"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces)
            (accessflags  *abstract*  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *UserPrincipalLookupService-class-table*
  (make-static-class-decls 
   *java.nio.file.attribute.UserPrincipalLookupService*))

(defconst *package-name-map* 
  ("java.nio.file.attribute.UserPrincipalLookupService" . "java.nio.file.attribute"))

