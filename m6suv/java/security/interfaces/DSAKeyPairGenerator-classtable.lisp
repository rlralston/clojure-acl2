; DSAKeyPairGenerator-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:40 CDT 2014.
;

(defconst *java.security.interfaces.DSAKeyPairGenerator*
 (make-class-def
      '(class "java.security.interfaces.DSAKeyPairGenerator"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "initialize"
                              (parameters (class "java.security.interfaces.DSAParams") (class "java.security.SecureRandom"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "initialize"
                              (parameters int boolean (class "java.security.SecureRandom"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces)
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")))))


(defconst *DSAKeyPairGenerator-class-table*
  (make-static-class-decls 
   *java.security.interfaces.DSAKeyPairGenerator*))

(defconst *package-name-map* 
  ("java.security.interfaces.DSAKeyPairGenerator" . "java.security.interfaces"))

