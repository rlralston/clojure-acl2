; RSAPrivateKey-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:40 CDT 2014.
;

(defconst *java.security.interfaces.RSAPrivateKey*
 (make-class-def
      '(class "java.security.interfaces.RSAPrivateKey"
            "java.lang.Object"
            (constant_pool
                        (LONG 5187144804936595022))
            (fields
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *public*  *static* ) 0))
            (methods
                        (method "getPrivateExponent"
                              (parameters )
                              (returntype . (class "java.math.BigInteger"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces "java.security.PrivateKey" "java.security.interfaces.RSAKey")
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")))))


(defconst *RSAPrivateKey-class-table*
  (make-static-class-decls 
   *java.security.interfaces.RSAPrivateKey*))

(defconst *package-name-map* 
  ("java.security.interfaces.RSAPrivateKey" . "java.security.interfaces"))

