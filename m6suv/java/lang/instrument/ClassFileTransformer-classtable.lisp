; ClassFileTransformer-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:33 CDT 2014.
;

(defconst *java.lang.instrument.ClassFileTransformer*
 (make-class-def
      '(class "java.lang.instrument.ClassFileTransformer"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "transform"
                              (parameters (class "java.lang.ClassLoader") (class "java.lang.String") (class "java.lang.Class") (class "java.security.ProtectionDomain") (array byte))
                              (returntype . (array byte))
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces)
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")))))


(defconst *ClassFileTransformer-class-table*
  (make-static-class-decls 
   *java.lang.instrument.ClassFileTransformer*))

(defconst *package-name-map* 
  ("java.lang.instrument.ClassFileTransformer" . "java.lang.instrument"))

