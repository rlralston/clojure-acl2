; ParameterizedType-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:36 CDT 2014.
;

(defconst *java.lang.reflect.ParameterizedType*
 (make-class-def
      '(class "java.lang.reflect.ParameterizedType"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "getActualTypeArguments"
                              (parameters )
                              (returntype . (array (class "java.lang.reflect.Type")))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getRawType"
                              (parameters )
                              (returntype . (class "java.lang.reflect.Type"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getOwnerType"
                              (parameters )
                              (returntype . (class "java.lang.reflect.Type"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces "java.lang.reflect.Type")
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")))))


(defconst *ParameterizedType-class-table*
  (make-static-class-decls 
   *java.lang.reflect.ParameterizedType*))

(defconst *package-name-map* 
  ("java.lang.reflect.ParameterizedType" . "java.lang.reflect"))

