; CookieStore-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:37 CDT 2014.
;

(defconst *java.net.CookieStore*
 (make-class-def
      '(class "java.net.CookieStore"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "add"
                              (parameters (class "java.net.URI") (class "java.net.HttpCookie"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "get"
                              (parameters (class "java.net.URI"))
                              (returntype . (class "java.util.List"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getCookies"
                              (parameters )
                              (returntype . (class "java.util.List"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getURIs"
                              (parameters )
                              (returntype . (class "java.util.List"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "remove"
                              (parameters (class "java.net.URI") (class "java.net.HttpCookie"))
                              (returntype . boolean)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "removeAll"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces)
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")))))


(defconst *CookieStore-class-table*
  (make-static-class-decls 
   *java.net.CookieStore*))

(defconst *package-name-map* 
  ("java.net.CookieStore" . "java.net"))
