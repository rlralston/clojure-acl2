; AppletStub-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:23 CDT 2014.
;

(defconst *java.applet.AppletStub*
 (make-class-def
      '(class "java.applet.AppletStub"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "isActive"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getDocumentBase"
                              (parameters )
                              (returntype . (class "java.net.URL"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getCodeBase"
                              (parameters )
                              (returntype . (class "java.net.URL"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getParameter"
                              (parameters (class "java.lang.String"))
                              (returntype . (class "java.lang.String"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getAppletContext"
                              (parameters )
                              (returntype . (class "java.applet.AppletContext"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "appletResize"
                              (parameters int int)
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces)
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")))))


(defconst *AppletStub-class-table*
  (make-static-class-decls 
   *java.applet.AppletStub*))

(defconst *package-name-map* 
  ("java.applet.AppletStub" . "java.applet"))
