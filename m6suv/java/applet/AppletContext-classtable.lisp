; AppletContext-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:23 CDT 2014.
;

(defconst *java.applet.AppletContext*
 (make-class-def
      '(class "java.applet.AppletContext"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "getAudioClip"
                              (parameters (class "java.net.URL"))
                              (returntype . (class "java.applet.AudioClip"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getImage"
                              (parameters (class "java.net.URL"))
                              (returntype . (class "java.awt.Image"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getApplet"
                              (parameters (class "java.lang.String"))
                              (returntype . (class "java.applet.Applet"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getApplets"
                              (parameters )
                              (returntype . (class "java.util.Enumeration"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "showDocument"
                              (parameters (class "java.net.URL"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "showDocument"
                              (parameters (class "java.net.URL") (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "showStatus"
                              (parameters (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "setStream"
                              (parameters (class "java.lang.String") (class "java.io.InputStream"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getStream"
                              (parameters (class "java.lang.String"))
                              (returntype . (class "java.io.InputStream"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getStreamKeys"
                              (parameters )
                              (returntype . (class "java.util.Iterator"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces)
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")))))


(defconst *AppletContext-class-table*
  (make-static-class-decls 
   *java.applet.AppletContext*))

(defconst *package-name-map* 
  ("java.applet.AppletContext" . "java.applet"))

