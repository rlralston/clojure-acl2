; TrayIconPeer-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:30 CDT 2014.
;

(defconst *java.awt.peer.TrayIconPeer*
 (make-class-def
      '(class "java.awt.peer.TrayIconPeer"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "dispose"
                              (parameters )
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "setToolTip"
                              (parameters (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "updateImage"
                              (parameters )
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "displayMessage"
                              (parameters (class "java.lang.String") (class "java.lang.String") (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "showPopupMenu"
                              (parameters int int)
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces)
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")))))


(defconst *TrayIconPeer-class-table*
  (make-static-class-decls 
   *java.awt.peer.TrayIconPeer*))

(defconst *package-name-map* 
  ("java.awt.peer.TrayIconPeer" . "java.awt.peer"))

