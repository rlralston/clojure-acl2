; ContainerPeer-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:30 CDT 2014.
;

(defconst *java.awt.peer.ContainerPeer*
 (make-class-def
      '(class "java.awt.peer.ContainerPeer"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "getInsets"
                              (parameters )
                              (returntype . (class "java.awt.Insets"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "beginValidate"
                              (parameters )
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "endValidate"
                              (parameters )
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "beginLayout"
                              (parameters )
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "endLayout"
                              (parameters )
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces "java.awt.peer.ComponentPeer")
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")))))


(defconst *ContainerPeer-class-table*
  (make-static-class-decls 
   *java.awt.peer.ContainerPeer*))

(defconst *package-name-map* 
  ("java.awt.peer.ContainerPeer" . "java.awt.peer"))

