; ScrollPanePeer-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:30 CDT 2014.
;

(defconst *java.awt.peer.ScrollPanePeer*
 (make-class-def
      '(class "java.awt.peer.ScrollPanePeer"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "getHScrollbarHeight"
                              (parameters )
                              (returntype . int)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getVScrollbarWidth"
                              (parameters )
                              (returntype . int)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "setScrollPosition"
                              (parameters int int)
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "childResized"
                              (parameters int int)
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "setUnitIncrement"
                              (parameters (class "java.awt.Adjustable") int)
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "setValue"
                              (parameters (class "java.awt.Adjustable") int)
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces "java.awt.peer.ContainerPeer")
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")))))


(defconst *ScrollPanePeer-class-table*
  (make-static-class-decls 
   *java.awt.peer.ScrollPanePeer*))

(defconst *package-name-map* 
  ("java.awt.peer.ScrollPanePeer" . "java.awt.peer"))

