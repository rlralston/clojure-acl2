; ScrollbarPeer-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:30 CDT 2014.
;

(defconst *java.awt.peer.ScrollbarPeer*
 (make-class-def
      '(class "java.awt.peer.ScrollbarPeer"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "setValues"
                              (parameters int int int int)
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "setLineIncrement"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "setPageIncrement"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces "java.awt.peer.ComponentPeer")
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")))))


(defconst *ScrollbarPeer-class-table*
  (make-static-class-decls 
   *java.awt.peer.ScrollbarPeer*))

(defconst *package-name-map* 
  ("java.awt.peer.ScrollbarPeer" . "java.awt.peer"))

