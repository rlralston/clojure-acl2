; PropertyChangeListener-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:31 CDT 2014.
;

(defconst *java.beans.PropertyChangeListener*
 (make-class-def
      '(class "java.beans.PropertyChangeListener"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "propertyChange"
                              (parameters (class "java.beans.PropertyChangeEvent"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces "java.util.EventListener")
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")))))


(defconst *PropertyChangeListener-class-table*
  (make-static-class-decls 
   *java.beans.PropertyChangeListener*))

(defconst *package-name-map* 
  ("java.beans.PropertyChangeListener" . "java.beans"))

