; PropertyEditor-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:31 CDT 2014.
;

(defconst *java.beans.PropertyEditor*
 (make-class-def
      '(class "java.beans.PropertyEditor"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "setValue"
                              (parameters (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getValue"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "isPaintable"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "paintValue"
                              (parameters (class "java.awt.Graphics") (class "java.awt.Rectangle"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getJavaInitializationString"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getAsText"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "setAsText"
                              (parameters (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getTags"
                              (parameters )
                              (returntype . (array (class "java.lang.String")))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getCustomEditor"
                              (parameters )
                              (returntype . (class "java.awt.Component"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "supportsCustomEditor"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "addPropertyChangeListener"
                              (parameters (class "java.beans.PropertyChangeListener"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "removePropertyChangeListener"
                              (parameters (class "java.beans.PropertyChangeListener"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces)
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")))))


(defconst *PropertyEditor-class-table*
  (make-static-class-decls 
   *java.beans.PropertyEditor*))

(defconst *package-name-map* 
  ("java.beans.PropertyEditor" . "java.beans"))
