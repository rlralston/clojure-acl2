; ItemListener-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:26 CDT 2014.
;

(defconst *java.awt.event.ItemListener*
 (make-class-def
      '(class "java.awt.event.ItemListener"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "itemStateChanged"
                              (parameters (class "java.awt.event.ItemEvent"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces "java.util.EventListener")
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")))))


(defconst *ItemListener-class-table*
  (make-static-class-decls 
   *java.awt.event.ItemListener*))

(defconst *package-name-map* 
  ("java.awt.event.ItemListener" . "java.awt.event"))

