; DragSourceListener-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:26 CDT 2014.
;

(defconst *java.awt.dnd.DragSourceListener*
 (make-class-def
      '(class "java.awt.dnd.DragSourceListener"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "dragEnter"
                              (parameters (class "java.awt.dnd.DragSourceDragEvent"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "dragOver"
                              (parameters (class "java.awt.dnd.DragSourceDragEvent"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "dropActionChanged"
                              (parameters (class "java.awt.dnd.DragSourceDragEvent"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "dragExit"
                              (parameters (class "java.awt.dnd.DragSourceEvent"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "dragDropEnd"
                              (parameters (class "java.awt.dnd.DragSourceDropEvent"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces "java.util.EventListener")
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")))))


(defconst *DragSourceListener-class-table*
  (make-static-class-decls 
   *java.awt.dnd.DragSourceListener*))

(defconst *package-name-map* 
  ("java.awt.dnd.DragSourceListener" . "java.awt.dnd"))

