; RenderableImage-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:29 CDT 2014.
;

(defconst *java.awt.image.renderable.RenderableImage*
 (make-class-def
      '(class "java.awt.image.renderable.RenderableImage"
            "java.lang.Object"
            (constant_pool
                        (STRING  "HINTS_OBSERVED"))
            (fields
                        (field "HINTS_OBSERVED" (class "java.lang.String") (accessflags  *class*  *final*  *public*  *static* ) 0))
            (methods
                        (method "getSources"
                              (parameters )
                              (returntype . (class "java.util.Vector"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getProperty"
                              (parameters (class "java.lang.String"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getPropertyNames"
                              (parameters )
                              (returntype . (array (class "java.lang.String")))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "isDynamic"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getWidth"
                              (parameters )
                              (returntype . float)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getHeight"
                              (parameters )
                              (returntype . float)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getMinX"
                              (parameters )
                              (returntype . float)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getMinY"
                              (parameters )
                              (returntype . float)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "createScaledRendering"
                              (parameters int int (class "java.awt.RenderingHints"))
                              (returntype . (class "java.awt.image.RenderedImage"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "createDefaultRendering"
                              (parameters )
                              (returntype . (class "java.awt.image.RenderedImage"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "createRendering"
                              (parameters (class "java.awt.image.renderable.RenderContext"))
                              (returntype . (class "java.awt.image.RenderedImage"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces)
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")))))


(defconst *RenderableImage-class-table*
  (make-static-class-decls 
   *java.awt.image.renderable.RenderableImage*))

(defconst *package-name-map* 
  ("java.awt.image.renderable.RenderableImage" . "java.awt.image.renderable"))

