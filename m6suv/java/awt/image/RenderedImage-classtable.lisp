; RenderedImage-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:29 CDT 2014.
;

(defconst *java.awt.image.RenderedImage*
 (make-class-def
      '(class "java.awt.image.RenderedImage"
            "java.lang.Object"
            (constant_pool)
            (fields)
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
                        (method "getColorModel"
                              (parameters )
                              (returntype . (class "java.awt.image.ColorModel"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getSampleModel"
                              (parameters )
                              (returntype . (class "java.awt.image.SampleModel"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getWidth"
                              (parameters )
                              (returntype . int)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getHeight"
                              (parameters )
                              (returntype . int)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getMinX"
                              (parameters )
                              (returntype . int)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getMinY"
                              (parameters )
                              (returntype . int)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getNumXTiles"
                              (parameters )
                              (returntype . int)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getNumYTiles"
                              (parameters )
                              (returntype . int)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getMinTileX"
                              (parameters )
                              (returntype . int)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getMinTileY"
                              (parameters )
                              (returntype . int)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getTileWidth"
                              (parameters )
                              (returntype . int)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getTileHeight"
                              (parameters )
                              (returntype . int)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getTileGridXOffset"
                              (parameters )
                              (returntype . int)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getTileGridYOffset"
                              (parameters )
                              (returntype . int)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getTile"
                              (parameters int int)
                              (returntype . (class "java.awt.image.Raster"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getData"
                              (parameters )
                              (returntype . (class "java.awt.image.Raster"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getData"
                              (parameters (class "java.awt.Rectangle"))
                              (returntype . (class "java.awt.image.Raster"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "copyData"
                              (parameters (class "java.awt.image.WritableRaster"))
                              (returntype . (class "java.awt.image.WritableRaster"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces)
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")))))


(defconst *RenderedImage-class-table*
  (make-static-class-decls 
   *java.awt.image.RenderedImage*))

(defconst *package-name-map* 
  ("java.awt.image.RenderedImage" . "java.awt.image"))

