; LayoutPath-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:26 CDT 2014.
;

(defconst *java.awt.font.LayoutPath*
 (make-class-def
      '(class "java.awt.font.LayoutPath"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "pointToPath"
                              (parameters (class "java.awt.geom.Point2D") (class "java.awt.geom.Point2D"))
                              (returntype . boolean)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "pathToPoint"
                              (parameters (class "java.awt.geom.Point2D") boolean (class "java.awt.geom.Point2D"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces)
            (accessflags  *abstract*  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *LayoutPath-class-table*
  (make-static-class-decls 
   *java.awt.font.LayoutPath*))

(defconst *package-name-map* 
  ("java.awt.font.LayoutPath" . "java.awt.font"))
