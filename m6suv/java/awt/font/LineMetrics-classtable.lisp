; LineMetrics-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:26 CDT 2014.
;

(defconst *java.awt.font.LineMetrics*
 (make-class-def
      '(class "java.awt.font.LineMetrics"
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
                        (method "getNumChars"
                              (parameters )
                              (returntype . int)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getAscent"
                              (parameters )
                              (returntype . float)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getDescent"
                              (parameters )
                              (returntype . float)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getLeading"
                              (parameters )
                              (returntype . float)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getHeight"
                              (parameters )
                              (returntype . float)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getBaselineIndex"
                              (parameters )
                              (returntype . int)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getBaselineOffsets"
                              (parameters )
                              (returntype . (array float))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getStrikethroughOffset"
                              (parameters )
                              (returntype . float)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getStrikethroughThickness"
                              (parameters )
                              (returntype . float)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getUnderlineOffset"
                              (parameters )
                              (returntype . float)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getUnderlineThickness"
                              (parameters )
                              (returntype . float)
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces)
            (accessflags  *abstract*  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *LineMetrics-class-table*
  (make-static-class-decls 
   *java.awt.font.LineMetrics*))

(defconst *package-name-map* 
  ("java.awt.font.LineMetrics" . "java.awt.font"))

