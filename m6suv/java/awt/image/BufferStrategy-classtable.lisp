; BufferStrategy-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:28 CDT 2014.
;

(defconst *java.awt.image.BufferStrategy*
 (make-class-def
      '(class "java.awt.image.BufferStrategy"
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
                        (method "getCapabilities"
                              (parameters )
                              (returntype . (class "java.awt.BufferCapabilities"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getDrawGraphics"
                              (parameters )
                              (returntype . (class "java.awt.Graphics"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "contentsLost"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "contentsRestored"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "show"
                              (parameters )
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "dispose"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 0) (max_locals . 1) (code_length . 1)
                                   (parsedcode
                                      (0 (return))
                                      (endofcode 1))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *abstract*  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *BufferStrategy-class-table*
  (make-static-class-decls 
   *java.awt.image.BufferStrategy*))

(defconst *package-name-map* 
  ("java.awt.image.BufferStrategy" . "java.awt.image"))
