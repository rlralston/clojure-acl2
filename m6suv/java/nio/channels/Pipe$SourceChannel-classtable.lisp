; Pipe$SourceChannel-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:38 CDT 2014.
;

(defconst *java.nio.channels.Pipe$SourceChannel*
 (make-class-def
      '(class "java.nio.channels.Pipe$SourceChannel"
            "java.nio.channels.spi.AbstractSelectableChannel"
            (constant_pool)
            (fields)
            (methods
                        (method "<init>"
                              (parameters (class "java.nio.channels.spi.SelectorProvider"))
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokespecial
					(methodCP "<init>" "java.nio.channels.spi.AbstractSelectableChannel" ((class "java.nio.channels.spi.SelectorProvider")) void)))
                                      (5 (return))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "validOps"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 2)
                                   (parsedcode
                                      (0 (iconst_1))
                                      (1 (ireturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.nio.channels.ReadableByteChannel" "java.nio.channels.ScatteringByteChannel")
            (accessflags  *abstract*  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *Pipe$SourceChannel-class-table*
  (make-static-class-decls 
   *java.nio.channels.Pipe$SourceChannel*))

(defconst *package-name-map* 
  ("java.nio.channels.Pipe$SourceChannel" . "java.nio.channels"))
