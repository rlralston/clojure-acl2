; SynchronousQueue$Transferer-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:44 CDT 2014.
;

(defconst *java.util.concurrent.SynchronousQueue$Transferer*
 (make-class-def
      '(class "java.util.concurrent.SynchronousQueue$Transferer"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class* )
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
                        (method "transfer"
                              (parameters (class "java.lang.Object") boolean long)
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *abstract*  *class* )
                              (code)))
            (interfaces)
            (accessflags  *abstract*  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *SynchronousQueue$Transferer-class-table*
  (make-static-class-decls 
   *java.util.concurrent.SynchronousQueue$Transferer*))

(defconst *package-name-map* 
  ("java.util.concurrent.SynchronousQueue$Transferer" . "java.util.concurrent"))

