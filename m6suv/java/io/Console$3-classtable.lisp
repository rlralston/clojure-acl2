; Console$3-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:31 CDT 2014.
;

(defconst *java.io.Console$3*
 (make-class-def
      '(class "java.io.Console$3"
            "java.io.PrintWriter"
            (constant_pool)
            (fields
                        (field "this$0" (class "java.io.Console") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.io.Console") (class "java.io.Writer") boolean)
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 3) (max_locals . 4) (code_length . 12)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.io.Console$3" (class "java.io.Console"))))
                                      (5 (aload_0))
                                      (6 (aload_2))
                                      (7 (iload_3))
                                      (8 (invokespecial
					(methodCP "<init>" "java.io.PrintWriter" ((class "java.io.Writer") boolean) void)))
                                      (11 (return))
                                      (endofcode 12))
                                   (Exceptions )
                                   (StackMap )))
                        (method "close"
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
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "InnerClasses")))))


(defconst *Console$3-class-table*
  (make-static-class-decls 
   *java.io.Console$3*))

(defconst *package-name-map* 
  ("java.io.Console$3" . "java.io"))

