; DontCareFieldPosition$1-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:41 CDT 2014.
;

(defconst *java.text.DontCareFieldPosition$1*
 (make-class-def
      '(class "java.text.DontCareFieldPosition$1"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "this$0" (class "java.text.DontCareFieldPosition") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.text.DontCareFieldPosition"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.text.DontCareFieldPosition$1" (class "java.text.DontCareFieldPosition"))))
                                      (5 (aload_0))
                                      (6 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "formatted"
                              (parameters (class "java.text.Format$Field") (class "java.lang.Object") int int (class "java.lang.StringBuffer"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 0) (max_locals . 6) (code_length . 1)
                                   (parsedcode
                                      (0 (return))
                                      (endofcode 1))
                                   (Exceptions )
                                   (StackMap )))
                        (method "formatted"
                              (parameters int (class "java.text.Format$Field") (class "java.lang.Object") int int (class "java.lang.StringBuffer"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 0) (max_locals . 7) (code_length . 1)
                                   (parsedcode
                                      (0 (return))
                                      (endofcode 1))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.text.Format$FieldDelegate")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "InnerClasses")))))


(defconst *DontCareFieldPosition$1-class-table*
  (make-static-class-decls 
   *java.text.DontCareFieldPosition$1*))

(defconst *package-name-map* 
  ("java.text.DontCareFieldPosition$1" . "java.text"))
