; DontCareFieldPosition-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:41 CDT 2014.
;

(defconst *java.text.DontCareFieldPosition*
 (make-class-def
      '(class "java.text.DontCareFieldPosition"
            "java.text.FieldPosition"
            (constant_pool)
            (fields
                        (field "INSTANCE" (class "java.text.FieldPosition") (accessflags  *class*  *final*  *static* ) -1)
                        (field "noDelegate" (class "java.text.Format$FieldDelegate") (accessflags  *class*  *final*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 4) (max_locals . 1) (code_length . 18)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (iconst_0))
                                      (2 (invokespecial
					(methodCP "<init>" "java.text.FieldPosition" (int) void)))
                                      (5 (aload_0))
                                      (6 (new (class "java.text.DontCareFieldPosition$1")))
                                      (9 (dup))
                                      (10 (aload_0))
                                      (11 (invokespecial
					(methodCP "<init>" "java.text.DontCareFieldPosition$1" ((class "java.text.DontCareFieldPosition")) void)))
                                      (14 (putfield (fieldCP "noDelegate" "java.text.DontCareFieldPosition" (class "java.text.Format$FieldDelegate"))))
                                      (17 (return))
                                      (endofcode 18))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getFieldDelegate"
                              (parameters )
                              (returntype . (class "java.text.Format$FieldDelegate"))
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "noDelegate" "java.text.DontCareFieldPosition" (class "java.text.Format$FieldDelegate"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 11)
                                   (parsedcode
                                      (0 (new (class "java.text.DontCareFieldPosition")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.text.DontCareFieldPosition" () void)))
                                      (7 (putstatic (fieldCP "INSTANCE" "java.text.DontCareFieldPosition" (class "java.text.FieldPosition"))))
                                      (10 (return))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *DontCareFieldPosition-class-table*
  (make-static-class-decls 
   *java.text.DontCareFieldPosition*))

(defconst *package-name-map* 
  ("java.text.DontCareFieldPosition" . "java.text"))

