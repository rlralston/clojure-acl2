; TextLine$2-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:26 CDT 2014.
;

(defconst *java.awt.font.TextLine$2*
 (make-class-def
      '(class "java.awt.font.TextLine$2"
            "java.awt.font.TextLine$Function"
            (constant_pool)
            (fields)
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aconst_null))
                                      (2 (invokespecial
					(methodCP "<init>" "java.awt.font.TextLine$Function" ((class "java.awt.font.TextLine$1")) void)))
                                      (5 (return))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "computeFunction"
                              (parameters (class "java.awt.font.TextLine") int int)
                              (returntype . float)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 5) (code_length . 17)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (invokestatic
					(methodCP "access$100" "java.awt.font.TextLine" ((class "java.awt.font.TextLine")) (array (class "sun.font.TextLineComponent")))))
                                      (4 (iload_2))
                                      (5 (aaload))
                                      (6 (astore 4))
                                      (8 (aload 4))
                                      (10 (iload_3))
                                      (11 (invokeinterface
					(methodCP "getCharAdvance" "sun.font.TextLineComponent" (int) float) 2))
                                      (16 (freturn))
                                      (endofcode 17))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "InnerClasses")))))


(defconst *TextLine$2-class-table*
  (make-static-class-decls 
   *java.awt.font.TextLine$2*))

(defconst *package-name-map* 
  ("java.awt.font.TextLine$2" . "java.awt.font"))
