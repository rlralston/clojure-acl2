; TextLine$1-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:26 CDT 2014.
;

(defconst *java.awt.font.TextLine$1*
 (make-class-def
      '(class "java.awt.font.TextLine$1"
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
                                   (max_stack . 3) (max_locals . 6) (code_length . 43)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (invokestatic
					(methodCP "access$100" "java.awt.font.TextLine" ((class "java.awt.font.TextLine")) (array (class "sun.font.TextLineComponent")))))
                                      (4 (iload_2))
                                      (5 (aaload))
                                      (6 (astore 4))
                                      (8 (aload_1))
                                      (9 (iload_2))
                                      (10 (invokestatic
					(methodCP "access$200" "java.awt.font.TextLine" ((class "java.awt.font.TextLine") int) int)))
                                      (13 (istore 5))
                                      (15 (aload_1))
                                      (16 (invokestatic
					(methodCP "access$300" "java.awt.font.TextLine" ((class "java.awt.font.TextLine")) (array float))))
                                      (19 (iload 5))
                                      (21 (iconst_2))
                                      (22 (imul))
                                      (23 (faload))
                                      (24 (aload 4))
                                      (26 (iload_3))
                                      (27 (invokeinterface
					(methodCP "getCharX" "sun.font.TextLineComponent" (int) float) 2))
                                      (32 (fadd))
                                      (33 (aload 4))
                                      (35 (iload_3))
                                      (36 (invokeinterface
					(methodCP "getCharAdvance" "sun.font.TextLineComponent" (int) float) 2))
                                      (41 (fadd))
                                      (42 (freturn))
                                      (endofcode 43))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "InnerClasses")))))


(defconst *TextLine$1-class-table*
  (make-static-class-decls 
   *java.awt.font.TextLine$1*))

(defconst *package-name-map* 
  ("java.awt.font.TextLine$1" . "java.awt.font"))
