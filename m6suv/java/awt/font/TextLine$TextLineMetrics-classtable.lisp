; TextLine$TextLineMetrics-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:26 CDT 2014.
;

(defconst *java.awt.font.TextLine$TextLineMetrics*
 (make-class-def
      '(class "java.awt.font.TextLine$TextLineMetrics"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "ascent" float (accessflags  *class*  *final*  *public* ) -1)
                        (field "descent" float (accessflags  *class*  *final*  *public* ) -1)
                        (field "leading" float (accessflags  *class*  *final*  *public* ) -1)
                        (field "advance" float (accessflags  *class*  *final*  *public* ) -1))
            (methods
                        (method "<init>"
                              (parameters float float float float)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 5) (code_length . 26)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (fload_1))
                                      (6 (putfield (fieldCP "ascent" "java.awt.font.TextLine$TextLineMetrics" float)))
                                      (9 (aload_0))
                                      (10 (fload_2))
                                      (11 (putfield (fieldCP "descent" "java.awt.font.TextLine$TextLineMetrics" float)))
                                      (14 (aload_0))
                                      (15 (fload_3))
                                      (16 (putfield (fieldCP "leading" "java.awt.font.TextLine$TextLineMetrics" float)))
                                      (19 (aload_0))
                                      (20 (fload 4))
                                      (22 (putfield (fieldCP "advance" "java.awt.font.TextLine$TextLineMetrics" float)))
                                      (25 (return))
                                      (endofcode 26))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *TextLine$TextLineMetrics-class-table*
  (make-static-class-decls 
   *java.awt.font.TextLine$TextLineMetrics*))

(defconst *package-name-map* 
  ("java.awt.font.TextLine$TextLineMetrics" . "java.awt.font"))
