; BreakIterator$BreakIteratorCache-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:41 CDT 2014.
;

(defconst *java.text.BreakIterator$BreakIteratorCache*
 (make-class-def
      '(class "java.text.BreakIterator$BreakIteratorCache"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "iter" (class "java.text.BreakIterator") (accessflags  *class*  *private* ) -1)
                        (field "locale" (class "java.util.Locale") (accessflags  *class*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.Locale") (class "java.text.BreakIterator"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 21)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "locale" "java.text.BreakIterator$BreakIteratorCache" (class "java.util.Locale"))))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (invokevirtual
					(methodCP "clone" "java.text.BreakIterator" () (class "java.lang.Object"))))
                                      (14 (checkcast (class "java.text.BreakIterator")))
                                      (17 (putfield (fieldCP "iter" "java.text.BreakIterator$BreakIteratorCache" (class "java.text.BreakIterator"))))
                                      (20 (return))
                                      (endofcode 21))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getLocale"
                              (parameters )
                              (returntype . (class "java.util.Locale"))
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "locale" "java.text.BreakIterator$BreakIteratorCache" (class "java.util.Locale"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "createBreakInstance"
                              (parameters )
                              (returntype . (class "java.text.BreakIterator"))
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "iter" "java.text.BreakIterator$BreakIteratorCache" (class "java.text.BreakIterator"))))
                                      (4 (invokevirtual
					(methodCP "clone" "java.text.BreakIterator" () (class "java.lang.Object"))))
                                      (7 (checkcast (class "java.text.BreakIterator")))
                                      (10 (areturn))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *BreakIterator$BreakIteratorCache-class-table*
  (make-static-class-decls 
   *java.text.BreakIterator$BreakIteratorCache*))

(defconst *package-name-map* 
  ("java.text.BreakIterator$BreakIteratorCache" . "java.text"))

